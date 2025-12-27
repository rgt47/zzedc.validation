#' SQL Code Generator for DSL Batch Validation
#'
#' Converts parsed DSL rules into SQL queries for batch validation.
#' Used for nightly QC checks across full dataset with cross-visit and cross-patient analysis.
#'
#' @keywords internal
#' @noRd

# ============================================================================
# SQL Code Generation from AST
# ============================================================================

#' Generate SQL query from DSL AST
#'
#' Converts a parsed DSL rule AST into a SQL query for batch validation.
#' The query returns rows that violate the validation rule.
#'
#' @param ast Abstract Syntax Tree from DSL parser
#' @param field_name Name of field being validated
#' @param table_name Name of data table (default: "edc_data")
#' @param context Validation context ("batch" for cross-visit/patient analysis)
#'
#' @return SQL query string that returns violation rows
#'
#' @keywords internal
generate_sql_from_ast <- function(ast, field_name, table_name = "edc_data", context = "batch") {

  # Generate the WHERE clause that identifies violations
  where_clause <- generate_sql_where_clause(ast, field_name)

  # Simple batch query - returns all rows violating the rule
  sql_query <- paste0(
    "SELECT subject_id, visit, ", field_name, ", entry_date, entry_user ",
    "FROM ", table_name, " ",
    "WHERE ", where_clause
  )

  return(sql_query)
}

#' Generate WHERE clause from AST
#'
#' Converts AST into a SQL WHERE clause that identifies violations.
#'
#' @param ast Abstract Syntax Tree
#' @param field_name Field name for simple comparisons
#'
#' @return SQL WHERE clause (without "WHERE" keyword)
#'
#' @keywords internal
generate_sql_where_clause <- function(ast, field_name = NULL) {

  if (is.null(ast)) {
    return("1 = 1")  # No validation
  }

  node_type <- ast$type

  # Route to appropriate SQL generator based on node type
  switch(node_type,
    "between" = generate_sql_between(ast, field_name),
    "comparison" = generate_sql_comparison(ast, field_name),
    "in_list" = generate_sql_in_list(ast, field_name),
    "not_in_list" = generate_sql_not_in_list(ast, field_name),
    "required" = generate_sql_required(ast, field_name),
    "and" = generate_sql_and(ast, field_name),
    "or" = generate_sql_or(ast, field_name),
    "if" = generate_sql_if(ast, field_name),
    "within_days_of" = generate_sql_within_days(ast, field_name),
    "date_comparison" = generate_sql_date_comparison(ast, field_name),
    # Default: violations detected if field is not null/empty (conservative)
    paste0(field_name, " IS NOT NULL AND ", field_name, " != ''")
  )
}

# ============================================================================
# SQL Generators for Individual Node Types
# ============================================================================

#' Generate SQL for between validation
#'
#' @keywords internal
generate_sql_between <- function(ast, field_name) {
  # Violation: value < min OR value > max
  field <- ast$field %||% field_name
  min_val <- ast$min
  max_val <- ast$max

  paste0(
    "NOT (CAST(", field, " AS REAL) >= ", min_val, " AND ",
    "CAST(", field, " AS REAL) <= ", max_val, ")"
  )
}

#' Generate SQL for comparison validation
#'
#' @keywords internal
generate_sql_comparison <- function(ast, field_name) {
  field <- ast$field %||% field_name
  op <- ast$operator
  value <- ast$value

  # Invert comparison to get violations
  inverted_op <- switch(op,
    "<" = ">=",
    "<=" = ">",
    ">" = "<=",
    ">=" = "<",
    "==" = "!=",
    "!=" = "==",
    op  # Unknown operator, keep as-is
  )

  # Handle string vs numeric values
  if (is.character(value)) {
    paste0("CAST(", field, " AS TEXT) ", inverted_op, " '", value, "'")
  } else {
    paste0("CAST(", field, " AS REAL) ", inverted_op, " ", value)
  }
}

#' Generate SQL for in list validation
#'
#' @keywords internal
generate_sql_in_list <- function(ast, field_name) {
  field <- ast$field %||% field_name
  values <- ast$values

  # Create SQL value list
  value_list <- paste0(
    "'", paste(values, collapse = "', '"), "'",
    collapse = ""
  )

  # Violation: value NOT in list
  paste0(field, " NOT IN (", value_list, ")")
}

#' Generate SQL for not in list validation
#'
#' @keywords internal
generate_sql_not_in_list <- function(ast, field_name) {
  field <- ast$field %||% field_name
  values <- ast$values

  # Create SQL value list
  value_list <- paste0(
    "'", paste(values, collapse = "', '"), "'",
    collapse = ""
  )

  # Violation: value IN list
  paste0(field, " IN (", value_list, ")")
}

#' Generate SQL for required validation
#'
#' @keywords internal
generate_sql_required <- function(ast, field_name) {
  field <- ast$field %||% field_name

  # Violation: field is NULL or empty string
  paste0("(", field, " IS NULL OR ", field, " = '')")
}

#' Generate SQL for AND logic
#'
#' @keywords internal
generate_sql_and <- function(ast, field_name) {
  left <- generate_sql_where_clause(ast$left, field_name)
  right <- generate_sql_where_clause(ast$right, field_name)

  # AND violation: either condition is violated (De Morgan's law)
  paste0("(", left, " OR ", right, ")")
}

#' Generate SQL for OR logic
#'
#' @keywords internal
generate_sql_or <- function(ast, field_name) {
  left <- generate_sql_where_clause(ast$left, field_name)
  right <- generate_sql_where_clause(ast$right, field_name)

  # OR violation: both conditions are violated (De Morgan's law)
  paste0("(", left, " AND ", right, ")")
}

#' Generate SQL for if/then/else logic
#'
#' @keywords internal
generate_sql_if <- function(ast, field_name) {
  condition <- generate_sql_where_clause(ast$condition, field_name)
  then_clause <- generate_sql_where_clause(ast$then_branch, field_name)
  else_clause <- if (!is.null(ast$else_branch)) {
    generate_sql_where_clause(ast$else_branch, field_name)
  } else {
    "0 = 1"  # No else, so always valid if condition false
  }

  # Violation: (condition TRUE AND then_violated) OR (condition FALSE AND else_violated)
  paste0(
    "(((", condition, ") AND (", then_clause, ")) OR ",
    "((NOT (", condition, ")) AND (", else_clause, ")))"
  )
}

#' Generate SQL for within N days validation
#'
#' @keywords internal
generate_sql_within_days <- function(ast, field_name) {
  check_field <- ast$check_field %||% field_name
  reference_field <- ast$reference_field
  days_tolerance <- ast$days %||% 30

  # Violation: difference > days_tolerance
  paste0(
    "ABS(CAST((julianday(", check_field, ") - julianday(", reference_field, ")) ",
    "AS INTEGER)) > ", days_tolerance
  )
}

#' Generate SQL for date comparison
#'
#' @keywords internal
generate_sql_date_comparison <- function(ast, field_name) {
  field <- ast$field %||% field_name
  op <- ast$operator
  reference <- ast$reference

  # Invert comparison for violations
  inverted_op <- switch(op,
    "<" = ">=",
    "<=" = ">",
    ">" = "<=",
    ">=" = "<",
    op
  )

  paste0(
    "DATE(", field, ") ", inverted_op, " DATE(", reference, ")"
  )
}

# ============================================================================
# Batch QC Query Builders
# ============================================================================

#' Generate cross-visit consistency query
#'
#' For rules like: "if visit == 'v2' then weight within 10% of {visit='v1'}.weight"
#' This generates SQL to check weight consistency across visits for each patient.
#'
#' @param subject_field Field containing subject ID (default: "subject_id")
#' @param field_name Field to validate
#' @param visit_comparisons List of visit pairs to compare (e.g., list(c("v1", "v2")))
#' @param tolerance Percentage tolerance (e.g., 10 for ±10%)
#' @param table_name Data table name (default: "edc_data")
#'
#' @return SQL query for cross-visit violations
#'
#' @keywords internal
generate_cross_visit_query <- function(field_name, visit_comparisons,
                                       tolerance = 10, subject_field = "subject_id",
                                       table_name = "edc_data") {

  # Build UNION of violation checks for each visit comparison
  union_queries <- list()

  for (visit_pair in visit_comparisons) {
    visit_ref <- visit_pair[1]
    visit_comp <- visit_pair[2]

    query <- paste0(
      "SELECT t2.", subject_field, ", t2.visit, t2.", field_name,
      " FROM ", table_name, " t1 ",
      "INNER JOIN ", table_name, " t2 ON t1.", subject_field, " = t2.", subject_field, " ",
      "WHERE t1.visit = '", visit_ref, "' AND t2.visit = '", visit_comp, "' ",
      "AND ABS(CAST(t2.", field_name, " AS REAL) - CAST(t1.", field_name, " AS REAL)) ",
      "> (CAST(t1.", field_name, " AS REAL) * ", tolerance, " / 100)"
    )

    union_queries[[length(union_queries) + 1]] <- query
  }

  paste(union_queries, collapse = " UNION ")
}

#' Generate cross-patient statistical outlier query
#'
#' Flags values that are outliers (e.g., > mean + 3*sd).
#' Useful for detecting data entry errors.
#'
#' @param field_name Field to analyze
#' @param num_std_dev Number of standard deviations for outlier threshold (default: 3)
#' @param table_name Data table name
#'
#' @return SQL query for outlier violations
#'
#' @keywords internal
generate_outlier_detection_query <- function(field_name, num_std_dev = 3,
                                             table_name = "edc_data") {

  paste0(
    "WITH stats AS (",
    "SELECT AVG(CAST(", field_name, " AS REAL)) as mean, ",
    "SQRT(AVG((CAST(", field_name, " AS REAL) - ",
    "(SELECT AVG(CAST(", field_name, " AS REAL)) FROM ", table_name, ")) * ",
    "(CAST(", field_name, " AS REAL) - ",
    "(SELECT AVG(CAST(", field_name, " AS REAL)) FROM ", table_name, "))) as stddev ",
    "FROM ", table_name, " ",
    "WHERE ", field_name, " IS NOT NULL ",
    ") ",
    "SELECT * FROM ", table_name, " d, stats s ",
    "WHERE ABS(CAST(d.", field_name, " AS REAL) - s.mean) > ", num_std_dev, " * s.stddev"
  )
}

#' Generate missing data pattern query
#'
#' Flags patients missing expected visits or required fields.
#' Example: "Flag if visit in ('v1','v2','v3') then visit_date required for each patient"
#'
#' @param subject_field Subject ID field name
#' @param visit_field Visit field name
#' @param required_field Field that should be present
#' @param required_visits Vector of visits where field is required
#' @param table_name Data table name
#'
#' @return SQL query for missing data violations
#'
#' @keywords internal
generate_missing_data_query <- function(subject_field = "subject_id",
                                        visit_field = "visit",
                                        required_field = NULL,
                                        required_visits = c("baseline", "week4", "week8"),
                                        table_name = "edc_data") {

  visit_list <- paste0("'", paste(required_visits, collapse = "', '"), "'")

  paste0(
    "SELECT ", subject_field, ", ", visit_field, ", ", required_field, " ",
    "FROM ", table_name, " ",
    "WHERE ", visit_field, " IN (", visit_list, ") ",
    "AND (", required_field, " IS NULL OR ", required_field, " = '')"
  )
}

# ============================================================================
# SQL Optimization Helpers
# ============================================================================

#' Add indexes to query for optimization
#'
#' Recommends indexes that would improve query performance.
#'
#' @param sql_query SQL query string
#' @param table_name Table being queried
#'
#' @return List of recommended index creation statements
#'
#' @keywords internal
recommend_indexes <- function(sql_query, table_name = "edc_data") {
  indexes <- list()

  # Check for common filtering patterns and recommend indexes
  if (grepl("WHERE subject_id", sql_query)) {
    indexes[[length(indexes) + 1]] <- paste0(
      "CREATE INDEX IF NOT EXISTS idx_", table_name, "_subject_id ",
      "ON ", table_name, " (subject_id)"
    )
  }

  if (grepl("WHERE visit", sql_query)) {
    indexes[[length(indexes) + 1]] <- paste0(
      "CREATE INDEX IF NOT EXISTS idx_", table_name, "_visit ",
      "ON ", table_name, " (visit)"
    )
  }

  if (grepl("WHERE.*entry_date", sql_query)) {
    indexes[[length(indexes) + 1]] <- paste0(
      "CREATE INDEX IF NOT EXISTS idx_", table_name, "_entry_date ",
      "ON ", table_name, " (entry_date)"
    )
  }

  # Composite index for common joins
  indexes[[length(indexes) + 1]] <- paste0(
    "CREATE INDEX IF NOT EXISTS idx_", table_name, "_subj_visit ",
    "ON ", table_name, " (subject_id, visit)"
  )

  return(indexes)
}

# ============================================================================
# Context Detection
# ============================================================================

#' Detect if DSL rule is for real-time or batch context
#'
#' Batch rules contain keywords like "cross_visit", "cross_patient", "each patient"
#' or reference other visit data with {visit='...'} syntax.
#'
#' @param rule_text Original DSL rule text
#'
#' @return "real-time" or "batch"
#'
#' @keywords internal
detect_validation_context <- function(rule_text) {
  batch_keywords <- c(
    "cross_visit", "cross_patient", "cross.patient",
    "each patient", "by visit", "window",
    "\\{visit=", "\\{subject=", "LAG\\(", "LEAD\\("
  )

  for (keyword in batch_keywords) {
    if (grepl(keyword, rule_text, ignore.case = TRUE)) {
      return("batch")
    }
  }

  return("real-time")
}

# ============================================================================
# SQL Query Execution Interface
# ============================================================================

#' Compile batch validation rule to SQL
#'
#' Full pipeline from DSL to executable SQL.
#'
#' @param rule_text DSL rule text
#' @param field_name Field being validated
#' @param table_name Data table name
#' @param parser DSLParser instance
#'
#' @return List with:
#'   - sql: Generated SQL query
#'   - context: "real-time" or "batch"
#'   - field: Field name
#'   - violations_expected: Expected violation count (for testing)
#'
#' @keywords internal
compile_batch_rule <- function(rule_text, field_name, table_name = "edc_data", parser = NULL) {

  # Detect context
  context <- detect_validation_context(rule_text)

  # Parse rule
  if (is.null(parser)) {
    source("R/validation_dsl_parser.R", local = TRUE)
    parser <- DSLParser$new()
  }

  ast <- parser$parse(rule_text)

  if (is.null(ast)) {
    return(list(
      sql = NULL,
      context = context,
      field = field_name,
      error = "Failed to parse rule"
    ))
  }

  # Generate SQL
  sql_query <- generate_sql_from_ast(ast, field_name, table_name, context)

  # Recommend indexes
  indexes <- recommend_indexes(sql_query, table_name)

  return(list(
    sql = sql_query,
    context = context,
    field = field_name,
    recommended_indexes = indexes,
    rule_text = rule_text
  ))
}
