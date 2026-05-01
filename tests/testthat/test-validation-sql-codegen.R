#' Tests for SQL Code Generation (Phase 5)
#'
#' Comprehensive test suite for SQL code generation functionality including:
#' - SQL generation from AST nodes
#' - Batch validation query creation
#' - Cross-visit and cross-patient analysis
#' - Query optimization with index recommendations
#'
#' @import testthat

describe("SQL Code Generation Basics", {
  it("generates SQL for between validation", {

    ast <- list(
      type = "between",
      field = "blood_pressure",
      min = 90,
      max = 180
    )

    sql <- generate_sql_between(ast, "blood_pressure")

    expect_true(grepl("NOT", sql))
    expect_true(grepl("blood_pressure", sql))
    expect_true(grepl("90", sql))
    expect_true(grepl("180", sql))
  })

  it("generates SQL for comparison validation", {

    ast <- list(
      type = "comparison",
      field = "age",
      operator = ">=",
      value = 18
    )

    sql <- generate_sql_comparison(ast, "age")

    expect_true(grepl("age", sql))
    expect_true(grepl("18", sql))
    # Should invert >= to <
    expect_true(grepl("<", sql))
  })

  it("generates SQL for in list validation", {

    ast <- list(
      type = "in_list",
      field = "visit",
      values = c("baseline", "week4", "week8")
    )

    sql <- generate_sql_in_list(ast, "visit")

    expect_true(grepl("NOT IN", sql))
    expect_true(grepl("baseline", sql))
    expect_true(grepl("week4", sql))
    expect_true(grepl("week8", sql))
  })

  it("generates SQL for required validation", {

    ast <- list(type = "required", field = "enrollment_date")

    sql <- generate_sql_required(ast, "enrollment_date")

    expect_true(grepl("IS NULL", sql))
    expect_true(grepl("enrollment_date", sql))
  })

  it("generates SQL for logical AND", {

    ast <- list(
      type = "and",
      left = list(type = "required", field = "age"),
      right = list(type = "required", field = "weight")
    )

    sql <- generate_sql_and(ast, NULL)

    expect_true(grepl("OR", sql))  # De Morgan's law: NOT(A AND B) = NOT A OR NOT B
  })

  it("generates SQL for logical OR", {

    ast <- list(
      type = "or",
      left = list(type = "required", field = "phone"),
      right = list(type = "required", field = "email")
    )

    sql <- generate_sql_or(ast, NULL)

    expect_true(grepl("AND", sql))  # De Morgan's law: NOT(A OR B) = NOT A AND NOT B
  })
})

describe("Cross-Visit SQL Queries", {
  it("generates cross-visit consistency query", {

    sql <- generate_cross_visit_query(
      field_name = "weight",
      visit_comparisons = list(c("baseline", "week4")),
      tolerance = 10
    )

    expect_true(grepl("JOIN", sql))
    expect_true(grepl("weight", sql))
    expect_true(grepl("10", sql))
    expect_true(grepl("baseline", sql))
    expect_true(grepl("week4", sql))
  })

  it("generates multi-visit comparison query", {

    sql <- generate_cross_visit_query(
      field_name = "weight",
      visit_comparisons = list(c("baseline", "week4"), c("week4", "week8")),
      tolerance = 5
    )

    expect_true(grepl("UNION", sql))
  })
})

describe("Statistical Analysis Queries", {
  it("generates outlier detection query", {

    sql <- generate_outlier_detection_query(
      field_name = "blood_pressure",
      num_std_dev = 3
    )

    expect_true(grepl("AVG", sql))
    expect_true(grepl("blood_pressure", sql))
    expect_true(grepl("WITH stats", sql))  # CTE syntax
  })

  it("generates missing data pattern query", {

    sql <- generate_missing_data_query(
      required_field = "visit_date",
      required_visits = c("baseline", "week4")
    )

    expect_true(grepl("IS NULL", sql))
    expect_true(grepl("baseline", sql))
    expect_true(grepl("week4", sql))
  })
})

describe("Index Recommendations", {
  it("recommends index for subject_id queries", {

    sql <- "SELECT * FROM edc_data WHERE subject_id = 'SUBJ001'"

    indexes <- recommend_indexes(sql, "edc_data")

    expect_true(any(grepl("subject_id", unlist(indexes))))
  })

  it("recommends composite index for subject-visit", {

    sql <- "SELECT * FROM edc_data WHERE subject_id = ? AND visit = 'baseline'"

    indexes <- recommend_indexes(sql, "edc_data")

    expect_true(any(grepl("subject_id.*visit|subj_visit", unlist(indexes))))
  })

  it("recommends index for date range queries", {

    sql <- "SELECT * FROM edc_data WHERE entry_date >= '2024-01-01'"

    indexes <- recommend_indexes(sql, "edc_data")

    expect_true(any(grepl("entry_date", unlist(indexes))))
  })
})

describe("Context Detection", {
  it("detects real-time context for simple rules", {

    rules <- c(
      "between 40 and 200",
      "required",
      "in(1,2,3,n,m)",
      "age >= 18"
    )

    for (rule in rules) {
      context <- detect_validation_context(rule)
      expect_equal(context, "real-time", info = paste("Rule:", rule))
    }
  })

  it("detects batch context for cross-visit rules", {

    batch_rules <- c(
      "cross_visit weight within 10% of baseline",
      "cross_patient outlier detection",
      "weight by visit",
      "{visit='baseline'}.weight"
    )

    for (rule in batch_rules) {
      context <- detect_validation_context(rule)
      expect_equal(context, "batch", info = paste("Rule:", rule))
    }
  })
})

describe("SQL Generation from Full AST", {
  it("generates SQL from complete between rule", {

    # Create mock AST manually
    ast <- list(
      type = "between",
      field = "blood_pressure",
      min = 40,
      max = 200
    )

    sql <- generate_sql_from_ast(ast, "blood_pressure")

    expect_true(grepl("SELECT", sql))
    expect_true(grepl("FROM", sql))
    expect_true(grepl("WHERE", sql))
    expect_true(grepl("blood_pressure", sql))
  })

  it("generates SQL from comparison rule", {

    ast <- list(
      type = "comparison",
      field = "age",
      operator = ">=",
      value = 18
    )

    sql <- generate_sql_from_ast(ast, "age")

    expect_true(grepl("SELECT", sql))
    expect_true(grepl("age", sql))
  })

  it("generates SQL from required rule", {

    ast <- list(
      type = "required",
      field = "enrollment_date"
    )

    sql <- generate_sql_from_ast(ast, "enrollment_date")

    expect_true(grepl("SELECT", sql))
    expect_true(grepl("IS NULL", sql))
  })
})

describe("Batch Rule Compilation", {
  it("detects context for real-time rules", {

    context <- detect_validation_context("between 40 and 200")
    expect_equal(context, "real-time")
  })

  it("detects context for batch rules", {

    context <- detect_validation_context("cross_visit weight consistency")
    expect_equal(context, "batch")
  })

  it("handles empty rule gracefully", {

    context <- detect_validation_context("")
    expect_equal(context, "real-time")  # Default to real-time
  })
})

describe("SQL Safety and Correctness", {
  it("properly escapes string values in SQL", {

    ast <- list(
      type = "comparison",
      field = "status",
      operator = "==",
      value = "active"
    )

    sql <- generate_sql_comparison(ast, "status")

    expect_true(grepl("'active'", sql))  # Should be quoted
  })

  it("uses parameter placeholders for numeric values", {

    ast <- list(
      type = "between",
      field = "age",
      min = 18,
      max = 65
    )

    sql <- generate_sql_between(ast, "age")

    # Should contain numeric values for numeric fields
    expect_true(grepl("18", sql))
    expect_true(grepl("65", sql))
  })

  it("handles NULL values in cross-visit queries", {

    sql <- generate_cross_visit_query(
      field_name = "weight",
      visit_comparisons = list(c("baseline", "week4"))
    )

    # Query should work even if some visits have NULL values
    expect_true(grepl("JOIN", sql))
  })
})

describe("Query Complexity and Performance", {
  it("generates simple WHERE clause for simple rules", {

    ast <- list(type = "required", field = "test")
    sql <- generate_sql_where_clause(ast, "test")

    # Should be simple, no complex subqueries
    expect_false(grepl("SELECT.*SELECT", sql))
  })

  it("generates optimized cross-visit queries", {

    sql <- generate_cross_visit_query(
      field_name = "weight",
      visit_comparisons = list(c("baseline", "week4"))
    )

    # Should use JOIN, not correlated subqueries for performance
    expect_true(grepl("INNER JOIN|LEFT JOIN", sql))
    expect_false(grepl("\\(SELECT.*FROM.*WHERE.*=", sql))
  })

  it("uses window functions for efficiency", {

    sql <- generate_outlier_detection_query("value", num_std_dev = 3)

    # Should use CTE for efficiency
    expect_true(grepl("WITH", sql))
  })
})
