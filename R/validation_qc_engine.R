#' Validation QC Engine
#'
#' Executes batch validation queries and stores violations in database.
#' Core component of the nightly QC system.
#'
#' @keywords internal
#' @noRd

# ============================================================================
# QC Execution
# ============================================================================

#' Execute batch validation rule
#'
#' Runs a compiled SQL query and returns violations.
#'
#' @param sql_query SQL query string
#' @param con Database connection
#' @param rule_id ID of the rule being executed (for logging)
#' @param rule_name Name of the rule (for reporting)
#'
#' @return Data frame of violations with columns: subject_id, visit, field, value, entry_date, entry_user
#'
#' @keywords internal
execute_qc_query <- function(sql_query, con, rule_id = NULL, rule_name = NULL) {

  if (is.null(sql_query) || sql_query == "") {
    message("No SQL query provided for rule ", rule_id, ": ", rule_name)
    return(data.frame())
  }

  tryCatch({
    violations <- RSQLite::dbGetQuery(con, sql_query)

    if (nrow(violations) > 0) {
      message("Rule ", rule_id, " (", rule_name, "): Found ", nrow(violations), " violations")
    } else {
      message("Rule ", rule_id, " (", rule_name, "): No violations")
    }

    return(violations)

  }, error = function(e) {
    warning("Error executing QC query for rule ", rule_id, ": ", e$message)
    return(data.frame())
  })
}

#' Log QC violations to database
#'
#' Stores violation records in the qc_violations table.
#'
#' @param violations Data frame of violations
#' @param rule_id QC rule ID
#' @param con Database connection
#' @param severity Severity level: "error", "warning", "info"
#' @param violation_type Type of violation: "range", "required", "pattern", etc.
#'
#' @return Number of violations logged
#'
#' @keywords internal
log_qc_violations <- function(violations, rule_id, con, severity = "warning", violation_type = "validation") {

  if (nrow(violations) == 0) {
    return(0)
  }

  tryCatch({
    # Prepare violation records
    violation_records <- data.frame(
      rule_id = rule_id,
      subject_id = violations$subject_id,
      visit = violations$visit %||% NA,
      field = violations$field %||% NA,
      current_value = as.character(violations[[ncol(violations)]]),  # Last column is the value
      violation_type = violation_type,
      severity = severity,
      detected_date = Sys.time(),
      resolved = FALSE,
      false_positive = FALSE,
      stringsAsFactors = FALSE
    )

    # Insert into database
    RSQLite::dbAppendTable(con, "qc_violations", violation_records)

    return(nrow(violation_records))

  }, error = function(e) {
    warning("Error logging violations: ", e$message)
    return(0)
  })
}

#' Execute all active QC rules
#'
#' Runs all active validation rules and logs violations.
#'
#' @param con Database connection
#' @param rules Data frame from qc_rules table with compiled SQL
#'
#' @return List with:
#'   - total_rules: Number of rules executed
#'   - total_violations: Total violations found
#'   - rules_with_violations: Count of rules that found violations
#'   - execution_time_ms: Total execution time in milliseconds
#'
#' @keywords internal
execute_all_qc_rules <- function(con, rules = NULL) {

  if (is.null(rules)) {
    # Load active rules from database
    tryCatch({
      rules <- RSQLite::dbGetQuery(con,
        "SELECT rule_id, rule_name, compiled_sql, severity, context FROM qc_rules WHERE active = 1"
      )
    }, error = function(e) {
      warning("Error loading QC rules: ", e$message)
      return(NULL)
    })
  }

  if (is.null(rules) || nrow(rules) == 0) {
    message("No active QC rules found")
    return(list(
      total_rules = 0,
      total_violations = 0,
      rules_with_violations = 0,
      execution_time_ms = 0
    ))
  }

  start_time <- Sys.time()
  total_violations <- 0
  rules_with_violations <- 0

  message("Executing ", nrow(rules), " QC rules...")

  for (i in 1:nrow(rules)) {
    rule_id <- rules$rule_id[i]
    rule_name <- rules$rule_name[i]
    sql_query <- rules$compiled_sql[i]
    severity <- rules$severity[i] %||% "warning"
    context <- rules$context[i] %||% "batch"

    # Execute query
    violations <- execute_qc_query(sql_query, con, rule_id, rule_name)

    # Log violations
    if (nrow(violations) > 0) {
      logged_count <- log_qc_violations(violations, rule_id, con, severity, context)
      total_violations <- total_violations + logged_count
      rules_with_violations <- rules_with_violations + 1
    }
  }

  execution_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000

  message("QC execution complete: ", total_violations, " violations found in ",
          rules_with_violations, " rules (", round(execution_time), " ms)")

  return(list(
    total_rules = nrow(rules),
    total_violations = total_violations,
    rules_with_violations = rules_with_violations,
    execution_time_ms = execution_time
  ))
}

# ============================================================================
# QC Status and Reporting
# ============================================================================

#' Get QC violations for a specific rule
#'
#' @param rule_id QC rule ID
#' @param con Database connection
#' @param resolved_only Include only resolved violations (default: FALSE)
#'
#' @return Data frame of violations
#'
#' @keywords internal
get_rule_violations <- function(rule_id, con, resolved_only = FALSE) {

  sql <- paste0(
    "SELECT * FROM qc_violations ",
    "WHERE rule_id = ? ",
    if (resolved_only) "AND resolved = 1" else "AND resolved = 0"
  )

  tryCatch({
    RSQLite::dbGetQuery(con, sql, params = list(rule_id))
  }, error = function(e) {
    warning("Error getting violations: ", e$message)
    return(data.frame())
  })
}

#' Get violations by subject
#'
#' @param subject_id Subject ID
#' @param con Database connection
#'
#' @return Data frame of all violations for this subject
#'
#' @keywords internal
get_subject_violations <- function(subject_id, con) {

  sql <- "SELECT * FROM qc_violations WHERE subject_id = ? AND resolved = 0"

  tryCatch({
    RSQLite::dbGetQuery(con, sql, params = list(subject_id))
  }, error = function(e) {
    warning("Error getting subject violations: ", e$message)
    return(data.frame())
  })
}

#' Get violations by severity
#'
#' @param severity Violation severity: "error", "warning", "info"
#' @param con Database connection
#' @param unresolved_only Include only unresolved violations (default: TRUE)
#'
#' @return Data frame of violations at this severity level
#'
#' @keywords internal
get_violations_by_severity <- function(severity, con, unresolved_only = TRUE) {

  sql <- paste0(
    "SELECT * FROM qc_violations ",
    "WHERE severity = ? ",
    if (unresolved_only) "AND resolved = 0" else ""
  )

  tryCatch({
    RSQLite::dbGetQuery(con, sql, params = list(severity))
  }, error = function(e) {
    warning("Error getting violations: ", e$message)
    return(data.frame())
  })
}

#' Get QC summary statistics
#'
#' Returns high-level metrics about current violations.
#'
#' @param con Database connection
#'
#' @return List with violation counts by type and severity
#'
#' @keywords internal
get_qc_summary <- function(con) {

  tryCatch({
    total_violations <- RSQLite::dbGetQuery(con,
      "SELECT COUNT(*) as count FROM qc_violations WHERE resolved = 0"
    )$count

    by_severity <- RSQLite::dbGetQuery(con,
      "SELECT severity, COUNT(*) as count FROM qc_violations WHERE resolved = 0 GROUP BY severity"
    )

    by_rule <- RSQLite::dbGetQuery(con,
      "SELECT r.rule_name, COUNT(v.violation_id) as count FROM qc_violations v
       INNER JOIN qc_rules r ON v.rule_id = r.rule_id
       WHERE v.resolved = 0 GROUP BY v.rule_id"
    )

    affected_subjects <- RSQLite::dbGetQuery(con,
      "SELECT COUNT(DISTINCT subject_id) as count FROM qc_violations WHERE resolved = 0"
    )$count

    return(list(
      total_violations = total_violations,
      violations_by_severity = as.list(setNames(by_severity$count, by_severity$severity)),
      violations_by_rule = as.list(setNames(by_rule$count, by_rule$rule_name)),
      affected_subjects = affected_subjects
    ))

  }, error = function(e) {
    warning("Error getting QC summary: ", e$message)
    return(list())
  })
}

#' Resolve a violation
#'
#' Marks a violation as resolved.
#'
#' @param violation_id Violation ID
#' @param con Database connection
#' @param notes Optional resolution notes
#' @param user_id User ID marking as resolved
#'
#' @return TRUE if successful
#'
#' @keywords internal
resolve_violation <- function(violation_id, con, notes = NULL, user_id = NULL) {

  tryCatch({
    update_sql <- "UPDATE qc_violations SET resolved = 1, resolved_date = ?, resolved_by = ?, resolution_notes = ? WHERE violation_id = ?"

    RSQLite::dbExecute(con, update_sql, params = list(
      Sys.time(),
      user_id,
      notes,
      violation_id
    ))

    return(TRUE)

  }, error = function(e) {
    warning("Error resolving violation: ", e$message)
    return(FALSE)
  })
}

#' Mark violation as false positive
#'
#' @param violation_id Violation ID
#' @param con Database connection
#'
#' @return TRUE if successful
#'
#' @keywords internal
mark_false_positive <- function(violation_id, con) {

  tryCatch({
    RSQLite::dbExecute(con,
      "UPDATE qc_violations SET false_positive = 1 WHERE violation_id = ?",
      params = list(violation_id)
    )
    return(TRUE)

  }, error = function(e) {
    warning("Error marking as false positive: ", e$message)
    return(FALSE)
  })
}

# ============================================================================
# QC Run History
# ============================================================================

#' Log QC execution run
#'
#' Records metadata about a QC execution run.
#'
#' @param con Database connection
#' @param rules_executed Number of rules executed
#' @param violations_found Number of violations found
#' @param execution_time_ms Execution time in milliseconds
#' @param status "success", "partial", or "failed"
#' @param error_message Error message if failed
#'
#' @return TRUE if logged successfully
#'
#' @keywords internal
log_qc_run <- function(con, rules_executed, violations_found, execution_time_ms, status = "success", error_message = NULL) {

  tryCatch({
    run_record <- data.frame(
      run_date = Sys.time(),
      rules_executed = rules_executed,
      violations_found = violations_found,
      execution_time_ms = round(execution_time_ms),
      status = status,
      error_message = error_message,
      stringsAsFactors = FALSE
    )

    RSQLite::dbAppendTable(con, "qc_run_history", run_record)

    return(TRUE)

  }, error = function(e) {
    warning("Error logging QC run: ", e$message)
    return(FALSE)
  })
}

#' Get recent QC run history
#'
#' Returns recent QC execution runs for monitoring.
#'
#' @param con Database connection
#' @param limit Number of recent runs to return (default: 10)
#'
#' @return Data frame of run history
#'
#' @keywords internal
get_recent_qc_runs <- function(con, limit = 10) {

  tryCatch({
    RSQLite::dbGetQuery(con,
      paste0("SELECT * FROM qc_run_history ORDER BY run_date DESC LIMIT ", limit)
    )
  }, error = function(e) {
    warning("Error getting run history: ", e$message)
    return(data.frame())
  })
}

# ============================================================================
# Bulk Operations
# ============================================================================

#' Clear all violations for a rule
#'
#' Clears violations from previous runs before re-running a rule.
#'
#' @param rule_id QC rule ID
#' @param con Database connection
#' @param only_false_positives Clear only marked false positives (default: FALSE)
#'
#' @return Number of violations deleted
#'
#' @keywords internal
clear_rule_violations <- function(rule_id, con, only_false_positives = FALSE) {

  sql <- if (only_false_positives) {
    "DELETE FROM qc_violations WHERE rule_id = ? AND false_positive = 1"
  } else {
    "DELETE FROM qc_violations WHERE rule_id = ?"
  }

  tryCatch({
    count <- RSQLite::dbExecute(con, sql, params = list(rule_id))
    message("Deleted ", count, " violations for rule ", rule_id)
    return(count)

  }, error = function(e) {
    warning("Error clearing violations: ", e$message)
    return(0)
  })
}

#' Bulk resolve violations
#'
#' Marks multiple violations as resolved.
#'
#' @param violation_ids Vector of violation IDs
#' @param con Database connection
#' @param notes Optional resolution notes
#'
#' @return Number of violations resolved
#'
#' @keywords internal
bulk_resolve_violations <- function(violation_ids, con, notes = NULL) {

  count <- 0

  for (vid in violation_ids) {
    if (resolve_violation(vid, con, notes)) {
      count <- count + 1
    }
  }

  message("Resolved ", count, " violations")
  return(count)
}
