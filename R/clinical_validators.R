#' Clinical Trial Validators
#'
#' Specialized validation functions for clinical trial data including:
#' - Date arithmetic and comparison
#' - Score calculations and ranges
#' - Visit-based conditional logic
#' - Missing data patterns

# ============================================================================
# Date Utilities
# ============================================================================

#' Get today's date
#'
#' Returns the current date as a Date object.
#' Used in validation rules like: screening_date <= today()
#'
#' @return Date object for today's date
#'
#' @keywords internal
get_today <- function() {
  Sys.Date()
}

#' Add days to a date
#'
#' Adds a specified number of days to a date.
#'
#' @param date Date object or date string
#' @param days Number of days to add (can be negative)
#'
#' @return New date after adding days
#'
#' @keywords internal
add_days <- function(date, days) {
  if (is.character(date)) {
    date <- as.Date(date)
  }
  date + days
}

#' Add weeks to a date
#'
#' Adds a specified number of weeks to a date.
#'
#' @param date Date object or date string
#' @param weeks Number of weeks to add
#'
#' @return New date after adding weeks
#'
#' @keywords internal
add_weeks <- function(date, weeks) {
  add_days(date, weeks * 7)
}

#' Add months to a date
#'
#' Adds a specified number of months to a date.
#'
#' @param date Date object or date string
#' @param months Number of months to add
#'
#' @return New date after adding months
#'
#' @keywords internal
add_months <- function(date, months) {
  if (is.character(date)) {
    date <- as.Date(date)
  }
  as.Date(paste0(
    format(date, "%Y-%m-"),
    format(as.Date(paste(format(date, "%Y-%m"), "01", sep = "-")) +
             (months + 1) * 35, "%d")
  ))
}

#' Calculate days between two dates
#'
#' Returns the number of days between two dates.
#'
#' @param date1 First date (Date object or string)
#' @param date2 Second date (Date object or string)
#'
#' @return Number of days between dates (can be negative)
#'
#' @keywords internal
days_between <- function(date1, date2) {
  if (is.character(date1)) {
    date1 <- as.Date(date1)
  }
  if (is.character(date2)) {
    date2 <- as.Date(date2)
  }
  as.numeric(difftime(date2, date1, units = "days"))
}

#' Check if date is within days of another date
#'
#' Checks if a date is within N days of a reference date.
#' Example: visit_date within 30 days of enrollment_date
#'
#' @param check_date Date to check
#' @param reference_date Reference date
#' @param days Number of days tolerance
#'
#' @return TRUE if check_date is within N days of reference_date
#'
#' @keywords internal
within_days_of <- function(check_date, reference_date, days) {
  if (is.character(check_date)) {
    check_date <- as.Date(check_date)
  }
  if (is.character(reference_date)) {
    reference_date <- as.Date(reference_date)
  }

  diff <- abs(as.numeric(difftime(check_date, reference_date, units = "days")))
  diff <= days
}

# ============================================================================
# Score Calculation Utilities
# ============================================================================

#' Calculate ADHD Rating Scale total score
#'
#' Sums inattention (9 items) and hyperactivity (9 items) subscales.
#' Total range: 0-54
#'
#' @param adhd_1_to_9 Vector or list of inattention subscale scores
#' @param adhd_10_to_18 Vector or list of hyperactivity subscale scores
#'
#' @return Total ADHD rating score (0-54)
#'
#' @keywords internal
calculate_adhd_score <- function(adhd_1_to_9, adhd_10_to_18) {
  inattention_sum <- sum(as.numeric(adhd_1_to_9), na.rm = TRUE)
  hyperactivity_sum <- sum(as.numeric(adhd_10_to_18), na.rm = TRUE)
  inattention_sum + hyperactivity_sum
}

#' Validate numeric score is within range
#'
#' Checks if a score is within expected minimum and maximum values.
#'
#' @param score The score value to validate
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#'
#' @return TRUE if score is in range, FALSE otherwise
#'
#' @keywords internal
score_in_range <- function(score, min, max) {
  if (is.na(score)) return(FALSE)
  score >= min && score <= max
}

# ============================================================================
# Visit and Context Utilities
# ============================================================================

#' Get visit sequence number
#'
#' Returns the visit number based on visit code.
#' Useful for ordering visits and calculating intervals.
#'
#' @param visit_code Visit code (e.g., "baseline", "week4", "week8")
#'
#' @return Visit sequence number or NA if unknown
#'
#' @keywords internal
get_visit_number <- function(visit_code) {
  visit_map <- list(
    "baseline" = 0,
    "screening" = -1,
    "week4" = 1,
    "week8" = 2,
    "week12" = 3,
    "month6" = 6,
    "month12" = 12
  )

  visit_map[[tolower(visit_code)]]
}

#' Check if visit is in a set of visits
#'
#' Checks if the current visit code matches any in a list.
#' Example: if visit in ('baseline', 'screening') then ...
#'
#' @param visit_code Current visit code
#' @param visit_list List of visit codes to check
#'
#' @return TRUE if visit_code is in visit_list
#'
#' @keywords internal
visit_in_list <- function(visit_code, visit_list) {
  tolower(visit_code) %in% tolower(visit_list)
}

# ============================================================================
# Composite Validators
# ============================================================================

#' Validate visit timing
#'
#' Checks if visit date is within expected window from baseline/enrollment.
#' Example: For week4 visit, check date is 28±3 days from baseline
#'
#' @param visit_code Visit code
#' @param visit_date Visit date
#' @param baseline_date Baseline/enrollment date
#'
#' @return TRUE if visit date is within expected window
#'
#' @keywords internal
validate_visit_timing <- function(visit_code, visit_date, baseline_date) {
  visit_windows <- list(
    "baseline" = c(0, 0),      # Same day
    "week4" = c(25, 31),       # Day 25-31
    "week8" = c(53, 59),       # Day 53-59
    "week12" = c(81, 87),      # Day 81-87
    "month6" = c(168, 198),    # Day 168-198
    "month12" = c(336, 396)    # Day 336-396
  )

  window <- visit_windows[[tolower(visit_code)]]
  if (is.null(window)) return(TRUE)  # Unknown visit, skip validation

  days_from_baseline <- days_between(baseline_date, visit_date)
  days_from_baseline >= window[1] && days_from_baseline <= window[2]
}

#' Check if value is valid for a visit
#'
#' Some fields have different validation rules based on visit type.
#' Example: ADHD rating only required at specific visits
#'
#' @param value Field value
#' @param visit_code Visit code
#' @param allowed_visits Vector of visits where this field should be completed
#'
#' @return TRUE if value valid for this visit
#'
#' @keywords internal
validate_value_for_visit <- function(value, visit_code, allowed_visits) {
  if (is.na(value) || is.null(value) || value == "") {
    # Missing value - only valid if visit not in allowed list
    !(tolower(visit_code) %in% tolower(allowed_visits))
  } else {
    # Value present - valid if visit in allowed list or no restriction
    TRUE
  }
}
