#' Tests for Clinical Validators (Phase 3)
#'
#' Comprehensive test suite for clinical trial validation features including:
#' - Date arithmetic and comparison
#' - Visit-based conditional logic
#' - Score calculations
#'
#' @import testthat

describe("Clinical Date Utilities", {
  it("adds days to a date", {
    source(file.path(getwd(), "../../R/clinical_validators.R"), local = TRUE)

    base_date <- as.Date("2024-01-15")
    result <- add_days(base_date, 30)
    expect_equal(result, as.Date("2024-02-14"))
  })

  it("subtracts days from a date", {
    source(file.path(getwd(), "../../R/clinical_validators.R"), local = TRUE)

    base_date <- as.Date("2024-01-15")
    result <- add_days(base_date, -7)
    expect_equal(result, as.Date("2024-01-08"))
  })

  it("adds weeks to a date", {
    source(file.path(getwd(), "../../R/clinical_validators.R"), local = TRUE)

    base_date <- as.Date("2024-01-15")
    result <- add_weeks(base_date, 4)
    expect_equal(result, as.Date("2024-02-12"))
  })

  it("calculates days between two dates", {
    source(file.path(getwd(), "../../R/clinical_validators.R"), local = TRUE)

    date1 <- as.Date("2024-01-01")
    date2 <- as.Date("2024-01-31")
    result <- days_between(date1, date2)
    expect_equal(result, 30)
  })

  it("checks if date is within N days of reference", {
    source(file.path(getwd(), "../../R/clinical_validators.R"), local = TRUE)

    check_date <- as.Date("2024-01-15")
    reference <- as.Date("2024-01-01")

    # 14 days apart, within 15 days
    expect_true(within_days_of(check_date, reference, 15))

    # 14 days apart, not within 10 days
    expect_false(within_days_of(check_date, reference, 10))
  })
})

describe("Clinical Visit Utilities", {
  it("gets visit sequence number", {
    source(file.path(getwd(), "../../R/clinical_validators.R"), local = TRUE)

    expect_equal(get_visit_number("screening"), -1)
    expect_equal(get_visit_number("baseline"), 0)
    expect_equal(get_visit_number("week4"), 1)
    expect_equal(get_visit_number("week8"), 2)
    expect_equal(get_visit_number("month12"), 12)
  })

  it("checks if visit is in a list", {
    source(file.path(getwd(), "../../R/clinical_validators.R"), local = TRUE)

    expect_true(visit_in_list("baseline", c("baseline", "week4", "week8")))
    expect_true(visit_in_list("BASELINE", c("baseline", "week4")))
    expect_false(visit_in_list("week12", c("baseline", "week4")))
  })

  it("validates visit timing windows", {
    source(file.path(getwd(), "../../R/clinical_validators.R"), local = TRUE)

    baseline_date <- as.Date("2024-01-01")

    # Week 4 visit should be day 25-31 from baseline
    week4_early <- as.Date("2024-02-05")  # Day 36 - too late
    week4_on_time <- as.Date("2024-01-28") # Day 27 - on time

    expect_true(validate_visit_timing("week4", week4_on_time, baseline_date))
    expect_false(validate_visit_timing("week4", week4_early, baseline_date))
  })
})

describe("Clinical Score Utilities", {
  it("calculates ADHD rating scale total", {
    source(file.path(getwd(), "../../R/clinical_validators.R"), local = TRUE)

    inattention_scores <- c(2, 1, 2, 3, 2, 1, 2, 1, 2)  # Sum: 16
    hyperactivity_scores <- c(1, 2, 2, 2, 1, 2, 1, 2, 1) # Sum: 14

    total <- calculate_adhd_score(inattention_scores, hyperactivity_scores)
    expect_equal(total, 30)
  })

  it("validates score is in range", {
    source(file.path(getwd(), "../../R/clinical_validators.R"), local = TRUE)

    # ADHD total ranges 0-54
    expect_true(score_in_range(30, 0, 54))
    expect_false(score_in_range(55, 0, 54))
    expect_false(score_in_range(-1, 0, 54))
    expect_false(score_in_range(NA, 0, 54))
  })
})

describe("Clinical Validation Integration", {
  it("validates complete screening visit data", {
    source(file.path(getwd(), "../../R/clinical_validators.R"), local = TRUE)

    baseline_date <- as.Date("2024-01-01")
    visit_date <- as.Date("2024-01-28")

    # Visit timing is correct
    timing_valid <- validate_visit_timing("week4", visit_date, baseline_date)
    expect_true(timing_valid)

    # Visit date is within 30 days of baseline
    date_check <- within_days_of(visit_date, baseline_date, 30)
    expect_true(date_check)
  })

  it("validates ADHD score for a visit", {
    source(file.path(getwd(), "../../R/clinical_validators.R"), local = TRUE)

    # Get ADHD scores for visit
    adhd_scores <- c(2, 1, 2, 3, 2, 1, 2, 1, 2, 1, 2, 2, 2, 1, 2, 1, 2, 1)

    # Calculate total
    total <- calculate_adhd_score(
      adhd_scores[1:9],   # Inattention
      adhd_scores[10:18]  # Hyperactivity
    )

    # Validate against range 0-54
    expect_true(score_in_range(total, 0, 54))
  })
})
