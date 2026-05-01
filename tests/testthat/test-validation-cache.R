#' Tests for Validation Cache (Phase 4)
#'
#' Comprehensive test suite for validation caching functionality including:
#' - Cache initialization and management
#' - Validator compilation and storage
#' - Form validation with cached validators
#' - Cache statistics and refresh
#'
#' @import testthat

describe("Validation Cache Initialization", {
  it("initializes empty validation cache", {

    cache <- initialize_validation_cache()

    expect_true(is.environment(cache))
    expect_equal(length(cache$validators), 0)
    expect_equal(length(cache$rules), 0)
    expect_equal(cache$compiled_count, 0)
  })

  it("sets up global validation cache", {

    # Clear existing cache if any
    if (exists(".validation_cache", envir = .GlobalEnv)) {
      rm(".validation_cache", envir = .GlobalEnv)
    }

    setup_global_validation_cache()

    expect_true(exists(".validation_cache", envir = .GlobalEnv))
    cache <- get_validation_cache()
    expect_true(is.environment(cache))
  })

  it("gets existing global cache", {

    # Setup cache first
    if (!exists(".validation_cache", envir = .GlobalEnv)) {
      setup_global_validation_cache()
    }

    cache1 <- get_validation_cache()
    cache2 <- get_validation_cache()

    # Should be the same environment
    expect_identical(cache1, cache2)
  })
})

describe("Validation Cache Storage and Retrieval", {
  it("stores and retrieves validator function", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    # Manually add a validator
    test_validator <- function(value, form_values) {
      if (value >= 18) TRUE else "Must be at least 18 years old"
    }

    cache$validators$age <- test_validator
    cache$rules$age <- "between 18 and 120"
    cache$compiled_count <- 1

    # Retrieve it
    retrieved_fn <- get_field_validator("age")
    expect_type(retrieved_fn, "closure")
    expect_equal(retrieved_fn(25, list()), TRUE)
    expect_true(is.character(retrieved_fn(15, list())))
  })

  it("returns NULL for non-existent field validator", {

    setup_global_validation_cache()

    validator <- get_field_validator("nonexistent_field")
    expect_null(validator)
  })

  it("retrieves original DSL rule text", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    cache$rules$blood_pressure <- "between 40 and 200"

    rule_text <- get_field_rule_text("blood_pressure")
    expect_equal(rule_text, "between 40 and 200")
  })

  it("checks if field has validation rule", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    cache$validators$has_rule <- function(v, f) TRUE

    expect_true(has_validation_rule("has_rule"))
    expect_false(has_validation_rule("no_rule"))
  })

  it("returns all fields with validation rules", {

    # Clear global cache to start fresh
    if (exists(".validation_cache", envir = .GlobalEnv)) {
      rm(".validation_cache", envir = .GlobalEnv)
    }

    setup_global_validation_cache()
    cache <- get_validation_cache()

    cache$validators$age <- function(v, f) TRUE
    cache$validators$weight <- function(v, f) TRUE
    cache$validators$height <- function(v, f) TRUE

    fields <- get_fields_with_rules()
    expect_equal(length(fields), 3)
    expect_true("age" %in% fields)
    expect_true("weight" %in% fields)
    expect_true("height" %in% fields)
  })
})

describe("Field Validation Execution", {
  it("validates field with cached validator", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    # Add a simple validator
    cache$validators$age <- function(value, form_values) {
      if (is.na(value)) return("Age is required")
      val <- as.numeric(value)
      if (val >= 18) TRUE else "Must be 18 or older"
    }

    # Valid value
    result1 <- validate_field("age", 25)
    expect_equal(result1, TRUE)

    # Invalid value
    result2 <- validate_field("age", 15)
    expect_true(is.character(result2))
    expect_match(result2, "18 or older")
  })

  it("returns TRUE for field without validation rule", {

    setup_global_validation_cache()

    # No rule for this field
    result <- validate_field("unvalidated_field", "any value")
    expect_equal(result, TRUE)
  })

  it("validates field with cross-field reference", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    # Validator that uses form_values for cross-field check
    cache$validators$visit_date <- function(value, form_values) {
      baseline <- form_values$baseline_date
      if (is.null(baseline)) return(TRUE)

      visit_date <- as.Date(value)
      baseline_date <- as.Date(baseline)
      days_diff <- as.numeric(difftime(visit_date, baseline_date, units = "days"))

      if (days_diff > 0) TRUE else "Visit date must be after baseline date"
    }

    # Valid cross-field
    result <- validate_field("visit_date", "2024-02-01", list(baseline_date = "2024-01-01"))
    expect_equal(result, TRUE)

    # Invalid cross-field
    result2 <- validate_field("visit_date", "2023-12-01", list(baseline_date = "2024-01-01"))
    expect_true(is.character(result2))
  })

  it("handles validation errors gracefully", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    # Validator that throws an error
    cache$validators$bad_validator <- function(value, form_values) {
      stop("Intentional error")
    }

    # Should return error message string, not throw
    result <- validate_field("bad_validator", "test")
    expect_true(is.character(result))
    expect_match(result, "Validation error")
  })
})

describe("Form Validation", {
  it("validates entire form successfully", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    # Add validators
    cache$validators$age <- function(value, form_values) {
      if (as.numeric(value) >= 18) TRUE else "Must be 18+"
    }
    cache$validators$weight <- function(value, form_values) {
      if (as.numeric(value) > 0) TRUE else "Weight must be positive"
    }

    form_data <- list(
      age = 25,
      weight = 75,
      unvalidated = "test"
    )

    result <- validate_form(form_data)

    expect_true(result$valid)
    expect_equal(length(result$errors), 0)
    expect_equal(length(result$field_results), 3)
    expect_true(all(unlist(lapply(result$field_results[1:2], function(x) x == TRUE))))
  })

  it("validates form with validation errors", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    # Add validators
    cache$validators$age <- function(value, form_values) {
      if (as.numeric(value) >= 18) TRUE else "Must be 18+"
    }
    cache$validators$weight <- function(value, form_values) {
      if (as.numeric(value) > 0) TRUE else "Weight must be positive"
    }

    form_data <- list(
      age = 15,
      weight = -50
    )

    result <- validate_form(form_data)

    expect_false(result$valid)
    expect_equal(length(result$errors), 2)
    expect_true("age" %in% names(result$errors))
    expect_true("weight" %in% names(result$errors))
  })

  it("validates mixed valid and invalid fields", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    # Add validators
    cache$validators$age <- function(value, form_values) {
      if (as.numeric(value) >= 18) TRUE else "Must be 18+"
    }
    cache$validators$weight <- function(value, form_values) {
      if (as.numeric(value) > 0) TRUE else "Weight must be positive"
    }

    form_data <- list(
      age = 25,        # Valid
      weight = -50     # Invalid
    )

    result <- validate_form(form_data)

    expect_false(result$valid)
    expect_equal(length(result$errors), 1)
    expect_true("weight" %in% names(result$errors))
    expect_equal(result$field_results$age, TRUE)
  })

  it("validates form with no rules", {

    # Clear cache
    if (exists(".validation_cache", envir = .GlobalEnv)) {
      rm(".validation_cache", envir = .GlobalEnv)
    }
    setup_global_validation_cache()

    form_data <- list(
      field1 = "value1",
      field2 = "value2"
    )

    result <- validate_form(form_data)

    expect_true(result$valid)
    expect_equal(length(result$errors), 0)
  })
})

describe("Cache Management", {
  it("clears validation cache", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    # Add some validators
    cache$validators$field1 <- function(v, f) TRUE
    cache$validators$field2 <- function(v, f) TRUE
    cache$compiled_count <- 2

    expect_equal(length(cache$validators), 2)

    clear_validation_cache()

    expect_equal(length(cache$validators), 0)
    expect_equal(length(cache$rules), 0)
    expect_equal(cache$compiled_count, 0)
  })

  it("returns cache statistics", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    # Add validators
    cache$validators$age <- function(v, f) TRUE
    cache$validators$weight <- function(v, f) TRUE
    cache$compiled_count <- 2

    stats <- get_cache_stats()

    expect_equal(stats$validators_count, 2)
    expect_equal(stats$compiled_count, 2)
    expect_equal(length(stats$fields_with_rules), 2)
    expect_true("age" %in% stats$fields_with_rules)
  })

  it("tracks last compiled time", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    before_time <- Sys.time()
    cache$last_compiled <- Sys.time()
    after_time <- Sys.time()

    stats <- get_cache_stats()
    expect_true(stats$last_compiled >= before_time)
    expect_true(stats$last_compiled <= after_time)
  })
})

describe("Clinical Trial Validation Scenarios", {
  it("validates age-based conditional validation", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    # Validator for blood pressure (age-dependent)
    # if age >= 65 then between 90 and 180 else between 110 and 200
    cache$validators$blood_pressure <- function(value, form_values) {
      age <- as.numeric(form_values$age %||% 45)
      bp <- as.numeric(value)

      if (age >= 65) {
        if (bp >= 90 && bp <= 180) TRUE else "For seniors: BP should be 90-180"
      } else {
        if (bp >= 110 && bp <= 200) TRUE else "For others: BP should be 110-200"
      }
    }

    # Senior patient
    result1 <- validate_field("blood_pressure", 150, list(age = 70))
    expect_equal(result1, TRUE)

    # Young patient
    result2 <- validate_field("blood_pressure", 150, list(age = 30))
    expect_equal(result2, TRUE)

    # Senior with low BP
    result3 <- validate_field("blood_pressure", 80, list(age = 70))
    expect_true(is.character(result3))
  })

  it("validates ADHD screening form with multiple rules", {

    setup_global_validation_cache()
    cache <- get_validation_cache()

    # Add multiple validators for ADHD screening
    cache$validators$age <- function(value, form_values) {
      age <- as.numeric(value)
      if (age >= 6 && age <= 18) TRUE else "ADHD rating for ages 6-18 only"
    }

    cache$validators$screening_date <- function(value, form_values) {
      date_val <- as.Date(value)
      today_date <- Sys.Date()
      if (date_val <= today_date) TRUE else "Screening date cannot be in future"
    }

    cache$validators$adhd_score <- function(value, form_values) {
      score <- as.numeric(value)
      if (score >= 0 && score <= 54) TRUE else "ADHD score must be 0-54"
    }

    # Validate complete form
    form_data <- list(
      age = 10,
      screening_date = "2024-01-15",
      adhd_score = 28
    )

    result <- validate_form(form_data)
    expect_true(result$valid)

    # Validate with one error
    form_data_bad <- list(
      age = 25,  # Out of range
      screening_date = "2024-01-15",
      adhd_score = 28
    )

    result2 <- validate_form(form_data_bad)
    expect_false(result2$valid)
    expect_true("age" %in% names(result2$errors))
  })
})
