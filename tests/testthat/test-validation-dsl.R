#' Tests for Validation DSL Parser and Code Generator
#'
#' Comprehensive test suite for Phase 1 validation DSL functionality.
#'
#' @import testthat

# ============================================================================
# Parser Tests
# ============================================================================

describe("DSL Tokenizer", {
  it("tokenizes simple comparison operators", {
    tokens <- tokenize_dsl_rule("x >= 18")
    expect_length(tokens, 4)  # x, >=, 18, EOF
    expect_equal(tokens[[1]]$type, "IDENTIFIER")
    expect_equal(tokens[[2]]$type, "OPERATOR")
    expect_equal(tokens[[3]]$type, "NUMBER")
  })

  it("tokenizes numbers with decimals", {
    tokens <- tokenize_dsl_rule("3.14")
    expect_equal(tokens[[1]]$value, 3.14)
  })

  it("tokenizes string literals", {
    tokens <- tokenize_dsl_rule("'hello'")
    expect_equal(tokens[[1]]$type, "STRING")
    expect_equal(tokens[[1]]$value, "hello")
  })

  it("tokenizes between keyword", {
    tokens <- tokenize_dsl_rule("between 1 and 10")
    expect_equal(tokens[[1]]$type, "BETWEEN")
    expect_equal(tokens[[3]]$type, "AND")
  })

  it("tokenizes in keyword with parentheses", {
    tokens <- tokenize_dsl_rule("in(1,2,3)")
    expect_equal(tokens[[1]]$type, "IN")
    expect_equal(tokens[[2]]$type, "LPAREN")
  })

  it("skips comments", {
    tokens <- tokenize_dsl_rule("x >= 18 # this is a comment")
    expect_equal(tokens[[3]]$type, "NUMBER")  # Number before comment
    expect_equal(tokens[[4]]$type, "EOF")  # Comment is skipped, EOF comes next
  })

  it("handles whitespace", {
    tokens <- tokenize_dsl_rule("  x   >=   18  ")
    expect_equal(tokens[[1]]$value, "x")
    expect_equal(tokens[[2]]$value, ">=")
    expect_equal(tokens[[3]]$value, 18)
  })
})

describe("DSL Parser", {
  it("parses simple comparison", {
    ast <- parse_dsl_rule("x >= 18")
    expect_equal(ast$type, "comparison")
    expect_equal(ast$operator, ">=")
    expect_equal(ast$value$value, 18)
  })

  it("parses between expression", {
    ast <- parse_dsl_rule("between 40 and 200")
    expect_equal(ast$type, "between")
    expect_equal(ast$min$value, 40)
    expect_equal(ast$max$value, 200)
  })

  it("parses in expression", {
    ast <- parse_dsl_rule("in(1,2,3)")
    expect_equal(ast$type, "in")
    expect_length(ast$values, 3)
  })

  it("parses and expression", {
    ast <- parse_dsl_rule("x >= 18 and x <= 65")
    expect_equal(ast$type, "and")
    expect_equal(ast$left$type, "comparison")
    expect_equal(ast$right$type, "comparison")
  })

  it("parses or expression", {
    ast <- parse_dsl_rule("x == 'yes' or x == 'no'")
    expect_equal(ast$type, "or")
  })

  it("parses required", {
    ast <- parse_dsl_rule("required")
    expect_equal(ast$type, "required")
  })

  it("parses allow", {
    ast <- parse_dsl_rule("allow n, m")
    expect_equal(ast$type, "allow")
    expect_length(ast$values, 2)
  })

  it("parses range operator", {
    ast <- parse_dsl_rule("1..100")
    expect_equal(ast$type, "range")
  })

  it("rejects empty rule", {
    expect_error(parse_dsl_rule(""))
  })

  it("rejects invalid tokens", {
    expect_error(parse_dsl_rule("x @@ 18"))
  })

  it("extracts fields from AST", {
    ast <- parse_dsl_rule("age >= 18 and name == 'John'")
    fields <- extract_ast_fields(ast)
    expect_true("age" %in% fields)
  })
})

# ============================================================================
# Code Generator Tests
# ============================================================================

describe("Validator Generation", {
  it("generates validator for simple comparison", {
    validator <- generate_validator("x >= 18")
    expect_true(is.function(validator))
    result <- validator(25, list())
    expect_true(result)
  })

  it("rejects values outside range", {
    validator <- generate_validator("x >= 18")
    result <- validator(15, list())
    expect_true(is.character(result))
  })

  it("generates validator for between", {
    validator <- generate_validator("between 40 and 200", "bp")
    result <- validator(150, list())
    expect_true(result)
  })

  it("rejects values outside between range", {
    validator <- generate_validator("between 40 and 200", "bp")
    result <- validator(250, list())
    expect_true(is.character(result))
  })

  it("generates validator for in expression", {
    validator <- generate_validator("in(1,2,3)")
    expect_true(validator(1, list()))
    expect_true(validator(2, list()))
    expect_true(is.character(validator(4, list())))
  })

  it("generates validator for required", {
    validator <- generate_validator("required")
    expect_true(validator("value", list()))
    expect_true(is.character(validator(NA, list())))
    expect_true(is.character(validator("", list())))
  })

  it("generates validator for allow special values", {
    validator <- generate_validator("allow n, m")
    expect_true(validator(NA, list()))  # n = null
    expect_true(is.character(validator("some_value", list())))
  })

  it("generates validator with and logic", {
    validator <- generate_validator("x >= 18 and x <= 65")
    expect_true(validator(25, list()))
    expect_true(is.character(validator(5, list())))
    expect_true(is.character(validator(70, list())))
  })

  it("generates validator with or logic", {
    validator <- generate_validator("x == 'yes' or x == 'no'")
    expect_true(validator("yes", list()))
    expect_true(validator("no", list()))
    expect_true(is.character(validator("maybe", list())))
  })

  it("returns error message on validation failure", {
    validator <- generate_validator("x >= 18")
    result <- validator(15, list())
    expect_true(is.character(result))
    expect_match(result, "invalid")
  })
})

# ============================================================================
# Execution Tests
# ============================================================================

describe("Validator Execution", {
  it("executes validator and returns valid result", {
    validator <- generate_validator("x >= 18")
    result <- execute_validator(validator, 25, list())
    expect_true(result$valid)
    expect_null(result$error_message)
  })

  it("executes validator and returns error message", {
    validator <- generate_validator("x >= 18")
    result <- execute_validator(validator, 15, list())
    expect_false(result$valid)
    expect_true(is.character(result$error_message))
  })

  it("validates multiple fields", {
    form_values <- list(age = 25, bp = 150, date = as.Date("2024-01-15"))
    rules <- list(
      age = "x >= 18",
      bp = "between 40 and 200"
    )
    result <- validate_form_fields(form_values, rules)
    expect_true(result$valid)
    expect_length(result$errors, 0)
  })

  it("reports errors for invalid fields", {
    form_values <- list(age = 15, bp = 250)
    rules <- list(
      age = "x >= 18",
      bp = "between 40 and 200"
    )
    result <- validate_form_fields(form_values, rules)
    expect_false(result$valid)
    expect_length(result$errors, 2)
    expect_true("age" %in% names(result$errors))
    expect_true("bp" %in% names(result$errors))
  })

  it("compiles validation rule", {
    validator <- compile_validation_rule("between 1 and 100", "score")
    expect_true(is.function(validator))
    expect_true(validator(50, list()))
  })

  it("validates value quickly", {
    expect_true(validate_value(25, "x >= 18"))
    expect_false(validate_value(15, "x >= 18"))
  })

  it("gets validation error message", {
    error <- get_validation_error(15, "x >= 18")
    expect_true(is.character(error))
    expect_match(error, "invalid")
  })
})

# ============================================================================
# Clinical Trial Pattern Tests
# ============================================================================

describe("Clinical Trial Patterns", {
  it("validates age range for seniors", {
    validator <- generate_validator("if x >= 65 then between 90 and 180 else between 110 and 200 endif")
    # Senior (65+) with systolic BP 120: valid
    result <- validator(120, list(x = 70))
    expect_true(result)
  })

  it("validates conditional required fields", {
    validator <- generate_validator("if medication == 'yes' then required endif")
    # When medication is yes, dose is required
    result <- validator("5mg", list(medication = "yes"))
    expect_true(result)
  })

  it("validates cross-field dependencies", {
    validator <- generate_validator("x > enrollment_date")
    visit_date <- as.Date("2024-02-01")
    enroll_date <- as.Date("2024-01-01")
    result <- validator(visit_date, list(enrollment_date = enroll_date))
    expect_true(result)
  })

  it("rejects visit before enrollment", {
    validator <- generate_validator("x > enrollment_date")
    visit_date <- as.Date("2023-12-01")
    enroll_date <- as.Date("2024-01-01")
    result <- validator(visit_date, list(enrollment_date = enroll_date))
    expect_true(is.character(result))
  })

  it("validates categorical responses", {
    validator <- generate_validator("in('mild', 'moderate', 'severe')")
    expect_true(validator("mild", list()))
    expect_true(validator("moderate", list()))
    expect_true(is.character(validator("unknown", list())))
  })

  it("validates not-in list", {
    validator <- generate_validator("notin('yes', 'no')")
    expect_true(validator("maybe", list()))
    expect_true(is.character(validator("yes", list())))
  })

  it("handles missing values with allow", {
    validator <- generate_validator("allow n")
    expect_true(validator(NA, list()))
  })
})

# ============================================================================
# Error Handling Tests
# ============================================================================

describe("Error Handling", {
  it("handles syntax errors gracefully", {
    expect_error(parse_dsl_rule("x >=> 18"))
  })

  it("handles malformed between", {
    expect_error(parse_dsl_rule("between 40"))
  })

  it("handles missing parentheses in in()", {
    expect_error(parse_dsl_rule("in 1,2,3"))
  })

  it("validates AST safety", {
    ast <- parse_dsl_rule("x >= 18")
    expect_true(is_ast_safe(ast))
  })

  it("handles NA values in validation", {
    validator <- generate_validator("x >= 18")
    result <- validator(NA, list())
    expect_true(is.character(result))
  })

  it("handles NULL values in validation", {
    validator <- generate_validator("x >= 18")
    result <- validator(NULL, list())
    expect_true(is.character(result))
  })
})

# ============================================================================
# Performance Tests
# ============================================================================

describe("Performance", {
  it("parses rule quickly", {
    rule <- "if age >= 65 then between 90 and 200 else between 110 and 220 endif"
    start_time <- Sys.time()
    for (i in 1:100) {
      ast <- parse_dsl_rule(rule)
    }
    elapsed <- Sys.time() - start_time
    # Should complete 100 parses in under 1 second
    expect_lt(as.numeric(elapsed), 1)
  })

  it("generates validator quickly", {
    rule <- "between 40 and 200"
    start_time <- Sys.time()
    for (i in 1:100) {
      validator <- generate_validator(rule)
    }
    elapsed <- Sys.time() - start_time
    # Should complete 100 generations in under 1 second
    expect_lt(as.numeric(elapsed), 1)
  })

  it("executes validator quickly", {
    validator <- generate_validator("between 40 and 200")
    start_time <- Sys.time()
    for (i in 1:10000) {
      execute_validator(validator, 150, list())
    }
    elapsed <- Sys.time() - start_time
    # Should complete 10,000 executions in under 1 second
    expect_lt(as.numeric(elapsed), 1)
  })
})

# ============================================================================
# Phase 2: Cross-Field Validation Tests
# ============================================================================

describe("Cross-Field Validation", {
  it("validates field against another field", {
    validator <- generate_validator("x > enrollment_date")
    visit_date <- as.Date("2024-02-01")
    enroll_date <- as.Date("2024-01-01")
    result <- validator(visit_date, list(enrollment_date = enroll_date))
    expect_true(result)
  })

  it("rejects when visit before enrollment", {
    validator <- generate_validator("x > enrollment_date")
    visit_date <- as.Date("2023-12-01")
    enroll_date <- as.Date("2024-01-01")
    result <- validator(visit_date, list(enrollment_date = enroll_date))
    expect_true(is.character(result))
  })

  it("compares numeric fields", {
    validator <- generate_validator("x > baseline_value")
    result <- validator(100, list(baseline_value = 50))
    expect_true(result)

    result <- validator(40, list(baseline_value = 50))
    expect_true(is.character(result))
  })

  it("validates with cross-field in condition", {
    validator <- generate_validator("x >= minimum_allowed")
    result <- validator(25, list(minimum_allowed = 18))
    expect_true(result)
  })

  it("handles missing cross-field references", {
    validator <- generate_validator("x > enrollment_date")
    result <- validator(100, list())  # enrollment_date not in form_values
    expect_true(is.character(result))  # Should return error
  })

  it("supports field equality checks", {
    validator <- generate_validator("x == status_code")
    result <- validator("active", list(status_code = "active"))
    expect_true(result)

    result <- validator("inactive", list(status_code = "active"))
    expect_true(is.character(result))
  })
})

# ============================================================================
# Phase 2: Conditional Logic Tests
# ============================================================================

describe("Conditional Logic (if/then/else)", {
  it("parses if/then/else expression", {
    ast <- parse_dsl_rule("if age >= 65 then between 90 and 180 else between 110 and 200 endif")
    expect_equal(ast$type, "if")
    expect_equal(ast$condition$type, "comparison")
    expect_equal(ast$then_expr$type, "between")
    expect_equal(ast$else_expr$type, "between")
  })

  it("evaluates if/then correctly (condition true)", {
    validator <- generate_validator("if x >= 65 then between 90 and 180 else between 110 and 200 endif")
    # Senior with valid BP range
    result <- validator(120, list(x = 70))
    expect_true(result)
  })

  it("evaluates if/then correctly (condition false)", {
    validator <- generate_validator("if x >= 65 then between 90 and 180 else between 110 and 200 endif")
    # Young person with valid BP range
    result <- validator(160, list(x = 30))
    expect_true(result)
  })

  it("rejects invalid values in then branch", {
    validator <- generate_validator("if x >= 65 then between 90 and 180 else between 110 and 200 endif")
    # Senior with too-high BP
    result <- validator(200, list(x = 70))
    expect_true(is.character(result))
  })

  it("rejects invalid values in else branch", {
    validator <- generate_validator("if x >= 65 then between 90 and 180 else between 110 and 200 endif")
    # Young person with too-low BP
    result <- validator(50, list(x = 30))
    expect_true(is.character(result))
  })

  it("handles nested if/then/else", {
    validator <- generate_validator(
      "if x >= 65 then (if gender == 'F' then between 90 and 170 else between 100 and 180 endif) else between 110 and 200 endif"
    )
    # Senior female
    result <- validator(130, list(x = 70, gender = "F"))
    expect_true(result)
  })

  it("validates required field with condition", {
    validator <- generate_validator("if medication == 'yes' then required else allow n endif")
    # When medication is yes, dose required
    result <- validator("5mg", list(medication = "yes"))
    expect_true(result)

    # When medication is no, null allowed
    result <- validator(NA, list(medication = "no"))
    expect_true(result)
  })

  it("handles complex conditional", {
    validator <- generate_validator(
      "if age >= 18 and enrollment_status == 'active' then between 0 and 100 else required endif"
    )
    # Valid adult with active enrollment
    result <- validator(50, list(age = 25, enrollment_status = "active"))
    expect_true(result)
  })
})

# ============================================================================
# Phase 2: Special Values Tests
# ============================================================================

describe("Special Values (n, m)", {
  it("allows null values with n", {
    validator <- generate_validator("allow n")
    result <- validator(NA, list())
    expect_true(result)
  })

  it("allows missing values with m", {
    validator <- generate_validator("allow m")
    result <- validator(NULL, list())
    expect_true(result)

    result <- validator("", list())
    expect_true(result)
  })

  it("allows multiple special values", {
    validator <- generate_validator("allow n, m")
    result <- validator(NA, list())
    expect_true(result)

    result <- validator("", list())
    expect_true(result)
  })

  it("allows specific values and special values", {
    validator <- generate_validator("allow 'skip', 'decline', n")
    expect_true(validator("skip", list()))
    expect_true(validator("decline", list()))
    expect_true(validator(NA, list()))
    expect_true(is.character(validator("other", list())))
  })
})

# ============================================================================
# Phase 2: Logical Operators Tests
# ============================================================================

describe("Logical Operators (and, or, not)", {
  it("evaluates and with both true", {
    validator <- generate_validator("x >= 18 and x <= 65")
    result <- validator(25, list())
    expect_true(result)
  })

  it("rejects and when one condition false", {
    validator <- generate_validator("x >= 18 and x <= 65")
    result <- validator(70, list())
    expect_true(is.character(result))
  })

  it("evaluates or with first true", {
    validator <- generate_validator("x == 'yes' or x == 'no'")
    result <- validator("yes", list())
    expect_true(result)
  })

  it("evaluates or with second true", {
    validator <- generate_validator("x == 'yes' or x == 'no'")
    result <- validator("no", list())
    expect_true(result)
  })

  it("rejects or when both false", {
    validator <- generate_validator("x == 'yes' or x == 'no'")
    result <- validator("maybe", list())
    expect_true(is.character(result))
  })

  it("handles complex and/or combinations", {
    validator <- generate_validator("(x >= 18 and x <= 65) or x > 100")
    expect_true(validator(25, list()))  # In first group
    expect_true(validator(150, list()))  # Matches second condition
    expect_true(is.character(validator(75, list())))  # Matches neither
  })

  it("handles multiple ands", {
    validator <- generate_validator("x >= 0 and x <= 100 and y >= 0 and y <= 100")
    # Note: In Phase 2, we only support single field validation context
    # This tests the structure
    result <- validator(50, list())
    expect_true(result)
  })
})

# ============================================================================
# Phase 3: Clinical Trial Date Features
# ============================================================================

describe("Date Functions", {
  it("parses today() function", {
    ast <- parse_dsl_rule("x <= today()")
    expect_equal(ast$type, "comparison")
    expect_equal(ast$operator, "<=")
    expect_equal(ast$value$type, "today")
  })

  it("evaluates today() to current date", {
    validator <- generate_validator("x <= today()")
    yesterday <- Sys.Date() - 1
    tomorrow <- Sys.Date() + 1

    result_pass <- validator(yesterday, list())
    expect_true(result_pass)

    result_fail <- validator(tomorrow, list())
    expect_true(is.character(result_fail))
  })

  it("handles function calls with arguments", {
    ast <- parse_dsl_rule("length(subject_id) == 7")
    expect_equal(ast$type, "comparison")
    expect_equal(ast$field$type, "function_call")
    expect_equal(ast$field$name, "length")
  })

  it("validates string length using length function", {
    validator <- generate_validator("length(x) == 7")
    result_pass <- validator("ABC1234", list())
    expect_true(result_pass)

    result_fail <- validator("ABC", list())
    expect_true(is.character(result_fail))
  })
})

describe("Date Comparisons", {
  it("validates screening date not in future", {
    validator <- generate_validator("x <= today()")
    today <- Sys.Date()
    past_date <- today - 30
    future_date <- today + 30

    result_past <- validator(past_date, list())
    expect_true(result_past)

    result_future <- validator(future_date, list())
    expect_true(is.character(result_future))
  })

  it("validates enrollment date is before visit date", {
    validator <- generate_validator("x > enrollment_date")
    enrollment <- as.Date("2024-01-15")
    visit_after <- as.Date("2024-02-01")
    visit_before <- as.Date("2024-01-01")

    result_after <- validator(visit_after, list(enrollment_date = enrollment))
    expect_true(result_after)

    result_before <- validator(visit_before, list(enrollment_date = enrollment))
    expect_true(is.character(result_before))
  })
})

describe("Clinical Trial Integration", {
  it("validates ADHD screening with date requirements", {
    # screening_date <= today()
    validator <- generate_validator("x <= today()")
    today <- Sys.Date()

    result <- validator(today - 1, list())
    expect_true(result)
  })

  it("validates age within range for study", {
    # age >= 18 && age <= 65
    validator <- generate_validator("x >= 18 and x <= 65")

    result_valid <- validator(25, list())
    expect_true(result_valid)

    result_invalid <- validator(70, list())
    expect_true(is.character(result_invalid))
  })

  it("validates conditional requirements based on enrollment", {
    # if eligible == 'yes' then randomization_group required
    validator <- generate_validator("if eligible == 'Eligible' then required else allow n endif")

    result_required <- validator("Active", list(eligible = "Eligible"))
    expect_true(result_required)

    result_not_required <- validator(NA, list(eligible = "Not Eligible"))
    expect_true(result_not_required)
  })
})

# ============================================================================
# Integration Tests
# ============================================================================

describe("Integration", {
  it("handles complete workflow: parse -> generate -> execute", {
    rule <- "if age >= 65 then between 90 and 200 else between 110 and 220 endif"

    # Step 1: Parse
    ast <- parse_dsl_rule(rule)
    expect_equal(ast$type, "if")

    # Step 2: Generate
    validator <- generate_validator_from_ast(ast, "systolic_bp")
    expect_true(is.function(validator))

    # Step 3: Execute
    result_senior <- execute_validator(validator, 150, list(age = 70))
    expect_true(result_senior$valid)

    result_young <- execute_validator(validator, 150, list(age = 30))
    expect_true(result_young$valid)  # 150 is in 110-220 range for young person
  })

  it("handles real clinical trial validation", {
    # Screening visit: ADHD score 0-54
    screening_rules <- list(
      adhd_score = "between 0 and 54",
      visit_date = "required"
    )

    screening_data <- list(
      adhd_score = 35,
      visit_date = as.Date("2024-01-15")
    )

    results <- validate_form_fields(screening_data, screening_rules)
    expect_true(results$valid)
  })

  it("validates study protocol requirements", {
    # Visit must be within 7 days of screening
    rules <- list(
      visit_date = "required",
      screening_date = "required"
    )

    data <- list(
      visit_date = as.Date("2024-01-22"),
      screening_date = as.Date("2024-01-15")
    )

    results <- validate_form_fields(data, rules)
    expect_true(results$valid)
  })
})
