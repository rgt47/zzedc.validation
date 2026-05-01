#' Validation Executor
#'
#' Runtime execution of compiled validation rules within Shiny applications.
#' Handles the evaluation of validator functions and error message formatting.
#'
#' @keywords internal
#' @noRd

# ============================================================================
# Validator Execution
# ============================================================================

#' Execute a validator function
#'
#' Runs a compiled validator function and handles error messages.
#'
#' @param validator Function with signature function(value, form_values)
#' @param value The value being validated
#' @param form_values Named list of all form field values
#'
#' @return List with elements:
#'   - valid: Logical TRUE if validation passed
#'   - error_message: Character string with error message (NULL if valid)
#'
#' @examples
#' \dontrun{
#' validator <- generate_validator("between 40 and 200", "blood_pressure")
#' result <- execute_validator(validator, 150, list())
#' }
#'
#' @export
execute_validator <- function(validator, value, form_values = list()) {
  if (!is.function(validator)) {
    stop("validator must be a function")
  }

  tryCatch(
    {
      result <- validator(value, form_values)

      if (isTRUE(result)) {
        list(valid = TRUE, error_message = NULL)
      } else if (is.character(result)) {
        list(valid = FALSE, error_message = result)
      } else {
        list(valid = !isFALSE(result), error_message = "Validation returned unexpected type")
      }
    },
    error = function(e) {
      list(valid = FALSE, error_message = sprintf("Validation error: %s", e$message))
    }
  )
}

#' Compile a validation rule to a validator function
#'
#' Shorthand for parse + generate. Converts a DSL string directly to a
#' ready-to-execute validator function.
#'
#' @param rule Character string with DSL validation rule
#' @param field_name Character string with field name (optional, for error messages)
#'
#' @return Function ready for execution
#'
#' @examples
#' \dontrun{
#' validator <- compile_validation_rule("x >= 18", "age")
#' result <- execute_validator(validator, 25, list())
#' }
#'
#' @export
compile_validation_rule <- function(rule, field_name = NULL) {
  generate_validator(rule, field_name)
}

# ============================================================================
# Batch Validation
# ============================================================================

#' Validate multiple fields
#'
#' Executes validation rules for multiple fields at once.
#'
#' @param form_values Named list of field values
#' @param validation_rules Named list where names are field names and values
#'        are DSL rule strings
#'
#' @return List with structure:
#'   - valid: Logical, TRUE if all fields pass
#'   - errors: Named list of error messages (only for invalid fields)
#'   - all_results: List of all validation results including passes
#'
#' @examples
#' \dontrun{
#' form_values <- list(
#'   age = 25,
#'   blood_pressure = 150,
#'   visit_date = as.Date("2024-01-15")
#' )
#'
#' rules <- list(
#'   age = "x >= 18",
#'   blood_pressure = "between 40 and 200"
#' )
#'
#' results <- validate_form_fields(form_values, rules)
#' }
#'
#' @export
validate_form_fields <- function(form_values, validation_rules) {
  if (!is.list(form_values)) {
    stop("form_values must be a list")
  }

  if (!is.list(validation_rules)) {
    stop("validation_rules must be a list")
  }

  errors <- list()
  all_results <- list()
  is_valid <- TRUE

  for (field_name in names(validation_rules)) {
    rule <- validation_rules[[field_name]]
    value <- form_values[[field_name]]

    tryCatch(
      {
        validator <- compile_validation_rule(rule, field_name)
        result <- execute_validator(validator, value, form_values)

        all_results[[field_name]] <- result

        if (!result$valid) {
          errors[[field_name]] <- result$error_message
          is_valid <- FALSE
        }
      },
      error = function(e) {
        all_results[[field_name]] <<- list(valid = FALSE, error_message = sprintf("Rule error: %s", e$message))
        errors[[field_name]] <<- sprintf("Rule error: %s", e$message)
        is_valid <<- FALSE
      }
    )
  }

  list(
    valid = is_valid,
    errors = errors,
    all_results = all_results
  )
}

# ============================================================================
# Shiny Integration Helpers
# ============================================================================

#' Create a Shiny validation observer
#'
#' Factory function to create a reactive validation system for a Shiny form field.
#'
#' @param field_name Character string with field name
#' @param rule Character string with DSL validation rule
#' @param input Shiny input object
#' @param session Shiny session object
#' @param error_output_id Character string with ID of element to show errors
#'
#' @return Reactive list with structure:
#'   - valid: Reactive logical
#'   - error_message: Reactive character or NULL
#'
#' @keywords internal
create_field_validator <- function(field_name, rule, input, session, error_output_id = NULL) {
  if (!is.character(field_name) || !is.character(rule)) {
    stop("field_name and rule must be character strings")
  }

  # Pre-compile the validator once
  validator <- compile_validation_rule(rule, field_name)

  # Return reactive expressions
  list(
    valid = reactive({
      value <- input[[field_name]]
      result <- execute_validator(validator, value, reactive_to_list(input))
      result$valid
    }),

    error_message = reactive({
      value <- input[[field_name]]
      result <- execute_validator(validator, value, reactive_to_list(input))
      if (result$valid) NULL else result$error_message
    })
  )
}

#' Convert Shiny input to list
#' @keywords internal
reactive_to_list <- function(input) {
  as.list(input)
}

# ============================================================================
# Error Message Formatting
# ============================================================================

#' Format validation error for display
#'
#' Formats error messages for user-friendly display in UI.
#'
#' @param error_message Character string with error message
#' @param field_name Character string with field name
#' @param custom_message Character string with custom message (optional)
#'
#' @return Formatted HTML string for display
#'
#' @keywords internal
format_validation_error <- function(error_message, field_name = NULL, custom_message = NULL) {
  if (is.null(error_message)) {
    return(NULL)
  }

  if (!is.character(error_message)) {
    return(NULL)
  }

  # If custom message provided, use it
  if (!is.null(custom_message) && is.character(custom_message)) {
    error_message <- custom_message
  }

  # Format as HTML span with styling
  sprintf(
    '<span style="color: #dc3545; font-size: 0.875em; margin-top: 0.25rem;">%s</span>',
    error_message
  )
}

#' Generate CSS class for invalid field
#'
#' Returns Bootstrap CSS class for styling invalid fields.
#'
#' @param is_valid Logical indicating if field is valid
#'
#' @return Character string with CSS class or empty string
#'
#' @keywords internal
get_field_css_class <- function(is_valid) {
  if (isTRUE(is_valid)) {
    "is-valid"
  } else if (isFALSE(is_valid)) {
    "is-invalid"
  } else {
    ""
  }
}

# ============================================================================
# Validation Summary
# ============================================================================

#' Create validation summary for display
#'
#' Creates a summary of validation results for display to users.
#'
#' @param validation_results List from validate_form_fields()
#'
#' @return Character string with HTML summary
#'
#' @keywords internal
create_validation_summary <- function(validation_results) {
  if (isTRUE(validation_results$valid)) {
    return(NULL)
  }

  if (length(validation_results$errors) == 0) {
    return(NULL)
  }

  errors_html <- ""
  for (field in names(validation_results$errors)) {
    error_msg <- validation_results$errors[[field]]
    errors_html <- paste0(
      errors_html,
      sprintf("<li><strong>%s:</strong> %s</li>", field, error_msg)
    )
  }

  sprintf(
    '<div class="alert alert-danger" role="alert"><strong>Validation Errors:</strong><ul>%s</ul></div>',
    errors_html
  )
}

# ============================================================================
# Quick Validation
# ============================================================================

#' Quick validation of a single value
#'
#' Simple wrapper for one-off validation without compilation overhead.
#'
#' @param value The value to validate
#' @param rule Character string with DSL validation rule
#' @param form_values Named list of form values (optional, for cross-field validation)
#'
#' @return Logical TRUE if valid, FALSE if invalid
#'
#' @examples
#' \dontrun{
#' is_valid_age <- validate_value(25, "x >= 18")
#' is_valid_bp <- validate_value(150, "between 40 and 200")
#' }
#'
#' @export
validate_value <- function(value, rule, form_values = list()) {
  tryCatch(
    {
      validator <- compile_validation_rule(rule)
      result <- execute_validator(validator, value, form_values)
      result$valid
    },
    error = function(e) {
      warning(sprintf("Validation error: %s", e$message))
      FALSE
    }
  )
}

#' Get validation error message for a value
#'
#' Returns the error message if validation fails, NULL if valid.
#'
#' @param value The value to validate
#' @param rule Character string with DSL validation rule
#' @param form_values Named list of form values (optional)
#'
#' @return Character string with error message, or NULL if valid
#'
#' @examples
#' \dontrun{
#' error <- get_validation_error(5, "x >= 18")
#' # error is: "Value 5 >= 18 is invalid"
#' }
#'
#' @export
get_validation_error <- function(value, rule, form_values = list()) {
  tryCatch(
    {
      validator <- compile_validation_rule(rule)
      result <- execute_validator(validator, value, form_values)
      result$error_message
    },
    error = function(e) {
      sprintf("Validation error: %s", e$message)
    }
  )
}
