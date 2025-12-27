#' Validation Function Cache
#'
#' Pre-compiles and caches validation functions for performance.
#' Validation rules are compiled once at app startup and reused for field-level validation.

# ============================================================================
# Cache Storage
# ============================================================================

#' Initialize validation cache
#'
#' Creates an environment to store pre-compiled validation functions.
#' Called once at app startup.
#'
#' @return Environment for storing validation cache
#'
#' @keywords internal
initialize_validation_cache <- function() {
  cache <- new.env()
  cache$validators <- list()      # Compiled validation functions: list[field_name] = function
  cache$rules <- list()            # Original DSL rules: list[field_name] = rule_text
  cache$compiled_count <- 0        # Number of rules compiled
  cache$last_compiled <- Sys.time()
  return(cache)
}

#' Create global validation cache
#'
#' Sets up the global validation cache if it doesn't exist.
#' Call this once in app initialization (global.R or server.R startup).
#'
#' @export
setup_global_validation_cache <- function() {
  if (!exists(".validation_cache", envir = .GlobalEnv)) {
    assign(".validation_cache", initialize_validation_cache(), envir = .GlobalEnv)
    message("Validation cache initialized")
  }
}

#' Get global validation cache
#'
#' @return The global validation cache environment
#'
#' @keywords internal
get_validation_cache <- function() {
  if (!exists(".validation_cache", envir = .GlobalEnv)) {
    setup_global_validation_cache()
  }
  get(".validation_cache", envir = .GlobalEnv)
}

# ============================================================================
# Cache Population - Load Rules from Data Dictionary
# ============================================================================

#' Load validation rules from data dictionary
#'
#' Loads validation rules from the database's field definitions and compiles them.
#' Called during app startup or when data dictionary is refreshed.
#'
#' @param con Database connection (SQLite)
#'
#' @return List with:
#'   - count: Number of rules loaded
#'   - fields_with_rules: Vector of field names with validation rules
#'   - errors: List of compilation errors (if any)
#'
#' @keywords internal
load_validation_rules_from_db <- function(con) {
  source_file <- system.file("R/validation_dsl_parser.R", package = "zzedc")
  source_file_codegen <- system.file("R/validation_dsl_codegen.R", package = "zzedc")

  if (source_file == "") {
    source("R/validation_dsl_parser.R", local = TRUE)
    source("R/validation_dsl_codegen.R", local = TRUE)
  } else {
    source(source_file, local = TRUE)
    source(source_file_codegen, local = TRUE)
  }

  cache <- get_validation_cache()
  errors <- list()
  rules_count <- 0

  tryCatch({
    # Get all fields with validation rules
    fields_with_rules <- RSQLite::dbGetQuery(con,
      "SELECT field, form_name, validation_rule FROM edc_fields WHERE validation_rule IS NOT NULL AND validation_rule != ''"
    )

    if (nrow(fields_with_rules) == 0) {
      message("No validation rules found in database")
      return(list(
        count = 0,
        fields_with_rules = character(),
        errors = list()
      ))
    }

    message("Loading ", nrow(fields_with_rules), " validation rules from database")

    # Compile each rule
    for (i in 1:nrow(fields_with_rules)) {
      field_name <- fields_with_rules$field[i]
      form_name <- fields_with_rules$form_name[i]
      rule_text <- fields_with_rules$validation_rule[i]

      if (is.na(rule_text) || rule_text == "") next

      tryCatch({
        # Parse the DSL rule
        parser <- DSLParser$new()
        ast <- parser$parse(rule_text)

        if (!is.null(ast)) {
          # Generate validator function
          validator_fn <- generate_validator_from_ast(ast, field_name)

          if (is.function(validator_fn)) {
            # Store in cache with field_name as key
            cache$validators[[field_name]] <- validator_fn
            cache$rules[[field_name]] <- rule_text
            rules_count <- rules_count + 1
          }
        }
      }, error = function(e) {
        error_msg <- paste0("Error compiling rule for field '", field_name, "': ", e$message)
        errors[[length(errors) + 1]] <<- error_msg
        warning(error_msg)
      })
    }

    cache$compiled_count <- rules_count
    cache$last_compiled <- Sys.time()

    message("Compiled ", rules_count, " validation rules successfully")

    return(list(
      count = rules_count,
      fields_with_rules = fields_with_rules$field,
      errors = errors
    ))

  }, error = function(e) {
    warning("Error loading validation rules: ", e$message)
    return(list(
      count = 0,
      fields_with_rules = character(),
      errors = list(e$message)
    ))
  })
}

# ============================================================================
# Cache Retrieval - Get Validators for Execution
# ============================================================================

#' Get validator for a specific field
#'
#' Retrieves the compiled validation function for a field.
#'
#' @param field_name Name of the field to get validator for
#'
#' @return Compiled validator function, or NULL if no rule exists
#'
#' @keywords internal
get_field_validator <- function(field_name) {
  cache <- get_validation_cache()
  cache$validators[[field_name]]
}

#' Get original DSL rule for a field
#'
#' Retrieves the original DSL text for a field's validation rule.
#' Useful for displaying to users or for debugging.
#'
#' @param field_name Name of the field
#'
#' @return DSL rule text, or NULL if no rule exists
#'
#' @keywords internal
get_field_rule_text <- function(field_name) {
  cache <- get_validation_cache()
  cache$rules[[field_name]]
}

#' Get all fields with validation rules
#'
#' @return Vector of field names that have validation rules
#'
#' @keywords internal
get_fields_with_rules <- function() {
  cache <- get_validation_cache()
  names(cache$validators)
}

#' Check if field has validation rule
#'
#' @param field_name Name of the field
#'
#' @return TRUE if field has a validation rule, FALSE otherwise
#'
#' @keywords internal
has_validation_rule <- function(field_name) {
  cache <- get_validation_cache()
  !is.null(cache$validators[[field_name]])
}

# ============================================================================
# Cache Execution - Validate Field Values
# ============================================================================

#' Validate a field value using cached validator
#'
#' Executes a field's validation rule and returns result.
#' This is the main function called during form submission.
#'
#' @param field_name Name of the field to validate
#' @param value The value to validate
#' @param form_values List of all form values (for cross-field validation)
#'
#' @return TRUE if validation passes, error message string if fails
#'
#' @keywords internal
validate_field <- function(field_name, value, form_values = list()) {
  validator_fn <- get_field_validator(field_name)

  # If no validator, return TRUE (valid)
  if (is.null(validator_fn)) {
    return(TRUE)
  }

  # Execute the validator
  tryCatch({
    result <- validator_fn(value, form_values)
    return(result)
  }, error = function(e) {
    # Return error message as validation failure
    return(sprintf("Validation error: %s", e$message))
  })
}

#' Validate all fields in a form
#'
#' Validates all provided form values against their cached rules.
#'
#' @param form_data List of form field values
#'
#' @return List with:
#'   - valid: TRUE if all validations pass, FALSE otherwise
#'   - errors: List of field-level validation errors (field_name = error_message)
#'   - field_results: Full results for each field (field_name = TRUE or error message)
#'
#' @export
validate_form <- function(form_data) {
  validation_results <- list(
    valid = TRUE,
    errors = list(),
    field_results = list()
  )

  for (field_name in names(form_data)) {
    value <- form_data[[field_name]]

    # Validate the field
    result <- validate_field(field_name, value, form_data)

    # Store full result
    validation_results$field_results[[field_name]] <- result

    # If validation failed, add to errors
    if (!isTRUE(result)) {
      validation_results$valid <- FALSE
      validation_results$errors[[field_name]] <- result
    }
  }

  return(validation_results)
}

# ============================================================================
# Cache Management - Invalidation and Refresh
# ============================================================================

#' Clear validation cache
#'
#' Clears all cached validators. Call when data dictionary is updated.
#'
#' @keywords internal
clear_validation_cache <- function() {
  cache <- get_validation_cache()
  cache$validators <- list()
  cache$rules <- list()
  cache$compiled_count <- 0
  message("Validation cache cleared")
}

#' Refresh validation cache from database
#'
#' Clears and reloads all validation rules from the database.
#'
#' @param con Database connection
#'
#' @keywords internal
refresh_validation_cache <- function(con) {
  clear_validation_cache()
  load_validation_rules_from_db(con)
}

#' Get cache statistics
#'
#' Returns information about the validation cache.
#'
#' @return List with cache statistics
#'
#' @keywords internal
get_cache_stats <- function() {
  cache <- get_validation_cache()
  list(
    validators_count = length(cache$validators),
    fields_with_rules = names(cache$validators),
    compiled_count = cache$compiled_count,
    last_compiled = cache$last_compiled
  )
}
