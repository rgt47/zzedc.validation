#' Form Branching Logic
#'
#' Enables conditional display of form fields based on values in other fields.
#' Uses client-side (shinyjs) and server-side validation.

#' Parse branching rule
#'
#' Parses a branching rule from metadata into conditions and actions.
#'
#' @param rule Character string with rule (e.g., "field1 == 'value'" or "field2 > 50")
#'
#' @return List with parsed rule components:
#'   - field: Field being evaluated
#'   - operator: Comparison operator (==, !=, <, >, <=, >=, in, not_in)
#'   - value: Value to compare against
#'
#' @keywords internal
parse_branching_rule <- function(rule) {
  if (!is.character(rule) || length(rule) == 0) {
    return(NULL)
  }

  # Pattern: field_name operator value
  # Examples: "gender == 'Female'", "age > 18", "state in ('CA', 'NY')"

  # Try to match common patterns
  if (grepl("==", rule)) {
    parts <- strsplit(rule, "==")[[1]]
    field <- trimws(parts[1])
    value <- trimws(parts[2])
    value <- gsub("['\"]", "", value)  # Remove quotes

    return(list(
      field = field,
      operator = "==",
      value = value
    ))
  }

  if (grepl("!=", rule)) {
    parts <- strsplit(rule, "!=")[[1]]
    field <- trimws(parts[1])
    value <- trimws(parts[2])
    value <- gsub("['\"]", "", value)

    return(list(
      field = field,
      operator = "!=",
      value = value
    ))
  }

  if (grepl(">", rule) && !grepl(">=", rule)) {
    parts <- strsplit(rule, ">")[[1]]
    field <- trimws(parts[1])
    value <- trimws(parts[2])

    return(list(
      field = field,
      operator = ">",
      value = as.numeric(value)
    ))
  }

  if (grepl("<", rule) && !grepl("<=", rule)) {
    parts <- strsplit(rule, "<")[[1]]
    field <- trimws(parts[1])
    value <- trimws(parts[2])

    return(list(
      field = field,
      operator = "<",
      value = as.numeric(value)
    ))
  }

  if (grepl(">=", rule)) {
    parts <- strsplit(rule, ">=")[[1]]
    field <- trimws(parts[1])
    value <- trimws(parts[2])

    return(list(
      field = field,
      operator = ">=",
      value = as.numeric(value)
    ))
  }

  if (grepl("<=", rule)) {
    parts <- strsplit(rule, "<=")[[1]]
    field <- trimws(parts[1])
    value <- trimws(parts[2])

    return(list(
      field = field,
      operator = "<=",
      value = as.numeric(value)
    ))
  }

  if (grepl(" in ", rule, ignore.case = TRUE)) {
    # Split by 'in' operator surrounded by spaces
    parts <- strsplit(rule, " in ")[[1]]

    if (length(parts) >= 2) {
      field <- trimws(parts[1])
      values_str <- trimws(parts[2])
      # Remove all parentheses and brackets
      values_str <- gsub("[()\\[\\]]", "", values_str)
      # Split by comma
      values <- strsplit(values_str, ",")[[1]]
      # Trim whitespace and remove quotes from each value
      values <- trimws(gsub("['\"]", "", values))
      # Remove empty values
      values <- values[nchar(values) > 0]

      return(list(
        field = field,
        operator = "in",
        value = values
      ))
    }
  }

  NULL
}

#' Evaluate branching condition
#'
#' Checks if a branching condition is met based on form values.
#'
#' @param rule Parsed rule from parse_branching_rule()
#' @param form_values Named list or data.frame of current form values
#'
#' @return Logical, TRUE if condition is met
#'
#' @keywords internal
evaluate_condition <- function(rule, form_values) {
  if (is.null(rule)) return(TRUE)

  field_value <- form_values[[rule$field]]

  if (is.null(field_value)) {
    return(FALSE)
  }

  switch(rule$operator,
    "==" = isTRUE(field_value == rule$value),
    "!=" = isTRUE(field_value != rule$value),
    ">" = isTRUE(as.numeric(field_value) > rule$value),
    "<" = isTRUE(as.numeric(field_value) < rule$value),
    ">=" = isTRUE(as.numeric(field_value) >= rule$value),
    "<=" = isTRUE(as.numeric(field_value) <= rule$value),
    "in" = isTRUE(field_value %in% rule$value),
    TRUE
  )
}

#' Generate JavaScript for branching logic
#'
#' Creates shinyjs code to show/hide elements based on field values.
#' Called when form values change.
#'
#' @param field_id ID of field whose value changed
#' @param field_value Current value of the field
#' @param conditional_fields List of fields with branching rules
#'
#' @return Character vector of shinyjs commands (or NULL)
#'
#' @keywords internal
generate_branching_js <- function(field_id, field_value, conditional_fields) {
  if (is.null(conditional_fields)) {
    return(NULL)
  }

  commands <- c()

  # Check each conditional field
  for (cond_field in names(conditional_fields)) {
    cond_config <- conditional_fields[[cond_field]]

    if (is.null(cond_config$show_if) && is.null(cond_config$hide_if)) {
      next
    }

    # Parse the rule
    rule_text <- cond_config$show_if %||% cond_config$hide_if
    rule <- parse_branching_rule(rule_text)

    if (is.null(rule) || rule$field != field_id) {
      next  # This rule doesn't apply to the field that changed
    }

    # Evaluate condition
    form_values <- list()
    form_values[[field_id]] <- field_value

    condition_met <- evaluate_condition(rule, form_values)

    # Determine action (show or hide)
    if (!is.null(cond_config$show_if)) {
      action <- if (condition_met) "show" else "hide"
    } else {
      action <- if (condition_met) "hide" else "show"
    }

    # Add command
    field_selector <- paste0("[data-field='", cond_field, "']")
    commands <- c(commands, paste0("Shiny.sendCustomMessage('branching_", action, "', '", cond_field, "');"))
  }

  if (length(commands) > 0) commands else NULL
}

#' Setup branching logic handlers
#'
#' Sets up reactive observers to handle field visibility changes.
#' Called once when the form is initialized.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param form_fields List of field definitions with branching rules
#' @param form_id Namespace ID for the form
#'
#' @keywords internal
setup_branching_logic <- function(input, output, session, form_fields, form_id = "form") {
  ns <- session$ns

  # Identify fields with branching conditions
  conditional_fields <- list()
  trigger_fields <- unique(c())

  for (field_name in names(form_fields)) {
    field_config <- form_fields[[field_name]]

    if (!is.null(field_config$show_if) || !is.null(field_config$hide_if)) {
      conditional_fields[[field_name]] <- field_config

      # Extract which fields trigger this condition
      rule_text <- field_config$show_if %||% field_config$hide_if
      rule <- parse_branching_rule(rule_text)

      if (!is.null(rule)) {
        trigger_fields <- c(trigger_fields, rule$field)
      }
    }
  }

  # Set up observers for each trigger field
  for (trigger_field in unique(trigger_fields)) {
    observeEvent(input[[trigger_field]], {
      field_value <- input[[trigger_field]]

      # Check each conditional field
      for (cond_field in names(conditional_fields)) {
        cond_config <- conditional_fields[[cond_field]]
        rule_text <- cond_config$show_if %||% cond_config$hide_if
        rule <- parse_branching_rule(rule_text)

        if (!is.null(rule) && rule$field == trigger_field) {
          # Evaluate condition
          form_values <- list()
          form_values[[trigger_field]] <- field_value

          condition_met <- evaluate_condition(rule, form_values)

          # Determine action
          if (!is.null(cond_config$show_if)) {
            action <- if (condition_met) "show" else "hide"
          } else {
            action <- if (condition_met) "hide" else "show"
          }

          # Execute visibility change
          field_wrapper <- paste0("[data-field='", cond_field, "']")
          if (action == "show") {
            shinyjs::show(cond_field)
          } else {
            shinyjs::hide(cond_field)
          }

          # Log the action for audit trail
          tryCatch({
            log_audit_event(
              user_id = "system",
              action = "BRANCHING_LOGIC",
              resource = paste0("field:", cond_field),
              details = jsonlite::toJSON(list(
                trigger_field = trigger_field,
                trigger_value = field_value,
                action = action
              )),
              status = "SUCCESS"
            )
          }, error = function(e) {
            # Silently fail - audit logging not critical
            NULL
          })
        }
      }
    }, ignoreNULL = FALSE)
  }

  invisible(list(
    conditional_fields = conditional_fields,
    trigger_fields = unique(trigger_fields)
  ))
}

#' Validate form with branching logic
#'
#' Ensures that all visible required fields are populated.
#' Accounts for conditionally displayed fields.
#'
#' @param form_data Named list of form values
#' @param form_fields List of field definitions with branching rules
#'
#' @return List with valid=logical, errors=character vector
#'
#' @keywords internal
validate_form_with_branching <- function(form_data, form_fields) {
  errors <- c()

  for (field_name in names(form_fields)) {
    field_config <- form_fields[[field_name]]

    # Check if field should be visible based on branching rules
    is_visible <- TRUE

    if (!is.null(field_config$show_if)) {
      rule <- parse_branching_rule(field_config$show_if)
      is_visible <- evaluate_condition(rule, form_data)
    } else if (!is.null(field_config$hide_if)) {
      rule <- parse_branching_rule(field_config$hide_if)
      is_visible <- !evaluate_condition(rule, form_data)
    }

    # If field is required and visible, validate it's filled
    if (is_visible && isTRUE(field_config$required)) {
      field_value <- form_data[[field_name]]

      if (is.null(field_value) || field_value == "" || (is.list(field_value) && length(field_value) == 0)) {
        errors <- c(
          errors,
          paste0(field_config$label %||% field_name, " is required")
        )
      }
    }
  }

  list(
    valid = length(errors) == 0,
    errors = errors
  )
}

#' Check if field is visible
#'
#' Determines if a field should be displayed based on current form values.
#'
#' @param field_name Name of field to check
#' @param field_config Field configuration with branching rules
#' @param form_values Named list of current form values
#'
#' @return Logical, TRUE if field should be visible
#'
#' @export
is_field_visible <- function(field_name, field_config, form_values) {
  # Field is visible by default
  is_visible <- TRUE

  if (!is.null(field_config$show_if)) {
    rule <- parse_branching_rule(field_config$show_if)
    is_visible <- evaluate_condition(rule, form_values)
  } else if (!is.null(field_config$hide_if)) {
    rule <- parse_branching_rule(field_config$hide_if)
    is_visible <- !evaluate_condition(rule, form_values)
  }

  is_visible
}
