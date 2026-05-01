#' Validation DSL Code Generator
#'
#' Converts parsed DSL rules (AST) into safe, executable R functions.
#' Generates closures with pre-compiled validation logic - no eval() calls.
#'
#' @keywords internal
#' @noRd

# ============================================================================
# Validation Function Generation
# ============================================================================

#' Generate a validator function from a DSL rule
#'
#' Compiles a parsed validation rule into an R function that can be executed
#' safely without eval(). The generated function returns TRUE if validation passes,
#' or an error message string if validation fails.
#'
#' @param rule Character string containing the DSL validation rule
#' @param field_name Character string with the field name being validated
#'
#' @return Function with signature function(value, form_values) that returns
#'         TRUE on success or error message string on failure
#'
#' @examples
#' \dontrun{
#' # Simple range validation
#' validator <- generate_validator("between 40 and 200", "blood_pressure")
#' result <- validator(150, list())
#'
#' # Comparison validation
#' validator <- generate_validator("x >= 18", "age")
#' result <- validator(25, list())
#' }
#'
#' @export
generate_validator <- function(rule, field_name = NULL) {
  if (!is.character(rule) || length(rule) != 1) {
    stop("Rule must be a single character string")
  }

  # Parse the rule into AST
  ast <- parse_dsl_rule(rule)

  # Generate the validator function
  generate_validator_from_ast(ast, field_name)
}

#' Generate validator from AST
#' @keywords internal
generate_validator_from_ast <- function(ast, field_name = NULL) {
  # Create the validator function
  function(value, form_values = list()) {
    evaluate_ast_node(ast, value, form_values, field_name)
  }
}

# ============================================================================
# AST Evaluation
# ============================================================================

#' Evaluate an AST node
#' @keywords internal
evaluate_ast_node <- function(node, value, form_values = list(), field_name = NULL) {
  if (!is.list(node) || !"type" %in% names(node)) {
    return(value)
  }

  type <- node$type

  switch(type,
    # Logical operations
    "and" = evaluate_and_node(node, value, form_values, field_name),
    "or" = evaluate_or_node(node, value, form_values, field_name),

    # Comparisons
    "comparison" = evaluate_comparison_node(node, value, form_values, field_name),
    "between" = evaluate_between_node(node, value, form_values, field_name),
    "in" = evaluate_in_node(node, value, form_values, field_name),
    "notin" = evaluate_notin_node(node, value, form_values, field_name),
    "range" = evaluate_range_node(node, value, form_values, field_name),

    # Special values
    "required" = evaluate_required_node(node, value, form_values, field_name),
    "allow" = evaluate_allow_node(node, value, form_values, field_name),

    # Control flow
    "if" = evaluate_if_node(node, value, form_values, field_name),

    # Date/Time operations (Phase 3)
    "date_arithmetic" = evaluate_date_arithmetic_node(node, value, form_values, field_name),
    "within_days" = evaluate_within_days_node(node, value, form_values, field_name),
    "today" = evaluate_today_node(node, value, form_values, field_name),
    "function_call" = evaluate_function_call_node(node, value, form_values, field_name),

    # Base types
    "field_value" = value,
    "literal" = node$value,
    "field" = {
      # Try to get from form_values first
      field_val <- form_values[[node$name]]
      # If not found and name is a special keyword, return the name
      if (is.null(field_val)) {
        if (node$name %in% c("n", "m")) {
          node$name  # Return as literal identifier for special values
        } else {
          NA  # Field not found
        }
      } else {
        field_val
      }
    },

    # Default: return value as-is
    value
  )
}

# ============================================================================
# Logical Operations
# ============================================================================

#' Evaluate AND operation
#' @keywords internal
evaluate_and_node <- function(node, value, form_values, field_name) {
  left_result <- evaluate_ast_node(node$left, value, form_values, field_name)
  if (is.character(left_result)) {
    return(left_result)  # Error in left branch
  }

  right_result <- evaluate_ast_node(node$right, value, form_values, field_name)
  if (is.character(right_result)) {
    return(right_result)  # Error in right branch
  }

  if (left_result && right_result) {
    return(TRUE)
  } else {
    return("AND condition failed")
  }
}

#' Evaluate OR operation
#' @keywords internal
evaluate_or_node <- function(node, value, form_values, field_name) {
  left_result <- evaluate_ast_node(node$left, value, form_values, field_name)
  if (isTRUE(left_result)) {
    return(TRUE)
  }

  right_result <- evaluate_ast_node(node$right, value, form_values, field_name)
  if (isTRUE(right_result)) {
    return(TRUE)
  }

  # Both failed, return left error (prefer first error)
  if (is.character(left_result)) {
    return(left_result)
  } else {
    return("OR condition failed")
  }
}

# ============================================================================
# Comparison Operations
# ============================================================================

#' Evaluate comparison operation
#' @keywords internal
evaluate_comparison_node <- function(node, value, form_values, field_name) {
  # Get operands
  if (is.list(node$field) && node$field$type == "field") {
    # Try to get from form_values, fallback to the value parameter
    left <- form_values[[node$field$name]]
    if (is.null(left)) {
      left <- value  # Use the value parameter if field not in form_values
    }
  } else if (is.list(node$field)) {
    left <- evaluate_ast_node(node$field, value, form_values, field_name)
  } else {
    left <- value  # Use value if no field specified
  }

  right <- node$value
  if (is.list(right) && right$type == "field") {
    right_val <- form_values[[right$name]]
    if (is.null(right_val)) {
      right_val <- value  # Fallback
    }
    right <- right_val
  } else if (is.list(right)) {
    right <- evaluate_ast_node(right, value, form_values, field_name)
  }

  # Handle NA/NULL
  if (is.na(left) || is.null(left)) {
    return("Value is missing")
  }

  operator <- node$operator

  # Perform comparison
  result <- switch(operator,
    "<" = left < right,
    "<=" = left <= right,
    ">" = left > right,
    ">=" = left >= right,
    "==" = left == right,
    "!=" = left != right,
    stop(sprintf("Unknown operator: %s", operator))
  )

  if (isTRUE(result)) {
    return(TRUE)
  } else {
    return(sprintf("Value %s %s %s is invalid", left, operator, right))
  }
}

#' Evaluate between operation
#' @keywords internal
evaluate_between_node <- function(node, value, form_values, field_name) {
  # Handle both "between X and Y" and "field between X and Y"
  if ("field" %in% names(node) && is.list(node$field) && node$field$type == "field") {
    check_value <- form_values[[node$field$name]]
  } else if ("field" %in% names(node) && !is.list(node$field)) {
    check_value <- node$field
  } else {
    check_value <- value
  }

  if (is.na(check_value) || is.null(check_value)) {
    return("Value is missing")
  }

  min_val <- node$min
  if (is.list(min_val)) {
    min_val <- evaluate_ast_node(min_val, value, form_values, field_name)
  }

  max_val <- node$max
  if (is.list(max_val)) {
    max_val <- evaluate_ast_node(max_val, value, form_values, field_name)
  }

  if (check_value >= min_val && check_value <= max_val) {
    return(TRUE)
  } else {
    return(sprintf("Value %s is not between %s and %s", check_value, min_val, max_val))
  }
}

#' Evaluate in operation
#' @keywords internal
evaluate_in_node <- function(node, value, form_values, field_name, named_field = TRUE) {
  # Get the value to check
  if (named_field && "field" %in% names(node) && !is.null(node$field) && is.list(node$field)) {
    check_value <- form_values[[node$field$name]]
  } else {
    check_value <- value
  }

  # Extract allowed values
  allowed <- list()
  if ("values" %in% names(node)) {
    for (val_node in node$values) {
      if (is.list(val_node)) {
        allowed <- c(allowed, evaluate_ast_node(val_node, value, form_values, field_name))
      } else {
        allowed <- c(allowed, val_node)
      }
    }
  }

  # Check if value is in allowed list
  if (check_value %in% allowed) {
    return(TRUE)
  } else {
    allowed_str <- paste(allowed, collapse = ", ")
    return(sprintf("Value '%s' is not in allowed values: %s", check_value, allowed_str))
  }
}

#' Evaluate notin operation
#' @keywords internal
evaluate_notin_node <- function(node, value, form_values, field_name, named_field = TRUE) {
  # Get the value to check
  if (named_field && "field" %in% names(node) && !is.null(node$field) && is.list(node$field)) {
    check_value <- form_values[[node$field$name]]
  } else {
    check_value <- value
  }

  # Extract disallowed values
  disallowed <- list()
  if ("values" %in% names(node)) {
    for (val_node in node$values) {
      if (is.list(val_node)) {
        disallowed <- c(disallowed, evaluate_ast_node(val_node, value, form_values, field_name))
      } else {
        disallowed <- c(disallowed, val_node)
      }
    }
  }

  # Check if value is NOT in disallowed list
  if (!(check_value %in% disallowed)) {
    return(TRUE)
  } else {
    disallowed_str <- paste(disallowed, collapse = ", ")
    return(sprintf("Value '%s' is not allowed. Disallowed: %s", check_value, disallowed_str))
  }
}

#' Evaluate range operation (field .. max)
#' @keywords internal
evaluate_range_node <- function(node, value, form_values, field_name) {
  check_value <- value
  if (is.na(check_value) || is.null(check_value)) {
    return("Value is missing")
  }

  min_val <- 1  # Default start for ranges
  if ("start" %in% names(node)) {
    min_val <- node$start
    if (is.list(min_val)) {
      min_val <- evaluate_ast_node(min_val, value, form_values, field_name)
    }
  }

  max_val <- node$end
  if (is.list(max_val)) {
    max_val <- evaluate_ast_node(max_val, value, form_values, field_name)
  }

  if (check_value >= min_val && check_value <= max_val) {
    return(TRUE)
  } else {
    return(sprintf("Value %s is not in range %s..%s", check_value, min_val, max_val))
  }
}

# ============================================================================
# Special Checks
# ============================================================================

#' Evaluate required check
#' @keywords internal
evaluate_required_node <- function(node, value, form_values, field_name) {
  # Check if value is NA or NULL
  if (is.na(value) || is.null(value)) {
    return("This field is required")
  }
  # Check if value is empty string (only for character values)
  if (is.character(value) && value == "") {
    return("This field is required")
  }
  return(TRUE)
}

#' Evaluate allow check (for missing/null values)
#' @keywords internal
evaluate_allow_node <- function(node, value, form_values, field_name) {
  # allow n,m means allow null (n) or missing (m)
  allowed <- node$values

  # Special values: n = null, m = missing
  for (val_node in allowed) {
    # Extract the actual value from the AST node
    if (is.list(val_node) && "value" %in% names(val_node)) {
      val_str <- val_node$value
    } else if (is.list(val_node)) {
      val_str <- evaluate_ast_node(val_node, value, form_values, field_name)
    } else {
      val_str <- val_node
    }

    # Check for special values - use isTRUE to handle NA comparisons
    if (isTRUE(val_str == "n") && is.na(value)) return(TRUE)
    if (isTRUE(val_str == "m") && (is.null(value) || value == "")) return(TRUE)
    if (isTRUE(val_str == value) && !is.na(val_str) && !is.na(value)) return(TRUE)
  }

  return("Value is not in allowed special values")
}

# ============================================================================
# Control Flow
# ============================================================================

#' Evaluate if expression
#' @keywords internal
evaluate_if_node <- function(node, value, form_values, field_name) {
  # Evaluate condition
  condition_result <- evaluate_ast_node(node$condition, value, form_values, field_name)

  # In if/then/else context, treat error messages as FALSE (condition not met)
  # Only TRUE means the condition is satisfied
  if (isTRUE(condition_result)) {
    # Condition is true, execute then branch
    return(evaluate_ast_node(node$then_expr, value, form_values, field_name))
  } else if (!is.null(node$else_expr)) {
    # Condition is false (or error), execute else branch
    return(evaluate_ast_node(node$else_expr, value, form_values, field_name))
  } else {
    return("If condition was false and no else branch")
  }
}

# ============================================================================
# Code Generation for Debugging
# ============================================================================

#' Generate readable R code from AST (for debugging)
#'
#' Creates a string representation of the AST as R code. Useful for
#' understanding what validation rules are doing.
#'
#' @param ast List representing the AST
#' @param depth Integer for indentation (internal use)
#'
#' @return Character string with R code representation
#'
#' @keywords internal
ast_to_r_code <- function(ast, depth = 0) {
  if (!is.list(ast) || !"type" %in% names(ast)) {
    return(as.character(ast))
  }

  indent <- paste0(rep("  ", depth), collapse = "")
  type <- ast$type

  switch(type,
    "and" = sprintf("%s(%s AND %s)",
      indent,
      ast_to_r_code(ast$left, depth + 1),
      ast_to_r_code(ast$right, depth + 1)
    ),
    "or" = sprintf("%s(%s OR %s)",
      indent,
      ast_to_r_code(ast$left, depth + 1),
      ast_to_r_code(ast$right, depth + 1)
    ),
    "comparison" = sprintf("%s%s %s %s",
      indent,
      ast_to_r_code(ast$field, depth),
      ast$operator,
      ast_to_r_code(ast$value, depth)
    ),
    "between" = sprintf("%s%s between %s and %s",
      indent,
      if (!is.null(ast$field)) ast_to_r_code(ast$field, depth) else "x",
      ast_to_r_code(ast$min, depth),
      ast_to_r_code(ast$max, depth)
    ),
    "in" = sprintf("%s%s %%in%% c(%s)",
      indent,
      if (!is.null(ast$field)) ast_to_r_code(ast$field, depth) else "x",
      paste(sapply(ast$values, function(v) ast_to_r_code(v, depth)), collapse = ", ")
    ),
    "required" = sprintf("%srequired", indent),
    "allow" = sprintf("%sallow(%s)", indent, paste(sapply(ast$values, function(v) ast_to_r_code(v, depth)), collapse = ", ")),
    "if" = sprintf(
      "%sif (%s) { %s } %s",
      indent,
      ast_to_r_code(ast$condition, depth + 1),
      ast_to_r_code(ast$then_expr, depth + 1),
      if (!is.null(ast$else_expr)) sprintf("else { %s }", ast_to_r_code(ast$else_expr, depth + 1)) else ""
    ),
    "literal" = sprintf("'%s'", ast$value),
    "field" = ast$name,
    sprintf("%s<%s>", indent, type)
  )
}

# ============================================================================
# Validation and Safety Checks
# ============================================================================

#' Check if an AST is safe to execute
#'
#' Validates that the AST doesn't contain any dangerous operations.
#'
#' @param ast List representing the AST
#'
#' @return Logical TRUE if safe, stops with error if not safe
#'
#' @keywords internal
is_ast_safe <- function(ast) {
  if (!is.list(ast) || !"type" %in% names(ast)) {
    return(TRUE)
  }

  # Check for disallowed types
  dangerous_types <- c("function", "call", "eval", "parse", "source", "system")
  if (ast$type %in% dangerous_types) {
    stop(sprintf("Dangerous AST node type: %s", ast$type))
  }

  # Recursively check all child nodes
  for (item in ast) {
    if (is.list(item) && length(item) > 0 && "type" %in% names(item)) {
      is_ast_safe(item)
    }
  }

  TRUE
}

# ============================================================================
# Date/Time Operations (Phase 3)
# ============================================================================

#' Evaluate date arithmetic operation
#' @keywords internal
evaluate_date_arithmetic_node <- function(node, value, form_values, field_name) {
  # Get the base date
  if (is.character(node$date) || is.list(node$date)) {
    if (is.list(node$date)) {
      base_date <- evaluate_ast_node(node$date, value, form_values, field_name)
    } else {
      base_date <- node$date
    }
  } else {
    base_date <- node$date
  }

  if (is.character(base_date)) {
    base_date <- as.Date(base_date)
  }

  # Get the number
  num_val <- node$number
  if (is.list(num_val)) {
    num_val <- evaluate_ast_node(num_val, value, form_values, field_name)
  }

  # Get the unit
  unit <- node$unit

  # Perform the arithmetic inline
  result <- switch(unit,
    "days" = base_date + num_val,
    "weeks" = base_date + (num_val * 7),
    "months" = base_date + (num_val * 30),  # Approximate
    "years" = base_date + (num_val * 365),  # Approximate
    base_date
  )

  result
}

#' Evaluate within days operation
#' @keywords internal
evaluate_within_days_node <- function(node, value, form_values, field_name) {
  # Get the date to check
  if (is.list(node$check_date)) {
    check_date <- evaluate_ast_node(node$check_date, value, form_values, field_name)
  } else {
    check_date <- node$check_date
  }

  # Get reference date from form values
  ref_date_name <- node$reference_date
  if (is.list(ref_date_name)) {
    ref_date <- evaluate_ast_node(ref_date_name, value, form_values, field_name)
  } else {
    ref_date <- form_values[[ref_date_name]]
    if (is.null(ref_date)) {
      return("Reference date not found in form values")
    }
  }

  # Get the number of days
  days <- node$days
  if (is.list(days)) {
    days <- evaluate_ast_node(days, value, form_values, field_name)
  }

  # Check if dates are valid
  if (is.na(check_date) || is.na(ref_date)) {
    return("Missing date value for within comparison")
  }

  # Convert to Date if needed
  if (is.character(check_date)) {
    check_date <- as.Date(check_date)
  }
  if (is.character(ref_date)) {
    ref_date <- as.Date(ref_date)
  }

  # Perform the check inline
  diff <- abs(as.numeric(difftime(check_date, ref_date, units = "days")))
  if (diff <= days) {
    return(TRUE)
  } else {
    return(sprintf("Date %s is not within %d days of %s", check_date, days, ref_date))
  }
}

#' Evaluate today() function
#' @keywords internal
evaluate_today_node <- function(node, value, form_values, field_name) {
  Sys.Date()
}

#' Evaluate function call node
#' @keywords internal
evaluate_function_call_node <- function(node, value, form_values, field_name) {
  func_name <- node$name
  args <- node$args

  # Evaluate arguments first
  eval_args <- list()
  if (!is.null(args) && length(args) > 0) {
    for (arg in args) {
      eval_args <- c(eval_args, list(evaluate_ast_node(arg, value, form_values, field_name)))
    }
  }

  # Map function names to implementations
  switch(func_name,
    "length" = {
      if (length(eval_args) == 0) {
        return("length() requires an argument")
      }
      nchar(as.character(eval_args[[1]]))
    },
    "today" = {
      Sys.Date()
    },
    "days_between" = {
      if (length(eval_args) < 2) {
        return("days_between() requires two arguments")
      }
      as.numeric(difftime(eval_args[[2]], eval_args[[1]], units = "days"))
    },
    "within_days_of" = {
      if (length(eval_args) < 3) {
        return("within_days_of() requires three arguments")
      }
      diff <- abs(as.numeric(difftime(eval_args[[1]], eval_args[[2]], units = "days")))
      diff <= eval_args[[3]]
    },
    # Default: unknown function
    sprintf("Unknown function: %s", func_name)
  )
}
