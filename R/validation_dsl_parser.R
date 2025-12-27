#' Validation DSL Parser
#'
#' Parses validation rule strings into Abstract Syntax Trees (AST) for code generation.
#' Supports a human-friendly validation language for clinical trial rules.
#'
#' @import methods

# ============================================================================
# Token Types and Constants
# ============================================================================

#' Token types for DSL lexer
TOKEN_TYPES <- list(
  EOF = "EOF",
  NUMBER = "NUMBER",
  STRING = "STRING",
  IDENTIFIER = "IDENTIFIER",
  OPERATOR = "OPERATOR",
  KEYWORD = "KEYWORD",
  LPAREN = "LPAREN",
  RPAREN = "RPAREN",
  LBRACE = "LBRACE",
  RBRACE = "RBRACE",
  COMMA = "COMMA",
  RANGE = "RANGE",
  BETWEEN = "BETWEEN",
  AND = "AND",
  OR = "OR",
  NOT = "NOT",
  IF = "IF",
  THEN = "THEN",
  ELSE = "ELSE",
  ENDIF = "ENDIF",
  IN = "IN",
  NOTIN = "NOTIN",
  REQUIRED = "REQUIRED",
  ALLOW = "ALLOW",
  # Date/Time operators
  WITHIN = "WITHIN",
  OF = "OF",
  DAYS = "DAYS",
  WEEKS = "WEEKS",
  MONTHS = "MONTHS",
  YEARS = "YEARS",
  TODAY = "TODAY",
  PLUS = "PLUS",
  MINUS = "MINUS"
)

# ============================================================================
# Token Class
# ============================================================================

#' Token representation
#' @keywords internal
Token <- function(type, value, line = 1, column = 1) {
  list(
    type = type,
    value = value,
    line = line,
    column = column
  )
}

# ============================================================================
# Lexer (Tokenizer)
# ============================================================================

#' Tokenize a DSL rule string
#'
#' Converts a DSL rule string into a list of tokens.
#'
#' @param rule Character string containing the validation rule
#'
#' @return List of tokens
#'
#' @examples
#' \dontrun{
#' tokens <- tokenize_dsl_rule("x >= 18")
#' }
#'
#' @keywords internal
tokenize_dsl_rule <- function(rule) {
  if (!is.character(rule) || length(rule) != 1) {
    stop("Rule must be a single character string")
  }

  tokens <- list()
  i <- 1
  line <- 1
  column <- 1
  rule <- trimws(rule)

  while (i <= nchar(rule)) {
    # Skip whitespace
    if (grepl("^\\s", substr(rule, i, i))) {
      if (substr(rule, i, i) == "\n") {
        line <- line + 1
        column <- 1
      } else {
        column <- column + 1
      }
      i <- i + 1
      next
    }

    # Comments (skip to end of line)
    if (substr(rule, i, i) == "#") {
      while (i <= nchar(rule) && substr(rule, i, i) != "\n") {
        i <- i + 1
      }
      next
    }

    # String literals (single or double quotes)
    if (substr(rule, i, i) %in% c("'", '"')) {
      quote_char <- substr(rule, i, i)
      start <- i
      i <- i + 1
      str_value <- ""
      while (i <= nchar(rule) && substr(rule, i, i) != quote_char) {
        if (substr(rule, i, i) == "\\") {
          i <- i + 1
          if (i <= nchar(rule)) {
            str_value <- paste0(str_value, substr(rule, i, i))
            i <- i + 1
          }
        } else {
          str_value <- paste0(str_value, substr(rule, i, i))
          i <- i + 1
        }
      }
      if (i <= nchar(rule)) i <- i + 1  # skip closing quote
      tokens <- c(tokens, list(Token(TOKEN_TYPES$STRING, str_value, line, column)))
      column <- column + (i - start)
      next
    }

    # Numbers (integers and decimals)
    if (grepl("^[0-9]", substr(rule, i, i))) {
      start <- i
      num_str <- ""
      has_dot <- FALSE
      while (i <= nchar(rule) && grepl("[0-9]", substr(rule, i, i))) {
        num_str <- paste0(num_str, substr(rule, i, i))
        i <- i + 1
      }
      # Check for decimal point (but not ..)
      if (i <= nchar(rule) && substr(rule, i, i) == "." &&
          i + 1 <= nchar(rule) && grepl("[0-9]", substr(rule, i + 1, i + 1))) {
        num_str <- paste0(num_str, ".")
        i <- i + 1
        while (i <= nchar(rule) && grepl("[0-9]", substr(rule, i, i))) {
          num_str <- paste0(num_str, substr(rule, i, i))
          i <- i + 1
        }
      }
      tokens <- c(tokens, list(Token(TOKEN_TYPES$NUMBER, as.numeric(num_str), line, column)))
      column <- column + nchar(num_str)
      next
    }

    # Keywords and identifiers
    if (grepl("^[a-zA-Z_]", substr(rule, i, i))) {
      start <- i
      ident <- ""
      while (i <= nchar(rule) && grepl("[a-zA-Z0-9_]", substr(rule, i, i))) {
        ident <- paste0(ident, substr(rule, i, i))
        i <- i + 1
      }

      # Check for keywords
      keyword_type <- match_keyword(tolower(ident))
      if (!is.na(keyword_type)) {
        tokens <- c(tokens, list(Token(keyword_type, tolower(ident), line, column)))
      } else {
        tokens <- c(tokens, list(Token(TOKEN_TYPES$IDENTIFIER, ident, line, column)))
      }
      column <- column + nchar(ident)
      next
    }

    # Operators and special characters
    rest <- substr(rule, i, nchar(rule))

    if (startsWith(rest, "..")) {
      tokens <- c(tokens, list(Token(TOKEN_TYPES$RANGE, "..", line, column)))
      i <- i + 2
      column <- column + 2
    } else if (startsWith(rest, "==")) {
      tokens <- c(tokens, list(Token(TOKEN_TYPES$OPERATOR, "==", line, column)))
      i <- i + 2
      column <- column + 2
    } else if (startsWith(rest, "!=")) {
      tokens <- c(tokens, list(Token(TOKEN_TYPES$OPERATOR, "!=", line, column)))
      i <- i + 2
      column <- column + 2
    } else if (startsWith(rest, "<=")) {
      tokens <- c(tokens, list(Token(TOKEN_TYPES$OPERATOR, "<=", line, column)))
      i <- i + 2
      column <- column + 2
    } else if (startsWith(rest, ">=")) {
      tokens <- c(tokens, list(Token(TOKEN_TYPES$OPERATOR, ">=", line, column)))
      i <- i + 2
      column <- column + 2
    } else if (substr(rest, 1, 1) == "<") {
      tokens <- c(tokens, list(Token(TOKEN_TYPES$OPERATOR, "<", line, column)))
      i <- i + 1
      column <- column + 1
    } else if (substr(rest, 1, 1) == ">") {
      tokens <- c(tokens, list(Token(TOKEN_TYPES$OPERATOR, ">", line, column)))
      i <- i + 1
      column <- column + 1
    } else if (substr(rest, 1, 1) == "(") {
      tokens <- c(tokens, list(Token(TOKEN_TYPES$LPAREN, "(", line, column)))
      i <- i + 1
      column <- column + 1
    } else if (substr(rest, 1, 1) == ")") {
      tokens <- c(tokens, list(Token(TOKEN_TYPES$RPAREN, ")", line, column)))
      i <- i + 1
      column <- column + 1
    } else if (substr(rest, 1, 1) == "{") {
      tokens <- c(tokens, list(Token(TOKEN_TYPES$LBRACE, "{", line, column)))
      i <- i + 1
      column <- column + 1
    } else if (substr(rest, 1, 1) == "}") {
      tokens <- c(tokens, list(Token(TOKEN_TYPES$RBRACE, "}", line, column)))
      i <- i + 1
      column <- column + 1
    } else if (substr(rest, 1, 1) == ",") {
      tokens <- c(tokens, list(Token(TOKEN_TYPES$COMMA, ",", line, column)))
      i <- i + 1
      column <- column + 1
    } else {
      stop(sprintf("Unexpected character '%s' at line %d, column %d", substr(rest, 1, 1), line, column))
    }
  }

  tokens <- c(tokens, list(Token(TOKEN_TYPES$EOF, "", line, column)))
  tokens
}

#' Match keyword token type
#' @keywords internal
match_keyword <- function(word) {
  switch(word,
    "between" = TOKEN_TYPES$BETWEEN,
    "and" = TOKEN_TYPES$AND,
    "or" = TOKEN_TYPES$OR,
    "not" = TOKEN_TYPES$NOT,
    "if" = TOKEN_TYPES$IF,
    "then" = TOKEN_TYPES$THEN,
    "else" = TOKEN_TYPES$ELSE,
    "endif" = TOKEN_TYPES$ENDIF,
    "in" = TOKEN_TYPES$IN,
    "notin" = TOKEN_TYPES$NOTIN,
    "required" = TOKEN_TYPES$REQUIRED,
    "allow" = TOKEN_TYPES$ALLOW,
    # Date/time keywords
    "within" = TOKEN_TYPES$WITHIN,
    "of" = TOKEN_TYPES$OF,
    "days" = TOKEN_TYPES$DAYS,
    "weeks" = TOKEN_TYPES$WEEKS,
    "months" = TOKEN_TYPES$MONTHS,
    "years" = TOKEN_TYPES$YEARS,
    "today" = TOKEN_TYPES$TODAY,
    NA
  )
}

# ============================================================================
# AST Node Factory
# ============================================================================

#' Create an AST node
#' @keywords internal
ast_node <- function(type, ...) {
  args <- list(...)
  c(list(type = type), args)
}

# ============================================================================
# Parser (Recursive Descent)
# ============================================================================

#' Parse a validation rule into an AST
#'
#' @param rule Character string containing the validation rule
#'
#' @return List representing the AST
#'
#' @examples
#' \dontrun{
#' ast <- parse_dsl_rule("x >= 18")
#' ast <- parse_dsl_rule("between 40 and 200")
#' }
#'
#' @export
parse_dsl_rule <- function(rule) {
  if (!is.character(rule) || length(rule) != 1) {
    stop("Rule must be a single character string")
  }

  tokens <- tokenize_dsl_rule(rule)
  parser <- Parser$new(tokens)
  parser$parse()
}

#' Parser class
#' @keywords internal
Parser <- R6::R6Class(
  "Parser",
  public = list(
    tokens = NULL,
    current = 1,

    initialize = function(tokens) {
      self$tokens <- tokens
      self$current <- 1
    },

    parse = function() {
      if (self$peek()$type == TOKEN_TYPES$EOF) {
        stop("Empty rule")
      }
      result <- self$parse_expression()
      if (self$peek()$type != TOKEN_TYPES$EOF) {
        stop(sprintf("Unexpected token '%s' at line %d", self$peek()$value, self$peek()$line))
      }
      result
    },

    parse_expression = function() {
      # expression = or_expression
      self$parse_or_expression()
    },

    parse_or_expression = function() {
      # or_expression = and_expression (OR and_expression)*
      left <- self$parse_and_expression()

      while (self$peek()$type == TOKEN_TYPES$OR) {
        self$consume(TOKEN_TYPES$OR)
        right <- self$parse_and_expression()
        left <- ast_node("or", left = left, right = right)
      }

      left
    },

    parse_and_expression = function() {
      # and_expression = comparison (AND comparison)*
      left <- self$parse_comparison()

      while (self$peek()$type == TOKEN_TYPES$AND) {
        self$consume(TOKEN_TYPES$AND)
        right <- self$parse_comparison()
        left <- ast_node("and", left = left, right = right)
      }

      left
    },

    parse_comparison = function() {
      # comparison = primary_comparison
      #            | if_expression
      #            | special_check

      if (self$peek()$type == TOKEN_TYPES$IF) {
        return(self$parse_if_expression())
      }

      if (self$peek()$type == TOKEN_TYPES$REQUIRED) {
        return(self$parse_required())
      }

      if (self$peek()$type == TOKEN_TYPES$ALLOW) {
        return(self$parse_allow())
      }

      self$parse_primary_comparison()
    },

    parse_primary_comparison = function() {
      # primary_comparison = between_expression
      #                    | in_expression
      #                    | notin_expression
      #                    | simple_comparison

      # Look ahead for 'between' keyword
      if (self$peek()$type == TOKEN_TYPES$BETWEEN) {
        return(self$parse_between())
      }

      # Look ahead for 'in' keyword (without preceding field)
      if (self$peek()$type == TOKEN_TYPES$IN) {
        return(self$parse_in_without_field())
      }

      # Look ahead for 'notin' keyword (without preceding field)
      if (self$peek()$type == TOKEN_TYPES$NOTIN) {
        return(self$parse_notin_without_field())
      }

      field <- self$parse_term()

      # Check for between/in/notin after field
      if (self$peek()$type == TOKEN_TYPES$BETWEEN) {
        return(self$parse_between_after_field(field))
      }

      if (self$peek()$type == TOKEN_TYPES$IN) {
        return(self$parse_in_after_field(field))
      }

      if (self$peek()$type == TOKEN_TYPES$NOTIN) {
        return(self$parse_notin_after_field(field))
      }

      # Simple comparison
      if (self$peek()$type == TOKEN_TYPES$OPERATOR) {
        op <- self$consume(TOKEN_TYPES$OPERATOR)$value
        right <- self$parse_term()
        return(ast_node("comparison", field = field, operator = op, value = right))
      }

      if (self$peek()$type == TOKEN_TYPES$RANGE) {
        # Parse range: field .. value
        self$consume(TOKEN_TYPES$RANGE)
        end <- self$parse_term()
        return(ast_node("range", field = field, start = field, end = end))
      }

      # If parse_term returned a complex expression (if/or/and), return it as-is
      # This happens when parsing parenthesized expressions like (if ... endif)
      if (is.list(field) && field$type %in% c("if", "or", "and")) {
        return(field)
      }

      # Just a field reference (truthy check)
      ast_node("field_value", field = field)
    },

    parse_in_without_field = function() {
      # in(value1, value2, ...)  (without preceding field)
      self$consume(TOKEN_TYPES$IN)
      self$consume(TOKEN_TYPES$LPAREN)
      values <- list()

      if (self$peek()$type != TOKEN_TYPES$RPAREN) {
        values <- c(values, list(self$parse_term()))
        while (self$peek()$type == TOKEN_TYPES$COMMA) {
          self$consume(TOKEN_TYPES$COMMA)
          values <- c(values, list(self$parse_term()))
        }
      }

      self$consume(TOKEN_TYPES$RPAREN)
      ast_node("in", field = NULL, values = values)
    },

    parse_notin_without_field = function() {
      # notin(value1, value2, ...)  (without preceding field)
      self$consume(TOKEN_TYPES$NOTIN)
      self$consume(TOKEN_TYPES$LPAREN)
      values <- list()

      if (self$peek()$type != TOKEN_TYPES$RPAREN) {
        values <- c(values, list(self$parse_term()))
        while (self$peek()$type == TOKEN_TYPES$COMMA) {
          self$consume(TOKEN_TYPES$COMMA)
          values <- c(values, list(self$parse_term()))
        }
      }

      self$consume(TOKEN_TYPES$RPAREN)
      ast_node("notin", field = NULL, values = values)
    },

    parse_between = function() {
      # between value AND value
      self$consume(TOKEN_TYPES$BETWEEN)
      min_val <- self$parse_term()
      self$consume(TOKEN_TYPES$AND)
      max_val <- self$parse_term()
      ast_node("between", min = min_val, max = max_val)
    },

    parse_between_after_field = function(field) {
      # field between value AND value
      self$consume(TOKEN_TYPES$BETWEEN)
      min_val <- self$parse_term()
      self$consume(TOKEN_TYPES$AND)
      max_val <- self$parse_term()
      ast_node("between", field = field, min = min_val, max = max_val)
    },

    parse_in_after_field = function(field) {
      # field in(value1, value2, ...)
      self$consume(TOKEN_TYPES$IN)
      self$consume(TOKEN_TYPES$LPAREN)
      values <- list()

      if (self$peek()$type != TOKEN_TYPES$RPAREN) {
        values <- c(values, list(self$parse_term()))
        while (self$peek()$type == TOKEN_TYPES$COMMA) {
          self$consume(TOKEN_TYPES$COMMA)
          values <- c(values, list(self$parse_term()))
        }
      }

      self$consume(TOKEN_TYPES$RPAREN)
      ast_node("in", field = field, values = values)
    },

    parse_notin_after_field = function(field) {
      # field notin(value1, value2, ...)
      self$consume(TOKEN_TYPES$NOTIN)
      self$consume(TOKEN_TYPES$LPAREN)
      values <- list()

      if (self$peek()$type != TOKEN_TYPES$RPAREN) {
        values <- c(values, list(self$parse_term()))
        while (self$peek()$type == TOKEN_TYPES$COMMA) {
          self$consume(TOKEN_TYPES$COMMA)
          values <- c(values, list(self$parse_term()))
        }
      }

      self$consume(TOKEN_TYPES$RPAREN)
      ast_node("notin", field = field, values = values)
    },

    parse_if_expression = function() {
      # if expression then expression (else expression)? endif
      self$consume(TOKEN_TYPES$IF)
      condition <- self$parse_expression()
      self$consume(TOKEN_TYPES$THEN)
      then_expr <- self$parse_expression()

      else_expr <- NULL
      if (self$peek()$type == TOKEN_TYPES$ELSE) {
        self$consume(TOKEN_TYPES$ELSE)
        else_expr <- self$parse_expression()
      }

      self$consume(TOKEN_TYPES$ENDIF)
      ast_node("if", condition = condition, then_expr = then_expr, else_expr = else_expr)
    },

    parse_required = function() {
      # required (unless field == value)?
      self$consume(TOKEN_TYPES$REQUIRED)
      ast_node("required")
    },

    parse_allow = function() {
      # allow value1, value2, ...
      self$consume(TOKEN_TYPES$ALLOW)
      values <- list()

      values <- c(values, list(self$parse_term()))
      while (self$peek()$type == TOKEN_TYPES$COMMA) {
        self$consume(TOKEN_TYPES$COMMA)
        values <- c(values, list(self$parse_term()))
      }

      ast_node("allow", values = values)
    },

    parse_term = function() {
      # term = literal | identifier | parenthesized_expression | function_call
      token <- self$peek()

      if (token$type == TOKEN_TYPES$LPAREN) {
        self$consume(TOKEN_TYPES$LPAREN)
        expr <- self$parse_expression()
        self$consume(TOKEN_TYPES$RPAREN)
        return(expr)
      }

      if (token$type == TOKEN_TYPES$NUMBER) {
        self$consume(TOKEN_TYPES$NUMBER)
        return(ast_node("literal", value = token$value, literal_type = "number"))
      }

      if (token$type == TOKEN_TYPES$STRING) {
        self$consume(TOKEN_TYPES$STRING)
        return(ast_node("literal", value = token$value, literal_type = "string"))
      }

      # Handle special function keywords
      if (token$type == TOKEN_TYPES$TODAY) {
        self$consume(TOKEN_TYPES$TODAY)
        # Expect parentheses for function call
        if (self$peek()$type == TOKEN_TYPES$LPAREN) {
          self$consume(TOKEN_TYPES$LPAREN)
          self$consume(TOKEN_TYPES$RPAREN)
        }
        return(ast_node("today"))
      }

      if (token$type == TOKEN_TYPES$IDENTIFIER) {
        self$consume(TOKEN_TYPES$IDENTIFIER)
        # Check if this is a function call (followed by LPAREN)
        if (self$peek()$type == TOKEN_TYPES$LPAREN) {
          # Parse function arguments
          self$consume(TOKEN_TYPES$LPAREN)
          args <- list()
          if (self$peek()$type != TOKEN_TYPES$RPAREN) {
            args <- c(args, list(self$parse_expression()))
            while (self$peek()$type == TOKEN_TYPES$COMMA) {
              self$consume(TOKEN_TYPES$COMMA)
              args <- c(args, list(self$parse_expression()))
            }
          }
          self$consume(TOKEN_TYPES$RPAREN)
          return(ast_node("function_call", name = token$value, args = args))
        } else {
          # Just a field reference
          return(ast_node("field", name = token$value))
        }
      }

      stop(sprintf("Expected term, got '%s' at line %d", token$value, token$line))
    },

    peek = function() {
      if (self$current > length(self$tokens)) {
        return(self$tokens[[length(self$tokens)]])  # Return EOF
      }
      self$tokens[[self$current]]
    },

    consume = function(expected_type) {
      token <- self$peek()
      if (token$type != expected_type) {
        stop(sprintf(
          "Expected %s but got %s ('%s') at line %d, column %d",
          expected_type, token$type, token$value, token$line, token$column
        ))
      }
      self$current <- self$current + 1
      token
    }
  )
)

# ============================================================================
# Validation and Analysis Functions
# ============================================================================

#' Extract fields referenced in an AST
#'
#' Returns all field names that are referenced in the validation rule.
#'
#' @param ast List representing the AST
#'
#' @return Character vector of field names
#'
#' @examples
#' \dontrun{
#' ast <- parse_dsl_rule("x >= 18")
#' fields <- extract_ast_fields(ast)
#' }
#'
#' @export
extract_ast_fields <- function(ast) {
  if (!is.list(ast)) return(character())

  fields <- character()

  # Check current node
  if ("field" %in% names(ast) && is.list(ast$field) && "name" %in% names(ast$field)) {
    fields <- c(fields, ast$field$name)
  }

  # Recursively check all list elements
  for (item in ast) {
    if (is.list(item) && length(item) > 0 && "type" %in% names(item)) {
      fields <- c(fields, extract_ast_fields(item))
    }
  }

  unique(fields)
}

#' Check if an AST represents a "required" rule
#'
#' @param ast List representing the AST
#'
#' @return Logical
#'
#' @keywords internal
is_required_rule <- function(ast) {
  is.list(ast) && ast$type == "required"
}

#' Check if an AST represents an "allow" rule
#'
#' @param ast List representing the AST
#'
#' @return Logical
#'
#' @keywords internal
is_allow_rule <- function(ast) {
  is.list(ast) && ast$type == "allow"
}

#' Get allowed values from an allow rule
#'
#' @param ast List representing the AST with type "allow"
#'
#' @return List of allowed values
#'
#' @keywords internal
get_allow_values <- function(ast) {
  if (!is_allow_rule(ast)) return(NULL)
  ast$values
}

#' Check if an AST represents context detection (real-time vs batch)
#'
#' @param ast List representing the AST
#'
#' @return Character string: "real-time" or "batch"
#'
#' @keywords internal
detect_validation_context <- function(ast) {
  # For Phase 1, everything is real-time
  # This will be extended in Phase 5 to detect batch patterns
  "real-time"
}
