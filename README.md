# zzedc.validation

A domain-specific language (DSL) for clinical trial data validation.

## Overview

`zzedc.validation` provides a human-friendly validation language for clinical research data. Write validation rules in plain English, and automatically generate:

- **R validators** for real-time Shiny form validation (<5ms per field)
- **SQL queries** for nightly batch quality control checks

## Installation

```r
# From local source
devtools::install("path/to/zzedc-validation")

# Or after publishing to GitHub
devtools::install_github("rgt47/zzedc-validation")
```

## Quick Start

### 1. Initialize Validation Cache

```r
library(zzedc.validation)

# Create global cache for compiled validators
setup_global_validation_cache()
```

### 2. Write Validation Rules

Write rules in the DSL:

```r
# Range validation
"between 18 and 65"

# Conditional logic
"if age >= 65 then between 90 and 180 else between 110 and 200 endif"

# Cross-field validation
"visit_date > baseline_date"

# Date arithmetic
"within 30 days of enrollment_date"

# Required fields
"required unless exemption_status == 'exempt'"
```

### 3. Compile and Execute

```r
# Compile a rule
validator <- compile_validation_rule(
  "between 40 and 200",
  field_name = "blood_pressure"
)

# Execute on field value
result <- validator(150, form_values = list())

# Result is TRUE if valid, or error message if invalid
if (result == TRUE) {
  message("Valid!")
} else {
  message("Error: ", result)
}
```

### 4. Validate Forms

```r
# Validate all fields in a form
form_data <- list(
  age = 45,
  blood_pressure = 150,
  visit_date = as.Date("2024-01-15")
)

result <- validate_form(form_data)

if (result$valid) {
  message("Form is valid!")
} else {
  message("Errors found:")
  print(result$errors)
}
```

## Features

### Real-Time Validation (Shiny Integration)

- **Sub-5ms validation** - Pre-compiled R functions
- **Field-level checks** - Immediate feedback during data entry
- **Cross-field validation** - Check field relationships
- **Conditional logic** - Complex if/then/else rules
- **Date arithmetic** - Time-based validation windows

### Batch QC (Nightly Checks)

- **Cross-visit consistency** - Track changes across study visits
- **Statistical outlier detection** - Flag unusual values
- **Missing data patterns** - Identify protocol deviations
- **SQL optimization** - Efficient batch processing
- **Violation tracking** - Database logging and resolution

## Core Functions

### Validation Setup
- `setup_global_validation_cache()` - Initialize validation system
- `compile_validation_rule()` - Compile DSL rule to R function

### Form Validation
- `validate_form(form_data)` - Validate all form fields
- `validate_form_field(field_name, value)` - Validate single field
- `parse_dsl_rule(rule_text)` - Parse DSL rule to AST

### Clinical Trial Utilities
- `add_days(date, days)` - Add/subtract days
- `add_weeks(date, weeks)` - Add/subtract weeks
- `add_months(date, months)` - Add/subtract months
- `days_between(date1, date2)` - Calculate days between dates
- `within_days_of(check_date, reference_date, days)` - Check if within N days
- `get_visit_number(visit_code)` - Get visit sequence number
- `validate_visit_timing(visit_code, visit_date, baseline_date)` - Check visit window
- `calculate_adhd_score(q1_to_9, q10_to_18)` - Calculate ADHD total score
- `score_in_range(score, min, max)` - Check if score in range
- `validate_value_for_visit(value, visit_code, allowed_visits)` - Check if value allowed for visit

### QC Engine
- `execute_all_qc_rules(con)` - Run all nightly QC rules
- `get_qc_summary(con)` - Get violation statistics
- `resolve_violation(violation_id, con)` - Mark violation as resolved
- `get_rule_violations(rule_id, con)` - Get violations for a rule
- `get_subject_violations(subject_id, con)` - Get violations for a subject

## Example: ADHD Trial

```r
library(zzedc.validation)
setup_global_validation_cache()

# Age validation: ADHD study only accepts 6-18 year olds
age_rule <- compile_validation_rule("between 6 and 18", "age")
age_rule(12, list())  # TRUE
age_rule(25, list())  # Error message

# Visit timing: Baseline must be before week 4 visit
visit_rule <- compile_validation_rule(
  "week4_date > baseline_date",
  "week4_date"
)
visit_rule(
  as.Date("2024-02-01"),
  list(baseline_date = as.Date("2024-01-01"))
)  # TRUE

# ADHD score calculation
score <- calculate_adhd_score(
  q1_to_9 = c(1, 2, 1, 0, 2, 1, 2, 1, 0),  # Sum = 10
  q10_to_18 = c(2, 1, 2, 1, 2, 1, 2, 1, 0)  # Sum = 12
)
# Total = 22 (valid range 0-54)
```

## Testing

```r
# Run all validation tests
devtools::test()

# Run specific test suite
devtools::test(filter = "validation-dsl")

# See detailed output
devtools::test(reporter = "summary")
```

## Performance

| Operation | Time | Notes |
|-----------|------|-------|
| Parse DSL rule | ~10ms | One-time at startup |
| Field validation | <5ms | Per field in Shiny |
| Form validation | <50ms | 10-field form |
| QC query | 100-500ms | ~10K records |
| Batch QC (50 rules) | 5-60 sec | Full dataset |

## Documentation

- **VALIDATION_DSL_GUIDE.md** - Complete DSL syntax with 30+ examples
- **help pages** - Use `?function_name` for detailed documentation
- **tests/testthat/** - 218+ test cases showing real-world usage

## License

GPL-3 + file LICENSE

## Author

Ronald G. Thomas (rgthomas47@gmail.com)

## Related Projects

- **zzedc** - Full EDC application built on zzedc.validation
- **zzcollab** - Framework for reproducible research in R
