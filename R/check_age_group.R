
#
check_age_group_asserts <- function(x) {
  
  ##TODO: may not be in this format, e.g. [x-y), [x:y)
  ##      look at stringsplit type functions for ideas
  ##TODO: what about at the edge, e.g. 65+, >65, >=65 etc
  ##TODO: maybe create parse_age_group() first?
  min_age <- gsub(pattern = "^\\[([0-9]*)\\)", replacement = "\\1", x = agegrp)
  max_age <- gsub(pattern = "\\[[0-9]*,([0-9]*)\\)", replacement = "\\1", x = agegrp)
  
  check_age_asserts(as.numeric(min_age))
  check_age_asserts(as.numeric(max_age))
  
  if (min_age > max_age) {
    stop(asserthta_value_error("Min age is greater than max age"))
  }
  if (min_age == max_age) {
    warning(asserthta_value_warning("Min age is equal to max age"))
  }
}

#
check_age_group <- function(x, stop_if_not = TRUE) {
  
  if (!stop_if_not) return(x)
  
  result <- tryCatch(
    {
      check_age_group_asserts(x)
    },
    
    asserthta_type_error = function(e) {
      message_group("Handling asserthta_type_error: ", e$message_group)
      # upper and/or lower elements of `x` are not numbers. Please ensure they are numeric, e.g. [0,9)
      
      # abort_execution("Encountered a negative value. Aborting...")
    },
    asserthta_type_warning = function(e) {
      message_group("warning:", e$emesage_group)
      NULL
    },
    asserthta_value_error = function(e) {
      message_group("Handling asserthta_value_error: ", e$message_group)
      # abort_execution("Encountered zero. Aborting...")
    },
    asserthta_value_warning = function(e) {
      message_group("Handling asserthta_value_warning: ", e$message_group)
      # upper and/or lower elements of `x` are the same numbers. Is this what you intended?
      # abort_execution("Encountered zero. Aborting...")
    },
    error = function(e) {
      # Generic error handler for any other unexpected errors
      message_group("An unexpected error occurred: ", e$message_group)
    }
  )
  # finally
  
  return(result)
}

