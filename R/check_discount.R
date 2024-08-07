
#
check_discount_asserts <- function(x) {
  if (!is.numeric(x)) {
    stop(asserthta_type_error("Non-integer error"))
  } else if (x < 0) {
    stop(asserthta_value_error("Negative value error"))
  } else if (x == 0) {
    warning(asserthta_value_warning("Zero value warning"))
  } else {
    invisible()
  }
}

#
check_discount <- function(x, stop_if_not = TRUE) {
  
  if (!stop_if_not) return(x)
  
  result <- tryCatch(
    {
      check_discount_asserts(x)
    },
    
    asserthta_type_error = function(e) {
      message("Handling asserthta_type_error: ", e$message)
      # abort_execution("Encountered a negative value. Aborting...")
    },
    asserthta_type_warning = function(e) {
      message("warning:", e$emesage)
      NULL
    },
    asserthta_value_error = function(e) {
      message("Handling asserthta_value_error: ", e$message)
      # abort_execution("Encountered zero. Aborting...")
    },
    error = function(e) {
      # Generic error handler for any other unexpected errors
      message("An unexpected error occurred: ", e$message)
    }
  )
  
  return(result)
}

