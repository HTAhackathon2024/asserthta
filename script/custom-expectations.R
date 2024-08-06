# Define custom error classes
asserthta_type_error <- function(message) {
  structure(list(message = message),
            class = c("asserthta_type_error", "error", "condition"))
}

asserthta_class_error <- function(message) {
  structure(list(message = message),
            class = c("asserthta_class_error", "error", "condition"))
}

# Function that might raise different custom exceptions
dangerous_function <- function(x) {
  if (x < 0) {
    stop(custom_error_a("Negative value error"))
  } else if (x == 0) {
    stop(custom_error_b("Zero value error"))
  } else {
    return(sqrt(x))
  }
}

# Main function that uses tryCatch to handle exceptions
main_function <- function(x) {
  result <- tryCatch(
    {
      # Code to execute that might cause an error
      dangerous_function(x)
    },
    
    custom_error_a = function(e) {
      message("Handling custom_error_a: ", e$message)
      abort_execution("Encountered a negative value. Aborting...")
    },
    
    custom_error_b = function(e) {
      message("Handling custom_error_b: ", e$message)
      abort_execution("Encountered zero. Aborting...")
    },
    error = function(e) {
      # Generic error handler for any other unexpected errors
      message("An unexpected error occurred: ", e$message)
    }
  )
  
  return(result)
}

# Helper function to abort execution
abort_execution <- function(reason) {
  stop(reason)
}

# Test cases
main_function(-1)  # Handling custom_error_a: Negative value error
main_function(0)   # Handling custom_error_b: Zero value error
main_function(4)   # 2



# i'm trying to push something