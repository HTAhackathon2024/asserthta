# Define custom error classes

asserthta_type_error <- function(message) {
  structure(list(message = message),
            class = c("asserthta_type_error", "error", "condition"))
}

asserthta_type_warning <- function(message) {
  structure(list(message = message),
            class = c("asserthta_type_warning", "warning", "condition"))
}

asserthta_value_error <- function(message) {
  structure(list(message = message),
            class = c("asserthta_value_error", "error", "condition"))
}

asserthta_value_warning <- function(message) {
  structure(list(message = message),
            class = c("asserthta_value_warning", "warning", "condition"))
}