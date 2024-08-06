#' Check Markov Trace
#'
#' This function checks the properties of a Markov trace conform to expectations,
#' consistent with the specified transition probability matrix.
#' That it is: numeric, values are between 0 and 1 with all rows summing to 1,
#' has the correct number of columns consistent with the transition probability
#' matrix, and that the final row corresponds to the eigenvector of the
#' transition probability matrix that has eigenvalue 1 (this is usually just
#' everyone in the dead state)
#' Also allows users to check that the dead state is monotonically decreasing (if provided).
#'
#' @param m_TR The markov trace to be checked.
#' @param m_P The transition probability matrix.
#' @param stop_if_not return error messages. The default (F) returns warnings.
#' @param confirm_ok if OK, return a message confirming all checks passed.
#' @param dead_state character vector length 1 denoting dead state (e.g. "D")
#'
#' @examples
#' \dontrun{
#' v_hs_names <- c("H", "S", "D")
#' n_hs <- length(v_hs_names)
#' n_t <- 10
#' 
#' #' v_hs_names <- c("H", "S", "D")
#' n_hs <- length(v_hs_names)
#' m_P <- matrix(data = 0, nrow = n_hs, ncol = n_hs,
#'               dimnames = list(v_hs_names, v_hs_names))
#' m_P["H", "S"] <- 0.3
#' m_P["H", "D"] <- 0.01
#' m_P["S", "D"] <- 0.1
#' m_P["S", "H"] <- 0.5
#' diag(m_P) <- (1 - rowSums(m_P))
#' 
#' x <- c(1, 0, 0)
#' m_TR <- x
#' n <- 1000
#' for (i in 1:n) {
#'   x <- x%*%m_P
#'   m_TR <- rbind(m_TR, x)
#' }
#' m_TR |> 
#'   dplyr::as_tibble() |> 
#'   dplyr::mutate(t = dplyr::row_number()) |> 
#'   tidyr::pivot_longer(cols = c(H, S, D)) |> 
#'   ggplot(aes(x = t, y = value, group = name, colour = name)) +
#'   geom_line()
#' 
#' check_markov_trace(m_TR = m_TR, m_P = m_P, dead_state = "D", confirm_ok = T)
#' 
#' m_TR[10, "D"] <- 0
#' m_TR[9, "S"] <- 1
#' check_markov_trace(m_TR = m_TR, m_P = m_P, stop_if_not = T, dead_state = "D", confirm_ok = T)
#' }
#'
#' @return A message indicating whether the matrix passed all the checks or an error message if any check failed.
#'
#' @import assertthat
#'
#' @export
check_markov_trace <- function(m_TR,
                               m_P,
                               dead_state = NULL,
                               confirm_ok = F,
                               stop_if_not = F) {
  
  # Check that the trace has two dimensions
  if (length(dim(m_TR)) != 2) stop("Markov Trace is not two-dimensional")
  
  # Check the trace is named!
  m_TR_colnames <- colnames(m_TR)
  if (any(is.na(m_TR_colnames)) | any(is.null(m_TR_colnames))) warning("Markov Trace is missing one or more column names")
  if (length(unique(m_TR_colnames)) != ncol(m_TR)) stop("Markov Trace has duplicate column names")
  
  # Start with no warnings
  no_warnings <- T
  
  # Check that the matrix contains numeric values
  if (!all(apply(m_TR, MARGIN = 2, is.numeric))) stop("Markov trace is not numeric")
  
  # Check that matrix values are between 0 and 1
  if (!all(m_TR >= 0 & m_TR <= 1)) {
    message <- "Markov Trace has values below 0 or above 1"
    no_warnings <- F
    if (stop_if_not) {
      stop(message)
    } else{
      warning(message)
    }
  }
  
  # Check that rows sum to 1, indicating valid transition probabilities
  if (any(abs(rowSums(m_TR) - 1) > 1E-08)){
    message <- "Rows of Markov Trace don't sum to 1."
    no_warnings <- F
    if (stop_if_not) {
      stop(message)
    } else{
      warning(message)
    }
  }
  
  # Check that the number of columns is consistent with the transition
  # probability matrix
  if (ncol(m_TR) != ncol(m_P)) {
    message <- "Markov Trace does not have same number of columns as transition
    probability matrix"
    no_warnings <- F
    if (stop_if_not) {
      stop(message)
    } else{
      warning(message)
    }
  }
  
  # Check that the final row corresponds to the eigenvector of the
  # transition probability matrix that has eigenvalue 1 (this is usually just
  # everyone in the dead state)
  diag <- eigen(t(m_P))
  if (m_TR[nrow(m_TR), ] - diag$vectors[, match(1, diag$values)] > 1E8) {
    message <- "Final state in Markov Trace is not as expected."
    no_warnings <- F
    if (stop_if_not) {
      stop(message)
    } else{
      warning(message)
    }
  }
  
  # Check dead state values are monotonic increasing
  if(!is.null(dead_state)){
    # Check that rows sum to 1, indicating valid transition probabilities
    if(!all(diff(x = m_TR[, dead_state]) >= 0)){
      no_warnings <- F
      message <- "Decreasing proportion in the dead state of trace, is this correct?"
      if (stop_if_not) {
        stop(message)
      } else{
        warning(message)
      }
    }
  }
  
  # Return a message indicating successful checks
  if(confirm_ok & no_warnings) return("Markov Trace passed all checks.")
}
