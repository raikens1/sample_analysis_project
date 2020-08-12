# ------------------------------------------------------------------------------
# DIAGNOSE DATA
# ------------------------------------------------------------------------------

#' Diagnose Cross-sectional
#'
#' @param df cross-sectional dataset
#' @param theta, parameters for diagnosis function (default \code{c(-10, 20, -5)})
#'
#' @return
#' @export
diagnose_cross_sectional <- function(df, theta = c(-10, 20)){
  n_row <- dim(df)[1]

  result <- df %>%
    mutate(p_diagnose = diagnosis_fn(severity, theta)) %>%
    mutate(diagnosed = rbinom(n_row, size = 1, p = p_diagnose)) %>%
    select(-p_diagnose)

  return(result)
}

#' Calculate Diagnosis Probability
#'
#' @param severity disease severity (in [0,1])
#' @param theta numeric vector giving parameters of diagnostic function (default
#'   \code{c(-10, 20)})
#'
#' @return
#' @export
diagnosis_fn <- function(severity, theta = c(-10, 20)){
  c <- 1/(1 + exp(-(theta[1])))

  return(1/(1 + exp(-(theta[1] + theta[2] * severity))) - c)
}
