# ------------------------------------------------------------------------------
# GENERATE UNDIAGNOSED DATA
# ------------------------------------------------------------------------------

#' Generate Cross Sectional Data
#'
#' @param n sample size
#' @param prevalence disease prevalence
#'
#' @return data frame of n individuals baseline binary covariate x and desease
#'   severity
#' @export
generate_cross_sectional <- function(n = 10000, prevalence = 0.15){
  result <- data.frame(
    x = (rbinom(n, 1, 0.5) == 1),
    disease = rbinom(n, 1, prevalence)
    ) %>%
    dplyr::mutate(severity = disease * runif(n))
}

