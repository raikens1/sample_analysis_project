# ------------------------------------------------------------------------------
# GENERATE UNDIAGNOSED DATA
# ------------------------------------------------------------------------------

generate_cross_sectional <- function(n = 10000, prevalence = 0.15){
  # generate data with id, disease, severity
  result <- data.frame(id = 1:n, disease = rbinom(n, 1, prevalence)) %>%
    dplyr::mutate(severity = disease * runif(n))
  
  return(result)
}

