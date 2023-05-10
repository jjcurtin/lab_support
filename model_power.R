model_power <- function(pc = NULL, pa = NULL, N = NULL, alpha = 0.05, power = NULL, 
                        f2 = NULL, partial_eta2 = NULL, delta_R2 = NULL, R2 = NULL) {
  

  # check that effect sizes were specified correctly
  if (sum(sapply(list(f2, partial_eta2, delta_R2), is.null)) != 2) {
    stop("Must specify exactly one of f2, partial_eta2, or delta_R2")
  }
  
  if (!is.null(delta_R2) & is.null(R2)) stop("Must specify R2 if providing delta_R2")
  
  
  # calculate f2 effect size based on effect size that was provided
  if (!is.null(partial_eta2)) {
    f2 <- partial_eta2/(1 - partial_eta2)
  } else partial_eta2 <- NA
  
  if (!is.null(delta_R2)) {
    f2 <- delta_R2/(1 - R2)
  } else {
    delta_R2 <- NA
    R2 <- NA
  }
  

  # set up u and v if needed
  u <- pa - pc
  
  if (!is.null(N)) {
    v <- N - pa
  } else {
    v <- NULL
  }
  
  
  # conduct power analysis
  results <- list()
  results$pwr <- pwr::pwr.f2.test(u = u, v = v, f2 = f2 , sig.level = alpha, power = power)
  
  results$summary <- dplyr::tibble(pa = pa,
                            pc = pc,
                            f2 = f2,
                            partial_eta2 = partial_eta2,
                            delta_R2 = delta_R2,
                            R2 = R2,
                            alpha = alpha,
                            N = v + pa,
                            power = results$pwr$power)
  
  return(results)
} 
