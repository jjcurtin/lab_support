library(broom)
library(stringr)

# temp data for testing
d <- tibble(y = rnorm(100), x = rnorm (100))



print_stats <- function(fit, effect, inc_pe = 0, inc_ci = 2, inc_es = 2){
# fit - a model object
# term - name of effect to report (character)
# inc_pe - include parameter estimate.  0 = no, > 0 = number of decimal places
# inc_ci - include 95% ci for parameter estimate.  0 = no, > 0 = number of decimal places
# inc_es - include standardized effect size.  0 = no, > 0 = number of decimal places
  
  coeffs <- tidy(fit)
  if (! effect %in% coeffs$term) stop(effect, " not including in fit")
  coeffs <- coeffs %>% 
    filter(term == effect)
  
  model_info <- glance(fit)
  
  if ("lm" %in% class(fit)) {
    results <- ""
    
    if (inc_ci) {
      ci <- confint(fit)
      ci <- ci[which(term == rownames(ci)), ]
      ci_1 <-  format(round(ci[1], digits = inc_ci), nsmall = inc_ci)
      ci_2 <-  format(round(ci[2], digits = inc_ci), nsmall = inc_ci)
      ci <- str_c("CI(b) = [", ci_1, ", ", ci_2, "]")
      results <- ci
    }
    
    if (inc_pe) {
      pe <- format(round(coeffs$estimate, digits = inc_pe), nsmall = inc_pe)
      pe <- str_c("b = ", pe)
      results <- str_c(results, pe, sep = ", ")
    }
    
    df <- model_info$df
    ts <- format(round(coeff$statistic, digits = 2), nsmall = 2)
    results <- str_c(results, t)
    
  }
  
}

# modelRmd <- function(effect, mod, B=1, CI=B, statistic='t',pe=2)
# {
#   results =''
#   
#   if(any(class(mod)=='anova')){
#     modsum = mod  #mod is already an anova model summary
#     
#     #partial eta squared
#     if (is.numeric(pe)){
#       pestat = (modsum[effect,'F value']*modsum[effect,'Df']) / ((modsum[effect,'F value']*modsum[effect,'Df']) + modsum['Residuals','Df'])
#       
#        if (round(pestat,pe) < (1/10^pe)) {
#         results = paste0(results, '*$\\eta_p^2$* < ', 1/10^pe, ', ')
#        } else {
#         results = paste0(results, '*$\\eta_p^2$* = ', sprintf(paste0('%.',pe,'f'), pestat), ', ')
#        }  
#     }
#     
#     #add F
#     results = paste0(results,'*F*(', modsum[effect,'Df'], ',',modsum['Residuals','Df'],') = ', sprintf('%.2f',modsum[effect,'F value']), ', ')
#     
#     #add p
#     if (round(modsum[effect,'Pr(>F)'],3) < 0.001){
#       results = paste0(results, '*p* <  .001')
#     }else{
#       results = paste0(results, '*p* =  ', sprintf('%.3f',modsum[effect,'Pr(>F)']))
#     }
#     
#   } 
#   else{
#     modsum = modelSummary(mod)  #get an lm model summary
#   
#     if (is.numeric(pe)){
#       pestat = (abs(coef(modsum)[effect,'t'])^2*1)/(abs(coef(modsum)[effect,'t'])^2*1 + mod$df.residual)
#       if (round(pestat,pe) < (1/10^pe)) {
#         results = paste0(results, '*$\\eta_p^2$* < ', 1/10^pe, ', ')
#       } else {
#         results = paste0(results, '*$\\eta_p^2$* = ', sprintf(paste0('%.',pe,'f'), pestat), ', ')
#       }  
#     }
#     if (is.numeric(B)){
#       results = paste0(results,'*b* = ', sprintf(paste0('%.',B,'f'),coef(mod)[effect]), ', ')
#     }
#     
#     if (is.numeric(CI)){
#       results = paste0(results,'*95% CI* = [', sprintf(paste0('%.',CI,'f'),confint(mod)[effect,1]), ',', sprintf(paste0('%.',CI,'f'),confint(mod)[effect,2]), '], ')
#     }
#     
#     #add t
#     if (statistic == 't'){
#       results = paste0(results,'*t*(', mod$df.residual, ') = ', sprintf('%.2f',abs(coef(modsum)[effect,'t'])), ', ')
#     }  
#     
#     #add p
#     if (round(coef(modsum)[effect,'Pr(>|t|)'],3) < 0.001){
#       results = paste0(results, '*p* <  .001')
#     }else{
#       results = paste0(results, '*p* =  ', sprintf('%.3f',coef(modsum)[effect,'Pr(>|t|)']))
#     }
#   
#   }
#   
#   
#   return(results)
# }