# contr_helmert_unit <- function(n, contrasts = TRUE, sparse = FALSE)
# Applies unit-weighting (setting range of cofficients = 1 for all contrasts)
# to base R contr.helmert.  See ?contr.helmert for more info
contr_helmert_unit <- function(n, contrasts = TRUE, sparse = FALSE) {
  
  c <- contr.helmert(n, contrasts, sparse)
  
  for (i in 1:ncol(c))
  {
    c[,i] = c[,i] / (max(c[,i]) - min(c[,i]))
  }  
  return(c)
}

# # Example
# data(iris) # load sample data
# contrasts(iris$Species)
# 
# contrasts(iris$Species) <- contr.helmert(n = length(levels(iris$Species)))
# contrasts(iris$Species) # with base helmert
# 
# contrasts(iris$Species) <- contr_helmert_unit(n = length(levels(iris$Species)))
# contrasts(iris$Species) # unit-weighted
