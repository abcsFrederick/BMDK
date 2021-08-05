#' Runs a Decision Tree using 3 different methods: class, anova (gini index),
#' and poisson.
#' 
#' This procedure uses each feature to perform a one-node split of all N samples
#' in the parent node into 2 daughter nodes using N-1 cut points, as in a
#' decision tree. This procedure's goal is to find the most optimal split to
#' identify between cases and controls for each feature. To do this, this function
#' identifies the smallest weighted value for each test for each feature.
#' 
#' @param dat a list containing 4 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat; minfeat, a list of min features for each feature
#' @return a list containing 3 different test result vectors
#' @export
#' 
dtgini <- function(dat)
{
  featDf <- as.data.frame(dat$feat)
  
  cresults <- numeric(ncol(dat$feat))
  gresults <- numeric(ncol(dat$feat))
  presults <- numeric(ncol(dat$feat))
  
  for (j in 1:ncol(dat$feat)) {

    # Decision Tree class
    cTree <- rpart(dat$case ~ featDf[ , j], data = featDf, method = 'class')
    if (is.null(cTree$splits[1,3])) {
      cresults[j] <- 0
    } else {cresults[j] <- cTree$splits[1,3]}
    
    # Decision Tree Gini Index
    gTree <- rpart(dat$case ~ featDf[ , j], data = featDf, method = 'anova')
    if (is.null(gTree$splits[1,3])) {
      gresults[j] <- 0
    } else {gresults[j] <- gTree$splits[1,3]}
    
    
    # Decision Tree poisson
    pTree <- rpart(dat$case ~ featDf[ , j], data = featDf, method = 'poisson')
    if (is.null(pTree$splits[1,3])) {
      presults[j] <- 0
    } else {presults[j] <- pTree$splits[1,3]}
    
  }
  
  return(list(cresults = cresults,     # Vector of decision tree class results
              gresults = gresults,     # Vector of decision tree gini index results
              presults = presults))    # Vector of decision tree poisson results
}
  