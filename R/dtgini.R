#' Runs a Decision Tree using 3 different methods: class, anova (gini index),
#' and poisson.
#' 
#' This procedure uses each feature to perform a one-node split of all N samples
#' in the parent node into 2 daughter nodes using N-1 cut points, as in a
#' decision tree. This procedure's goal is to find the most optimal split to
#' identify between cases and controls for each feature. To do this, this function
#' identifies the smallest weighted value for each test for each feature.
#' 
#' @param dat a list containing 3 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat
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
    cTree <- rpart(dat$case ~ featDf[ , j], data = featDf,
                   method = 'class',
                   control = list(maxdepth = 1))
    if (is.null(cTree$split[4])) {
      cresults[j] <- 0
    } else { cresults[j] <- cTree$split[4] }
    
    
    # Decision Tree Gini Index
    gTree <- rpart(dat$case ~ featDf[ , j], data = featDf,
                   method = 'anova',
                   control = list(maxdepth = 1))
    if (is.null(gTree$split[4])) {
      gresults[j] <- 0
    } else { gresults[j] <- gTree$split[4] }
    
    
    # Decision Tree poisson
    pTree <- rpart(dat$case ~ featDf[ , j], data = featDf,
                   method = 'poisson',
                   control = list(maxdepth = 1))
    if (is.null(pTree$split[4])) {
      presults[j] <- 0
    } else { presults[j] <- pTree$split[4] }
    
  }
  
  return(list(cresults = cresults,     # Vector of decision tree class results
              gresults = gresults,     # Vector of decision tree gini index results
              presults = presults))    # Vector of decision tree poisson results
}
  