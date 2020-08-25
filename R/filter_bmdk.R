#' Runs the BMDK features data through a series of filtering methods
#' 
#' Utilizes the Wilcoxon Rank Sum test, the t test, and the Decision Tree Gini
#' Index to identify the significance of each feature.
#'
#' @param dat a list containing 3 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat
#' @return dat a list containing 4 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat; testresults, a list of statistical test results
#' @export
#' @importFrom stats t.test
#' @importFrom stats wilcox.test
filter_bmdk <- function(dat)
{
  # Initialize vectors to store the results of each test
  wresults <- numeric(ncol(dat$feat))
  tresults <- numeric(ncol(dat$feat))
  gresults <- numeric(ncol(dat$feat))
  
  # Run the features data through a series of tests and identify each datum's significance
  for (i in 1:ncol(dat$feat))
  {
    # Wilcoxon Rank Sum test
    wresults[i] <- wilcox.test(dat$feat[dat$case == 1,i], dat$feat[dat$case == 0,i], exact = F)[[3]]
    
    # Two Sample t-test
    tresults[i] <- t.test(dat$feat[dat$case == 1,i], dat$feat[dat$case == 0,i])[[3]]
  }
  
  # Decision Tree Gini Index
  gresults <- dtgini(dat)
  
  # Store all of the test results in testresults, a list of numeric vectors
  testresults <- list(wresults,
                      tresults,
                      gresults)
  
  # Name each element in testresults
  ### NOTE: How to do this so it is not hardcoded?? ###
  names(testresults) <- c('wresults', 'tresults', 'gresults')
  
  # Add testresults to dat
  dat$testresults <- testresults
  
  return(dat)
}
