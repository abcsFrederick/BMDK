#' Write documentation here
#'
#' @param dat a list containing 3 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat
#' @return NEED TO WRITE
#' @export

filter_bmdk <- function(dat)
{
  # Initialize vectors to store the results of each test
  wresults <- numeric(ncol(dat$feat))
  tresults <- numeric(ncol(dat$feat))
  
  # Run the features data through a series of tests and identify each datum's p-value
  for(i in 1:ncol(dat$feat))
  {
    # Wilcoxon Rank Sum test
    wresults[i] <- wilcox.test(dat$feat[dat$case == 1,i], dat$feat[dat$case == 0,i], exact = F)[[3]]
    
    # Two Sample t-test
    tresults[i] <- t.test(dat$feat[dat$case == 1,i], dat$feat[dat$case == 0,i])[[3]]
  }
  
  # Store all of the test results in testresults, a list of numeric vectors
  testresults <- list(wresults,
                      tresults)
  names(testresults) <- c('wresults', 'tresults')
  
  ### ERROR: MULTI-ARGUMENT RETURNS ARE NOT PERMITTED ###
  return(dat,         # List containing 3 elements
         testresults) # List of numeric vectors
}