#' Runs the extreme algorithm on the features data.
#' 
#' The algorithm orders all intensities from lowest to highest and starting
#' from both extremes finds the maximum number of samples from a single State
#' before a sample from another State is observed.
#' 
#' @param dat a list containing 4 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat; minfeat, a list of min features for each feature
#' @return eresults, a vector of extreme algorithm maximum values.
#' @export
extreme <- function(dat)
{
  
  eresults <- numeric(nrow(dat$feat))
  
  for (j in 1:ncol(dat$feat)) {
    
    idxSorted <- order(dat$feat[ , j])
    
    lowerMax <- 1
    upperMax <- length(dat$case)
    
    continue <- TRUE
    startStatus <- dat$case[idxSorted[1]]
    
    while (continue == TRUE && lowerMax < length(dat$case)) {
      if (dat$case[idxSorted[lowerMax + 1]] == startStatus) {
        lowerMax <- lowerMax + 1
      }
      else { continue <- FALSE}
    }
    
    continue <- TRUE
    startStatus <- dat$case[idxSorted[length(dat$case)]]
    
    while (continue == TRUE && upperMax > 1) {
      if(dat$case[idxSorted[upperMax - 1]] == startStatus) {
        upperMax <- upperMax - 1
      }
      else { continue <- FALSE}
    }
    
    eresults[j] <- max(lowerMax, length(dat$case) - upperMax + 1)
  }
  
  return(eresults)
}
