#' Removes correlated features from the topfeatures list
#' 
#' Identify the features in topfeatures with the highest max values, and removes
#' Pearson correlated features to them.
#'
#' @param dat a list containing 5 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat; testresults, a list of statistical test results;
#'        topfeatures, a list of top features names
#' @return dat a list containing 5 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat; testresults, a list of statistical test results;
#'        topfeatures, a list of top features names
#' @export
#' @importFrom stats cor

removeFeatures_bmdk <- function(dat) {
  
  top <- dat$topfeatures
  topmax <- numeric(length(top))
  
  for(k in 1:length(top)) {
    topmax[k] <- dat$maxfeat[which(colnames(dat$feat) == top[k])]
  }
  
  topfeaturesfiltered <- c()
  
  while(length(top) > 1) {
    maxval <- max(topmax)
    idx <- which(topmax == maxval)
    topfeaturesfiltered[length(topfeaturesfiltered) + 1] <- top[idx]
    
    top <- top[-idx]
    topmax <- topmax[-idx]
    
    corrVec <- cor(dat$feat[, topfeaturesfiltered[length(topfeaturesfiltered)]],
                   dat$feat[, top], method = 'pearson')
    j <- length(corrVec)
    while(j > 0) {
      if(corrVec[j] > 0.7) {
        top <- top[-j]
        topmax <- topmax[-j]
      }
      j <- j - 1
    }
  }
  
  topfeaturesfiltered[length(topfeaturesfiltered) + 1] <- top[1]
  
  dat$topfeatures <- topfeaturesfiltered
  
  return(dat)
}
