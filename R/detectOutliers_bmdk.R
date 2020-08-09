#' Detect outliers in the BMDK data
#'
#' Using the k Nearest Neighbours Distance method, detect outliers in the data and
#' remove them. After removing the outliers, renormalize the data.
#'
#' @param dat a list containing 3 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat
#' @return List containing 3 elements (case, feat, maxfeat) with the outliers removed

detectOutliers_bmdk <- function(dat)
{
  # Look for outliers using the OutlierDetection package
  # Store the locations of the outliers in outlierlocs
  outlierlocs <- nn(dat$feat, k = 2, Method = 'manhattan', cutoff = 0.95, boottimes = 100)[[2]]
  
  
  if (length(outlierlocs) > 0) {
    # Remove outliers from the training set
    dat$case <- dat$case[-outlierlocs]
    dat$feat <- dat$feat[-outlierlocs, ]
    
    # Renormalize dat using the function renormalize_bmdk()
    renormalize_bmdk(dat)
    
  }
  
  return(dat)
}