#' Detect outliers in the BMDK data
#'
#' Using the k Nearest Neighbours Distance method, detect outliers in the data
#' and remove them. After removing the outliers, renormalize the data.
#'
#' @param dat a list containing 3 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat
#' @return List containing 3 elements (case, feat, maxfeat) with the outliers removed
#' @export 
#' @importFrom stats sd
detectOutliers_bmdk <- function(dat)
{
  nnInfo <- knn(nrow= ncol(dat$feat), ncol= nrow(dat$feat), f= c(t(dat$feat)))
  nnDist <- nnInfo[[1]]
  nnIdx <- nnInfo[[2]]
  
  upperBound <- mean(nnDist) + 3*sd(nnDist)
  
  outlierlocs <- nnIdx[nnDist > upperBound]
  
  if (length(outlierlocs) > 0)
  {
    # Remove outliers from the training set
    dat$case <- dat$case[-outlierlocs]
    dat$feat <- dat$feat[-outlierlocs, ]
    
    # Renormalize dat using the function renormalize_bmdk()
    renormalize_bmdk(dat)
    
  }
  
  return(dat)
}