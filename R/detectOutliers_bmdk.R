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
#' @importFrom 
detectOutliers_bmdk <- function(dat)
{
  sourceCpp('knnC.cpp') # Where should this line ideally be located? README file?
  nnInfo <- knnC(nrow= ncol(dat$feat), ncol= nrow(dat$feat), f= c(t(dat$feat)))
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

#################### Old knn algoritm written in R ###################
# # Search for Outliers:
# nnIdx <- numeric(nrow(dat$feat))
# 
# nnDist <- numeric(nrow(dat$feat))
# 
# for (i in 1:nrow(dat$feat)) {
# 
#   neighborsDist <- rep(Inf, nrow(dat$feat))
# 
#   for (k in 1:nrow(dat$feat)) {
# 
#     # Is this bad practice? Should I change this?
#     if (k == i) { neighborsDist[k] <- Inf}
#     else {
#       neighborsDist[k] <- sum( abs(dat$feat[i, ] - dat$feat[k, ])) }
#   }
# 
#   nnDist[i] <- min(neighborsDist)
#   nnIdx[i] <- which(neighborsDist == nnDist[i])
# }

############################################################################
