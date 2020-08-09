#' Renormalize the BMDK features data
#'
#' After outliers are removed, renormalize the features data and store the
#' new max value of each feature.
#'
#' @param dat a list containing 3 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat
#' @return List containing 3 elements (case, feat, maxfeat) renormalized after outliers
#'         are removed

renormalize_bmdk <- function(dat)
{
  # After removing outliers, reevaluate the max values of each column (feature) in dat
  ### NEED TO FIGURE OUT HOW TO MULTIPLE MATRIX COLUMNS BY A VECTOR ###
  dat$feat <- dat$feat*dat$maxfeat
  
  # Reevaluate the max feature of each column
  dat$maxfeat <- apply(dat$feat, 2, max)
  
  # Renormalize dat
  dat$feat <- apply(dat$feat, 2, function(.x){.x / max(.x, na.rm = TRUE)})
  
  return(dat)
}