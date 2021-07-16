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
  # After removing outliers, unnormalize dat
  dat$feat <- t(t(dat$feat) * (dat$maxfeat - dat$minfeat) + dat$minfeat)
  
  # Reevaluate the max feature of each column
  dat$maxfeat <- apply(dat$feat, 2, max)
  
  # Reevaluate the min feature of each column
  dat$minfeat <- apply(dat$feat, 2, min)
  
  # Renormalize dat
  dat$feat <- apply(dat$feat, 2,
                    function(.x){(.x - min(.x, na.rm = TRUE)) /
                        (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))})
  
  return(dat)
}
