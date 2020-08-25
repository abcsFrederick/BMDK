#' Runs a Decision Tree Gini Index
#' 
#' This procedure uses each feature to perform a one-node split of all N samples
#' in the parent node into 2 daughter nodes using N-1 cut points, as in a
#' decision tree. This procedure's goal is to find the most optimal split to
#' identify between cases and controls for each feature. To do this, this function
#' identifies the smallest weighted Gini Index value for each feature.
#'
#' @param dat a list containing 3 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat
#' @return finalresults a list of Gini Indices
#' @export
dtgini <- function(dat)
{
  # Initialize finalresults, a list with length equal to the number of features
  # in dat$feat
  finalresults <- numeric(ncol(dat$feat))
  
  # Record the number of samples (rows) to be used in the Gini Index calculations
  numsamples <- nrow(dat$feat)
  
  # For each feature (column) in dat$feat...
  for (j in 1:ncol(dat$feat))
  {
    # Initialize colresults, a list used to store the weighted Gini Indices of
    # one column
    colresults <- numeric(nrow(dat$feat)-1)
    
    # Sort column j in ascending order and store the sorted indices in idxsorted
    idxsorted <- order(dat$feat[ , j])
    
    # ...Move from feature value to feature value and calculate the Gini Index
    for (i in 1:(nrow(dat$feat)-1))
    {
      # If feature value [idxsorted[i],j] is not equal to feature value
      # [idxsorted[i+1],j], calculate the Gini Index
      if (dat$feat[idxsorted[i], j] != dat$feat[idxsorted[i+1], j])
      {
        
        # First, calculate the midpoint between value [i,j] and value [i+1,j]
        # *** NOTE: Do I even need to calculate the midpoint? ***
        #midpt <- 0.5 * (dat$feat[i, j] + dat$feat[i+1, j])
        
        # Split the values into two daughter nodes at the value midpt and calculate
        # both nodes' Gini Index
        
        # Daughter Node 1
        d1numcases <- sum(dat$case[idxsorted[1:i]] == 1)
        d1numcontrols <- sum(dat$case[idxsorted[1:i]] == 0)
        d1length <- length(idxsorted[1:i])
        
        d1gini <- 1 - ((d1numcases/d1length)^2 + (d1numcontrols/d1length)^2)
        
        # Daughter Node 2
        idxend <- idxsorted[(i+1):length(idxsorted)]
        
        d2numcases <- sum(dat$case[idxend] == 1)
        d2numcontrols <- sum(dat$case[idxend] == 0)
        d2length <- length(idxend)
        
        d2gini <- 1 - ((d2numcases/d2length)^2 + (d2numcontrols/d2length)^2)
        
        # Calculate the weighted sum for this split and store it in colresults
        finalgini <- (d1gini * (d1length/numsamples)) + (d2gini * (d2length/numsamples))
        
        colresults[i] <- finalgini
      }
    }
    
    # For each feature, find the smallest weighted Gini Index and store it in finalresults
    finalresults[j] <- min(colresults)
  }
  
  return(finalresults)
}