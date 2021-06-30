#' Runs a Decision Tree Gini Index, Decision Tree Information Gain, and a Fisher Test
#' 
#' This procedure uses each feature to perform a one-node split of all N samples
#' in the parent node into 2 daughter nodes using N-1 cut points, as in a
#' decision tree. This procedure's goal is to find the most optimal split to
#' identify between cases and controls for each feature. To do this, this function
#' identifies the smallest weighted Gini Index value and smallest weighted 
#' Information Gain value for each feature.
#' 
#' This procedure also uses a Fisher Test to find the most optimal split to
#' identify between cases and controls for each feature by minimizing the Fisher
#' Test p-value.
#' 
#' @param dat a list containing 3 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat
#' @return finalresults a list of Gini Indices, Information Gain sums, and Fisher
#'         Test p-values.
#' @importFrom stats fisher.test
#' @export
dtgini <- function(dat)
{
  # Initialize gfinalresults, ifinalresults, and fcolresults, lists with column length equal
  # to the number of features in dat$feat
  gfinalresults <- numeric(ncol(dat$feat))
  ifinalresults <- numeric(ncol(dat$feat))
  ffinalresults <- numeric(ncol(dat$feat))
  
  # Record the number of samples (rows) to be used in the Gini Index calculations
  numsamples <- nrow(dat$feat)
  
  # For each feature (column) in dat$feat...
  for (j in 1:ncol(dat$feat)) {
    # Initialize gcolresults, a list used to store the weighted Gini Indices of
    # one column
    gcolresults <- numeric(nrow(dat$feat)-1)
    
    # Initialize icolresults, a list used to store the Information Gain values
    # of one column
    icolresults <- numeric(nrow(dat$feat)-1)
    
    # Initialize fcolresults, a list used to store the Fisher Test p-values
    # of one column
    fcolresults <- numeric(nrow(dat$feat)-1)
    
    # Sort column j in ascending order and store the sorted indices in idxsorted
    idxsorted <- order(dat$feat[ , j])
    
    # ...Move from value to value and calculate the Gini Index and Information
    # Gain
    for (i in 1:(nrow(dat$feat)-1))
    {
      
      # If neither value is NA...
      if (!is.na(dat$feat[idxsorted[i], j]) && !is.na(dat$feat[idxsorted[i+1], j])) {
        
        # ...And if feature value [idxsorted[i],j] is not equal to feature value
        # [idxsorted[i+1],j], calculate the Gini Index and Information Gain
        if (dat$feat[idxsorted[i], j] != dat$feat[idxsorted[i+1], j])
        {
          ############# GINI INDEX #############
          
          # Split the values into two daughter nodes and calculate both nodes'
          # Gini Index
          
          # Daughter Node 1
          d1numcases <- sum(dat$case[idxsorted[1:i]] == 1)
          d1numcontrols <- sum(dat$case[idxsorted[1:i]] == 0)
          d1length <- length(idxsorted[1:i])
          d1caseprob <- d1numcases/d1length
          d1controlprob <- d1numcontrols/d1length
          
          d1gini <- 1 - (d1caseprob^2 + d1controlprob^2)
          
          # Daughter Node 2
          idxend <- idxsorted[(i+1):length(idxsorted)]
          
          d2numcases <- sum(dat$case[idxend] == 1)
          d2numcontrols <- sum(dat$case[idxend] == 0)
          d2length <- length(idxend)
          d2caseprob <- d2numcases/d2length
          d2controlprob <- d2numcontrols/d2length
          
          d2gini <- 1 - (d2caseprob^2 + d2controlprob^2)
          
          # Calculate the weighted sum for this split and store it in gcolresults
          gcolresults[i] <- (d1gini * (d1length/numsamples)) + (d2gini * (d2length/numsamples))
          
          
          ############# INFORMATION GAIN #############
          
          # Calculate the Information Gain values of both daughter nodes
          
          # Daughter Node 1
          if (d1caseprob == 0 | d1controlprob == 0) {
            
            d1infog <- 0
            
          } else {
            d1infog <- -((d1caseprob*log(d1caseprob)) +
                           (d1controlprob*log(d1controlprob)))
          }
          
          # Daughter Node 2
          if (d2caseprob == 0 | d2controlprob == 0) {
            
            d2infog <- 0
            
          } else {
            d2infog <- -((d2caseprob*log(d2caseprob)) +
                           (d2controlprob*log(d2controlprob)))
          }
          
          icolresults[i] <- sum(d1infog * (d1length/numsamples), d2infog * (d2length/numsamples))
          
          
          ############# FISHER TEST #############
          cTable <- matrix(c(d1numcases, d1numcontrols, d2numcases, d2numcontrols),
                      nrow = 2, byrow = TRUE,
                      dimnames = list(c('below cutoff', 'above cutoff'), c('case', 'control')))
          fcolresults[i] <- fisher.test(cTable, conf.int = FALSE)$p.value
        }
      }
    }
    
    # Calculate the weighted sum Information Gain for this split and store it
    # in icolresults
    icolresults[i] <- sum(d1infog * (d1length/numsamples), d2infog * (d2length/numsamples))
    
    # For each feature, find the smallest weighted Gini Index and store it
    # in gfinalresults
    gfinalresults[j] <- min(gcolresults)
    
    # For each feature, find the smallest weighted Information Gain value and
    # store it in ifinalresults
    ifinalresults[j] <- min(icolresults)
    
    # For each feature, find the smallest Fisher Test p-value and
    # store it in ffinalresults
    ffinalresults[j] <- min(fcolresults)
  }
  
  # Combine gfinalresults, ifinalresults, and ffinalresults into a single list to return
  finalresults <- list(gfinalresults,
                       ifinalresults,
                       ffinalresults)
  
  names(finalresults) <- c('gresults', 'iresults', 'fresults')
  
  return(finalresults)
}
