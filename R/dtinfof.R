#' Runs a Decision Tree Information Gain and a Fisher Test
#' 
#' This procedure uses each feature to perform a one-node split of all N samples
#' in the parent node into 2 daughter nodes using N-1 cut points, as in a
#' decision tree. This procedure's goal is to find the most optimal split to
#' identify between cases and controls for each feature. To do this, this function
#' identifies the smallest weighted Information Gain value for each feature.
#' 
#' This procedure also uses a Fisher Test to find the most optimal split to
#' identify between cases and controls for each feature by minimizing the Fisher
#' Test p-value.
#' 
#' @param dat a list containing 3 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat
#' @return finalresults a list of Information Gain sums, and Fisher Test p-values.
#' @importFrom stats fisher.test
#' @export
dtinfof <- function(dat)
{
  # Initialize vectors to store each feature's minimum test statistic for each
  # test
  ifinalresults <- numeric(ncol(dat$feat))
  ffinalresults <- numeric(ncol(dat$feat))
  
  # Record the number of samples (rows)
  numsamples <- nrow(dat$feat)
  
  # For each feature (column) in dat$feat...
  for (j in 1:ncol(dat$feat)) {
    
    # Initialize vectors to store the values of one column (InfoGain, Fisher)
    icolresults <- numeric(nrow(dat$feat)-1)
    fcolresults <- numeric(nrow(dat$feat)-1)
    
    # Sort column j in ascending order and store the sorted indices in idxsorted
    idxsorted <- order(dat$feat[ , j])
    
    # ...iterate through the rows and calculate the Information Gain and Fisher
    # Test p-value
    for (i in 1:(nrow(dat$feat)-1))
    {
      
      # If neither value is NA, and if feature value [idxsorted[i],j] is not
      # equal to feature value [idxsorted[i+1],j], run the tests
      if (!is.na(dat$feat[idxsorted[i], j]) && !is.na(dat$feat[idxsorted[i+1], j])) {
        if (dat$feat[idxsorted[i], j] != dat$feat[idxsorted[i+1], j])
        {
          ############# Daughter Node Values #########
          
          d1numcases <- sum(dat$case[idxsorted[1:i]] == 1)
          d1numcontrols <- sum(dat$case[idxsorted[1:i]] == 0)
          d1length <- length(idxsorted[1:i])
          d1caseprob <- d1numcases/d1length
          d1controlprob <- d1numcontrols/d1length
          
          idxend <- idxsorted[(i+1):length(idxsorted)]
          d2numcases <- sum(dat$case[idxend] == 1)
          d2numcontrols <- sum(dat$case[idxend] == 0)
          d2length <- length(idxend)
          d2caseprob <- d2numcases/d2length
          d2controlprob <- d2numcontrols/d2length
          
          
          ############# INFORMATION GAIN #############

          # Daughter 1
          if (d1caseprob == 0 | d1controlprob == 0) {
            d1infog <- 0
          } else {
            d1infog <- -((d1caseprob*log2(d1caseprob)) +
                           (d1controlprob*log2(d1controlprob)))
          }
          
          # Daughter 2
          if (d2caseprob == 0 | d2controlprob == 0) {
            d2infog <- 0
          } else {
            d2infog <- -((d2caseprob*log2(d2caseprob)) +
                           (d2controlprob*log2(d2controlprob)))
          }
          
          icolresults[i] <- sum(d1infog * (d1length/numsamples),
                                d2infog * (d2length/numsamples))
          
          
          ############# FISHER TEST #############

          cTable <- matrix(c(d1numcases, d1numcontrols, d2numcases, d2numcontrols),
                      nrow = 2, byrow = TRUE,
                      dimnames = list(c('below cutoff', 'above cutoff'), c('case', 'control')))
          
          fcolresults[i] <- fisher.test(cTable, conf.int = FALSE)$p.value
        }
      }
    }
    
    # Find the minimum test statistic for each test per feature
    ifinalresults[j] <- min(icolresults)
    ffinalresults[j] <- min(fcolresults)
  }
  
  # Package the results into a list to return
  finalresults <- list(ifinalresults,
                       ffinalresults)
  
  names(finalresults) <- c('iresults', 'fresults')
  
  return(finalresults)
}
