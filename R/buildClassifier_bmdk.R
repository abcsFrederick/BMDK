#' Builds the distance-dependent k-Nearest Neighbors Classifier based on the
#' topfeatures list. Constructs a one-feature, two-feature, and three-feature
#' model and selects the best feature(s) for each model.
#'
#' @param dat a list containing 5 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat; testresults, a list of statistical test results;
#'        topfeatures, a list of top features names
#' @return
#' @export
#' @importFrom stats cor

buildClassifier_bmdk <- function(dat, m = 'manhattan', numNN = 6) {
  
  # Vectors to store the results of ddknn threshold contingency tables
  results <- numeric(length(dat$topfeatures))
  sensitivity <- numeric(length(dat$topfeatures))
  specificity <- numeric(length(dat$topfeatures))
  unknowns <- numeric(length(dat$topfeatures))
  
  top <- dat$feat[ , dat$topfeatures] # Top features subset of the features data
  
  for (j in 1:ncol(top)) { # For each feature in dat$topfeatures...
    
    distMat <- as.matrix(dist(top[, j], method = m)) # Distance matrix
    
    constant <- max(distMat) / 20 # Probability constant, alpha
    
    diag(distMat) <- Inf
    
    # Threshold model contingency table and unknown counter
    threshCTable <- matrix(rep(0, 4), nrow = 2)
    dimnames(threshCTable) = list(c('predicted case', 'predicted control'),
                                  c('actual case', 'actual control'))
    
    threshNumUnknown <- 0
    
    for (k in 1:ncol(distMat)) { # For each sample in the distance matrix...
      
      caseProb <- 0
      controlProb <- 0
      unknownProb <- 0
      
      for (i in 1:numNN) { # Calculate the 6 nearest neighbors to sample k...
        
        minDist <- min(distMat[, k]) # Min distance from sample k
        minIdx <- which(distMat[, k] == minDist) # Row index
        minName <- rownames(distMat)[minIdx] # Name of the nn sample
        minCase <- dat$case[which(rownames(dat$feat) == minName)] # Case status of nn sample
        
        p <- constant / minDist # Probability of same case status as nn
        
        # Add to the caseProb count if nn is a case, add to the controlProb
        # count if nn is a control
        if (minCase == 1) {
          caseProb <- caseProb + p
        } else { controlProb <- controlProb + p}
        
        unknownProb <- unknownProb + 0.1
        
        distMat[minIdx, k] <- Inf
      }
      
      # Normalize the probability values
      pSum <- caseProb + controlProb + unknownProb
      caseProb <- caseProb / pSum
      controlProb <- controlProb / pSum
      unknownProb <- unknownProb / pSum 
      
      actualCase <- dat$case[which(rownames(dat$feat) == colnames(distMat)[k])]
      
      # Update the threshold contingency table
      if (caseProb > 0.5) {
        if(actualCase == 1) {
          threshCTable[1,1] <- threshCTable[1,1] + 1 # True positive
        } else{
          threshCTable[1,2] <- threshCTable[1,2] + 1 # False positive
        }
      } else if(controlProb > 0.5) {
        if(actualCase == 1) {
          threshCTable[2,1] <- threshCTable[2,1] + 1 # False negative
        } else{
          threshCTable[2,2] <- threshCTable[2,2] + 1 # True negative
        }
      } else { # Unknown probability > 0.5
        threshNumUnknown <- threshNumUnknown + 1
      }
    }
    
    # Determine the sensitivity, specificity, and test statistic for feature j
    tp <- threshCTable[1, 1] # True positive
    tn <- threshCTable[2, 2] # True negative
    fp <- threshCTable[1, 2] # False positive
    fn <- threshCTable[2, 1] # False negative
     
    sensitivity[j] <- tp / (tp + fn) * 100
    specificity[j] <- tn / (tn + fp) * 100
    unknowns[j] <- threshNumUnknown /length(top[, j])
    results[j] <- sensitivity[j] + specificity[j] - unknowns[j] # Test statistic
  }
  
  # Select the best feature based on the threshold model
  bestFeat <- max(results)
  bestIdx <- which(results == bestFeat)
  bestFeatName <- colnames(top)[bestIdx]
  bestSensitivity <- sensitivity[bestIdx]
  bestSpecificity <- specificity[bestIdx]
  bestUnknown <- unknowns[bestIdx]
  
  # Generate the maximum likelihood model results using the bestFeat
  distMat <- as.matrix(dist(top[, bestIdx], method = m))
  constant <- max(distMat) / 20
  diag(distMat) <- Inf
  maxCTable <- matrix(rep(0, 4), nrow = 2)
  dimnames(maxCTable) = list(c('predicted case', 'predicted control'),
                                c('actual case', 'actual control'))
  maxNumUnknown <- 0
  
  for (k in 1:ncol(distMat)) {
    
    caseProb <- 0
    controlProb <- 0
    unknownProb <- 0
    
    for (i in 1:numNN) { # Calculate the 6 nearest neighbors to sample k...
      
      minDist <- min(distMat[, k]) # Min distance from sample k
      minIdx <- which(distMat[, k] == minDist) # Row index
      minName <- rownames(distMat)[minIdx] # Name of the nn sample
      minCase <- dat$case[which(rownames(dat$feat) == minName)] # Case status of nn sample
      
      p <- constant / minDist # Probability of same case status as nn
      
      # Add to the caseProb count if nn is a case, add to the controlProb
      # count if nn is a control
      if (minCase == 1) {
        caseProb <- caseProb + p
      } else { controlProb <- controlProb + p}
      
      unknownProb <- unknownProb + 0.1
      
      distMat[minIdx, k] <- Inf
    }
    
    # Normalize the probability values
    pSum <- caseProb + controlProb + unknownProb
    caseProb <- caseProb / pSum
    controlProb <- controlProb / pSum
    unknownProb <- unknownProb / pSum
    
    actualCase <- dat$case[which(rownames(dat$feat) == colnames(distMat)[k])]
    
    # Update maximum likelihood table
    maxProb <- max(caseProb, controlProb, unknownProb)
    
    if (maxProb == caseProb) {
      
      if(actualCase == 1) {
        maxCTable[1,1] <- maxCTable[1,1] + 1 # True positive
      } else {
        maxCTable[1,2] <- maxCTable[1,2] + 1 # False positive
        }
    } else if (maxProb == controlProb) {
      
      if(actualCase == 1) {
        maxCTable[2,1] <- maxCTable[2,1] + 1 # False negative
      } else {
        maxCTable[2,2] <- maxCTable[2,2] + 1 # True negative
      }
    } else { # Unknown is the highest
      maxNumUnknown <- maxNumUnknown + 1
    }
  }
  
  # Determine the maximum likelihood model sensitivity, specificity, and test
  # statistic for the top feature
  tp <- maxCTable[1, 1] # True positive
  tn <- maxCTable[2, 2] # True negative
  fp <- maxCTable[1, 2] # False positive
  fn <- maxCTable[2, 1] # False negative
  
  maxSensitivity <- tp / (tp + fn) * 100
  maxSpecificity <- tn / (tn + fp) * 100
  maxUnknown <- maxNumUnknown /length(top[, bestIdx])
  maxScore <- maxSensitivity + maxSpecificity - maxUnknown
  
  cat('The best feature for a one-feature model is', bestFeatName, '\n')
  
  cat('\nThreshold Model: \nScore:', bestFeat,'\nSensitivity:', bestSensitivity, '\nSpecificity:',
      bestSpecificity, '\nNumber of Unknowns:', bestUnknown * length(top[ ,bestIdx]), '\n')
  
  cat('\nMaximum Likelihood Model: \nScore:', maxScore, '\nSensitivity:', maxSensitivity, '\nSpecificity:',
       maxSpecificity, '\nNumber of Unknowns:', maxUnknown * length(top[ ,bestIdx]))
}
