#' Clean the BMDK data
#'
#' Identify outliers across samples and across features and set identified
#' values to NA. Features and samples that contain a threshold ratio of NA
#' values are removed.
#'
#' @param dat a list containing 3 elements: case, a list of case/control
#' statuses; feat, a matrix of normalized feature data; maxfeat, a list of max
#' features from each column in feat
#' @return List containing 3 elements (case, feat, maxfeat) with relevant NA
#' values and necessary features/samples removed
#' 
#' @export 
#' @importFrom 
clean_bmdk <- function(dat, naThreshold = 0.05)
{
  
  f <- dat$feat
  
  # Calculate the mean and standard deviation of each feature column
  featMeans <- numeric(ncol(f))
  featStds <- numeric(ncol(f))
  for (i in 1:ncol(f)) {
    featMeans[i] <- mean(f[ , i])
    featStds[i] <- sd(f[ , i])
  }
  
  
  # If any sample has a value >4𝜎 away from the mean in either direction,
  # convert the value to NA
  lowerThresholds <- featMeans - 4*featStds
  upperThresholds <- featMeans + 4*featStds
  
  for (j in 1:ncol(f)) {
    for (k in 1:nrow(f)) {
      if (f[k, j] > upperThresholds[j] | f[k, j] < lowerThresholds[j]) {
        f[k, j] <- NA;
      }
    }
  }
  
  # If any feature column contains >0.05 NA values, remove the feature column
  # and display a warning message
  boolF <- is.na(f)
  numFeatEntries <- nrow(f)
  
  for (i in 1:ncol(f)) {
    
    featNARatio <- sum(boolF[ , i]) / numFeatEntries
    
    if (featNARatio > naThreshold) {
      dat$feat <- dat$feat[ , -i]
      dat$maxfeat <- dat$maxfeat[-i]
      
      # Display warning message that the feature was removed
      #########
    }
  }
  
  # If any sample row contains >0.05 NA values, remove the sample and display
  # a warning message
  
  numSampEntries <- ncol(f)
  
  for (j in 1:nrow(f)) {
    
    sampNARatio <- sum(boolF[j, ]) / numSampEntries
    
    if (sampNARatio > naThreshold) {
      dat$case <- dat$case[-j]
      dat$feat <- dat$feat[-j, ]
      
      # Display warning message that the sample was removed
      #########
    }
  }
  
  renormalize_bmdk(dat)
  
  return (dat)
}
