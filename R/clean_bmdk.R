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
  
  # Calculate the mean and standard deviation of each feature column
  featMeans <- apply(dat$feat, 2, mean, na.rm = TRUE)
  featStds <- apply(dat$feat, 2, sd, na.rm = TRUE)
  
  # If any sample has a value >4𝜎 away from the mean in either direction,
  # convert the value to NA
  lowerThresholds <- featMeans - 4*featStds
  upperThresholds <- featMeans + 4*featStds
  
  for (j in 1:ncol(dat$feat)) { # By column
    for (k in 1:nrow(dat$feat)) { # By row
      
      if (!is.na(dat$feat[k, j])) {
        if (dat$feat[k, j] > upperThresholds[j] | dat$feat[k, j] < lowerThresholds[j]) {
          dat$feat[k, j] <- NA;
        }
      }
      
    }
  }
  
  # If any feature column contains >0.05 NA values, remove the feature column
  # and display a warning message
  boolF <- is.na(dat$feat)
  numFeatEntries <- nrow(dat$feat)
  fNames <- colnames(dat$feat)
  
  for (i in 1:ncol(dat$feat)) {
    
    featNARatio <- sum(boolF[ , i]) / numFeatEntries
    
    if (featNARatio > naThreshold) {
      dat$feat <- dat$feat[ , -i]
      dat$maxfeat <- dat$maxfeat[-i]
      
      # Display warning message that the feature was removed
      warning('Feature column removed: ', fNames[i])
    }
  }
  
  # If any sample row contains >0.05 NA values, remove the sample and display
  # a warning message
  numSampEntries <- ncol(dat$feat)
  sNames <- rownames(dat$feat)
  
  #sampNARatio <- apply(boolF, 2, function(.x){sum(.x) / numSampEntries})
  
  for (j in 1:nrow(dat$feat)) {
    
    sampNARatio <- sum(boolF[ , j]) / numFeatEntries
    
    if (sampNARatio > .05) {
      dat$case <- dat$case[-j]
      dat$feat <- dat$feat[-j, ]
      
      # Display warning message that the sample was removed
      warning('Sample row removed: ', sNames[j])
    }
  }

  renormalize_bmdk(dat)
  
  return (dat)
  
}
