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
clean_bmdk <- function(dat, naThreshold = 0.05, sdMultiplier = 4)
{
  
  # Calculate the mean and standard deviation of each feature column
  featMeans <- apply(dat$feat, 2, mean, na.rm = TRUE)
  featStds <- apply(dat$feat, 2, sd, na.rm = TRUE)
  
  # If any sample has a value >4𝜎 away from the mean in either direction,
  # convert the value to NA
  lowerThresholds <- featMeans - sdMultiplier*featStds
  upperThresholds <- featMeans + sdMultiplier*featStds
  
  dat$feat <- apply(dat$feat, 1, function(x) {x[x > upperThresholds | x < lowerThresholds] <- NA; return(x)}) %>%
    t()
  
  # If any feature column contains >0.05 NA values, remove the feature column
  # and display a warning message
  boolF <- is.na(dat$feat)
  
  featNARatio <- (apply(boolF, 2, sum) / nrow(dat$feat)) > naThreshold
  
  if (sum(featNARatio) > 0) {
    warning('Removing features with excessive missingness: ',
            paste(colnames(dat$feat)[featNARatio], collapse = ', '))
    
    dat$feat <- dat$feat[ ,-which(featNARatio)]
    dat$maxfeat <- dat$maxfeat[-which(featNARatio)]
  }
  
  # If any sample row contains >0.05 NA values, remove the sample and display
  # a warning message
  sampNARatio <- (apply(boolF, 1, sum) / ncol(dat$feat)) > naThreshold
  
  if (sum(sampNARatio) > 0) {
    warning('Removing samples with excessive missingness: ',
            paste(rownames(dat$feat)[sampNARatio], collapse = ', '))
    
    dat$feat <- dat$feat[-which(sampNARatio), ]
    dat$case <- dat$case[-which(sampNARatio)]
    
    renormalize_bmdk(dat)
  }
  
  return(dat)
}