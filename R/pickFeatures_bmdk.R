#' Write documentation here
#'
#' @param dat a list containing 4 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat; testresults, a list of statistical test results
#' @return NEED TO WRITE
#' @export
#' @importFrom magrittr %>%

pickFeatures_bmdk <- function(dat)
{
  
  # Identify a unique list of the top features from both tests
  dat$topfeatures <- unique(c(colnames(dat$feat)[order(dat$testresults$wresults)[1:10]],
                          colnames(dat$feat)[order(dat$testresults$tresults)[1:10]]))
  
  # Run the list of top features through a Pearson Correlation
  # Store the Pearson Correlation coefficients in corrcalculations
  corrcalculations <- cor(dat$feat[ ,dat$topfeatures], method = "pearson")
  
  # Identify any features with a high correlation (0.8 <= r < 1.0)
  corrfeatures <- which(corrcalculations >= 0.5 & corrcalculations < 1.0, arr.ind = T)
  
  if (length(corrfeatures) > 0)
  {
    # If highly correlated features are identified, retrieve those feature names
    # Each row in corrfeaturepairs contains the names of a pair of correlated features
    corrfeaturepairs <- matrix(c(rownames(corrcalculations[corrfeatures[,1], corrfeatures[,2]]),
                                 colnames(corrcalculations[corrfeatures[,1], corrfeatures[,2]])), 
                               nrow = nrow(corrfeatures), ncol = ncol(corrfeatures))
    
    
    for (i in nrow(corrfeaturepairs))
    {
      if(corrfeaturepairs[i,1] %in% dat$topfeatures &
         corrfeaturepairs[i,2] %in% dat$topfeatures)
      {
        
        # For each pair of features, identify which has a smaller max value in the original data
        featuremaxes <- c(dat$maxfeat[corrfeaturepairs[i,1]],
                          dat$maxfeat[corrfeaturepairs[i,2]])
        
        minfeature <- min(featuremaxes)
        
        # Throw out the feature in topfeatures that has the smaller max value
        dat$topfeatures <- dat$topfeatures[-match(corrfeaturepairs[i, match(minfeature,
                                                                    featuremaxes)],
                                          dat$topfeatures)]
      }
    }
  }
  
  return(dat)
}