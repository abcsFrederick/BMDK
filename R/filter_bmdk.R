#' Runs the BMDK features data through a series of filtering methods
#' 
#' Utilizes 10 filters to identify to significance of each feature.
#' 
#' The filters are: Wilcoxon Rank Sum test, t test, Decision Tree Gini Index,
#' Decision Tree Information Gain, Kolmogorov-Smirnov test, Fisher test,
#' Extreme algorithm, Decision Tree class, Decision Tree poisson, and Logistic
#' Regression.
#'
#' @param dat a list containing 3 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat
#' @return dat a list containing 4 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat; testresults, a list of statistical test results
#' @export
#' @importFrom stats t.test
#' @importFrom stats wilcox.test
#' @importFrom stats ks.test
filter_bmdk <- function(dat)
{
  # Initialize vectors to store the results of each test
  wresults <- numeric(ncol(dat$feat)) # Wilcoxon
  tresults <- numeric(ncol(dat$feat)) # T-test
  ksresults <- numeric(ncol(dat$feat)) # K-S Test
  lresults <- numeric(ncol(dat$feat)) # Logistic Regression
  vresults <- numeric(ncol(dat$feat)) # Variance
  pcresults <- numeric(ncol(dat$feat)) # Pearson Cor
  kcresults <- numeric(ncol(dat$feat)) # Kendall Cor
  scresults <- numeric(ncol(dat$feat)) # Spearman Cor
  
  # Run the features data through a series of tests and identify each datum's significance
  for (i in 1:ncol(dat$feat))
  {
    # Wilcoxon Rank Sum test
    wresults[i] <- wilcox.test(dat$feat[dat$case == 1,i], dat$feat[dat$case == 0,i], exact = F)[[3]]
    
    # Two Sample t-test
    tresults[i] <- t.test(dat$feat[dat$case == 1,i], dat$feat[dat$case == 0,i])[[3]]
    
    # Kolmogorov-Smirnov test (K-S test)
    ksresults[i] <- suppressWarnings(ks.test(dat$feat[dat$case == 1,i], dat$feat[dat$case == 0,i])[[2]])
    
    # Logistic Regression (General Linear Model)
    mylogit <- glm(dat$case ~ dat$feat[ , i], family = 'binomial')
    lresults[i] <- coef(summary(mylogit))['dat$feat[, i]', 'Pr(>|z|)']
    
    # Variance Test
    vcon <- var(dat$feat[dat$case == 0, i], na.rm = TRUE)
    vcase <- var(dat$feat[dat$case == 1, i], na.rm = TRUE)
    vfeat <- var(dat$feat[, i], na.rm = TRUE)
    vconresult <- vcon / length(dat$feat[dat$case == 0, i])
    vcaseresult <- vcase / length(dat$feat[dat$case == 1, i])
    vterm1 <- length(dat$feat[, i]) / vfeat
    vresults[i] <- vterm1*(vconresult + vcaseresult)
    
    # Correlation Tests (Pearson, Kendall, Spearman)
    pcresults[i] <- cor.test(dat$case, dat$feat[, i], method = 'pearson',
                             exact = FALSE, na.action = 'na.omit')$p.value
    kcresults[i] <- cor.test(dat$case, dat$feat[, i], method = 'kendall',
                             exact = FALSE, na.action = 'na.omit')$p.value
    scresults[i] <- cor.test(dat$case, dat$feat[, i], method = 'spearman',
                             exact = FALSE, na.action = 'na.omit')$p.value
  } 
  
  # Decision Tree Information Gain and Fisher Test
  inforesults <- dtinfof(dat)
  iresults <- inforesults$iresults
  fresults <- inforesults$fresults
  
  # Extreme Algorithm
  eresults <- extreme(dat)
  
  # Decision Tree Gini Index, class, and poisson
  giniresults <- dtgini(dat)
  gresults <- giniresults$gresults
  cresults <- giniresults$cresults
  presults <- giniresults$presults
  
  # Store all of the test results in testresults, a list of numeric vectors
  testresults <- list(wresults, tresults,
                      gresults, iresults,
                      ksresults, fresults,
                      eresults, cresults,
                      presults, lresults,
                      vresults, pcresults,
                      kcresults, scresults)
  
  # Name each element in testresults
  ### NOTE: Can we do this so it is not hardcoded?? ###
  names(testresults) <- c('wresults', 'tresults', 'gresults', 'iresults',
                          'ksresults', 'fresults', 'eresults', 'cresults',
                          'presults', 'lresults', 'vresults', 'pcresults',
                          'kcresults', 'scresults')
  
  # Add testresults to dat
  dat$testresults <- testresults
  
  return(dat)
}
