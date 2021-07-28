#' Picks the top features to be used in the BMDK classifier
#' 
#' Takes the top features from each of the filtering methods and finds the
#' union. Then runs those features through a Pearson Correlation test and
#' identifies correlated pairs. Removes the feature with the smaller max value
#' of each pair from the top features list. 
#'
#' @param dat a list containing 4 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat; testresults, a list of statistical test results
#' @return dat a list containing 5 elements: case, a list of case/control statuses;
#'        feat, a matrix of normalized feature data; maxfeat, a list of max features
#'        from each column in feat; testresults, a list of statistical test results;
#'        topfeatures, a list of top features names
#' @export
#' @importFrom stats cor
#' @importFrom magrittr %>%

pickFeatures_bmdk <- function(dat)
{
  
  # Identify a unique list of the top features from both tests
  wtop <- colnames(dat$feat)[order(dat$testresults$wresults)[1:10]]
  ttop <- colnames(dat$feat)[order(dat$testresults$tresults)[1:10]]
  gtop <- colnames(dat$feat)[order(dat$testresults$gresults, decreasing = TRUE)[1:10]]
  itop <- colnames(dat$feat)[order(dat$testresults$iresults)[1:10]]
  kstop <- colnames(dat$feat)[order(dat$testresults$ksresults)[1:10]]
  ftop <- colnames(dat$feat)[order(dat$testresults$fresults)[1:10]]
  ltop <- colnames(dat$feat)[order(dat$testresults$lresults)[1:10]]
  pctop <- colnames(dat$feat)[order(dat$testresults$pcresults)[1:10]]
  kctop <- colnames(dat$feat)[order(dat$testresults$kcresults)[1:10]]
  sctop <- colnames(dat$feat)[order(dat$testresults$scresults)[1:10]]
  vtop <- colnames(dat$feat)[order(dat$testresults$vresults)[1:10]]
  ctop <- colnames(dat$feat)[order(dat$testresults$cresults, decreasing = TRUE)[1:10]]
  ptop <- colnames(dat$feat)[order(dat$testresults$presults, decreasing = TRUE)[1:10]]
  
  eSorted <- sort(dat$testresults$eresults, decreasing = TRUE)
  endIdx <- 10
  
  while (eSorted[endIdx] == eSorted[endIdx + 1]) {
    endIdx <- endIdx + 1
  }
  
  etop <- colnames(dat$feat)[order(dat$testresults$eresults, decreasing = TRUE)[1:endIdx]]
  
  dat$topfeatures <- c(wtop, ttop,
                       gtop, itop,
                       kstop, ftop,
                       etop, ltop,
                       pctop, kctop,
                       sctop, vtop,
                       ctop, ptop) %>%
                      unique()
  
  return(dat)
}