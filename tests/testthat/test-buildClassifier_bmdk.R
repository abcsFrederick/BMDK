context("test-buildClassifier_bmdk()")

test_that("buildClassifier_bmdk()", {
  dat <- read_bmdk(system.file('extdata', 'BMDK_train_subset_modified3.txt', package = 'BMDK'))
  dat$topfeatures <- c("FEAT0002", "FEAT0006", "FEAT0001", "FEAT0051", "FEAT0055",
                         "FEAT0054", "FEAT0005", "FEAT0050", "FEAT0004", "FEAT0012")
  
  buildClassifier_bmdk(dat)
})

test_that("buildClassifier_bmdk() results commpared to fortran results", {
  dat <- read_bmdk(system.file('extdata', 'BMDK_train_subset_modified3.txt', package = 'BMDK'))
  dat$topfeatures <- c("FEAT0001", "FEAT0002", "FEAT0005", "FEAT0051", "FEAT0053",
                       "FEAT0056")
  
  buildClassifier_bmdk(dat)
})