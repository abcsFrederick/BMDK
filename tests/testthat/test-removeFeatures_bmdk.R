context("test-removeFeatures_bmdk()")

test_that("removeFeatures_bmdk() removes the correct features from a single cluster", {
  dat <- read_bmdk(system.file('extdata', 'BMDK_train_subset_with_corr.txt', package = 'BMDK'))
  dat$topfeatures <- c("FEAT0003", "FEAT0002", "FEAT0006", "FEAT0001", "FEAT0051",
                       "FEAT0052", "FEAT0005", "FEAT0050", "FEAT0015", "FEAT0004",
                       "FEAT0012", "FEAT0027", "FEAT0019", "FEAT0047")
  
  test <- removeFeatures_bmdk(dat)
  
  i <- 1
  check <- TRUE
  expectedtop <- c("FEAT0003", "FEAT0002", "FEAT0006", "FEAT0001", "FEAT0052",
                   "FEAT0005", "FEAT0050", "FEAT0015", "FEAT0004", "FEAT0012",
                   "FEAT0027", "FEAT0019", "FEAT0047")
  while(check == TRUE && i < length(expectedtop)) {
    check <- which(dat$topfeatures == expectedtop[i]) != 0
    i <- i + 1
  }
  
  expect_equal(check, TRUE)
  expect_equal(length(test$topfeatures), 13)
})

test_that("removeFeatures_bmdk() removes the correct features from multiple clusters", {
  dat <- read_bmdk(system.file('extdata', 'BMDK_train_subset_modified2.txt', package = 'BMDK'))
  dat$topfeatures <- c("FEAT0003", "FEAT0002", "FEAT0053", "FEAT0006", "FEAT0001",
                       "FEAT0051", "FEAT0052", "FEAT0054", "FEAT0005", "FEAT0050",
                       "FEAT0004", "FEAT0027", "FEAT0012", "FEAT0015")
  
  test <- removeFeatures_bmdk(dat)
  
  i <- 1
  check <- TRUE
  expectedtop <- c("FEAT0003", "FEAT0002", "FEAT0006", "FEAT0001", "FEAT0052",
                   "FEAT0054", "FEAT0005", "FEAT0050", "FEAT0004", "FEAT0027",
                   "FEAT0012", "FEAT0015")
  while(check == TRUE && i < length(expectedtop)) {
    check <- which(dat$topfeatures == expectedtop[i]) != 0
    i <- i + 1
  }
  
  expect_equal(check, TRUE)
  expect_equal(length(test$topfeatures), 12)
})

test_that("removeFeatures_bmdk() removes the correct features from multiple chained clusters", {
  # Chained clustering
  dat <- read_bmdk(system.file('extdata', 'BMDK_train_subset_modified3.txt', package = 'BMDK'))
  dat$topfeatures <- c("FEAT0003", "FEAT0002", "FEAT0053", "FEAT0006", "FEAT0001",
                       "FEAT0051", "FEAT0052", "FEAT0055", "FEAT0054", "FEAT0005",
                       "FEAT0050", "FEAT0004", "FEAT0012", "FEAT0056")
  
  test <- removeFeatures_bmdk(dat)
  
  i <- 1
  check <- TRUE
  expectedtop <- c("FEAT0002", "FEAT0006", "FEAT0001", "FEAT0051", "FEAT0055",
                   "FEAT0054", "FEAT0005", "FEAT0050", "FEAT0004", "FEAT0012")
  while(check == TRUE && i < length(expectedtop)) {
    check <- which(dat$topfeatures == expectedtop[i]) != 0
    i <- i + 1
  }
  
  expect_equal(check, TRUE)
  expect_equal(length(test$topfeatures), 10)
})
