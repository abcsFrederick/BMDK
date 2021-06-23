context("test-clean_bmdk")

test_that("clean_bmdk returns a valid object", {
  
  tst <- read_bmdk(system.file('extdata', 'BMDK_train.txt', package = 'BMDK')) %>%
    clean_bmdk()

  expect_is(tst, 'list')
  
  #Number of samples should be 100
  expect_equal(nrow(tst$feat), 100)
  expect_equal(length(tst$case), 100)
  
  # Number of features should be 5000
  expect_equal(ncol(tst$feat), 5000)
  expect_equal(length(tst$maxfeat), 5000)
  
  # None of the maxfeat entries should be NA / NaN
  expect_equal(sum(is.na(tst$maxfeat)), 0)
})

test_that("clean_bmdk returns the correct warnings", {
  
  w <- capture_warnings({
    read_bmdk(system.file('extdata', 'BMDK_NAs.txt', package = 'BMDK')) %>%
      clean_bmdk()
    })
  expect_match(w, 'Removing features with excessive missingness: FEAT0002', all = FALSE)
  expect_match(w, 'Removing samples with excessive missingness: SAMP002', all = FALSE)
})

test_that("clean_bmdk removes the NA features and samples when whole row/columns are filled with NAs", {
  
  tst <- read_bmdk(system.file('extdata', 'BMDK_NAs.txt', package = 'BMDK')) %>%
    clean_bmdk()

  # Number of samples should be 99
  expect_equal(nrow(tst$feat), 99)
  expect_equal(length(tst$case), 99)
  
  # Number of features should be 1099
  expect_equal(ncol(tst$feat), 1099)
  expect_equal(length(tst$maxfeat), 1099)
  
  # None of the maxfeat entries should be NA / NaN
  expect_equal(sum(is.na(tst$maxfeat)), 0)
})

test_that("clean_bmdk removes the NA features and samples with scattered NAs", {
  
  w <- capture_warnings({
    read_bmdk(system.file('extdata', 'BMDK_NAs02.txt', package = 'BMDK')) %>%
      clean_bmdk()
    })
  expect_match(w, 'Removing features with excessive missingness: FEAT0008, FEAT0020', all = FALSE)
  expect_match(w, 'Removing samples with excessive missingness: SAMP004, SAMP149', all = FALSE)
  
  tst <- expect_warning(read_bmdk(system.file('extdata', 'BMDK_NAs02.txt', package = 'BMDK')) %>%
                          clean_bmdk())
  
  # Number of samples should be 98
  expect_equal(nrow(tst$feat), 98)
  expect_equal(length(tst$case), 98)
  
  # Number of features should be 498
  expect_equal(ncol(tst$feat), 498)
  expect_equal(length(tst$maxfeat), 498)
  
  # None of the maxfeat entries should be NA / NaN
  expect_equal(sum(is.na(tst$maxfeat)), 0)
})
