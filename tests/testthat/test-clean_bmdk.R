context("test-clean_bmdk")

test_that("clean_bmdk returns a valid object", {
  
  #tst <- read_bmdk(system.file('extdata', 'BMDK_train.txt', package = 'BMDK')) %>%
  #clean_bmdk()
  tst <- read_bmdk('/Users/jihvieirala/Documents/BMDK/inst/extdata/BMDK_train.txt') %>%
    clean_bmdk()
  
  expect_is(tst, 'list')
})

test_that("clean_bmdk returns the correct warnings", {
  
  #tst <- read_bmdk(system.file('extdata', 'BMDK_NAs.txt', package = 'BMDK')) %>%
  #clean_bmdk()
  expect_warning(read_bmdk('/Users/jihvieirala/Documents/BMDK/inst/extdata/BMDK_NAs.txt') %>%
                   clean_bmdk(), 'Feature column removed: FEAT0002')
  
  expect_warning(read_bmdk('/Users/jihvieirala/Documents/BMDK/inst/extdata/BMDK_NAs.txt') %>%
                   clean_bmdk(), 'Sample row removed: SAMP001')
})

test_that("clean_bmdk removes the NA features and samples", {
  
  #tst <- read_bmdk(system.file('extdata', 'BMDK_NAs.txt', package = 'BMDK')) %>%
  #clean_bmdk()
  tst <- expect_warning(read_bmdk('/Users/jihvieirala/Documents/BMDK/inst/extdata/BMDK_NAs.txt') %>%
                          clean_bmdk())
  
  # Number of samples should be 99
  expect_equal(nrow(tst$feat), 99)
  expect_equal(length(tst$case), 99)
  
  # Number of features should be 1099
  expect_equal(ncol(tst$feat), 1099)
  expect_equal(length(tst$maxfeat), 1099)
  
  # None of the maxfeat entries should be NA / NaN
  expect_equal(sum(is.na(tst$maxfeat)), 0)
})
