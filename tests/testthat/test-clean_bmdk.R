context("test-clean_bmdk")

test_that("clean_bmdk returns a valid object", {
  
  #tst <- read_bmdk(system.file('extdata', 'BMDK_NAs.txt', package = 'BMDK')) %>%
  #clean_bmdk()
  tst <- read_bmdk('/Users/jihvieirala/Documents/BMDK/inst/extdata/BMDK_NAs.txt') %>%
    clean_bmdk()
  
  expect_is(tst, 'list')
})

test_that("clean_bmdk removes the NA features and samples", {
  
  #tst <- read_bmdk(system.file('extdata', 'BMDK_NAs.txt', package = 'BMDK')) %>%
  #clean_bmdk()
  tst <- read_bmdk('/Users/jihvieirala/Documents/BMDK/inst/extdata/BMDK_NAs.txt') %>%
    clean_bmdk()
  
  expect_equal(nrow(tst$feat), 99)
  expect_equal(ncol(tst$feat), 1099)
})