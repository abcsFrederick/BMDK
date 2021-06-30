context("test-extreme")

test_that("extreme returns a valid object", {
  
  case <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
  feat <- matrix(c(1:100), nrow = 10)
  maxfeat <- apply(feat, 2, max, na.rm = TRUE)
  
  dat <- list('case' = case,
              'feat' = feat,
              'maxfeat' = maxfeat)
  
  tst <- extreme(dat)
  
  # Should return a numeric vector
  expect_is(tst, 'numeric')
})

test_that("extreme returns the correct values", {
  
  case <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
  feat <- matrix(c(1:100), nrow = 10)
  maxfeat <- apply(feat, 2, max, na.rm = TRUE)
  
  dat <- list('case' = case,
              'feat' = feat,
              'maxfeat' = maxfeat)
  
  tst <- extreme(dat)
  
  # All of the max values should be 6
  expect_equal(length(tst), 10)
  expect_equal(tst, rep(6, 10))
  
  #----------------------------------------------#
  
  case <- c(0, 1, 0, 0, 0, 0, 1, 0, 1, 1)
  feat <- matrix(c(1:100), nrow = 10)
  maxfeat <- apply(feat, 2, max, na.rm = TRUE)
  
  dat <- list('case' = case,
              'feat' = feat,
              'maxfeat' = maxfeat)
  
  tst <- extreme(dat)
  
  # All of the max values should be 2
  expect_equal(length(tst), 10)
  expect_equal(tst, rep(2, 10))
  
  #----------------------------------------------#
  
  case <- c(0, 0, 1, 0, 0, 0, 1, 0, 0, 1)
  feat <- matrix(c(1:100), nrow = 10)
  maxfeat <- apply(feat, 2, max, na.rm = TRUE)
  
  dat <- list('case' = case,
              'feat' = feat,
              'maxfeat' = maxfeat)
  
  tst <- extreme(dat)
  
  # All of the max values should be 2
  expect_equal(length(tst), 10)
  expect_equal(tst, rep(2, 10))
})

test_that("extreme handles ties correctly", {
  
  case <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
  feat <- matrix(c(1:100), nrow = 10)
  maxfeat <- apply(feat, 2, max, na.rm = TRUE)
  
  dat <- list('case' = case,
              'feat' = feat,
              'maxfeat' = maxfeat)
  
  tst <- extreme(dat)
  
  # All of the max values should be 5
  expect_equal(length(tst), 10)
  expect_equal(tst, rep(5, 10))
})

test_that("extreme returns the correct values with data of a single status", {
  
  case <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  feat <- matrix(c(1:100), nrow = 10)
  maxfeat <- apply(feat, 2, max, na.rm = TRUE)
  
  dat <- list('case' = case,
              'feat' = feat,
              'maxfeat' = maxfeat)
  
  tst <- extreme(dat)
  
  # All of the max values should be 10
  expect_equal(length(tst), 10)
  expect_equal(tst, rep(10, 10))
})
