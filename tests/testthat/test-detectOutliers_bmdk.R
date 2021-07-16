context("test-detectOutliers_bmdk")

test_that("detectOutliers_bmdk() does not alter the data when no outliers are present", {
  
  mat0<- matrix(data= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow= 3, ncol= 4)
  case <- rep(1, nrow(mat0))
  maxfeat <- apply(mat0, 2, max, na.rm = TRUE)
  minfeat <- apply(mat0, 2, min, na.rm = TRUE)
  
  dat0 <- list(feat = mat0,
               case = case,
               maxfeat = maxfeat,
               minfeat = minfeat)

  tst0 <- detectOutliers_bmdk(dat0)
  
  expect_is(tst0, 'list')
  expect_equal(nrow(tst0$feat), nrow(mat0))
  expect_equal(tst0$feat, mat0)
  expect_equal(tst0$case, case)
  expect_equal(tst0$maxfeat, maxfeat)
  expect_equal(tst0$minfeat, minfeat)
})

test_that("detectOutliers_bmdk() removes a single outlier correctly", {
  
  set.seed(923847)
  x <- runif(100)
  y <- x + rnorm(100, sd = .05)
  z <- x + rnorm(100, sd = .05)
  mat <- cbind(x,y,z)
  mat1 <- rbind(mat, c(0, 1, 1))
  
  case <- rep(1, nrow(mat1))
  maxfeat <- apply(mat1, 2, max, na.rm = TRUE)
  minfeat <- apply(mat1, 2, min, na.rm = TRUE)
  
  dat1 <- list(feat = mat1,
              case = case,
              maxfeat = maxfeat,
              minfeat = minfeat)
  
  tst1 <- detectOutliers_bmdk(dat1)
  
  expect_is(tst1, 'list')
  expect_equal(length(tst1$case), length(case) - 1)
  expect_equal(nrow(tst1$feat), nrow(mat1) - 1)
  expect_equal(tst1$maxfeat, maxfeat)
  expect_equal(tst1$minfeat, minfeat)
})

test_that("detectOutliers_bmdk() removes multiple outliers correctly", {
  
  mat2 <- system.file('extdata', 'detectOutliers_tst.txt', package = 'BMDK') %>%
    utils::read.table()
  
  case <- rep(1, nrow(mat2))
  maxfeat <- apply(mat2, 2, max, na.rm = TRUE)
  minfeat <- apply(mat2, 2, min, na.rm = TRUE)
  
  dat2 <- list(feat = mat2,
               case = case,
               maxfeat = maxfeat,
               minfeat = minfeat)
  
  tst2 <- detectOutliers_bmdk(dat2)
  
  expect_equal(length(tst2$case), length(case) - 3)
  expect_equal(nrow(tst2$feat), nrow(mat2) - 3)
  
  expect_false(isTRUE(all.equal(tst2$feat[1, ], mat2[1, ])))
  expect_false(isTRUE(all.equal(tst2$feat[50, ], mat2[50, ])))
  
  # The samples occurring after the outliers are still present in the dataset
  # (the outliers were deleted from highest to lowest index)
  expect_equal(tst2$feat[1, ], mat2[2, ])
  expect_equal(tst2$feat[49, ], mat2[51, ])
  expect_equal(tst2$feat[100, ], mat2[102, ])
})
