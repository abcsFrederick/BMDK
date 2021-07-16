context("test-filter_bmdk")

test_that("variance test returns the correct values", {
    
    mat0<- matrix(data= c(1, 2, 3, 4, 5, 6, 2, 4, 6, 8, 10, 12, 3, 4, 7, 11, 15, 9),
                  nrow= 6, ncol= 3)
    case <- c(0, 0, 0, 1, 1, 1)
    maxfeat <- apply(mat0, 2, max, na.rm = TRUE)
    minfeat <- apply(mat0, 2, min, na.rm = TRUE)
    
    dat0 <- list(feat = mat0,
                 case = case,
                 maxfeat = maxfeat,
                 minfeat = minfeat)
    
    tst0 <- suppressWarnings(filter_bmdk(dat0))
    
    expect_is(tst0, 'list')
    expect_equal(round(tst0$testresults$vresults, digits = 3), c(0.327, 0.327, 0.521))
})
