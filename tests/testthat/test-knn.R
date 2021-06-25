context("test-knn")

test_that("knn returns a valid object", {
    
    mat0<- matrix(data= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow= 3, ncol= 4)
    nrowT0<- ncol(mat0) # Number of features, 4
    ncolT0<- nrow(mat0) # Number of samples, 3
    nnIdx0 <- numeric(ncolT0)
    nnDist0 <- numeric(ncolT0)
    
    tst0 <- knn(nrow= nrowT0, ncol= ncolT0, f= c(t(mat0)))
    
    expect_is(tst0, 'list')
})

test_that("knn returns the correct values", {
    
    mat1<- matrix(data= c(2, 4, 6, 8, 1, 3, 5, 7, 10, 20, 30, 40, 9, 8, 7, 6),
                  nrow= 4, ncol= 4, byrow= TRUE)
    nrowT1<- ncol(mat1) # Number of features, 4
    ncolT1<- nrow(mat1) # Number of samples, 4
    nnIdx1 <- numeric(ncolT1)
    nnDist1 <- numeric(ncolT1)
    
    tst1 <- knn(nrow= nrowT1, ncol= ncolT1, f= c(t(mat1)))
    
    expect_equal(tst1[[1]], c(1, 1, 17.5, 3.5))
    expect_equal(tst1[[2]], c(1, 0, 3, 0))
})

test_that("knn handles NAs correctly", {
    
    mat2<- matrix(data= c(6, 2, 1, 3, 4, NA, 7, 3, 5, NA, 10, 20),
                  nrow= 3, ncol= 4)
    nrowT2<- ncol(mat2) # Number of features, 4
    ncolT2<- nrow(mat2) # Number of samples, 3
    nnIdx2 <- numeric(ncolT2)
    nnDist2 <- numeric(ncolT2)
    
    tst2 <- knn(nrow= nrowT2, ncol= ncolT2, f= c(t(mat2)))
    
    expect_equal(tst2[[1]], c(3, 3, 3.5))
    expect_equal(tst2[[2]], c(1, 0, 0))
})

