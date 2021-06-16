context("test-knnC")

test_that("knnC returns a valid object", {
    
    mat<- matrix(data= c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow= 3, ncol= 4)
    nrowT<- ncol(mat) # Number of features, 4
    ncolT<- nrow(mat) # Number of samples, 3
    nnIdx <- numeric(ncolT)
    nnDist <- numeric(ncolT)
    
    tst <- knnC(nrow= nrowT, ncol= ncolT, f= c(t(mat)))
    
    expect_is(tst, 'List')
    
})
