context("test-read_bmdk")

test_that("read_bmdk returns a valid object", {
    
    tst <- read_bmdk(system.file('extdata', 'BMDK_test.txt', package = 'BMDK'))
    
    expect_is(tst, 'list')
    expect_is(tst[[1]], 'integer')
    expect_is(tst[[2]], 'matrix')
})

# outlier tests

# missing data tests