context("test-read_bmdk")

test_that("read_bmdk returns a valid object", {
    
    tst <- read_bmdk('file path goes here')
    
    expect_is(tst, 'data.frame')
    
})