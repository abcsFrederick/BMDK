context("test-read_bmdk")

test_that("read_bmdk returns a valid object", {
    
    tst <- read_bmdk(system.file('extdata', 'BMDK_test.txt', package = 'BMDK'))
    
    expect_is(tst, 'list')
    expect_is(tst[[1]], 'integer')
    expect_is(tst[[2]], 'matrix')
})

test_that("read_bmdk normalizes correctly", {
    
    lines <- readLines(system.file('extdata', 'BMDK_test.txt', package = 'BMDK'), n = 2)
    splitlines <- strsplit(lines, split = '\t', fixed = TRUE)

    sid <- splitlines[[1]][-1] # Sample IDs
    unnormedDat <- utils::read.table(system.file('extdata', 'BMDK_test.txt', package = 'BMDK'),
                             skip = 2) # Feature data
    
    datnames <- unnormedDat[, 1] # Strip feature names
    unnormedDat <- unnormedDat[, -1] %>% # Transpose
        t()
    
    # Label dimensions of feature matrix
    dimnames(unnormedDat) <- list(sid,
                          datnames)
    
    tst <- read_bmdk(system.file('extdata', 'BMDK_test.txt', package = 'BMDK'))
    
    expect_equal(t(t(tst$feat) * (tst$maxfeat - tst$minfeat) + tst$minfeat), unnormedDat)
})

# test_that("read_bmdk coerces strings to NAs", {
#     
#     #tst <- read_bmdk(system.file('extdata', 'BMDK_withText.txt', package = 'BMDK'))
#     #root <- system('git rev-parse --show-toplevel', intern = TRUE)
#     #tst <- read_bmdk(paste0(root, '/inst/extdata/BMDK_withText.txt'))
#     tst <- read_bmdk('/Users/jihvieirala/Documents/BMDK/inst/extdata/BMDK_withText.txt')
#     
#     expect_equal(tst$feat[1:5, 1], rep(NA, 5))
#     
# })
