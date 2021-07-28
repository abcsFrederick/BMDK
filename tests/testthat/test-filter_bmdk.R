context("test-filter_bmdk")

test_that("variance test returns the correct values of a matrix", {
    
    dat1 <- read_bmdk(system.file('extdata', 'BMDK_train_subset.txt', package = 'BMDK'))
    tst1 <- filter_bmdk(dat1)
    vtop <- colnames(tst1$feat)[order(tst1$testresults$vresults)[1:10]]
    vAll <- colnames(tst1$feat)[order(round(tst1$testresults$vresults, 5))]
    
    altResults <- utils::read.table(system.file('extdata', 'vartest_scores.txt', package = 'BMDK'))
    vtopAlt <- rownames(altResults)[order(altResults$vartest)[1:10]]
    vAllAlt <- rownames(altResults)[order(altResults$vartest)]
    
    expect_equal(vtop, vtopAlt)
    expect_equal(vAll, vAllAlt)
})

test_that("decision tree information gain returns the correct values", {
    
    dat2 <- read_bmdk(system.file('extdata', 'BMDK_train_subset.txt', package = 'BMDK'))
    tst2 <- filter_bmdk(dat2)
    iAll <- colnames(tst2$feat)[order(log(2) - tst2$testresults$iresults, decreasing = TRUE)]
    itop <- colnames(tst2$feat)[order(log(2) - tst2$testresults$iresults, decreasing = TRUE)[1:10]]
    itopAsc <- colnames(tst2$feat)[order(tst2$testresults$iresults)[1:10]]
    
    altResults <- utils::read.table(system.file('extdata', 'dtinfg_scores.txt', package = 'BMDK'))
    iAllAlt <- rownames(altResults)[order(altResults$Score, decreasing = TRUE)]
    itopAlt <- rownames(altResults)[order(altResults$Score, decreasing = TRUE)[1:10]]
    print(itop)
    expect_equal(itop, itopAlt)
    expect_equal(iAll, iAllAlt)
    expect_equal(itopAsc, itopAlt)
})


