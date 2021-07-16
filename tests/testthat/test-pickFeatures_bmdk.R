context("test-pickFeatures_bmdk()")

test_that("pickFeatures_bmdk() selects the top 10 extreme algorithm features", {
  # No ties present
  case <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
  feat <- matrix(c(1:169), nrow = 13)
  sid <- c('S001', 'S002', 'S003', 'S004', 'S005', 'S006', 'S007', 'S008',
           'S009', 'S010', 'S011', 'S012', 'S013')
  featnames <- c('F001', 'F002', 'F003', 'F004', 'F005', 'F006', 'F007', 'F008',
                 'F009', 'F010', 'F011', 'F012', 'F013')
  dimnames(feat) <- list(sid,
                        featnames)
  maxfeat <- apply(feat, 2, max, na.rm = TRUE)
  minfeat <- apply(feat, 2, min, na.rm = TRUE)
  
  # Sorted in ascending order
  wresults <- c(1:13)
  tresults <- c(1:13)
  ksresults <- c(1:13)
  lresults <- c(1:13)
  vresults <- c(1:13)
  pcresults <- c(1:13)
  kcresults <- c(1:13)
  scresults <- c(1:13)
  fresults <- c(1:13)
  # Sorted in descending order
  gresults <- c(13:1)
  iresults <- c(13:1)
  cresults <- c(13:1)
  presults <- c(13:1)
  
  # No ties with the 10th feature are present
  eresults <- c(10, 10, 7, 6, 5, 5, 4, 3, 3, 3, 2, 1, 1) # Sorted descending
  
  testresults <- list(wresults, tresults, gresults, iresults,
                      ksresults, fresults, eresults, cresults,
                      presults, lresults, vresults, pcresults,
                      kcresults, scresults)
  names(testresults) <- c('wresults', 'tresults', 'gresults', 'iresults',
                          'ksresults', 'fresults', 'eresults', 'cresults',
                          'presults', 'lresults', 'vresults', 'pcresults',
                          'kcresults', 'scresults')
  
  
  dat <- list('case' = case,
              'feat' = feat,
              'maxfeat' = maxfeat,
              'minfeat' = minfeat,
              'testresults' = testresults)
  
  tst <- pickFeatures_bmdk(dat)
  
  expect_is(tst, 'list')
  expect_equal(tst$topfeatures, c("F001", "F002", "F003", "F004", "F005",
                                  "F006", "F007", "F008", "F009", "F010"))
})

test_that("pickFeatures_bmdk() selects the top 11 extreme algoritm features", {
  # Single tie present
  case <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
  feat <- matrix(c(1:169), nrow = 13)
  sid <- c('S001', 'S002', 'S003', 'S004', 'S005', 'S006', 'S007', 'S008',
           'S009', 'S010', 'S011', 'S012', 'S013')
  featnames <- c('F001', 'F002', 'F003', 'F004', 'F005', 'F006', 'F007', 'F008',
                 'F009', 'F010', 'F011', 'F012', 'F013')
  dimnames(feat) <- list(sid,
                         featnames)
  maxfeat <- apply(feat, 2, max, na.rm = TRUE)
  minfeat <- apply(feat, 2, min, na.rm = TRUE)
  
  # Sorted in ascending order when selecting
  wresults <- c(1:13)
  tresults <- c(1:13)
  ksresults <- c(1:13)
  lresults <- c(1:13)
  vresults <- c(1:13)
  pcresults <- c(1:13)
  kcresults <- c(1:13)
  scresults <- c(1:13)
  fresults <- c(1:13)
  # Sorted in descending order when selecting
  gresults <- c(13:1)
  iresults <- c(13:1)
  cresults <- c(13:1)
  presults <- c(13:1)
  
  # A single tie with the 10th feature is present
  eresults <- c(10, 10, 7, 6, 5, 5, 4, 3, 3, 3, 3, 1, 1) # Sorted descending
  
  testresults <- list(wresults, tresults, gresults, iresults,
                      ksresults, fresults, eresults, cresults,
                      presults, lresults, vresults, pcresults,
                      kcresults, scresults)
  names(testresults) <- c('wresults', 'tresults', 'gresults', 'iresults',
                          'ksresults', 'fresults', 'eresults', 'cresults',
                          'presults', 'lresults', 'vresults', 'pcresults',
                          'kcresults', 'scresults')
  
  
  dat <- list('case' = case,
              'feat' = feat,
              'maxfeat' = maxfeat,
              'minfeat' = minfeat,
              'testresults' = testresults)
  
  tst <- pickFeatures_bmdk(dat)
  
  expect_equal(tst$topfeatures, c("F001", "F002", "F003", "F004", "F005",
                                  "F006", "F007", "F008", "F009", "F010",
                                  "F011"))
})

test_that("pickFeatures_bmdk() selects the top 13 extreme algoritm features", {
  # Multiple ties present
  case <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
  feat <- matrix(c(1:196), nrow = 14)
  sid <- c('S001', 'S002', 'S003', 'S004', 'S005', 'S006', 'S007', 'S008',
           'S009', 'S010', 'S011', 'S012', 'S013', 'S014')
  featnames <- c('F001', 'F002', 'F003', 'F004', 'F005', 'F006', 'F007', 'F008',
                 'F009', 'F010', 'F011', 'F012', 'F013', 'F014')
  dimnames(feat) <- list(sid,
                         featnames)
  maxfeat <- apply(feat, 2, max, na.rm = TRUE)
  minfeat <- apply(feat, 2, min, na.rm = TRUE)
  
  # Sorted in ascending order when selecting
  wresults <- c(1:14)
  tresults <- c(1:14)
  ksresults <- c(1:14)
  lresults <- c(1:14)
  vresults <- c(1:14)
  pcresults <- c(1:14)
  kcresults <- c(1:14)
  scresults <- c(1:14)
  fresults <- c(1:14)
  # Sorted in descending order when selecting
  gresults <- c(14:1)
  iresults <- c(14:1)
  cresults <- c(14:1)
  presults <- c(14:1)
  
  # Features 11-13 are tied with the 10th feature
  eresults <- c(15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 6, 6, 6, 3) # Sorted descending
  
  testresults <- list(wresults, tresults, gresults, iresults,
                      ksresults, fresults, eresults, cresults,
                      presults, lresults, vresults, pcresults,
                      kcresults, scresults)
  names(testresults) <- c('wresults', 'tresults', 'gresults', 'iresults',
                          'ksresults', 'fresults', 'eresults', 'cresults',
                          'presults', 'lresults', 'vresults', 'pcresults',
                          'kcresults', 'scresults')
  
  
  dat <- list('case' = case,
              'feat' = feat,
              'maxfeat' = maxfeat,
              'minfeat' = minfeat,
              'testresults' = testresults)
  
  tst <- pickFeatures_bmdk(dat)
  
  expect_equal(tst$topfeatures, c("F001", "F002", "F003", "F004", "F005",
                                  "F006", "F007", "F008", "F009", "F010",
                                  "F011", "F012", 'F013'))
})

