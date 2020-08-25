#' Read BMDK data
#'
#' Read in a test or training dataset with the proper format for BMDK.
#'
#' @param f path to input file
#' @return A list with case/control status, a matrix, and max feature value formatted and tidied for BMDK
#' @examples
#' test_data <- read_bmdk(system.file('extdata', 'BMDK_test.txt', package = 'BMDK'))
#' @export
#' @importFrom magrittr %>%

read_bmdk <- function(f)
{
    # Use readLines to fetch sample IDs, case status
    lines <- readLines(f, n = 2)
    splitlines <- strsplit(lines, split = '\t', fixed = TRUE)
    
    # Create sid, a character vector of sample IDs
    sid <- splitlines[[1]][-1]
    
    # Create case, an integer vector of case/control status
    # 0 represents controls, 1 represents cases
    case = splitlines[[2]][-1] %>%
           as.integer()
    
    # Read in feature data
    dat <- utils::read.table(f, skip = 2)

    # Strip out V1 (feature names)
    datnames <- dat[, 1]
    
    # Transpose
    dat <- dat[, -1] %>%
        t()
    
    # Convert dat to a numeric matrix and label the rows and columns
    dimnames(dat) <- list(sid,         # Row names (Sample IDs)
                          datnames)    # Column names (Feature names)
    
    # Store the max value of each column (feature) in dat
    maxfeat <- apply(dat, 2, max)
    
    # Normalize dat
    dat <- apply(dat, 2, function(.x){.x / max(.x, na.rm = TRUE)})
    
    return(list(case = case,        # Integer vector
                feat = dat,         # Numeric matrix
                maxfeat = maxfeat)) # Numeric vector
}
