#' Read BMDK data
#'
#' Read in a test or training dataset with the proper format for BMDK.
#'
#' @param f path to input file
#' @return A matrix formatted and tidied for BMDK
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
    sid <- splitlines[[1]] %>%
        unlist()
    sid <- sid[-1]
    
    # Create case, an integer vector of case/control status
    case = splitlines[[2]] 
    case <- case[-1] %>%
        unlist() %>%
        as.integer()
    
    # Read in feature data
    dat <- utils::read.table(f, skip = 2, nrows = 10)

    # Strip out V1 (feature names)
    datnames <- dat[, 1] %>%
        as.vector()
    
    # Transpose
    dat <- dat[, -1] %>%
        t()
    
    # Convert dat to a numeric matrix and label the rows and columns
    dat <- as.numeric(dat) %>%
        matrix(ncol = ncol(dat),
               dimnames = list(sid, # Row names (Sample IDs)
               datnames))           # Column names (Feature names)
    
    ##########################OLD CODE################################
    # modify / remove this chunk of code
    # # Save all of the feature names of the data set
    # datnames <- dat[1, -c(1:2)] %>%
    #     as.vector()
    # 
    # # Save the sample IDs column
    # sid <- dat[-1, 1]
    # 
    # # Save the case/control status column
    # case <- dat[-1, 2] %>%
    #     as.integer()
    # 
    # # Save the matrix of the features data
    # dat <- dat[-1, -(1:2)]
    # 
    # # Get the total number of features
    # n <- ncol(dat)
    # 
    # # Convert the matrix to a data.frame
    # dat <- as.numeric(dat) %>%
    #        matrix(ncol = n,
    #               dimnames = list(sid,           # row names
    #                               datnames)) %>% # column names
    # 
    # # Add the case/control status
    # dat <- cbind(sid, case, dat)
    
    return(list(case = case, # Integer vector
                feat = dat)) # Numeric matrix
}
