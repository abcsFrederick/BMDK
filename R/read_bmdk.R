#' Read BMDK data
#'
#' Read in a test or training dataset with the proper format for BMDK.
#'
#' @param f path to input file
#' @return A data.frame formatted and tidied for BMDK
#' @examples
#' test_data <- read_bmdk(system.file('extdata', 'BMDK_test.txt', package = 'BMDK'))
#' @export
#' @importFrom magrittr %>%
read_bmdk <- function(f)
{
    dat <- utils::read.table(f, nrows = 10) %>%
        t()
    
    # Save all of the column names of the data set
    datnames <- dat[1, ] %>%
        as.vector()
    
    # Save the sample IDs column
    sid <- dat[-1, 1]
    
    # Save the case/control status column
    case <- dat[-1, 2] %>%
        as.integer()
    
    # Save the matrix of the features data
    dat <- dat[-1, -(1:2)]
    
    # Get the total number of features
    n <- ncol(dat)
    
    # Convert the matrix to a data.frame
    dat <- dat[, ] %>%
        as.numeric() %>%
        matrix(ncol = n) %>%
        as.data.frame()
    
    # Add the feature names
    names(dat) <- datnames[(3:length(datnames))]
    
    # Add the case/control status
    dat <- cbind(a = case, dat)
    colnames(dat)[1] <- datnames[2]
    
    # Add the sample IDs
    dat <- cbind(b = sid, dat)
    colnames(dat)[1] <- datnames[1]
    
    return(dat)
}