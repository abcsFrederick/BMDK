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
    # use readLines to fetch sample IDs, case status
    
    # read in feature data
    dat <- utils::read.table(f, skip = 2)
    
    # strip out V1 (feature names)
    
    # transpose
    
    # change column names to feature names
    
    # add row names for sample IDs
    
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
    
    return(list(case = case, # integer vector
                feat = dat)) # numeric matrix
}
