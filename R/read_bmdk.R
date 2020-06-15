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
    dat <- utils::read.table(f) %>%
        t()
    
    sid <- dat[,1]
    case <- dat[,2]
    
    dat <- dat[,-(1:2)]
    
    # these get lost - add this back in after converting to numeric
    # dimnames(dat)[[2]] <- dat[1,]
    
    # get number of features
    n <- 5000
    
    dat <- dat[-1,] %>%
        as.numeric() %>%
        matrix(ncol = n)
        # convert to a data.frame
    
    # add names
    # add case/control status
    # add sample IDs
    
    return(dat)
}