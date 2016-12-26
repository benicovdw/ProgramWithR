best <- function(state, outcome){
    ## read data from file, choose columns that we need and give short names
    cdat <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    cdat <- cdat[,c(2,7,11,17,23)]
    names(cdat) <- c("name", "state", "attack", "failure", "pneumonia")

    ## get a list of statenames from cdat:
    stnames <- unique(cdat$state) ;
    ## check that "state" as passed with the function is valid, else exit
    if ( !(is.element(state, stnames)) ) {
        print("invalid state");
        return(state)  }

    ## make outnames a list of valid values for "outcome"
    outnames <- c("heart attack", "heart failure", "pneumonia")
    ## test if "outcome" as passed with the function is valid, else exit
    if ( !(is.element(outcome, outnames)) ) {
        print("invalid outcome");
        return(outcome)  }

    ## map the "outcome" names to column numbers
    outnrs <- c("heart attack" = 3, "heart failure" = 4, "pneumonia" = 5)
    ## choose the correct nr for the desired outcome
    outnr <-  as.numeric(outnrs[outcome])
    ## subset cdat to only contain data from the desired state
    stdat <- cdat[cdat$state == state,]
    names(stdat) <- c("name", "state", "attack", "failure", "pneumonia")
    ## get the best rate value from the required column
    brate <- min( as.numeric(stdat[, outnr]), na.rm = TRUE)
    ## get a sorted list of all the names where the rate is best
    bname <- stdat[ as.numeric(stdat[,outnr]) == brate, 1]
    return( sort(bname) )

}
