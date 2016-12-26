rankhospital <- function(state, outcome, num = "best"){
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

    ## order stdat according to outnr columns and read into bname
    bname <- stdat[order( as.numeric(stdat[,outnr]), stdat[,1] ) , c(1,outnr)]

    if (num == "best") { num <- 1 }
    if (num == "worst") { num <- which.max(bname[,2]) }
    return(bname[num,])

}
