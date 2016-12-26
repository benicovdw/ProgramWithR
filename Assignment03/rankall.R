rankall <- function (outcome, num = "best") {
    ## read data from file,
    cdat <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings="Not Available");
    cdat <- cdat[,c(2,7,11,17,23)]  ## choose columns that we need
    names(cdat) <- c("name", "state", "attack", "failure", "pneumonia") ## give short names

    stnames <- unique(cdat$state)  ## get list of statenames from cdat
    stnames <- sort(stnames)
    outnames <- c("heart attack" = 3, "heart failure" = 4, "pneumonia" = 5)  ## list of valid "outcome"
    if (   !(   is.element(outcome, names(outnames) ) ## test if "outcome" is valid, else exit
    ) ) { print("invalid outcome"); return(outcome);  }

    outnr <-  outnames[outcome] ## choose the correct nr for outcome
    if (num == "best") { num <- 1 }
    if (num == "worst") { getlastrow <- TRUE } else { getlastrow <- FALSE }

    hname <- data.frame()  ## make an empty hname
    for (x in stnames) {   ## loop through all the statenames
        stdat <- cdat[cdat$state == x,] ## subset cdat to only contain data from state
        names(stdat) <- c("name", "state", "attack", "failure", "pneumonia") ## give short names

        ## order stdat according to outnr columns and read into bname
        bname <- stdat[ order(stdat[,outnr], stdat[,1]) , c(1,2,outnr)]
        bname <- na.omit(bname) ; ## take our the NA's
        if (getlastrow) { num <- nrow(bname) } ## if outcome == worst get last row nr
        hname <- rbind(hname, bname[num,])
    }
    return(na.omit(hname))
}
