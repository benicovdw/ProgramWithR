best <- function (state, outcome) {
    ## read data from file,
    cdat <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings="Not Available");
    cdat <- cdat[,c(2,7,11,17,23)]  ## choose columns that we need
    names(cdat) <- c("name", "state", "attack", "failure", "pneumonia") ## give short names

    stnames <- unique(cdat$state)  ## get list of statenames from cdat
    if ( !( is.element(state, stnames)  ## check if "state" is valid, else exit
            ) ) { print("invalid state"); return(state);  }

    outnames <- c("heart attack" = 3, "heart failure" = 4, "pneumonia" = 5)  ## list of valid "outcome"
    if (   !(   is.element(outcome, names(outnames) ) ## test if "outcome" is valid, else exit
                ) ) { print("invalid outcome"); return(outcome);  }

    outnr <-  outnames[outcome] ## choose the correct nr for outcome
    stdat <- cdat[cdat$state == state,]  ## subset cdat to only contain data from state
    names(stdat) <- c("name", "state", "attack", "failure", "pneumonia")  ## give short names

    brate <- min( stdat[, outnr], na.rm = TRUE)  ## get the best rate outnr column
    bname <- stdat[ stdat[,outnr] == brate, 1]  ## sorted list of names whith bestrate
    return( sort(bname) ) ## return bname
}
