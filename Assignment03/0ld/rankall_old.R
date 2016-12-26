rankall <- function(outcome, num = "best"){
    ## read data from file, choose columns that we need and give short names
    cdat <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    cdat <- cdat[,c(2,7,11,17,23)]
    names(cdat) <- c("name", "state", "attack", "failure", "pneumonia")

    ## get a list of statenames from cdat:
    stnames <- unique(cdat$state) ;
    rm(cdat)

    source("rankhospital.R")

    Hnames <- data.frame()

    for (x in stnames) {
       Hnames <- rbind( Hnames, c( rankhospital(x, outcome, num) , x ) )
    }

    return(Hnames)
}
