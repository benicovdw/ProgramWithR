complete <- function(directory, id = 1:332) {
    # set the working folder and read the filenames into a list
    setwd(directory)
    filelist <- list.files(".", "csv")
    l <- length(id)
    # create a data frame to hold the complete cases values
    ccframe <- data.frame(1:l,1)
    names(ccframe) <- c("id", "nobs")
        # for every file read it into a data frame and get the
        # number of complete cases
        for (x in 1:l) {
            data <- read.csv(filelist[id[x]])
            cc <- complete.cases(data)
            nrcc <- length(cc[cc == TRUE])
            # read the values into ccframe
            ccframe[x,1] <- id[x]
            ccframe[x,2] <- nrcc
        }
    # return complete cases frame
    ccframe
}
