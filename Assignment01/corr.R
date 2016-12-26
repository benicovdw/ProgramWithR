corr <- function(directory, threshold = 0) {
    # set the working folder and read the filenames into a list
    setwd(directory)
    filelist <- list.files(".", "csv")
    # create the vector to hold the values of the correlations
    corvect <- 0:0
    cvi <- 1
    # for every file read it into a data frame and get the
    # number of complete cases
    for (x in 1:length(filelist)) {
        data <- read.csv(filelist[x])
        cc <- complete.cases(data)
        nrcc <- length(cc[cc == TRUE])
        # if the nr of complete cases are > threshold then subset
        # nitrate and sulfate, remove the incomplete rows & calculate
        # the correlation ni su and add it to the correlation vector
        # (increment the correlation vector counter cvi)
        if (nrcc > threshold) {
            ni <- data["nitrate"]
            su <- data["sulfate"]
            ni <- ni[cc,]
            su <- su[cc,]
            corvect[cvi] <- cor(ni, su)
            cvi <- cvi +1
        }
    }
    # return the correlation vector
    corvect
}
