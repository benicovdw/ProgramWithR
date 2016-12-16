pollutantmean <- function(directory, pollutant, id = 1:332) {
    # set the working folder and read the filenames into a list
    setwd(directory)
    filelist <- list.files(".", "csv")
    # read the 1st csv file into a data frame
    data <- read.csv(filelist[id[1]])
    # determine the length of the id's
    l <- length(id)
    # if we only want one file's values skip the for loop
    if (l > 1){
        # for all the files according to the id vector "append"
        # the next file by reading it and rbining it to the
        # existing data frame
        for (x in 2:l) {
            data <- rbind(data, read.csv(filelist[id[x]]))
        }
    }
    # subset the required pollutant and clean
    workdata <- data[pollutant]
    # id the subset of non NA values
    clean <- !is.na(workdata)
    # get the clean data set and calc the mean
    workdata <- workdata[clean]
    mean(workdata)
}
