pollutantmean <- function(directory, pollutant, id = 1:332) {

    filelist <- list.files("specdata", "csv")

    data <- read.csv(filelist[id[1]])

    if (length(id) > 1){
        for (x in 2: length(id)) {
            data <- rbind(data, read.csv(filelist[id[x]]))
        }
    }

    workdata <- data[pollutant]
    mean(workdata[!is.na(workdata)])
}
