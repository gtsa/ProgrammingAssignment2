pollutantmean <- function (directory, pollutant, id = 1:332) {
        ## "directory" is a char vector of length 1 indicating the location of the CSV files
        ## "pollutant" is a char vector of length 1 indicationg the name of the pollutant 
        ## for which we will calculate the mean; either "sulfate" or " nitrate".
        ## "id" is an integer vector indicaton the montior ID numbers to be used
        ## Return the mean of the pollutant across all monitors list in the "id" vector (ignoring NA values) 
        
        
        ## Define the col of the file/df that corresponds to the different types of pollutant
        if (pollutant == "sulfate") {
                p <-2
        } else if (pollutant == "nitrate") {
                p <-3
        } else {print("You must give the type of pollutant")}
        whole <- data.frame("Date"=NULL, "Sulfate"=NULL, "Nitrate"=NULL, "ID"=NULL) # initialize a void df "whole" to fill it with the loop
        for(i in id) { ## do the same procedure for each file of the chosen seq (id)
                address_dir <- paste(directory, "/", sprintf("%03.0f", i), ".", "csv", sep="") ## constrction of the path
                f <- read.csv(address_dir, header = TRUE) ## read the file
                f <- f[-which(is.na(f[,p])),] ## Remove the rows where the pollutant (p) is NA
                whole <- rbind(whole,f) ## add the rest of the rows under the previous verion of "whole"
                }
        mean(whole[,p])
}
