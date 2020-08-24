complete <- function (directory, id = 1:332) {
        ## "directory" is a char vector of length 1 indicating the location of the CSV files
        ## "id" is an integer vector indicaton the montior ID numbers to be used
        
        ## return a data frame of the form: id nobs where nobs is the number of complete cases
        whole <- data.frame("id"=NULL,"nobs"=NULL) ## initialize a void df "whole" to fill it with the loop
        for(i in id) { ## do the same procedure for each file of the chosen seq (id)
                address_dir <- paste(directory, "/", sprintf("%03.0f", i), ".", "csv", sep="") ## constrction of the path
                f <- read.csv(address_dir, header = TRUE) ## read the file
                sulf <- f[,2] ## choose only the sulfate column's value
                nitr <- f[,3] ## choose only the nitrate column's value
                cc <- complete.cases(sulf,nitr) ## define as TRUE only where both has a value (other than NA)
                s <- sum(cc) ## calculate the sum since T=1 and F=0 
                each <- data.frame(i,s) ## make a single row df with id and nobs
                whole <- rbind(whole,each) ## bind this df under the previous verion of df "whole"
                }
        whole
        }
}