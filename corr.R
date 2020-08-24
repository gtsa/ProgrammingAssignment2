corr <- function (directory, threshold = 0) {
        ## "directory" is a char vector of length 1 indicating the location of the CSV files
        ## "threshold" is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        ## Return a numeric vector of correlations
        cor_vect <- NULL ## initialie a void vector to fill with the correlations
        for(i in 1:332) { ## do the same procedure for each file of the directory
                address_dir <- paste(directory, "/", sprintf("%03.0f", i), ".", "csv", sep="") ## constrction of the path
                f <- read.csv(address_dir, header = TRUE) ## read the file
                sulf <- f[,2] ## choose only the sulfate column's value
                nitr <- f[,3] ## choose only the nitrate column's value
                cc <- complete.cases(sulf,nitr) ## define as TRUE only where both has a value (other than NA)
                s <- sum(cc) ## calculate the sum since T=1 and F=0
                if (s > threshold) { ## if the number of complete cases than threshold, do a follows
                        sulf_clean <- sulf [which(cc==TRUE)] ## Select only the sulfate values that correspond to the complete cases
                        nitr_clean <- nitr [which(cc==TRUE)] ## Select only the nitrate values that correspond to the complete cases
                        cori <- cor(sulf_clean, nitr_clean) ## Calculate their correlation
                        cor_vect <- c(cor_vect,cori) ## concatenate this cor to the principal one
                }
        }
        cor_vect
}