library(readr)
library(stringi)

pollutantmean <- function(directory, pollutant, id=1:332) {
        ## 'directory' is a character vector of lenght 1, indicating the
        ## location of the CSV files
        
        ## 'pollutant' is character vector of lenght 1, indicating the
        ## name f the pollutant for which we calulte the mean,
        ## either "sulfate" or "nitrate"
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        listf <- list.files(directory)
        
        one_monitor_mean <- function (table, column) {
                c <- (table[, c(column)])
                uc <- unlist(c)
                mean( uc , na.rm = TRUE)
                #unlist required to create vector out of table
        }
        
        
        monitor_filename <- function(monitor_number) {
                pmn <- stri_pad_left(monitor_number,
                                     width = 3,
                                     pad = "0")
                name<-paste0(pmn, ".csv")
                file.path(directory,name)
        }
        
        vals <- vector()
        
        for (monitor_number in id) {
                #construct filename
                f <- monitor_filename(monitor_number)
                #load data
                t <-
                        read.csv(f)
                #,
                 #                col_types = "Dnni")
                
               # means[monitor_number] <- one_monitor_mean(t, pollutant)
                vals <- c(vals, t[,pollutant])
        }
        
        mean(vals, na.rm = TRUE)
}