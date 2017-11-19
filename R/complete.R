complete <- function (directory, id = 1:332)
{
        monitor_filename <- function(monitor_number) {
                pmn <- stri_pad_left(monitor_number,
                                     width = 3,
                                     pad = "0")
                name<-paste0(pmn, ".csv")
                file.path(directory,name)
        }
        
        listf <- list.files(directory)
        
        #result-table
        rt <- matrix ( ncol = 2, nrow = 332)
        colnames(rt) <- c("id", "nobs")
        nobs <- vector()
        
        
        #load each file
        for (i in id) {
                #construct filename
                f <- monitor_filename(i)
                #load data
                t <-
                        read.csv(f) 
                #Store result complete cases in vector n
                cc <- complete.cases(t)
                cct <- cc[cc == TRUE]
                nobs <- c(nobs, length(cct))
                #print(n)
        }
        
        #cbind rt
        rt <- cbind(id,nobs)
        print(as.data.frame(rt))
        
}