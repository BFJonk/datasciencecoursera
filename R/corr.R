corr <- function (directory, threshold = 0) {
        
        monitor_filename <- function(monitor_number) {
                pmn <- stri_pad_left(monitor_number,
                                     width = 3,
                                     pad = "0")
                name<-paste0(pmn, ".csv")
                file.path(directory,name)
        }
        
        listf <- list.files(directory)
        
        
        number_of_good <- function (t) {
                cc <- complete.cases(t)
                cct <- cc[cc == TRUE]
                number_of_good <- length(cct)
        }
        
        #result-vectors
        nobs <- vector(mode="numeric")
        corr_vector <- vector(mode="numeric")
        
        #load each file and calculate corr
        for (f in listf) {
                #construct filename
                fn <- file.path(directory,f)
                #load data
                t<-data.frame
                t <-read_csv(file.path(directory,f)) 
                #calculate nobs
                nobs <- c(nobs, number_of_good(t))
                if (number_of_good(t) > threshold) {
                        x <- as.data.frame(t[, c("nitrate","sulfate")])
                        if (class(x[1,1]) == "numeric") {
                                corr <- cor(x, use = "complete.obs")
                                corr_vector <- c(corr_vector, corr[1, 2])
                        }        
                }
                
        
                
        }
        
        print(corr_vector)
}