best <- function(state, outcome) {
        ## The best funtion does three things
        ## 1. It reads the 'outcome data' (outcome-of-care-measures.csv) from file
        ## 2. It Checks that state and outcome are valid
        ## 3. Return hospital name in that state with lowest 30-day death
        
                ## Nb. test funtion with : 
                ## validState <- unique(data$State)
                ## sapply(validState, best, "heart attack")
                ## Double value in best("MT", "heart attack")

        ## 1. Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        
        ## coerce relevant columns to numeric
        data[,11] <- as.numeric(data[,11])##[11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"                            
        data[,17] <- as.numeric(data[,17])##[17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"                           
        data[,23] <- as.numeric(data[,23])##[23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"                               

        
        
        ## 2. Check the input variables: if state and outcome are valid
         ## If an invalid state value is passed to best, 
         ## the function should throw an error via the stop function with the exact message “invalid state”.
         ## If an invalid outcome value is passed to best, the function should throw an error
         ## via the stop function with the exact message “invalid outcome”.
        
        ##valid values in vector  
        validState <- unique(data$State)
        validOutcome <- c("heart attack", "heart failure", "pneumonia")
        
        ##if not valid, stop
        if(!state %in% validState) {stop("invalid state")}
        if(!outcome %in% validOutcome) {stop("invalid Outcome")}
        
        
        
        ## 3. Return hospital name in that state with lowest 30-day death
         ## If there is a tie for the best hospital for a given outcome, 
         ## then the hospital names should be sorted in alphabetical order 
         ## and the first hospital in that set should be chosen 
         ## (i.e. if hospitals “b”, “c”, and “f” are tied for best, then hospital “b” should be returned).
        
        ##Subset for only data of selected State
        stateData <- data[data$State==state,]
        
        ##calculate the lowest Mortality Rate from the State
        minHA <- min(stateData[,11], na.rm = T)
        minHF <- min(stateData[,17], na.rm = T)
        minPN <- min(stateData[,23], na.rm = T)
        
        #select all Hospitals with the lowest Mortality Rate from the State
        HospMinHA <- stateData[stateData[,11]==minHA, 2]
        HospMinHF <- stateData[stateData[,17]==minHF, 2]
        HospMinPN <- stateData[stateData[,23]==minPN, 2]
        
        #return based on "outcome" whished for       
        if (outcome == "heart attack") {
                return <- sort(HospMinHA, na.last = NA) ## sort and remove NA values
        }
        if (outcome == "heart failure") {
                return <- sort(HospMinHF, na.last = NA)
        } 
        if (outcome == "pneumonia") {
                return <- sort(HospMinPN, na.last = NA)
        } 
                
                
        return[] ## return first element of sorted lost
}

