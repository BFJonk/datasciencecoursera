rankhospital <- function(state, outcome, num) {
        ## The best funtion does three things
        ## 1. It reads the 'outcome data' (outcome-of-care-measures.csv) from file
        ## 2. It checks that the input parmaters are valid
        ## 3. It returns the hospital name in the given state with the given rank wrt 30-day death rate
        

        ## 1. Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## coerce relevant columns to numeric
        data[,11] <- as.numeric(data[,11])##[11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"                            
        data[,17] <- as.numeric(data[,17])##[17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"                           
        data[,23] <- as.numeric(data[,23])##[23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"                               
        
        ## 2. Check the input variables: if state and outcome and num are valid
        ## If an invalid state value is passed, 
        ## the function should throw an error via the stop function 
        ## with the exact message “invalid state”, “invalid outcome” or "invalid rank" 
        
        ##valid values in vector  
        validState <- unique(data$State)
        validOutcome <- c("heart attack", "heart failure", "pneumonia")
        validNum <- c("best", "worst")
        
        ##if not valid, stop
        if(!state %in% validState) {stop("invalid state")}
        if(!outcome %in% validOutcome) {stop("invalid Outcome")}
        if(!num %in% validNum) {
                if (!is.numeric(num)) stop("invalid Rank")}
        
        
        ## 3. Return hospital name in that state with lowest 30-day death
        ## If there is a tie for the best hospital for a given outcome, 
        ## then the hospital names should be sorted in alphabetical order 
        ## and the first hospital in that set should be chosen 
        ## (i.e. if hospitals “b”, “c”, and “f” are tied for best, then hospital “b” should be returned).
        
        ##Subset for only data of selected State
        stateData <- data[data$State==state,]
        
        
        ## Rank based on Outcome and Hospital.Name
        if (outcome == "heart attack") {
                ##outcome based on stateData[,11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                rankedData <- stateData[order(stateData[,11]     ##order by MR_rate 
                                        ,stateData$Hospital.Name ##order by Name
                                        , na.last = NA),]        ##skip NAs
                item <- returnRankorWorst(num, rankedData)
        }        
        if (outcome == "heart failure") {
                ##outcome based on stateData[,17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
                rankedData <- stateData[order(stateData[,17] ##order by MR_rate & 
                                              ,stateData$Hospital.Name ##order by Name
                                              , na.last = NA),] ##skip NAs
                item <- returnRankorWorst(num, rankedData)
        }
        if (outcome == "pneumonia") {
                ##outcome based on stateData[,23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
                rankedData <- stateData[order(stateData[,23] ##order by MR_rate 
                                              ,stateData$Hospital.Name ##order by Name
                                              , na.last = NA),] ##skip NAs
                item <- returnRankorWorst(num, rankedData)
        } 
        
        ## return  Hospital.Name 
        item$Hospital.Name
}

returnRankorWorst <- function (num, rankedData) {
        ##this function returns specfic rank from rankedData
        ## or 'best' (first rank) or 'worst' (last rank)
        
        
        ## interpret "best"
        if (num == "best") {num <- 1}
        
        ## return rank or tail if worst
        if (num != "worst") {
                ## set return value
                return <- rankedData[num, ]
        } else {
                ## if num == "worst", return last value
                return <- tail(rankedData, 1)
                }
        return
}