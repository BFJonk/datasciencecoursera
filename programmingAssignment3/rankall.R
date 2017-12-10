rankall <- function(outcome, num = "best") {
        ## This function does 3 things
        ## 1. it reads the 'outcome data' (outcome-of-care-measures.csv) from file
        ## 2. It checks that the input parmaters are valid
        ## 3. For each state, it returns a 2-column data frame containing the hospital
        ##    in each state that has the ranking specified in num.
        
        library(dplyr)
        
        ## 1. Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## coerce relevant columns to numeric
        data[,11] <- as.numeric(data[,11])##[11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"                            
        data[,17] <- as.numeric(data[,17])##[17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"                           
        data[,23] <- as.numeric(data[,23])##[23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
        
        
        ## 2. Check the input variables: if state and outcome and num are valid
        ## If an invalid state value is passed, 
        ## the function should throw an error via the stop function 
        ## with the exact message “invalid outcome” or "invalid rank" 
        
        ##valid values in vector  
        validOutcome <- c("heart attack", "heart failure", "pneumonia")
        validNum <- c("best", "worst")
        validStates <- unique(data$State)
        
        
        
        ##if not valid, stop
        if(!outcome %in% validOutcome) {stop("invalid Outcome")}
        if(!num %in% validNum) {
                if (!is.numeric(num)) stop("invalid Rank")}
        
        ## 3. For each state, it returns a 2-column data frame containing the hospital
        ##    in each state that has the ranking specified in num.
        sd <- split(data, data$State)
        
        ## Rank based on Outcome and Hospital.Name
        if (outcome == "heart attack") {
                ##outcome based on stateData[,11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                rankedData <- lapply(sd, sortdf, 11) ## sortdf will order 'sd' based on field 11 & Hospital.Name
                
        }        
        if (outcome == "heart failure") {
                ##outcome based on stateData[,17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                rankedData <- lapply(sd, sortdf, 17) ## sortdf will order 'sd' based on field 17 & Hospital.Name
                
        }
        if (outcome == "pneumonia") {
                ##outcome based on stateData[,23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                rankedData <- lapply(sd, sortdf, 23) ## sortdf will order 'sd' based on field 23 & Hospital.Name
                
        }
        
        ## Now, with the ranked data, return the requested rank (num), for each state       
        items <- lapply(rankedData, returnRankorWorst, num=num)
        ## regroup
        l <- do.call("rbind", items) # , deparse.level = 2
        
        ##set correct headers
        colnames(l) <- c("hospital", "state") 
        
        l
        
} 

returnRankorWorst <- function (rankedData, num) {
        ## this function returns specfic rank from rankedData
        ## or 'best' (first rank) or 'worst' (last rank)
        
        num
        
        ## interpret "best"
        if (num == "best") {num <- 1}
        
        ##check if 
        if (num == "worst") {
                ## if num == "worst", return last value
                t <- tail(rankedData, 1)
                r <- c(t$Hospital.Name, t[,7])
        } else if (num > length(rankedData)) {
                ##if num>rank: return NAs, except for State
                # return2 <- rankedData[num, ]
                # return2$State <- rankedData[1,7]
                # return <- return2
                r <- c(NA, rankedData[1,7])
                r
        } else {
                r <- c(rankedData[num, 2], rankedData[num, 7])
        }
        returnRankorWorst <- r
        returnRankorWorst
}
# ldf <- function(df) {  ##selects second Column
#         df[,c(2,7)] } 
# ldf(items)

sortdf <- function (df, arg) {
        ## This function orders a dataframe, on the arg-th argument & Hosptal Name
        rankedData <- df[order(df[,arg]     ##order by MR_rate 
                                      , df$Hospital.Name ##order by Name
                                      , na.last = NA),]        ##skip NAs
}