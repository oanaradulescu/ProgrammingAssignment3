rankhospital <- function(state, condition, num) {
        outcome <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        
        if (state %in% outcome$State == F) stop("invalid state")
        
        outcome <- outcome[outcome$State == state,]
        
        keywords <- c("^Hospital\\.30.Day\\.Death\\.\\.Mortality\\.\\.Rates\\.from\\.", unlist(strsplit(condition, " ")))
        pattern <- paste(keywords, collapse = ".*")
        
        mortality <- outcome[,grepl(pattern[1], names(outcome), ignore.case = T), drop = F]
        
        if (ncol(mortality) == 0) stop("invalid outcome")
        
        names(mortality)[1] <- "Mortality.Rate"
        
        df <- data.frame(cbind(outcome$State, mortality$"Mortality.Rate",  outcome$"Hospital.Name"))
        names(df) <- c("State", "Mortality.Rate", "Hospital.Name")
        
        df <- df[ order(as.numeric(as.character(df[,"Mortality.Rate"])), df[,"Hospital.Name"], df[,"State"]), ]
        df.worst <- df[ order(as.numeric(as.character(df[,"Mortality.Rate"])), df[,"Hospital.Name"], df[,"State"], decreasing = T), ]
        
        
        if (num == "best") { 
                as.character(df[1,]$"Hospital.Name")
        } else {
                if (num == "worst") {
                        as.character(df.worst[1,]$"Hospital.Name")
                } else {
                        
                        as.character(df[num,]$"Hospital.Name")
                }
        }
        
        
}