rankall <- function(condition, num) {
        outcome <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        
        keywords <- c("^Hospital\\.30.Day\\.Death\\.\\.Mortality\\.\\.Rates\\.from\\.", unlist(strsplit(condition, " ")))
        pattern <- paste(keywords, collapse = ".*")
        
        mortality <- outcome[,grepl(pattern[1], names(outcome), ignore.case = T), drop = F]
        
        if (ncol(mortality) == 0) stop("invalid outcome")
        
        names(mortality)[1] <- "mr"
        
        df <- data.frame(cbind(outcome$State, mortality$mr,  outcome$"Hospital.Name"))
        names(df) <- c("s", "mr", "hn")
        
        df$mr <- as.numeric(as.character(df$mr))
        
        dff <- by(df, df$s, function(df) df[ order(df[, "mr"], df[, "hn"]), ])
        dff.worst <- by(df, df$s, function(df) df[ order(df[, "mr"], df[, "hn"], decreasing = T), ])
        
        r <- NULL
        
        if (num == "best") {
                hn <- as.vector(sapply(dff, function(df) df[1,"hn"]))
                s <- as.vector(sapply(dff, function(df) df[1,"s"]))
                r <- cbind(hn, s)
                
        } else {
                if (num == "worst") {
                        hn <- as.vector(sapply(dff.worst, function(df) df[1,"hn"]))
                        s <- as.vector(sapply(dff.worst, function(df) df[1,"s"]))
                        r <- cbind(hn, s)
                        
                } else {
                        hn <- as.vector(sapply(dff, function(df) df[num,"hn"]))
                        s <- as.vector(sapply(dff, function(df) df[num,"s"]))
                        r <- cbind(hn, s)
                        
                }
        }
        
        r <- data.frame(r)
        names(r) <- c("hospital", "state")
        r
}