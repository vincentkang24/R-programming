best <- function(state, outcome) { 
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        dataframe <- as.data.frame(cbind(data[ ,2],    # hospital
                                         data[ ,7],    # state
                                         data[ ,11],   # heart attack
                                         data[ ,17],   # heart failure
                                         data[ ,23]),    # pneumonia
                                   stringsAsFactors = FALSE)
        colnames(dataframe) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
       
         ## Check that state and outcome are valid
        if (!state %in% dataframe[,"state"]){
                stop('invalid state')}
        else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop('invalid outcome')}
        else {
                sd <- which(dataframe[, "state"] == state)
                td <- dataframe[sd, ]    # extracting data for the called state
                od <- as.numeric(td[, eval(outcome)])
                min_val <- min(od, na.rm = TRUE)
                result  <- td[, "hospital"][which(od== min_val)]
                output  <- result[order(result)]
        }
        ## Return hospital name in that state with lowest 30-day death
        return(output)
        
}