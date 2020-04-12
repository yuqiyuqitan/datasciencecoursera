## 2. Find the best hospital in a a state
## The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specifed outcome
## in that state.

best <- function(state, outcome){
  #read in data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  colnames(data)[c(11,17,23)] = c("heart attack", "heart failure", "pneumonia")
  
  #check the state and outcome are valid
  if(!(state %in% unique(data$State))){
    stop("invalid state")
  }
  
  if(!(outcome %in% c("heart attack","heart failure", "pneumonia"))){
    stop("invalid outcome") 
  }
  
  #subset data based on state and make outcome column numeric
  data_state = data[which(data$State == state),]
  suppressWarnings({
    data_state[,outcome] = as.numeric(data_state[,outcome])
  })
 
  
  if(nrow(data_state) == 0){
    stop(paste0("no data on ", outcome, " in ", state))
  }
  
  #subset data bsed on min mortality rate on outcome and sort
  min_data = data_state[which(data_state[,outcome] == min(data_state[,outcome],na.rm = T)),]
  min_data = min_data[order(min_data$Hospital.Name),]
  
  #return hospital name with loewest 30-day death
  return(min_data$Hospital.Name[1])
}
