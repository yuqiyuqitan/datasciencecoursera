## 3. rank hospital
## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, #two character abbreviation name
                         outcome, 
                         num = "best" #rank of a hosptial
                         ){

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
  
  #check num input
  if(!is.numeric(num) & !(num %in% c("best", "worst"))){
    stop("wrong num input")
  }
  
  #subset data based on state and make outcome column numeric
  data_state = data[which(data$State == state),]
  suppressWarnings({
    data_state[,outcome] = as.numeric(data_state[,outcome])
  })
  
  
  if(nrow(data_state) == 0){
    stop(paste0("no data on ", outcome, " in ", state))
  }
  
  
  #return best hospital
  if(num=="best"){
    min_data = data_state[which(data_state[,outcome] == min(data_state[,outcome],na.rm = T)),]
    min_data = min_data[order(min_data$Hospital.Name),]
    
    #return hospital name with loewest 30-day death
    return(min_data$Hospital.Name[1])
  }
  
  #return worst hospital 
  if(num == "worst"){
    max_data = data_state[which(data_state[,outcome] == max(data_state[,outcome],na.rm = T)),]
    max_data = max_data[order(max_data$Hospital.Name),]
    
    #return hospital name with loewest 30-day death
    return(max_data$Hospital.Name[1])
  }
  
  #remove hospital with NAs in outcome
  data_state = data_state[!is.na(data_state[,outcome]),]
  
  #return NA if the num is greater the num of hospitals or is negative
  if(num > nrow(data_state) | num <= 0){
    return(NA)
  }
  
  #if num is within bounds of parameters
  data_state = data_state[order(data_state$Hospital.Name),]
  data_state$rank = rank(data_state[, outcome], ties.method="first")
  
  return(data_state[which(data_state$rank == num),"Hospital.Name"][1])
  
}