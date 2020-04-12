## 4. Rank hopstials in all states
## The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num.

rankall <- function(outcome, num="best"){
  
  #read in data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  colnames(data)[c(11,17,23)] = c("heart attack", "heart failure", "pneumonia")
  
  if(!(outcome %in% c("heart attack","heart failure", "pneumonia"))){
    stop("invalid outcome") 
  }
  
  #check num input
  if(!is.numeric(num) & !(num %in% c("best", "worst"))){
    stop("wrong num input")
  }
  
  #create empty dataframe to return ranking
  rank_df = data.frame(matrix(NA,nrow = length(unique(data$State)), ncol = 2))
  colnames(rank_df) = c("hospital","state")
  rank_df$state = sort(unique(data$State))
  
  for(i in 1:nrow(rank_df)){
    state = rank_df[i,"state"]
    #subset data based on state and make outcome column numeric
    data_state = data[which(data$State == state),]
    suppressWarnings({
      data_state[,outcome] = as.numeric(data_state[,outcome])
    })
    
    if(nrow(data_state) == 0){
      rank_df[i,"hospital"] = NA
    } else if(num == "best"){
      min_data = data_state[which(data_state[,outcome] == min(data_state[,outcome],na.rm = T)),]
      min_data = min_data[order(min_data$Hospital.Name),]
      rank_df[i, "hospital"] = min_data$Hospital.Name[1]
    } else if(num == "worst"){
      max_data = data_state[which(data_state[,outcome] == max(data_state[,outcome],na.rm = T)),]
      max_data = max_data[order(max_data$Hospital.Name),]
      rank_df[i, "hospital"] = max_data$Hospital.Name[1]
    } else if(num > nrow(data_state)){
      rank_df[i,"hospital"] = NA
    } else{
      data_state = data_state[order(data_state$Hospital.Name),]
      data_state$rank = rank(data_state[, outcome], ties.method="first")
      rank_df[i, "hospital"] = data_state[which(data_state$rank == num),"Hospital.Name"][1]
      
    }
    
  }
  return(rank_df)
}