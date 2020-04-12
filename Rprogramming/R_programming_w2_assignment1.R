#part1
pollutantmean <- function(directory, pollutant, id = 1:332){
  csv = list.files(directory, pattern = ".csv")
  csv = csv[id]
  mean = c()
  for (i in 1:length(csv)) {
    path = paste0(directory, csv[i])
    tmp = read.csv(path)
    mean = c(mean,tmp[,pollutant])
  }
  
  return(mean(mean, na.rm = TRUE))
}

# directory = "~/Downloads/specdata/"
# pollutant = "nitrate"
# id = 70:72
# pollutanmean(directory, pollutant, id)

#part2
complete <- function(directory, id = 1:332){
  df = data.frame(matrix(NA,nrow = length(id), ncol = 2))
  colnames(df) = c("id","nobs")
  csv = list.files(directory, pattern = ".csv")
  csv = csv[id]
  
  for (i in 1:length(id)) {
    df[i,"id"] = i
    path = paste0(directory, csv[i])
    tmp = read.csv(path)
    tmp = tmp[complete.cases(tmp),]
    df[i,"id"]=i
    df[i, "nobs"] = nrow(tmp)
    
  }
  return(df)
  
}

#complete("~/Downloads/specdata/", c(2, 4, 8, 10, 12))

#part3
corr <- function(directory, threshold = 0){
  df = complete(directory)
  df = df[which(df$nobs>threshold),]
  corr = c()
  csv = list.files(directory, pattern = ".csv")
  
  if(nrow(df) ==0){
    return(corr)
  }else{
    for(i in 1:nrow(df)){
      path = paste0(directory, csv[df[i,"id"]])
      tmp = read.csv(path)
      tmp = tmp[complete.cases(tmp),]
      corr = c(corr, cor(tmp$sulfat, tmp$nitrate))
    }
    return(corr)
  }
  
}

#cr <- corr(directory = "~/Downloads/specdata/", threshold = 400)
#head(cr)

#quiz
library(dplyr)
directory = "~/Downloads/specdata/"
round(pollutantmean(directory, "sulfate", 1:10), digits = 3)

round(pollutantmean(directory, "nitrate", 70:72), digits = 3)

round(pollutantmean(directory, "sulfate", 34), digits = 3)

round(pollutantmean(directory, "nitrate"), digits = 3)

cc <- complete(directory = directory, c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete(directory, 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete(directory, 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr(directory)                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr(directory, 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr(directory, 2000)                
n <- length(cr)                
cr <- corr(directory, 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
