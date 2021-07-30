


#hist(data[,11],main="30-day death rates from heart attack")

best <- function(state,outcome){
  file_path="./r_programming/"
  data <- read.csv(paste0(file_path,"outcome-of-care-measures.csv"),
                   colClasses="character")
  
  which.state <- which(data$State==state)
  which.col <- numeric(0)
  if(outcome=="heart attack"){
    which.col <- 11
  }else if (outcome=="heart failure"){
    which.col <- 17
  }else if (outcome=="pneumonia"){
    which.col <- 23
  }else{
    message("invalid outcome")
    return(0)
  }
  #browser()
  data.filtered <- data[which.state,c(2,7,which.col)]
  data.filtered <- data.filtered[complete.cases(data.filtered),]
  data.filtered[,2] <- as.numeric(data.filtered[,2])
  data.filtered[,3] <- as.numeric(data.filtered[,3])
  
  data.ordered <- data.filtered[order(data.filtered[,3],data.filtered[,1]),]
  
  return(data.ordered$Hospital.Name[1])
}

rankhospital <- function(state,outcome,num="best"){
  file_path="./r_programming/"
  data <- read.csv(paste0(file_path,"outcome-of-care-measures.csv"),
                   colClasses="character")
  #browser()
  which.state <- which(data$State==state)
  which.col <- numeric(0)
  if(outcome=="heart attack"){
    which.col <- 11
  }else if (outcome=="heart failure"){
    which.col <- 17
  }else if (outcome=="pneumonia"){
    which.col <- 23
  }else{
    message("invalid outcome")
    return(0)
  }
  data.filtered <- data[which.state,c(2,7,which.col)]
  data.filtered[,3] <- as.numeric(data.filtered[,3])
  data.filtered <- data.filtered[complete.cases(data.filtered),]
  data.ordered <- data.filtered[order(data.filtered[,3],data.filtered[,1]),]
  
  if(num=="best"){
    which.num <- 1
  }else if(num=="worst"){
    which.num <- length(data.ordered$Hospital.Name)
  }else{
    which.num <- as.numeric(num)
  }
  
  return(data.ordered$Hospital.Name[which.num])
}

rankall <- function(outcome,num="best"){
  file_path="./r_programming/"
  data <- read.csv(paste0(file_path,"outcome-of-care-measures.csv"),
                   colClasses="character")
  #browser()
  state.vect <- unique(data[,7])
  state.vect <- state.vect[order(state.vect)]
  
  hospticals <- sapply(state.vect,rankhospital,outcome,num)
}