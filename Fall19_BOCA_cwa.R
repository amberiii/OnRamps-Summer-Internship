## Setting the Training Data
na.zero <- function (x) {
  x[is.na(x)] <- 4
  x[x == 1] <- 0
  return(x)
}
# Merging all text cells into one
# Final Format: EID: ids, text: all text in one single cell, target: label positive - 4 /negative - 0
processTrain <- function(data_raw){
  data_ <- cbind.data.frame(data_raw[-c(1)])
  # browser()
  data_ <- data_[-c(8)]
  # browser()
  data_[is.na(data_)] <- ''
  data_new <- apply(data_, 1, paste, collapse=" ")
  data_a <- cbind(data_raw[c(1)], data_new, cbind(data_raw[c(8)]))
  data_a <- na.zero(data_a)
  names(data_a) <- c("ids", "text", "target")
  return(data_a)
}

?apply

setwd("C:/Users/jl67386/Box/OnRamps/Student Associates/2. Jiawen (Scarlett) Li/CWA")
mark <- read.csv("(2019-07-26) High_Priority_SER Marked.csv")
drop <- c("X","Q7","Q18")
mark = mark[,!(names(mark) %in% drop)]
temp = processTrain(mark)

train <- data.frame(read.csv('train.csv'))
train <- rbind(train, temp)

write.csv(train, "train_Marked.csv", row.names = F)

for (i in 1:8){
  temp = processTrain(read.csv((paste("Data_Labeled_",i,".csv", sep = ''))))
  train <- rbind(train, temp)
}

## Setting the test data
# setwd("C:/Users/jl67386/Box/OnRamps/Student Associates/2. Jiawen (Scarlett) Li/CWA")
processTest <- function(data_raw){
  data_ <- cbind.data.frame(data_raw[-c(1)])
  data_[is.na(data_)] <- ''
  data_new <- apply(data_, 1, paste, collapse=" ")
  data_a <- cbind(data_raw[c(1)], data_new)
  data_a <- na.zero(data_a)
  names(data_a) <- c("ids", "text")
  return(data_a)
}
na.zero <- function (x) {
  x[is.na(x)] <- 4
  x[x == 1] <- 0
  return(x)
}

# testQList is the list of questions we want to conduct analysis on
BOCA <- read.csv("Student Beginning-of-Course Assessment (19-20)_August 21, 2019.csv")
testQList <- c("Q4", "Q19","Q20", "Q21", "Q12", "Q14","Q39")

BOCA <- BOCA[-c(1,2),] #took out the first two rows (see raw excel file)

# studentInfoList includes two extra columns(DOB - Q7, OnRamps Course - Q18) to help identify the student
studentInfoList <- c("Q4", "Q5","Q6","Q7","Q8","Q18","Q19","Q20", "Q21", "Q12", "Q14","Q39")

test = processTest(BOCA[,names(BOCA) %in% testQList])
write.csv(test, "test_8_21.csv")

## EID Data Extract
# After we got the EID list from Python, we use the list to match EID with the original file and get that student row
studentIndentify = BOCA[,names(BOCA) %in% studentInfoList]
IDS = read.csv("EIDs_low_8_20.csv", header = F)
CWAnalysis = studentIndentify[studentIndentify$Q4 %in% IDS$V1,]
write.csv(CWAnalysis, "Low_8_20.csv")

