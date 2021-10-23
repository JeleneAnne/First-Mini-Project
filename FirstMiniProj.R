#All files should have the same directory
#1 Mean
pollutantMean <- function(directory, pollutant, id = 1:332) {
  #Creates a list of all the files  
   files <- list.files(directory, full.names = TRUE)
  
  # Function for indicating a file
  if (length(files[id])==1){
    mean(read.csv(files[id])[,pollutant], na.rm=1)
  }
  
  # Indicates many files in a row
  else {
    datas <- data.frame()
    for (i in 1:length(files[id])){
      datas <- rbind(datas, read.csv(files[i]))
    }
    #Calculates the mean
    mean(datas[,pollutant], na.rm=1)
  }
}


#2 Complete

complete <- function(directory, id = 1:332) {
  
  #Creates a list of all the files 
  files <- list.files(directory, full.names = 1)
  
  #Creates an empty data frame
  complete_files <- data.frame(id=NA, nobs=NA)
  
  #Function for the sum of all complete cases and enumerating the complete case by index
  for (i in id) {
    complete_files[i, 1] <- i
    complete_files[i, 2] <- sum(complete.cases(read.csv(files[i])))
  }
  
  complete_files
}


#3 Correlations
corr <- function(directory, threshold = 0) {
  id = 1:332
  
  #Creates list of all the files
  filename <- list.files(directory, full.names = TRUE)
  

  #Creates an empty data set  
  result <-vector(mode="numeric", length=0)
  
  for(i in seq(filename)) {
    
    #Reads the files
    airquality <- read.csv(filename[i])
    good <- complete.cases(airquality)
    airquality <- airquality[good, ]
    if (nrow(airquality) > threshold) {
     
      #Identifies the correlation
      correlation <- cor(airquality[["sulfate"]], airquality[["nitrate"]])
      result <- append(result, correlation)
      #Prints the correlation
    }
  }
  result
}


#4 Histogram

#Reads the files
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
str(outcome)
head(outcome)
ncol(outcome)
names(outcome)


outcome[, 11] <- as.numeric(outcome[, 11])

#Prints the histogram 
hist(outcome[, 11],
     main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
     xlab = "Deaths",
     ylab = "Frequency",
)