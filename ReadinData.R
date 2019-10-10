ReadInData <-function(path, pattern) {

  filenames <- list.files(path=path, pattern=pattern, full.names = FALSE)
  output<-DoForEachFile(filenames)
  return(output)
}

#z = 1  #row counter for output dataframe
#xdf <- data.frame(matrix(nrow = 20, ncol = 3))


DoForEachFile <- function(filenames,z=1,xdf=data.frame(matrix(nrow=10,ncol=3))) {
  for (f in filenames) {
    ##read data into 'dat' dataframe from csv file    
    dat <- read.csv(file = file.path("data/Raw",f), header = TRUE)
    ##create second dataframe without first column (subject numbers or 'infants')
    dat2 <- dat[,2:length(dat[1,])]
    days <- names(dat2)

 #   z = 1  #row counter for output dataframe (xfd) 
    aa = 1  #idmarker.  increments along with id number (or infants)

    infants <- dat[,1]  ##create a vector of subject numbers from first column of input file

    for (j in infants) {
       weights <- GetWeights(aa,dat2)

       output <- SetData(weights,days)
       if (!is.na(output[1,1])) {

          xdf[z:(z+length(output[,1])-1),1] <- j
          xdf[z:(z+length(output[,1])-1),2:3] <- output[,1:2]
          z = z + length(output[,1])
       }
aa = aa + 1
          }
    aa = 1
    }
return(xdf)
}

GetWeights <- function(idmarker,data) {
  ##function gets measurements, one row at a time (by idmarker) and returns in 'weights', including NA's
  weights <- data[idmarker,]
  return(weights)
}

SetData <- function(weight_vector,days_vector,y=1,z=1) {
  # function takes 'weight vector' and returns dataframe with non-missing values in columns along with day of measurement
  # y = nonmissing data counter.  increments with each non-missing data entry.  keeps track of correct entry line for output file
  # z = day counter.  Incrments with each loop.  keeps track of the day measurement was taken
  miniDataFrame <- data.frame(matrix(nrow = 1, ncol = 2))
  
  for (k in weight_vector) {

    if (!is.na(k)) {
      miniDataFrame[y,1] <- as.numeric(substring(days_vector[z],2,nchar(days_vector[z])))
      miniDataFrame[y,2] <- k
      y = y+1
      }
    z = z+1
  }
  return(miniDataFrame)
}
