pollutantmean <- function(directory , pollutant = 'sulfate' , id = 1:100){
  
  if(grep("specdata", directory) == 1) {
    directory <- ("C:/Users/muhammad.atta/Documents/specdata/")
  }
  
  all_file <-  as.character(list.files(directory))

  file_Paths <- paste(directory,all_file , sep="")
  mean_vector = c()

  for (i in id) {
    
    con <- file(file_Paths[i],'r')
    file_read <- read.csv(con,header = T , sep = ",")
    head(file_read)
    na_removed <- file_read[!is.na(file_read[,pollutant]),pollutant]
    mean_vector <- c(mean_vector,na_removed)
    on.exit(close(con))
  }
  
  result <- mean(mean_vector)
  
  return(round(result,3))
  
}

complete <- function(directory  , id = 1:100){
  
  if(grep("specdata", directory) == 1) {
    directory <- ("C:/Users/muhammad.atta/Documents/specdata/")
  }
  
  all_file <-  as.character(list.files(directory))
  
  file_Paths <- paste(directory,all_file , sep="")
  nobs = numeric()
  
  for (i in id) {
    
    con <- file(file_Paths[i],'r')
    file_read <- read.csv(con,header = T , sep = ",")
    head(file_read)
    nobs = c(nobs,sum(complete.cases(file_read)))
    on.exit(close(con))
  }
  
  result <- data.frame(id,nobs)
  
  return(result)
  
}

corr <- function(directory, threshold = 0) {
  if(grep("specdata", directory) == 1) {
    directory <- ("C:/Users/muhammad.atta/Documents/specdata/")
  }
  
  all_file <-  as.character(list.files(directory))
  
  file_Paths <- paste(directory,all_file , sep="")
  nobs = numeric()
  
    tcorr <- function(fname){
      data <- read.csv(file.path(directory, fname))
      nobs <- sum(complete.cases(data))
      if (nobs > threshold) {
        return (cor(data$nitrate, data$sulfate, use="complete.obs"))
      }
      
    }

    tcorrs <- sapply(list.files(directory), tcorr) #get all correlations + NULLs
    tcorrs <- unlist(tcorrs[!sapply(tcorrs, is.null)]) #remove NULLs
    return (tcorrs)
    
}