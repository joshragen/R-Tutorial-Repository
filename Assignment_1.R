pollutantmean <- function(directory, pollutant, id = 1:332) {
      setwd(directory)
      

      finalm<-c()
      for(i in seq_along(id)) {
            fil <- list.files()
            #gets id object to include the 0 in the file names
            table <- read.csv(paste0(fil[i]), comment.char="")
            #extract table requested by id
            colinc <- table[pollutant]
            #make table of pollutant column
            colmean <- (colinc[complete.cases(colinc),])
            #puts real values into table
            finalm<-c(finalm, colmean)
      }
      print(mean(finalm))
      setwd("../")
}


complete <- function(directory, id = 1:332) {
      setwd(directory)
      nobs <- c()
      fil <- list.files()
      for (i in seq_along(id)){
            table <- read.csv(paste(fil[id[i]]))
            #extract table requested by id
            comp <- (table[complete.cases(table),])
            nobs <- c(nobs, nrow(comp))
      }
      tabs <- data.frame(id, nobs)
      print(tabs)
      setwd("../")
}

corr <- function(directory, threshold = 0) {
      setwd(directory)
      fil <- list.files()
      correl <- c()
      for(i in seq_along(fil)){
            table <- read.csv(paste(fil[i]))
            comp <- (table[complete.cases(table),])
            if(nrow(comp) > threshold) {
                  correl <- c(correl, cor(comp$nitrate, comp$sulfate))
            }
      }
      setwd("../")
      correl <- correl
}