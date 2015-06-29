pollutantmean <- function(directory, pollutant, id = 1:332) {
      table <- read.csv(directory/id)
      
      while(count <= length(id)) {
            colmeaninc <- which(table$pollutant)
            ignore <- is.na(colmeaninc)
            colmean <- colmeaninc[!ignore]
            count + 1
      }
      mean(colmean)
}