# Multiple Sites:
library(dplyr) # For row numbers and column selection
library(tidyr) # For the unite function

test <- function(Sites = NULL, NSubjects, RRatio = NULL){
  
  # Start with an empty data matrix
  matt = matrix(NA, nrow = length(Sites), ncol = NSubjects)
  dimnames(matt) = list(Sites)
  for (i in Sites){
    for (j in NSubjects){
      matt[i, ] = rep(i, NSubjects)
    }
  }
  
  # Return the transpose of the matrix:
  matt = as.data.frame(t(matt))
  
  # Adding the numbers to the end:
  for (column in matt){
    matt[column, ] = if_else(row_number(column) < 10,
                             true = paste(column, "0", row_number(column), sep = ""),
                             false = paste(column, row_number(column), sep = ""))
  }
  row.names(matt) = NULL
  
  #Select the first column & rows between [nrow(data) -> NSubjects]:
  matt = matt[NSubjects+1:(nrow(matt)-NSubjects), 1]
  
  ########### PUT THE T's and C's section in HERE!!!!!!!!!!!!!!!!!!
  for (i in (NSubjects+1)){
    timesT = NSubjects*(RRatio/(RRatio+1))
    timesC = (NSubjects - timesT)
    TLC = data.frame(TorC = sample(t(rep(c("T", "C"), times = c(timesT, timesC)))))
  }
  TLC = data.frame(TorC = rep(TLC$TorC, times = length(Sites)))
  
  # Shuffle the data randomly:
  matt = sample(matt)
  
  # Turn into a data.frame:
  matt = as.data.frame(matt)
  
  result = data.frame(c(TLC, matt))
  
  result = result %>%
    unite(Codes, c("matt", "TorC"), sep = "")
  # Return the end result:
  return(result)
}


FINAL <- test(Sites = c("AAA", "BBB", "CCC"), NSubjects = 30, RRatio = 1)







