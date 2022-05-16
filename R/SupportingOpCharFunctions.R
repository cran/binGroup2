# Start supporting functions for operating characteristics functions
########################################################################
# This function converts a numeric vector to a character string, 
#   where values are separated by commas with no spaces
# This function is to be used when converting a vector of individual
#   indices or group sizes in a group testing algorithm to a character 
#   string to be displayed in a single column in a matrix/data frame

vec2string <- function(vec) {
  res.string <- vec[1]
  if (length(vec) > 1) {
    for (i in 2:length(vec)) {
      res.string <- paste(res.string, vec[i], sep = ",")
    }
  }
  res.string
}




# This function takes a matrix of individual accuracy measures, 
#   and finds the indices for each unique row of results
# The output is a matrix of the unique individual accuracy measures
#   with an additional column specifying the indices for all individuals
#   with the same accuracy measure values

get.unique.index <- function(results, col.num, rowlabel = NULL) {
  
  if (is.null(dim(results))) {
    results <- matrix(data = results, nrow = 1, ncol = length(results), 
                      dimnames = list(rowlabel, names(results)))
  }
  
  results <- as.data.frame(results)
  rows <- rownames(results)
  
  # account for individuals with same results as the first row
  if (nrow(results) == 1) {
    index <- rows
  } else {
    index <- rows[which(results[,col.num] == results[1,col.num])]
  }
  
  index.string <- vec2string(index)
  
  new <- cbind(results[index[1],], as.character(index.string))
  included <- as.character(index)
  
  # keep going, until all individuals are accounted for
  while (length(included) < nrow(results)) {
    new.start <- as.character(min(as.numeric(suppressWarnings(rows[rows != included]))))
    index <- rows[which(results[,col.num] == results[new.start,col.num])]
    index.string <- vec2string(index)
    new <- rbind(new, cbind(results[index[1],], as.character(index.string)))
    included <- as.character(sort(as.numeric(c(included, index))))
  }
  
  colnames(new)[length(colnames(new))] <- c("individuals")
  rownames(new) <- NULL
  new
}




# Brianna Hitt - 03.03.2020
#   Check whether all individual have equal accuracy measures
#   If not, list all the indices for each unique row
#   If yes, the individuals column will read "All"

check.all.equal <- function(results, col.num) {
  
  results <- as.data.frame(results)
  rows <- rownames(results)
  
  # account for individuals with same results as the first row
  index <- rows[which(results[,col.num] == results[1,col.num])]
  
  if (length(index) == nrow(results)) {
    index.string <- "All"
    new <- cbind(results[1,], as.character(index.string))
    colnames(new)[length(colnames(new))] <- c("individuals")
    rownames(new) <- NULL
    new
  }
}




# This function creates a vector of group sizes from a group membership matrix
get.pools <- function(group.nums){
  pools.table <- table(group.nums)
  pool.szs <- pools.table[[1]]
  for (i in 2:dim(pools.table)) {
    pool.szs <- c(pool.szs, pools.table[[i]])
  }
  pool.szs
}

#