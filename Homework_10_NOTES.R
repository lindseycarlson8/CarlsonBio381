# Homework 10 - Review with Emily 
# 30 March 2022
# LSC 

# Nested for loop ---------------
# create a sample matrix 
mat <- matrix(sample(1:10, size = 9),
              nrow=3,
              ncol=3)
print(mat)

# writing the for loop
for(i in 1:nrow(mat)){
  for(j in 1:ncol(mat)){
    print(mat[i,j])
}
}

# Part 2: Putting custom functions in for loops 
# Simulating temp data
site1 <- runif(min = 60, max = 70, n = 10)
site2 <- runif(min = 60, max = 70, n = 10)
site3 <- runif(min = 40, max = 50, n = 10)

# put them in a df:
temps.df <- data.frame(site1, site3, site3)

# functionto convert from F to C 
degf.to.degc <- function(x){
  degc <- (x-32) * (5/9)
  
  return(degc)
}

# repeat function using a for loop
for(i in 1:ncol(temps.df)){
 print(degf.to.degc(x  = temps.df[,i]))
}