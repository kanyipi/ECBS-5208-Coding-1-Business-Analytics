#########################
## Assignment 1        ##
##      CLASS 1        ##
##  Deadline:          ##
##  2021/10/13 13:30   ##
#########################

##
# Task: fill out the blank parts with the needed commands

# 1) Add the command which clears the environment
rm( list = ls() )

# 2) Create a string variable, which states: 'This is my first assignment in R!'
str_var <- "This is my first assignment in R!"

# 3) Decide with the proper command whether this is truly a string variable or not:
is.character(str_var)
  
# 4) Create vector 'v', which contains the values of: 3, 363, 777, 2021, -987 and Inf
v <- c(3, 363, 777, 2021, -987, Inf)
  
# 5) Multiply this vector with 10 and name it as v_10
v_10 <- v*10  

# 6) Create a list, which contains 'str_var' variable and 'v' vector
mL <- list(str_var,v)
  
# 7) Get the value of 'Inf' out of this 'mL' variable with indexing
  
mL[[2]][6]
# +1) decide whether the former extracted value is infinite or not. 
#   The result should be a logical value.
1/0 == mL[[2]][6]
  
  
  