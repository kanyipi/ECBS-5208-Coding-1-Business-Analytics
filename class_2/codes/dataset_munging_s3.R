##########################
## Data Munging         ##
##      CLASS 2         ##
##       CEU            ##
##########################

#
# Data Munging with hotel dataset

rm( list = ls() )
library( tidyverse )

# Import raw data
raw_df <- read_csv( "https://osf.io/yzntm/download" )
# as this is a large file we are going to save it
# data_dir <- paste0( getwd() , '/data/' )
# write_csv( raw_df , paste0( data_dir , '/raw/hotelbookingdata.csv' ) )

# Have glimpse on data
glimpse( raw_df )

# Add a new variable
df <- mutate( raw_df , nnights = 1 )
rm(raw_df)
###
# Data cleaning - separating character vector with separator 
# Check accomotationtype, note typeof is character
select( df , accommodationtype )

# Clean accommodationtype column: separate the characters at @ 
#   and create two new variables: "garbage" and "acc_type"
df <- separate( df , accommodationtype , "@" ,
                into = c("garbage","acc_type") )

# Remove the variable garbage (do not select)
df <- select( df , -garbage )

# Create a factor variable for acc_type (use of factors will be clear in the next class)
df <- mutate( df , acc_type = factor( acc_type ) )
# check if it is a factor
is.factor( df$acc_type )


##
# Task - creating a numeric vector w simple separation
#   1) Correct the guestreviewrating into simple numeric variable
df <- separate( df , guestreviewsrating , "/" ,
                into = c("garbage","guestrev_type") )
df <- select( df , -garbage )

typeof( df$guestrev_type )
#   2) check with `typeof()`
df$ratings <- as.numeric(df$guestrev_type)
#   3) convert the variable into a numeric variable


##
# Create a numeric vector with complicated separation
# How to deal with distance measure:
df$center1distance
# we have two numeric values than the format of the distance "miles"
# to get it right let us check first how to find patterns in characters:
eg1 <- "Coding is 123 fun!"
# Find numeric values in a vector and replace it
gsub( "12" , "extra fun" , eg1 )
# Find and replace any numeric value in a simple expression
gsub("[0-9\\.]"," extra fun," , eg1)
# Find all non-numeric values (^ sign) and replace with "" (none -> remove it)
gsub("[^0-9\\.]","" , eg1)

# Create new numeric vectors using the distance measures
df <- mutate( df , 
              distance = as.numeric(gsub("[^0-9\\.]","", center1distance ) ),
              distance_alter = as.numeric(gsub("[^0-9\\.]","", center2distance ) ) )

##
# Task:
#  1) use separate() command instead of mutate and gsub (utilize that the decimals are not changing)
#  2) do not forget the type! 


###
## Rename variables
# with tidy approach it is recommended to use human-readable vector names as well!
df <- rename( df ,  ratingta_count = rating2_ta_reviewcount,
              country = addresscountryname,
              stars = starrating,
              city = s_city)
##
# Task:
#   also rename the following variables as follows:
       



####
## Find missing values - filtering
# look at one of our key variable: ratings
# we can tabulate the frequencies of the ratings
table( df$ratings , useNA = "ifany" )
# What can we do with the NA values?
# First check them with 'filter'
filter( df , is.na( ratings ) )
# if reasonable we can drop them, but there needs to be good reason for that!
df <- filter( df , !is.na( ratings ) )

##
# Task:
# Do the same for missing id-s and argue what to do with them! 
filter( df , is.na( hotel_id ) )
df <- filter( df , !is.na( hotel_id ) )

####
## Correcting wrongly documented observations:
# In case of `stars` there are only values from 0-5
table( df$stars , useNA = "ifany" )
# what does 0 star means? It is missing, but recorded as 0...
# we need to set these values to NA: re-define the stars variable:
df <- mutate( df , stars = na_if( stars , 0 ) )
table( df$stars , useNA = "ifany" )


###
## Filter out duplicates:
# 1) exact match for each values for a given observations
# Count the number of duplicates
sum( duplicated( df ) )
# Remove duplicates
df <- filter( df , !duplicated( df ) )

# 2) Remove duplicates to specific variables, that are important to us
# use of subset function
sub_df <- subset( df , select = c( country , hotel_id ) )
df <- filter( df , !duplicated( 
  subset( df , select = c( country,hotel_id,distance,
                           stars, ratings, price, year, month,
                           weekend, holiday ) ) ) )

###
## Task: Get specific dataset used in DA1 course:
#   1) Get hotels only from Vienna
#   2) Filter out the following observations:
#       - in date: 2017, November and 0s week (multiple conditions)
#       - with Hotel types which has stars between 3 and 4
#       - and drop observations which has price more than 1000 EUR.
hotel_vienna <- filter(df , city == 'Vienna' ,
                        year == 2017 ,
                        month == 11 ,
                        weekend == 0 ,
                        acc_type == 'Hotel' ,
                        stars >= 3 ,
                        stars <= 4 ,
                        price <1000)


##
# Make data table more `pretty`
# Can arrange the values in increasing order
hotel_vienna <- arrange( hotel_vienna , price )
# in case of decreasing order
hotel_vienna <- arrange( hotel_vienna , desc( price ) )


# Writing out csv as clean data
data_out <- "GitHub/ECBS-5208-Coding-1-Business-Analytics/class_2"
write_csv( hotel_vienna , paste0( data_out,
                                  "/data/clean/hotel_vienna_restricted.csv"))




#sqldf is an R package for runing SQL statements on R data frames,
#optimized for convenience. The user simply specifies an SQL statement
#in R using data frame names in place of table names and a database
#with appropriate table layouts/schema is automatically created,
#the data frames are automatically loaded into the database,
#the specified SQL statement is performed, the result is read back
#into R and the database is deleted all automatically behind the
#scenes making the database's existence transparent to the user 
#who only specifies the SQL statement. Surprisingly this can 
#at times be even faster than the corresponding pure R calculation
#(although the purpose of the project is convenience and not speed)
#SQLite by default but knows MySQL. SQLite, H2, MySQL and PostgreSQL
#its very good when you know one of the languages of either R or SQL and want to get to know the other better

hotel_vienna
install.packages('sqldf')
library("sqldf")



sqldf("select * from hotel_vienna")

limit5 <- sqldf("select * from hotel_vienna limit 5")
limit5

sqldf("select count(hotel_id) from hotel_vienna where price<200")

join1 <- sqldf("select rating_count, hotel_id from hotel_vienna")

join2 <- sqldf("select center1distance, hotel_id from hotel_vienna")

joined <- sqldf("select * from join1 inner join join2 using(hotel_id)")
joined

#you can also insert, nest select, group, use basic functions:avg count
#you cannot store proc, trigger, event
