##########################
##     HOMEWORK         ##
##      CLASS 2         ##
##       CEU            ##
##########################

# 0) Clear work space
rm( list = ls() )
# 1) Load both data from github page and inspect (summary,glimpse)
#   Hint: you will need the `raw.github...` url address
dfschools <- read_csv(url("https://raw.githubusercontent.com/kanyipi/ECBS-5208-Coding-1-Business-Analytics/master/class_2/data/assignment_2/raw/CASchools_schools.csv"))
dfscores <- read_csv(url("https://raw.githubusercontent.com/kanyipi/ECBS-5208-Coding-1-Business-Analytics/master/class_2/data/assignment_2/raw/CASchools_scores.csv"))

glimpse(dfschools)
summary(dfschools)

glimpse(dfscores)
summary(dfscores)


# 2) Merge the two data table into one called `df`

df <- full_join( dfschools , dfscores , by = "district" )

# 3) Put the district variable into numeric format

df <- mutate( df , district = as.numeric( district ) )
is.numeric(df$district)

# 4) Create two new variables from `school_county`: 
#     - school should contain only the name of the school - character format
#     - county should only contain the name of the county - factor format

df <- separate( df , school_county , " - " ,
                into = c("school","county") )
df <- mutate( df , county = factor( county ) )
is.factor(df$county)
is.character(df$school)

# 5) Find missing values, write 1 sentence what and why to do what you do and execute.
# as they seems to be completly random, we can drop these observations

#I check each column for na values
filter( df , is.na( district ) )
filter( df , is.na( school ) )
filter( df , is.na( county ) )
filter( df , is.na( students ) )
filter( df , is.na( teachers ) )
filter( df , is.na( english ) )
df <- filter( df , !is.na( english ) )
filter( df , is.na( math ) )
df <- filter( df , !is.na( math ) )
filter( df , is.na( read ) )
df <- filter( df , !is.na( read ) )

# 6) Create a new variable called `score` which averages the english, math and read scores

df <- mutate( df , score = rowMeans(select(df,c(math,english,read))))

# 7) Find the county which has the largest number of schools in the data 
#     and calculate the average and the standard deviation of the score.

summary(df)
#sonoma
  
mean(df$score)
sd(df$score)

