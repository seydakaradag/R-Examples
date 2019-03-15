## ############################################################################
## R Introduction and Best Practice
## R Basics
## May 2016
## ############################################################################

#rm(list=ls())

## ============================================================================
## global variables
## ============================================================================

path.raw <- "C:/Projects/Vodafone_TR/"
#path.raw <- "C:/data-sync/teradata-work/teradata-project/training-knowledge-exchange/r-training-general/"
path.r <- paste0(path.raw, "r-scripts/")
path.dat <- paste0(path.raw, "data/")
path.tmp <- paste0(path.raw, "tmp/")

## ############################################################################
## ############################################################################
##
## INTRODUCTION TO R -- R BASICS
##
## ############################################################################
## ############################################################################

## ============================================================================
## Introduction
## ============================================================================

## Intro:
## - R = programming language
##       whole environment for statistical computing and programming!
## - pros: flexible, extendable, lots of functions,
##     easy interaction w/ other progs, ...
## - cons: in-memory, sometimes slow, command-line / scripting style

## IDEs:
## - Tinn-R / RStudio / Eclipse
## - shortcuts for easy interaction

## GUI Extentions:
## - R Commander -- like SPSS base (not as comprehensive)
## - Rattle -- data mining



## ============================================================================
## R Basics
## ============================================================================


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## R as a calculator
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1 + 1                ## addition
1.5 - 2              ## subtraction
5 * 2                ## multiplication
5 / 2                ## division
2 ^ 6                ## power
sqrt(64)             ## square root
exp(1)               ## exponentiation
sin(90)              ## sine
asin(0.8939967)      ## inverse sine
cos(0)               ## cosine
#acos(1)              ## inverse-cosine
tan(3)               ## tan
pi                   ## pi
abs(-1)              ## absolute
100 %% 97            ## mod (remainder)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## conditionals
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1 == 1     ## equality
1 = 1      ## assignment
1 < 1      ## less than
2 > 1      ## greater than
2 != 1     ## not equal


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## vectors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

vek1 <- c(1, 5, 7, 10)            # vector with 4 elements
vek1
vek2 <- 1:30                      # sequence from 1 to 30
vek2

c(1, 3, c(1, 2))                  # appending vectors within vectors

vek3 <- seq(0, 100, by=10)        # 0 to 100 in steps of 10
vek3
vek4 <- rep(1, times = 7)         # 1 repeated 7 times
vek4

vek5 <- rep(c(0,1), times = 7)    # 0, 1 repeated 7 times
vek5

v1 <- 1:3
v2 <- c(1, 2, 3)
v3 <- seq(from=1, to=3, by=1)

v1
v2
v3


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## functions
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## - R: functions entered in consoloe or in IDE
## - evaluated immediately
## - results from functions can be assigned to a variable (!)
## - if not assigned, then printed
## - e.g. mean()

rnorm(10)            ## generates vector of 10 values, normally distributed
a <- rnorm(10)       ## saves values in a variable "a"
a                    ## show variable

mean(a)              ## show mean of variable a
ma <- mean(a)        ## assign mean to variable ma
ma

## parameters:
## - passed in parenthesis
## - identified by their order
## - or passed as "named" arguments
## - use = sign (instead of <- sign)


library(Ecdat)             ## load package Ecdat (financial data sets)
data(Computers)            ## load dataset

## - need to install package if not already there install.packages('Ecdat')

spd <- Computers$speed     ## variable speed from data frame Computers (ignore for now)
ccd <- Computers$cd

mw <- mean(spd, trim=0, na.rm=TRUE)    ## mean
mw
mw <- mean(spd, trim = 0.1)            ## trimmed mean
mw

spd2 <- c(spd, NA)                     ## add NA to the vector (just for the example)
mw <- mean(spd2)
mw                                     ## mean returns NA
mw <- mean(spd2, na.rm=TRUE)
mw                                     ## mean excludes NAs and evaluataes the numbers

## - functions are generic
## - what they do depends on argument!
## - e.g., summary

summary(spd)
summary(ccd)
summary(Computers)

a <- rnorm(10)
a
A                         ## case sensitive


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## help
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Getting help:
## - internally

help(mean)
?mean

example(mean)

help(package="RJDBC")
help(package="gdata")

installed.packages()

help.search("mean")      ## help files containing "mean"
??mean
??"mean"

apropos("mean")          ## function names containing "mean"
example("mean")

vignette("xlsx")
vignette("caret")

## Getting help:
## - internet
## - sources listed on:      http://www.r-project.org
##   . for example: nabble R
##   . just google: "nabble r"
## - Stack Overflow
## - or just google CRAN + something for finding packages
##   . show CRAN view(s)     https://cran.r-project.org/web/views/



## ============================================================================
## data types
## ============================================================================

## - R is a programming language
## - power comes from that
## - and analytical functions that are provided --> combination!

## hence:
## - little bit of basics here

## data types:
## - integer and real number (number)
## - complex numbers
## - logical values (resulting from if clauses and comparisons)
## - string values (text data)
## plus:
## - special symbols: NaN, NA, NULL, Inf, -Inf

0/0            ## Not a Number
1/0            ## Infinity
-1/0           ## Negative infinity

mode(1)        ## data type
typeof(1)      ## internal type  ## integer
typeof(1L)                       ## integer literal
typeof(1.1L)                     ## double (unsucessfully tried to convert to numeric)
typeof(1+1i)                     ## complex

mode("1")

mode(TRUE)
mode(FALSE)
mode(T)
mode(F)
mode(true)
mode(f)

mode(1+3i)

a <- rnorm(10)
a
mode(a)

a <- 0/0
a
mode(a)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## factors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

f1 <- c("red", "green", "blue", "blue", "red", "red")
f1
mode(f1)

levels(f1)
str(f1)
class(f1)

ff1 <- as.factor(f1)
ff1
levels(ff1)
str(ff1)
class(ff1)

## better:
## create a numeric variable and assign labels
## (like a lookup table)

f2 <- c(1, 2, 3, 1, 2, 2, 1, 2)
f2
ff2 <- as.factor(f2)
ff2

levels(ff2)
levels(ff2) <- list("good"=1, "better"=2, "best"=3)
ff2
levels(ff2)
str(ff2)

## in one step:

f3 <- c(1, 2, 3, 1, 2, 2, 1, 2)
ff3 <- factor(f3,
              levels=c(1, 2, 3),
              labels=c("good", "better", "best")
)
ff3
levels(ff3)

## ordered factor:

f4 <- c(2, 1, 3, 2, 1, 1, 3, 1)
ff4 <- factor(f4,
              levels=c(1, 2, 3),
              labels=c("good", "better", "best")
)
ff4 <- as.ordered(ff4)
ff4

## ordered factor, second way:

f5 <- c(1, 2, 3, 1, 2, 2, 1, 2)
ff5 <- factor(f5,
              levels=c(1, 2, 3),
              labels=c("good", "better", "best"),
              ordered=TRUE
)
ff5

## compare ordered factors
ff5 < ff4

## cannot compare normal (unordered) factors
ff5 = factor(ff5, ordered = FALSE)
ff5 < ff4

is.factor(ff3)
is.ordered(ff3)

is.factor(ff4)
is.ordered(ff4)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## vector calculations
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

vek1 <- 1:3
vek2 <- 5:3
vek3 <- 1:6

vek1
vek2

vek1 * 2    # 1*2 2*2 3*2
vek1 + vek2 # 1+5 2+4 3+3

## if vectors are not of the same length, shorter one is repeated!!!

vek1
vek3

vek1+vek3  # 1+1 2+2 3+3 1+4 2+5 3+6

1:3 + 1:2


## accessing elements of vectors:

## by brackets:
vek1 <- 11:20
vek1

vek1[2]

vek1[5:10]

vek1[c(2, 4)]

## exclusion of elements: minus sign
vek1[-2]
vek1[-(2:4)]
vek1[-c(1, 3, 5, 7)]

## logical indexing:
vek1 <- 1:6
vek1

vek1 < 4          # vector of logical values
vek1[vek1 < 4]    # show elements where condition is TRUE

age <- 1:6
sex <- c("m", "m", "f", "f", "m", "m")

sex

age[sex=="m"]

length(vek1)


## ============================================================================
## exercise VECTORS:
## ============================================================================

## Tasks:
## - generate vector x: sequence from 1 to 10
## - generate vector y: elements 10, 2, 14, 7, 8
## - generate vector z: sequence from 20 to 11
## - generate vector w: elements 42, 12
## - generate vector yw: y followed by elements of w
## - x + z ?
## - x + y ?
## - y + w ?
## - generate vector with TRUE/FALSE values that indicate
##   whether the corresponding element of z is greater than 17
## - show all elements of z that are greater than 17
## - show all elements of x except the second and the third

## ============================================================================
## solution to exercise VECTORS:
## ============================================================================

## save script as ex11a.R
## e-mail solution script to john.odonoghue@teradata.com

## ============================================================================
## data structures
## ============================================================================

## - matrix:
##   . two-dimensional
##   . only one data type

mymat <- matrix(1:6, ncol=2, byrow=FALSE)
mymat

mymat <- matrix(1:6, ncol=2, byrow=TRUE)
mymat


class(mymat)
mode(mymat)
typeof(mymat)

str(mymat)

## - array
##   . multi-dimensional
##   . only one data type (like matrix, but more dimensions)

mymatrixarray <- array(1:21, dim=c(3, 4))
mymatrixarray
class(mymatrixarray)

myarray <- array(1:(3*4*2), dim=c(3,4,2))
myarray

class(myarray)
mode(myarray)
typeof(myarray)

str(myarray)


## - list:
##   . multiple variables of different type
##   . and different length
##   . more flexible than table!

mylist <- list(
  "item1" = c(1, 2, 3),
  "item2" = c("item", "two"))
mylist

class(mylist)
mode(mylist)
typeof(mylist)

str(mylist)

## lists very powerful
## - for programming
## - for automated multiple analyses (similar, but on different data sets)


## - data frame:
##   . multiple variables of different type
##   . and SAME length
##   . like a table: similar to SPSS or data base

mydf <- data.frame(
  "col1" = 1:3,
  "col2"=c("one", "two", "three"))
mydf

class(mydf)
mode(mydf)
typeof(mydf)
mode(mydf$col1)
typeof(mydf$col1)

str(mydf)

## Computers data that we have used before: data.frame
class(Computers)
str(Computers)

## ============================================================================
## data frames
## ============================================================================

## focus on data frames here:
## - demonstration of some functions that can be applied
## - using internal data set
## - from package Ecdat

install.packages("Ecdat")

library(Ecdat)   # load package Ecdat (financial data sets)
data(Computers)  # load dataset

summary(Computers$speed)
summary(Computers$cd)

## important functions:

str(Computers)    # structure of data frame
nrow(Computers)   # number of rows
ncol(Computers)   # number of columns
dim(Computers)    # dimensions (rows, columns)
names(Computers)  # variable names
head(Computers)   # first 6 rows
head(Computers, n=10)
tail(Computers)   # last 6 rows
tail(Computers, n=3)
Computers.new <- edit(Computers)    ## everything has to be assigned!
fix(Computers)
View(Computers)

## [[note]]:
## don't use R's edit() for editing data!
## (reproductibilty -- use code)
## don't sue R's edit() for inspecting data (use other programs)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## accessing data frames
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## - like in matrix notation: row, column

## accessing rows:

Computers[1,]      # first row, all columns (data frame)
Computers["1",]    # row with name "1", all columns (data frame)

rownames(Computers.new)


Computers.new <- Computers[-1,]

rownames(Computers.new)


## accessing columns:

Computers[,2]      # second column, all rows    (vector)
Computers[,"price"]  # column "price", all rows (vector)

Computers["price"]   # column "price"       (data frame)

head(Computers[,"price"])
head(Computers["price"])


head(Computers[c("price", "speed")])

Computers[1]         # first column         (data frame)

str(Computers)

Computers$price      # column "price"           (vector)

names(Computers)

Computers$pr
Computers$pri
Computers[,"pri"]

varname <- "price"
Computers[,varname]
Computers$varname

## accessing single elements:

Computers[3, 1]        # row 3, column 1 (price in third row)
Computers$price[3]     # 3rd element of col. "price"
Computers[3, "price"]  # same as above

Computers[,"price"][3] # same as above

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## accessing data frames
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## by logical indexing:

Computers.cd <- Computers[Computers$cd=="yes",]
head(Computers.cd)
dim(Computers.cd)

## using the subset() function:

Computers.nocd <- subset(Computers,
		subset= (Computers$cd != "yes") )
head(Computers.nocd)
dim(Computers.nocd)

Computers.both <- subset(Computers,
  subset= (Computers$cd %in% c("yes", "no")) )
dim(Computers.both)

Computers.slow <- subset(Computers,
		subset= (Computers$speed <= 33))
dim(Computers.slow)

## ============================================================================
## exercise DATA FRAMES AND DESCR STATS:
## ============================================================================

## Tasks:
## - install and load the package Ecdat
## - load the data set Computers
## - calculate some descriptive statistics (see below: mean, sd)
## - select the subsample of computers with 17 inch screens
## - report descriptive statistics for this subgroup (mean, sd)

## ============================================================================
## solution to exercise DATA FRAMES AND DESCR STATS
## ============================================================================

## save script as ex11b.R
## e-mail solution script to john.odonoghue@teradata.com
