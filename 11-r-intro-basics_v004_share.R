## ############################################################################
## R Introduction and Best Practice
## R Basics
## June 2015
## ############################################################################

#rm(list=ls())

## ============================================================================
## global variables
## ============================================================================

path.raw <- "/Users/in186000/data-td-sync/teradata-project/training-knowledge-exchange/r-training-general/"
#path.raw <- "C:/data-sync/teradata-work/teradata-project/training-knowledge-exchange/r-training-general/"
path.r <- paste0(path.raw, "r-scripts/")
path.dat <- paste0(path.raw, "data/")


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

## parameters:
## - passed in parenthesis
## - identified by their order
## - or passed as "named" arguments
## - use = sign (instead of <- sign)

library(Ecdat)             ## load package Ecdat (financial data sets)
data(Computers)            ## load dataset

spd <- Computers$speed     ## variable speed from data frame Computers (ignore for now)
ccd <- Computers$cd

mw <- mean(spd, trim=0, na.rm=TRUE) 
mw
mw <- mean(spd, na.rm=TRUE)
mw

## - functions are generic
## - what they do depends on argument!
## - e.g., summary

summary(spd)
summary(ccd)
summary(Computers)

## Packages
## - R has some basic functionality
## - rest comes in packages
## - stored in online repositories
##   like CRAN or bioconductor
## - installation:
##   . by command line: install.packages()
##   . by menu: packages -> install packages
## - once installed, load: library()

install.packages("RJDBC")   # quotes needed here
library(RJDBC)              # no quotes needed 
                            # (RJDBC is an R object already)


## Packages from Bioconductor:
## http://www.bioconductor.org/install/

#source("http://bioconductor.org/biocLite.R")
#biocLite()
#biocLite("impute")
#
#library(impute)

## installing from file:
#install.packages("filename.zip", repos=NULL)

## - some packages: ... 

## Getting help:
## - internally

help(mean)
?mean

help(package="RJDBC")


help.search("mean")
??mean
??"mean"

apropos("mean")
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
##   . show CRAN view(s)


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
## - special symbols: NaN, NA, NULL

mode(1)   ## data type
typeof(1) ## internal type

mode("1")

mode(TRUE)

mode(1+3i)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## vectors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

vek1 <- 1:30               # sequence from 1 to 30
vek1
vek2 <- c(1, 5, 7, 10)     # vector with 4 elements
vek2
vek3 <- seq(0, 100, by=10) # 0 to 100 in steps of 10
vek3
vek4 <- rep(1, 7)          # 1, repeated 7 times
vek4

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## factors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

f1 <- c("red", "green", "blue", "blue", "red", "red")
levels(f1)
str(f1)
class(f1)

ff1 <- as.factor(f1)
levels(ff1)
str(ff1)
class(ff1)

## better: 
## create a numeric variable and assign labels 
## (like a lookup table)

f2 <- c(1, 2, 3, 1, 2, 2, 1, 2)
ff2 <- as.factor(f2)
ff2

levels(ff2) <- list("good"=1, "better"=2, "best"=3)
ff2
levels(ff2)

## in one step:

f3 <- c(1, 2, 3, 1, 2, 2, 1, 2)
ff3 <- factor(f2, 
              levels=c(1, 2, 3), 
              labels=c("good", "better", "best")
)
ff3
levels(ff3)

## ordered factor:

ff4 <- as.ordered(ff3)
ff4


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
vek1 * 2    # 1*2 2*2 3*2
vek1 + vek2 # 1+5 2+4 3+3

## if vectors are not of the same length, shorter one is repeated!!!

vek1+vek3  # 1+1 2+2 3+3 1+4 2+5 3+6

## accessing elements of vectors:

## by brackets:
vek1 <- 1:10
vek1[2]
vek1[5:10]

## exclusion of elements: minus sign
vek1[-2]
vek1[-(2:4)]

## logical indexing:
vek1 <- 1:6
vek1 < 4          # vector of logical values
vek1[vek1 < 4]    # show elements where condition is TRUE

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

x <- 1:10                 ## or: seq(from=1, to=10, by=1)
y <- c(10, 2, 14, 7, 8)   ## or: scan() 
z <- 20:11                ## or: seq(from=20, to=10, by=-1)
w <- c(42, 12)
yw <- c(y, w)
x + z
x + y                     ## y is repeated! (=recycled) 

## ============================================================================
## data structures
## ============================================================================

## - matrix:
##   . two-dimensional
##   . only one data type

mymat <- matrix(1:6, ncol=2, byrow=TRUE)
mymat

class(mymat)
mode(mymat)
typeof(mymat)

str(mymat)

## - array
##   . multi-dimensional
##   . only one data type (like matrix, but more dimensions)

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

mylist <- list("item1" = c(1, 2, 3), "item2" = c("item", "two"))
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

mydf <- data.frame("col1" = 1:3, "col2"=c("one", "two", "three"))
mydf

class(mydf)
mode(mydf)
typeof(mydf)

str(mydf)

## Computers data that we have used before: data.frame
str(Computers)

## ============================================================================
## data frames
## ============================================================================

## focus on data frames here:
## - demonstration of some functions that can be applied
## - using internal data set
## - from package Ecdat

library(Ecdat)   # load package Ecdat (financial data sets)
data(Computers)  # load dataset

## important functions:

str(Computers)    # structure of data frame
nrow(Computers)   # number of rows
ncol(Computers)   # number of columns
dim(Computers)    # dimensions (rows, columns)
names(Computers)  # variable names
head(Computers)   # first 6 rows
tail(Computers)   # last 6 rows
Computers.new <- edit(Computers)    ## everything has to be assigned!

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

## accessing columns:

Computers[,2]      # second column, all rows    (vector)
Computers[,"price"]  # column "price", all rows (vector)

Computers["price"]   # column "price"       (data frame)
Computers[1]         # first column         (data frame)

Computers$price      # column "price"           (vector)

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

## using the subset() function:

Computers.nocd <- subset(Computers, 
		subset= (Computers$cd != "yes") )
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

## install package:
install.packages("Ecdat")

## load library and data:
library(Ecdat)
data(Computers)

## inspect data:
str(Computers)

## calculate some descriptive statistics:
mean(Computers$price)
sd(Computers$price)

mean(Computers$speed)
sd(Computers$speed)

mean(Computers$hd)
sd(Computers$hd)

## select a subset:
comp.17 <- subset(Computers, subset= Computers$screen == 17)

dim(comp.17)

## calculate some descriptive stats
mean(comp.17$price)
sd(comp.17$price)

mean(comp.17$speed)
sd(comp.17$speed)

mean(comp.17$hd)
sd(comp.17$hd)



