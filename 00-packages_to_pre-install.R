## install packages plus any packages they depend on (dependencies = TRUE)
install.packages(c("gdata", "ggplot2", "MASS", "car", "rgl", "nortest",
                   "lmtest", "RODBC", "RJDBC", "readr", "xlsx", "readxl",
                   "datasets", "corrplot", "psych", "sm", "ggmap",
                   "googleVis", "reshape2", "devtools", "rCharts", "plyr",
                   "GGally", "caret", "polycor", "Metrics", "fmsb", "pscl",
                   "rpart", "rattle", "e1071", "ROCR", "party",
                   "randomForest",  "cluster", "fpc", "clue", "dendextend",
                   "dendextendRcpp", "Ecdat", "curl", "Rcpp"),
                 dependencies = TRUE)

## install rCharts library from GitHub
## need to load devtools and Rcpp libraries before installing
library(devtools)
library(Rcpp)
install_github('ramnathv/rCharts')

## install packages and dependencies no longer available on CRAN from biconductor
source("https://bioconductor.org/biocLite.R")
biocLite(c("RBGL","Rgraphviz", "graph", "pkgDepTools", "datasets"))
biocLite("datasets")
