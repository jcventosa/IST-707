####################################
# Juan C. Ventosa
# IST-707 Data Analysis
# HW4: Clustering Analysis
# Data: Air Crashes and Fatalities Since 1908
# Date: 7/26/2020
###################################

# Libraries
library(tidyverse)
library(factoextra)
library(cluster)
library(gridExtra)
library(dplyr)
library(plyr)
library(VIM)
library(lubridate)
library(DT)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(stringi)
library(Matrix)
library(tidytext)
library(mclust)

# Data

FedPapers<- read.csv("https://raw.githubusercontent.com/jcventosa/IST-707/master/fedPapers85.csv")
# Data Cleaning and Transformation
# Check data structure and data types
dim(FedPapers)
# 85 objects and 72 variables
str(FedPapers)
# Contains two nominal variables (author and file) and 70 numeric variables.
# Date and Time columns will need to be converted to date/time data types

# Check for missing and duplicated data
sum(is.na(AirCrash))
nrow(AirCrash[duplicated(AirCrash),])

SummaryWC <- wordcloud(colnames(OriginalDTM),color = TRUE)

AirYear$Score <- cut(AirYear$Year, breaks =
                       c(1907,1929,1949,1969,1989,Inf),
                     labels =c("1907-1929","1930-1949","1950-1969","1970-1989","1990-2009"))
AirYear <- data.frame(AirYear)
AirYear$Y1990-Y2009 <- ifelse(AirYear$AirYear > 1989 ,1,0)
AirYear$Y1930-Y1949 <- ifelse(AirYear$AirYear > 1949,0, ifelse(AirYear$AirYear < 1930,0,1))
AirYear$Y1950-Y1969 <- ifelse(AirYear$AirYear > 1969,0, ifelse(AirYear$AirYear < 1950,0,1))
AirYear$Y1970-Y1989 <- ifelse(AirYear$AirYear > 1989,0, ifelse(AirYear$AirYear < 1970,0,1))
AirYear$Y1908-Y1929 <- ifelse(AirYear$AirYear > 1929,0,1)
