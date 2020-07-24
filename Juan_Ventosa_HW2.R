####################################
# Name:   Juan C Ventosa
# Course: IST707 Data Analysis
# Assignment: HW2 Data Storyteller
###################################
# Libraries
library(ggplot2)
library(dplyr)
library(scales)
library(corrplot)
library(gridExtra)
library(ggpubr)

# Dataset: Import data.storyteller.csv
DataStory <- read.csv("~/Downloads/data-storyteller (1).csv")
###################################
# Data Cleaning and Transformations

# Check data type
str(DataStory)

# Convert $Section data type from integer to factor
DataStory$Section <- factor(DataStory$Section)

# Check summary of data to find missing or unusual information
summary(DataStory)
nrow(DataStory[duplicated(DataStory),])
# No missing or duplicated data. Note: $Very.Ahead values are all 0.

# Simplifying column names
colnames(DataStory) <- c('School','Section','VeryAhead','Middling','Behind','MoreBehind',
                         'VeryBehind','Completed')
# Add SectionTotals coloumn to DataStory
DataStory$SectionTotals <- (DataStory$Middling+ DataStory$Behind 
                            + DataStory$MoreBehind +DataStory$VeryBehind + DataStory$Completed)
head(DataStory)

#####################################
# Exploratory Data Analysis

# Present Sum, Percent, Stand. Dev., Min, Max and Range by Completion Status
# into a dataframe named BasePerformance
sum(DataStory$SectionTotals) # = 1601 Students
BasePerformance <- data.frame(Completion_Status =colnames(DataStory[,3:8]),
              Sum = (c(sum(DataStory[,3]),sum(DataStory[,4]),sum(DataStory[,5]),sum(DataStory[,6])
                      ,sum(DataStory[,7]),sum(DataStory[,8]))),
          Percent = (c(sum(DataStory[,3])/1601, sum(DataStory[,4])/1601,sum(DataStory[,5])/1601,
                      sum(DataStory[,6])/1601 ,sum(DataStory[,7])/1601,sum(DataStory[,8]/1601))),
Standard_Deviation = c(sd(DataStory[,3]),sd(DataStory[,4]),sd(DataStory[,5]),
                      sd(DataStory[,6]),sd(DataStory[,7]),sd(DataStory[,8])),
             Minimum = c(min(DataStory[,3]),min(DataStory[,4]), min(DataStory[,5]),min(DataStory[,6]),
                       min(DataStory[,7]),min(DataStory[,8])),
             Maximum = c(max(DataStory[,3]),max(DataStory[,4]),max(DataStory[,5]),max(DataStory[,6]),
                         max(DataStory[,7]),max(DataStory[,8])),
               Range = c(max(DataStory[,3])-min(DataStory[,3]),max(DataStory[,4])-min(DataStory[,4]),
                         max(DataStory[,5])-min(DataStory[,5]),max(DataStory[,6])-min(DataStory[,6]),
                         max(DataStory[,7])-min(DataStory[,7]),max(DataStory[,8])-min(DataStory[,8])))
BasePerformance$Percent <-percent(BasePerformance$Percent)
BasePerformance

# Plot overall distribution of Students by Completion Status
boxplot(DataStory[,3:8],main ="Student Completion Status Boxplots",xlab = "Completion Status",
        ylab="Student Count")

# Aggregate the sum and central tendencies of Section Totals by School 
# into a dataframe "SchoolSummary"
SchoolSummary <- cbind(aggregate(DataStory$Section,by=list(School = DataStory$School),FUN =length),
                aggregate(DataStory$SectionTotals, by=list(School = DataStory$School),FUN =sum),
                aggregate(DataStory$SectionTotals,by=list(School = DataStory$School),FUN =mean),
                aggregate(DataStory$SectionTotals,by=list(School = DataStory$School),FUN =median))
SchoolSummary<-SchoolSummary[,-c(3,5,7)]
colnames(SchoolSummary) <- c("School", "Sections Per School", "Cumulative Section Size", 
                             "Section Mean Size","Section Median Size")
SchoolSummary[,4:5] <-format(round(SchoolSummary[,4:5]),nsmall = 0)
SchoolSummary

# Create a correlation matrix comparing Section Sizes with proprortion of completion statuses
SizeStatusCor <- cbind(DataStory[,4:8]/DataStory$SectionTotals,SectionSize = DataStory$SectionTotals)
colnames(SizeStatusCor) <- c('% Middling', '% Behind', '% More Behind', '% Very Behind',
                             '% Completed', 'Section Size')
SizeStatusCor <- cor(SizeStatusCor)
library(corrplot)
SizeStatusCor
corrplot(SizeStatusCor, method = "number")

# Scatterplots and trendlines comparing Section Size with Completed status and 
# Section Size with Very Behind status. Combine the graphs with grid.arrange.
SectionXCompleted <- ggplot(DataStory, aes(x=SectionTotals, y=Completed/SectionTotals)) + 
                    geom_point(aes(colour =School))  + 
                    geom_smooth(method =lm, se=FALSE, formula = y~x) + 
                    stat_regline_equation(aes(label = paste(..eq.label..,..rr.label..,sep="~~")),
                                          label.x = 45, label.y = 0.5) +                 
                    labs(x="Section Size", y="Perrcent Completed")
SectionXCompleted
SectionXVeryBehind <- ggplot(DataStory, aes(x=SectionTotals, y=VeryBehind/SectionTotals)) + 
                    geom_point(aes(colour =School))  + 
                    geom_smooth(method =lm, se=FALSE)+
                    stat_regline_equation(aes(label = paste(..eq.label..,..rr.label..,sep="~~")),
                                          label.x = 45, label.y = 0.3) +
                    labs(x="Section Size", y="Percent Very Behind")
SectionXVeryBehind
grid.arrange(SectionXVeryBehind,SectionXCompleted,nrow =2)

# Create Area Graph of Sections by Performance in comparison to BasePerformance
# Prep DataStory data for stack graph by moving it into 3 columns: Section,Performance, Count
# First place student counts into a matrix named DCount with no header
DCount <- as.matrix(DataStory[,4:8])
colnames(DCount) <- NULL
# Change matrix into a vector and include BasePerformance counts for comparison
DCount <- c(BasePerformance[2:6,2],DCount[1,],DCount[2,],DCount[3,],DCount[4,],DCount[5,],
            DCount[6,],DCount[7,],DCount[8,],DCount[9,],DCount[10,],DCount[11,],DCount[12,],
            DCount[13,],DCount[14,],DCount[15,],DCount[16,],DCount[17,],DCount[18,],DCount[19,],
            DCount[20,],DCount[21,],DCount[22,],DCount[23,],DCount[24,],DCount[25,],DCount[26,],
            DCount[27,],DCount[28,],DCount[29,],DCount[30,])

# Create new data frame with columns School, Performance and Count
AreaPerformance <-data.frame(
                Section = rep(c('All','A1','A2','A3','A4','A5','A6','A7','A8','A9',
                                'A10','A11','A12','A13','B1','B2','B3','B4','B5','B6','B7','B8',
                                'B9','B10','B11','B12','C1','C2','C3','D1','E1'),each = 5),
            Performance = rep(c('Middling','Behind','MoreBehind','VeryBehind','Completed'),31),
                  Count = DCount)
AreaPerformance$Performance <- factor(AreaPerformance$Performance,
                           levels = c('Completed','Middling','Behind','MoreBehind','VeryBehind'))
AreaPerformance$Section <- factor(AreaPerformance$Section, 
                                levels = c('All','E1','A5','A4','A10','A1','A3','A2','A7','A8',
                                           'A11','A9','A6','A13','B12','B3','B8','B7','B6','B5',
                                           'B11','B9','B1','A12','C2','C1','B10','D1','B4','B2',
                                           'C3'))
AreaPerformance

# Use AreaPerformance dataframe to create area chart
pAreaPerformance <- ggplot(AreaPerformance, aes(x=Section,y=Count)) +
  geom_col(aes(fill=Performance), position = "fill") +
  ggtitle("Student Completion Rates by Section") +labs(y="Percent of Students",x="Section-Student Size") + 
  scale_x_discrete(labels = 
                   c('All'='ALL-1601','E1'='E1-116','A5' ='A5-85','A4'='A4-85','A10'='A10-83',
                     'A1'='A1-81','A3'='A3-80','A2'='A2-80','A7'='A7-79','A8'='A8-74',
                     'A11'='A11-69','A9'= 'A11-65','A6'= 'A6-58','A13'='A13-56','B12'='B12-47',
                     'B3'= 'B3-47','B8'='B8-46','B7'='B7-45','B6'='B6-40','B5'='B5-40',
                     'B11'='B11-39','B9'='B9-39','B1'='B1-39','A12'='A12-37','C2'='C2-36',
                     'C1'='C1-36','B10'='B10-28','D1'='D1-22','B4'='B4-18','B2'='B2-18',
                     'C3'='C3-13')) +
  theme(axis.text.x = element_text(size = 8, angle = 90))

pAreaPerformance

# Create stack graph of completion rates by school and all schools combined
# Firtst Aggregate number of students by school in dataframe SumbySchool
SumbySchool <- aggregate(cbind(DataStory$VeryAhead, DataStory$Middling,DataStory$Behind, 
                               DataStory$MoreBehind, DataStory$VeryBehind, DataStory$Completed,
                               DataStory$SectionTotals),by=list(DataStory$School),FUN =sum)
colnames(SumbySchool) <-c('School','VeryAhead','Middling','Behind','MoreBehind','VeryBehind',
                          'Completed','Total')
SumbySchool

# Prep SumbySchool data for stack graph with three columns: School,Performance, Count
# First place student counts into a matrix named PeformCount with no header
PerformCount <- as.matrix(SumbySchool[,3:7])
colnames(PerformCount) <- NULL
# Change matrix into a vector and include BasePerformance counts for comparison
PerformCount <- c(BasePerformance[2:6,2],PerformCount[1,],PerformCount[2,],PerformCount[3,],
                  PerformCount[4,],PerformCount[5,])

# Create new data frame with columns School, Performance and Count
StackPerformance <-data.frame(
                    School = rep(c('All','A','B','C','D','E'),each = 5),
               Performance = rep(c('Middling','Behind','MoreBehind','VeryBehind','Completed'),6),
                     Count = PerformCount)
StackPerformance$Performance <- factor(StackPerformance$Performance,
                                levels = c('Completed','Middling','Behind','MoreBehind',
                                           'VeryBehind'))
StackPerformance$School <- factor(StackPerformance$School, 
                                  levels = c('All','A','B','C','D','E'))
StackPerformance

# Use StackPerformance dataframe to create stack chart
pPerformancebySchool <- ggplot(StackPerformance, aes(x=School,y=Count, width=.7)) +
  geom_col(aes(fill=Performance), position = "fill") + 
  ggtitle("Student Completion Rates by School") + labs(y="Percent of Students")

pPerformancebySchool

