options(warn=-1)
require(lubridate)
require(grid)
require(ggplot2)
require(dplyr)
require(stringr)
require(reshape2)
require(gridExtra)
require(forecast)
require(astsa)
# ----------------------------------------------------------------
# ----------------------------------------------------------------
# Preprocess Starts
# ----------------------------------------------------------------
# ----------------------------------------------------------------


#날짜 - 전력사용량 - 기상(외부온도)
data(edas_sample1)
trainData <- edas_sample1$train
testData <- edas_sample1$test

#set data types using built-in preprocess function
trainData <- EDAS.preprocess.datatypes(trainData)
testData <- EDAS.preprocess.datatypes(testData)

#do the time expansion...
expTrainData <- EDAS.preprocess.time.expand(trainData$eval)
expTestData <- EDAS.preprocess.time.expand(testData$eval)

#eval date factors from 'current_date' columns
expTrainData <- EDAS.transform.datefactor(expTrainData)
expTestData <- EDAS.transform.datefactor(expTestData)

# ----------------------------------------------------------------
# $indextable
#           type value
#1     workingday     1
#2 non-workingday     2
#3     specialday     3
#
#$workingdays
#[1] 1 2 3 4 5
#
#$nonworkingdays
#[1] 6 7
#
#$specialdays
#NULL
#
#$eval
# ----------------------------------------------------------------

expTrainData <- EDAS.transform.timefactor(expTrainData$eval)
expTestData <- EDAS.transform.timefactor(expTestData$eval)

# ----------------------------------------------------------------
# $indextable
# type value
# 1     workinghours     1
# 2 non-workinghours     2
#
# $workinghours
# [1]  9 10 11 12 13 14 15 16 17 18
#
# $nonworkinghours
# [1]  0  1  2  3  4  5  6  7  8 19 20 21 22 23
#
# $eval
# ----------------------------------------------------------------

preprocessedTrainingSet <- expTrainData$eval[,MDate:=lapply(collect_date,
                                                            function(x) as.POSIXct(format(x,"%Y-%m-%d"),format="%Y-%m-%d")),
                                             by=collect_date]
preprocessedTestSet <- expTestData$eval[,MDate:=lapply(collect_date,
                                                       function(x) as.POSIXct(format(x,"%Y-%m-%d"),format="%Y-%m-%d")),
                                        by=collect_date]

# ----------------------------------------------------------------
#              collect_date PowerUsage Temperature dayOfweek datetype hours timetype      MDate
#1: 2015-01-01 00:00:00         NA        NA          4        1       00        2      2015-01-01
#2: 2015-01-01 00:15:00        171.36   -6.90000      4        1       00        2      2015-01-01
# ----------------------------------------------------------------

# Fill missing data with average
preprocessedTrainingSet <- EDAS.preprocess.fillmissingdata(preprocessedTrainingSet, "next")
preprocessedTestSet <- EDAS.preprocess.fillmissingdata(preprocessedTestSet, "next")

# ----------------------------------------------------------------
#              collect_date PowerUsage Temperature dayOfweek datetype hours timetype      MDate
# 1: 2015-01-01 00:00:00    171.360    -6.90000         4        1    00        2      2015-01-01
# 2: 2015-01-01 00:15:00    171.360    -6.90000         4        1    00        2      2015-01-01
# 3: 2015-01-01 00:30:00    167.760    -6.90000         4        1    00        2      2015-01-01
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# Preprocess Completed
# ----------------------------------------------------------------
# ----------------------------------------------------------------


# daily similarity analysis begins...
#set data types using built-in preprocess function
dailyBasedTrainingSet <- preprocessedTrainingSet[,list(dayOfweek=max(dayOfweek,na.rm=T),
                                                       datetype=max(datetype,na.rm=T),
                                                       PowerUsage = list(PowerUsage),
                                                       Temperature = list(Temperature)),
                                                 by=MDate]

dailyBasedTestSet <- preprocessedTestSet[,list(dayOfweek=max(dayOfweek,na.rm=T),
                                               datetype=max(datetype,na.rm=T),
                                               PowerUsage = list(PowerUsage),
                                               Temperature = list(Temperature)),
                                         by=MDate]

dailyBasedTrainingSet$ChainG <- paste(dailyBasedTrainingSet$dayOfweek, dailyBasedTrainingSet$datetype, sep="-")
dailyBasedTestSet$ChainG <- paste(dailyBasedTestSet$dayOfweek, dailyBasedTestSet$datetype, sep="-")

# ----------------------------------------------------------------
# ----------------------------------------------------------------
#Hypothesis 1 - datetype
#Hypothesis 2 - datetype / dayOfweek
# ----------------------------------------------------------------
# ----------------------------------------------------------------

targetH1Type <- dailyBasedTestSet$datetype[1]
targetH2Type <- dailyBasedTestSet$ChainG[1]

h1TrainingSet <- dailyBasedTrainingSet[datetype==targetH1Type] #629 matched data points
h2TrainingSet <- dailyBasedTrainingSet[ChainG==targetH2Type] #125 matched data points

getBasicTrendPlots <- function(trainingset){
  mG1 <- matrix(unlist(trainingset[,"Temperature"]), nrow=96)
  mG1 <- reshape2::melt(mG1) #var1 <- row_index & var2 <- column index
  mG1$value <- as.numeric(as.character(mG1$value))
  g1tmp <- ggplot(mG1, aes(Var1, value, group=Var2)) + geom_line(cex=0.4) + theme_bw() + ylab("Temperature") +
    xlab("index")

  mG1 <- matrix(unlist(trainingset[,"PowerUsage"]), nrow=96)
  mG1 <- reshape2::melt(mG1) #var1 <- row_index & var2 <- column index
  mG1$value <- as.numeric(as.character(mG1$value))
  g1Power <- ggplot(mG1, aes(Var1, value, group=Var2)) + geom_line(cex=0.4) + theme_bw() + ylab("PowerUsage") +
    xlab("index")

  gridExtra::grid.arrange(g1Power, g1tmp)
}

getTemperatureSimilarity <- function(trainingDT, targetTestDT) {

  measure_eucliDist <- function(x, feature=NULL){
    return(sqrt( sum( (as.numeric( (unlist(targetTestDT[,feature,with=FALSE]) ) ) - as.numeric( unlist(x) )) ^ 2 )))
  }

  trainingDT[,Temp_Dist:=measure_eucliDist(Temperature, feature="Temperature"), by=MDate]
}
h1TrainingSet <- getTemperatureSimilarity(h1TrainingSet, dailyBasedTestSet)
h1TrainingSet <- h1TrainingSet[order(Temp_Dist),] #re-order datasets based on temperature distances.. low -> high
h1TrainingTop10 <- h1TrainingSet[1:10,]
h1TrainingTop2 <- h1TrainingSet[1:5,]
getBasicTrendPlots(h1TrainingTop10) #

dev.off()
powerUsageTrends = ts( as.numeric(unlist(h1TrainingTop10$PowerUsage)), frequency = 96)
sarima.for(powerUsageTrends,96,1, 0, 0,0,1,1,96)
options(warn=0)
