options(warn=-1)
set.seed(10)
require(rnn)
require(stringr)
require(ggplot2)
require(gridExtra)
require(data.table)

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# Preprocess Starts
# ----------------------------------------------------------------
# ----------------------------------------------------------------


#날짜 - 전력사용량 - 기상(외부온도,습도,엔트로피)
data(edas_sample1)
trainData <- edas_sample1$train
testData <- edas_sample1$test

#set data types using built-in preprocess function
trainData <- EDAS.preprocess.datatypes(trainData)
testData <- EDAS.preprocess.datatypes(testData)

#do the time expansion...
expTrainData <- EDAS.preprocess.time.expand(trainData$eval)
expTestData <- EDAS.preprocess.time.expand(testData$eval)

# Fill missing data with average
preprocessedTrainingSet <- EDAS.preprocess.fillmissingdata(expTrainData, "next")
preprocessedTestSet <- EDAS.preprocess.fillmissingdata(expTestData, "next")

# set data types...
preprocessedTrainingSet <- EDAS.preprocess.datatypes(preprocessedTrainingSet)$eval
preprocessedTestSet <- EDAS.preprocess.datatypes(preprocessedTestSet)$eval

preprocessedTrainingSet <- preprocessedTrainingSet[,
                                                   (c("prev_Power","prev_Temperature")):=shift(.SD, type="lag", n=96),
                                                   .SDcols=c("PowerUsage","Temperature")
                                                   ]
preprocessedTrainingSet <- na.omit(preprocessedTrainingSet)
oneWeek <- 4*24*7
twoWeeks <- oneWeek*2
preprocessedTrainingSet <- tail(preprocessedTrainingSet, twoWeeks)

featureX1 <- int2bin(preprocessedTrainingSet$prev_Power, 8)
featureX2 <- int2bin(preprocessedTrainingSet$prev_Temperature, 8)
training_featureSet <- array( c(featureX1,featureX2), dim=c(dim(featureX1),2) )
train_Y  <- int2bin(preprocessedTrainingSet$PowerUsage, length=8)
indexSequence <- seq(1,length(preprocessedTrainingSet$PowerUsage),by=1)
testSequence <- seq(100,2000,by=100)
testSequence <- seq(100,200,by=100)
buildTimes <- vector(mode="list")
rmseAC <- vector(mode="list")
epochNumbers <- vector(mode="list")
visualisationPlots <- list()
plotIndex <- 1
for (testNo in testSequence){
  ptm <- proc.time()
  model <- NULL
  model <- trainr(Y=train_Y,
                  X=training_featureSet,
                  learningrate = 0.01,
                  hidden_dim = 10,
                  network_type = "rnn",
                  numepochs= testNo,
                  batch_size = 96*4
  )

  buildTime <- proc.time() - ptm
  predicted_Y <- predictr(model, training_featureSet)
  evaluated_RMSE <- sqrt(mean((bin2int(train_Y) - bin2int(predicted_Y)^2), na.rm=T))

  rmseAC <- append(rmseAC, evaluated_RMSE)
  epochNumbers <- append(epochNumbers, testNo)
  buildTimes <- append(buildTimes, unname(buildTime[3]))
  resultDF <- data.frame(timeindex=indexSequence,
                         Actual=bin2int(train_Y),
                         Predicted=bin2int(predicted_Y))
  predictionResultPlot <- ggplot(resultDF) + geom_line(aes(x=timeindex,y=Actual,colour='red')) +
    geom_line(aes(x=timeindex,y=Predicted,colour='blue')) + theme_bw() +
    xlab("index(1=15m)") + ylab("Power") +
    ggtitle(paste0("Trainingset: Actual vs Predicted: numepochs=", testNo))

    visualisationPlots[[plotIndex]] <- predictionResultPlot
    plotIndex <- plotIndex + 1
}

grid.arrange(grobs=visualisationPlots, ncol = 2, nrow=2)

testResults <- data.frame(rmse=unlist(rmseAC), epochCnt=unlist(epochNumbers),
                          elapsedTime=unlist(buildTimes))
#plot(colMeans(model$error),type='l',
#     xlab='epoch',
#     ylab='errors')
#predicted_Y <- predictr(model, training_featureSet)
plot(bin2int(train_Y), col = "red", type= "l", main = "Actual vs predicted: testing set", ylab = "Y,Yp")
lines(bin2int(predicted_Y), type="l", col="blue")
legend("topleft", c("Predicted", "Real"),
       col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
options(warn=0)

