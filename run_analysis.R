
## Load data function
Load_data <- function(path="") {
    foldPath = "./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset"
    filePath = paste(foldPath, path, sep="")
    read.table(filePath)
}

## result
Result <- function(){
    ## Load test data
    xTest <- Load_data("/test/X_test.txt")
    yTest <- Load_data("/test/y_test.txt")
    subjectTest <- Load_data("/test/subject_test.txt")

    ## Load train data
    xTrain <- Load_data("/train/X_train.txt")
    yTrain <- Load_data("/train/y_train.txt")
    subjectTrain <- Load_data("/train/subject_train.txt")
    
    ## Label data
    features <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt", stringsAsFactors=F)
    names(xTest) <- features[,2]
    names(xTrain) <- features[,2]
    activityLabel <- Load_data("/activity_labels.txt")
    yTest[,2] <- activityLabel[yTest[,1],2]
    yTrain[,2] <- activityLabel[yTrain[,1],2]
    names(yTest) <- c("ActivityID", "ActivityLabel")
    names(yTrain) <- c("ActivityID", "ActivityLabel")
    names(subjectTest) <- "subject"
    names(subjectTrain) <- "subject"
    
    ## Select data
    meanStdFeatures <- features[grep("mean\\(\\)|std\\(\\)", features$V2),2]

    xTest <- xTest[, meanStdFeatures]
    xTrain <- xTrain[, meanStdFeatures]
    
    ## Merge data
    testData <- cbind(as.data.table(subjectTest), yTest, xTest)
    trainData <- cbind(as.data.table(subjectTrain), yTrain, xTrain)
    data <- rbind(testData, trainData)
    
    idLabels <- c("subject", "ActivityID", "ActivityLabel")
    dataLabels <- setdiff(colnames(data), idLabels)
    meltData <- melt(data, id = idLabels, measure.vars = dataLabels)
    
    tidyData <- dcast(meltData, subject + ActivityLabel ~ variable, mean)
    
    write.table(tidyData, file = "./tidyData.txt", row.name = F)

}


