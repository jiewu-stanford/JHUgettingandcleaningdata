## load necessary packages
library(data.table)     ## load library for data.table(), fread()
library(dplyr)          ## load library for select()
library(reshape2)       ## load library for melt()
library(tidyr)          ## load library for separate()
## download data file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "dataFiles.zip")
## unzip and list files
unzip(zipfile = "dataFiles.zip")
list.files('./UCI HAR Dataset',recursive=TRUE)


####################################################################
# 1. merge training and test set
####################################################################
## Load train datasets
subjectTrain <- fread("./UCI HAR Dataset/train/subject_train.txt")
subjectTest <- fread("./UCI HAR Dataset/test/subject_test.txt")
activityTrain <- fread("./UCI HAR Dataset/train/Y_train.txt")
activityTest <- fread("./UCI HAR Dataset/test/Y_test.txt")
XTrain <- fread("./UCI HAR Dataset/train/X_train.txt")
XTest <- fread("./UCI HAR Dataset/test/X_test.txt")
## combine test and train subject
subjectDT <- rbind(subjectTrain, subjectTest)
setnames(subjectDT, "V1", "subjectCode")
## combine test and train activity
activityDT <- rbind(activityTrain, activityTest)
setnames(activityDT, "V1", "activityCode")
## combine X feature codes
DT <- rbind(XTrain,XTest)
## merge columns
DT <- cbind(subjectDT, DT)
DT <- cbind(DT,activityDT)


####################################################################
# 2. extract only the mean and standard deviation for each mesure
####################################################################
## get the feature code table
featureDT <- fread("./UCI HAR Dataset/features.txt",
                                    col.names=c("indx", "feature"))
## extract the mean and standard deviation measures
featureDT <- featureDT[grepl('mean\\(\\)|std\\(\\)',feature)]
## add a column of feature code
featureDT$featureCode <- featureDT[,paste0('V',indx)]
DTmeanstd <- select(DT,c('subjectCode',featureDT$featureCode,'activityCode'))


####################################################################
# 3. uses descriptive activity names to name the activities
####################################################################
activitylabel <- fread("./UCI HAR Dataset/activity_labels.txt")
setnames(activitylabel, names(activitylabel), c("indx", "activity"))


####################################################################
# 4. label data set using descriptive activity names
####################################################################
## label activity
DTmeanstdlabeled <- merge(DTmeanstd,activitylabel,by.x='activityCode',
                                    by.y='indx',all.x=TRUE)
setkey(DTmeanstdlabeled,subjectCode,activityCode,activity)
## melt to add a column of feature code
DTtbl <- melt(DTmeanstdlabeled,key(DTmeanstdlabeled),
                                    variable.name='featureCode')
## label feature
DTl <- merge(DTtbl,featureDT,by='featureCode',all.x=TRUE)


####################################################################
# 5. find the average of each variable for each activity and each subject
####################################################################
## remove brackets
DTl$feature <- gsub("\\(\\)","",DTl$feature)
## check the following command for separate result
## separate(featureDT,feature,c('measure','type','axis'),sep='-')
DTtidy <- separate(DTl,feature,c('measure','type','axis'),sep='-')
DTtidy <- DTtidy[,c('subjectCode','activity','measure','type','axis','value')]
DTtidy$measure <- gsub('^t','time.',DTtidy$measure)
DTtidy$measure <- gsub('^f','frequency.',DTtidy$measure)
## check result
## head(DT[,c('subjectCode','activityCode','V1')])
## head(DTtidy[DTtidy$activity=='STANDING',])
DTstats <- DTtidy[, list(count=.N, average=mean(value)), by=c('subjectCode',
                                    'activity','measure','type','axis')]
head(DTstats)
## output data table
write.table(DTstats,file='./tidydata.txt',row.name=FALSE)