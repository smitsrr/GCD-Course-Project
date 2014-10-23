#Course project for Getting and Cleaning Data. 10/23/2014
library(plyr); library(dplyr)
#load the data for training
trainData<- read.table("X_train.txt")
id<- read.table("subject_train.txt")
act<- read.table("y_train.txt")
TR<- cbind(id, act, trainData)

testData<- read.table("X_test.txt")
id2<-read.table("subject_test.txt")
act2<- read.table("y_test.txt")
tst<- cbind(id2, act2, testData)

mergedData<-rbind(TR, tst)  #append the two datasets

names<- read.table("features.txt")  #the names are the second column. 
n<- as.character(names[, 2]  )
nm<- c("ID", "activity")
names<- c(nm, n)
colnames(mergedData)<-names  #name the columns

#subset for those variables that contain either "ean" or "std"
a<- mergedData[grep("ean", colnames(mergedData))]
y<- mergedData[grep("std", colnames(mergedData))]
b<- mergedData[, 1:2]
z<- cbind(b, a, y)  #dataset with just means and STDs

#reclassify the activity with a name instead of a number. 
activities<- c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
values <- c(1, 2, 3, 4, 5, 6)
x<- z[,2]   
Activity <- mapvalues(x, from = values, to = activities) 
z2<- cbind(Activity, z)
z2$activity <- NULL

#Rename the columns to be meaninful names
colnames <- c(names(z2))
colnames2 <- gsub("-", "", colnames)
colnames3 <- sub("(", "", colnames2, fixed = TRUE)
colnames4 <- sub(")", "", colnames3, fixed = TRUE)
colnames5 <- sub("mean", "Mean", colnames4, fixed = TRUE)
colnames6 <- sub("std", "Std", colnames5, fixed = TRUE)
colnames7 <- sub("x", "X", colnames6, fixed = TRUE)
colnames(z2) <- colnames7

table <- tbl_df(z2)

#New data frame that is just the average of each variable for each activity and each subject
#group or subset by subject
meanstbl <- group_by(table, Activity, ID) %>%
        summarise_each(funs(mean))

write.table(meanstbl, "courseProject.txt", row.names = FALSE)  #this willb e the final step)
