## 1. Merges the training and the test sets to create one data set.

# read file into dataset and combine
list.files(path = ".")  # check the folders and files under the directory
list.files(path = "./test")
list.files(path = "./train")
x_test <- read.table("./test/X_test.txt")
x_train <- read.table("./train/X_train.txt")
x <- rbind(x_train,x_test)
y_test <- read.table("./test/Y_test.txt")
y_train <- read.table("./train/Y_train.txt")
y <- rbind(y_train,y_test)
s_test <- read.table("./test/subject_test.txt")
s_train <- read.table("./train/subject_train.txt")
s <- rbind(s_train,s_test)
alldata <- cbind(s,y,x)
 

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
ftr <- read.table("./features.txt")
ftr_mean <- ftr[grep(".*mean.*", ftr$V2, ignore.case=TRUE),]
ftr_sdv <- ftr[grep(".*std.*", ftr$V2, ignore.case=TRUE),]
ftr_sub <- rbind(ftr_mean,ftr_sdv)

# the following code seems to work as well
ftr_sub2 <- ftr[grep("std|mean", ftr$V2, ignore.case=TRUE),]


## 3. Uses descriptive activity names to name the activities in the data set
colnames(alldata)[1:2] <- c("subject", "activity.code")
act <- read.table("./activity_labels.txt")
head(act)
colnames(act) <- c("activity.code","activity")
act[, 2] = tolower(gsub("_", " ", as.character(act[, 2])))
alldata_n <- merge(alldata, act, by = "activity.code")
alldata_n2 <- cbind(alldata_n[,2],alldata_n[,564],alldata_n[,3:563])

## 4. Appropriately labels the data set with descriptive variable names.
ftr[,2] <- as.character(ftr[,2])
vars <- ftr[,2]
colnames(alldata_n2) <- c("subject","activity",vars)
write.table(alldata_n2, "cln_mrg_data.txt")


## 5. From the data set in step 4, creates a second, independent tidy data set 
    # with the average of each variable for each activity and each subject.

install.packages("reshape2")
library(reshape2)
alldata_n2$activity <- as.factor(alldata_n2$activity)
alldata_n2$subject <- as.factor(alldata_n2$subject)
data_melted <- melt(alldata_n2, id = c("subject", "activity"))
data_avg <- dcast(data_melted, subject + activity ~ variable, mean)
data_avg <- data_avg[order(data_avg$subject,data_avg$activity)]

write.table(data_avg, "average_data.txt")
