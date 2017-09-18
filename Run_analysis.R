#You should create one R script called run_analysis.R that does the following. 

## 1.Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set.
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.


##Creating the working sapace
getwd()
if(!file.exists("./Course_project")){dir.create("./Course_project")}
setwd("./Course_project")

##Getting data

Url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(Url, dest="./database")
unzip(zipfile = "./database", exdir = "../Course_project")
list.files()
setwd("../UCI HAR Dataset")

## 1.Merging the training and the test sets into one data set.


Train <- Test <- NULL

if(is.null(Train)) { Train <<- read.table("./train/X_train.txt") }
if(is.null(Test))  { Test  <<- read.table("./test/X_test.txt") }
merged <- rbind(Train, Test)
name<-read.table("./features.txt")[,2]
names(merged)<-name
str(merged)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

matchingdata<-grep("(mean|std)\\(\\)", names(merged))
extracted_data<-merged[, matchingdata]
str(extracted_data)



## 3. Uses descriptive activity names to name the activities in the data set.

#Get the activity data and map to nicer names:
yTrain <- read.table("train/y_train.txt")
yTest  <- read.table("test/y_test.txt")
yMerged <- rbind(yTrain, yTest)[, 1]

activityNames <-c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
activities <- activityNames[yMerged]


## 4.  Appropriately label the data set with descriptive variable names.
# Change t to Time, f to Frequency, mean() to Mean and std() to StdDev
# Remove extra dashes and BodyBody naming error from original feature names

names(extracted_data) <- gsub("^t", " Time", names(extracted_data))
names(extracted_data) <- gsub("^f", " Frequency ", names(extracted_data))
names(extracted_data) <- gsub("-mean\\(\\)", " Mean ", names(extracted_data))
names(extracted_data) <- gsub("-std\\(\\)", " StdDev ", names(extracted_data))
names(extracted_data) <- gsub("-", "", names(extracted_data))
names(extracted_data) <- gsub("BodyBody ", " Body ", names(extracted_data))

str(extracted_data)

# Add activities and subject with nice names
subjectTrain <- read.table("train/subject_train.txt")
subjectTest  <- read.table("test/subject_test.txt")
subjects <- rbind(subjectTrain, subjectTest)[, 1]

tidy <- cbind(Subject = subjects, Activity = activities, extracted_data)



## 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.

final_tidy<-tidy%>% group_by(Activity, Subject)%>% summarise_each(funs(mean(., na.rm=TRUE)))
final_tidy<-arrange(final_tidy, Subject)
summary(table(final_tidy))
write.table(final_tidy, file="result.txt", row.names = False )