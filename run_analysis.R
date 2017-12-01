##########################################################################################
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set.
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.
##########################################################################################


if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("reshape2")) {
  install.packages("reshape2")
}

if (!require("plyr")) {
  install.packages("plyr")
}

require("data.table")
require("reshape2")
require("plyr")

#################################################################
#1. Merges the training and the test sets to create one data set.
#################################################################

# Reads Test Data Files (x_test.txt, y_test.txt, subject_test.txt) from the test folder
subject_test <- read.table('./UCI HAR Dataset/test/subject_test.txt')
X_test <- read.table('./UCI HAR Dataset/test/x_test.txt')
y_test <- read.table('./UCI HAR Dataset/test/y_test.txt')

# create a set column for DF
set <- 'test'
# merges the first two data (subject_test and y_test).
# uses cbind so rows remain in original sort.
testSY <- cbind(subtject_test,set,y_test)

# merges third data of the test (x_test).
test <- cbind(testSY,x_test)

# Reads the train data files
subject_train <- read.table('./UCI HAR Dataset/train/subject_train.txt')
x_train <- read.table('./UCI HAR Dataset/train/x_train.txt')
y_train <- read.table('./UCI HAR Dataset/train/y_train.txt')

# creates a set column
set <- 'train'

# merges the first two data (subject_train and y_train).
# uses cbind so rows remain in original sort.
tran_subject_y <- cbind(subject_train,set,y_train)

# merges the third data (x_train) 
train<- cbind(tran_subject_y,x_train)

# merges the test and train data tables
combine_test_train <- rbind(test,train)

# clean up the environment
rm(test,subtject_test,y_test,x_test,testSY,train,subject_train,y_train,x_train,tran_subject_y)

############################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################

# reads the column names from feature.txt
features_column_names <- read.table('./UCI HAR Dataset/features.txt',stringsAsFactors=FALSE)[[2]]

features_column_names <- c("subject","set","activity", features_column_names)

#set remaining column names from features.txt
names(combine_test_train) <- features_column_names

# discard data columns without "mean' or "std"
features_column_names <- names(combine_test_train)
selected_columns <- c("subject" ,"set", "activity", grep("-(mean|std)\\(\\)", features_column_names, value=TRUE))
combine_test_train <- combine_test_train[,selected_columns]

############################################################################################
# 3. Uses descriptive activity names to name the activities in the data set.
############################################################################################

# replace value in the activity column with their text equivelents from activity_labels.txt
combine_test_train$activity[combine_test_train$activity==1] <- "walking"
combine_test_train$activity[combine_test_train$activity==2] <- "walking upstairs"
combine_test_train$activity[combine_test_train$activity==3] <- "walking downstairs"
combine_test_train$activity[combine_test_train$activity==4] <- "sitting"
combine_test_train$activity[combine_test_train$activity==5] <- "standing"
combine_test_train$activity[combine_test_train$activity==6] <- "lying down"

############################################################################################
# 4. Appropriately labels the data set with descriptive variable names.
############################################################################################

# translates the names of the columns
features_column_names <- names(combine_test_train)
features_column_names <- gsub(pattern="^t",replacement="time",x=features_column_names)
features_column_names <- gsub(pattern="^f",replacement="freq",x=features_column_names)
features_column_names <- gsub(pattern="-?mean[(][)]-?",replacement="Mean",x=features_column_names)
features_column_names <- gsub(pattern="-?std[()][)]-?",replacement="Std",x=features_column_names)
features_column_names <- gsub(pattern="-?meanFreq[()][)]-?",replacement="MeanFreq",x=features_column_names)
features_column_names <- gsub(pattern="BodyBody",replacement="Body",x=features_column_names) 
names(combine_test_train) <- features_column_names

############################################################################################
# 5. From the data set in step 4, creates a second, independent tidy data set. 
############################################################################################

# calculates the average mean and exports it into a tidy data set.

id_labels   = c("subject","set","activity")
data_labels = setdiff(features_column_names(combine_test_train), id_labels)
melt_data   = melt(combine_test_train, id = id_labels, measure.vars = data_labels)

tidy_data   = dcast(melt_data, subject + activity ~ variable, mean)
write.table(tidy_data, file = "./tidy_data.txt")