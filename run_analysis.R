
###########################################################################
# Reading test data
###########################################################################
subject_test <- read_table('subject_test.txt', col_names = FALSE)
X_test <- read_table('X_test.txt', col_names = FALSE)
y_test <- read_table('y_test.txt', col_names = FALSE)

###########################################################################
# Reading train data
###########################################################################
subject_train <- read_table('subject_train.txt', col_names = FALSE)
X_train <- read_table('X_train.txt', col_names = FALSE)
y_train <- read_table('y_train.txt', col_names = FALSE)

############################################################################
# Reading Feature Data
############################################################################
features <- read_delim('features.txt', col_names = FALSE, delim = " ")
colnames(features) <- c('id', 'variable')

features <- features %>%
  group_by(variable) %>%
  mutate(tot = n(),
         num = 1:n())

#############################################################################
# Creating unique names for features variables
#############################################################################
features <- features %>%
  ungroup() %>%
  mutate(variable = ifelse(tot > 1, paste0(variable,'-',num), variable))


###############################################################################
# Adding variable names to columns
###############################################################################
colnames(subject_test) <- 'subject'
colnames(X_test) <- features$variable
colnames(y_test) <- 'activity'

################################################################################
# Binding subject and test database
################################################################################
test <- bind_cols(subject_test, y_test) %>%
  mutate(source = 'test') %>%
  bind_cols(X_test)



#################################################################################
# Add variable names to columns
#################################################################################
colnames(subject_train) <- 'subject'
colnames(X_train) <- features$variable
colnames(y_train) <- 'activity'

##################################################################################
# Bind subject and train database
##################################################################################
train <- bind_cols(subject_train, y_train) %>%
  mutate(source = 'train') %>%
  bind_cols(X_train)
##################################################################################
# Binding test and train database
##################################################################################
dataset <- bind_rows(test, train)

##########################################################################################
# Filtering required measurements on the mean and standard deviation for each measurement. 
##########################################################################################
dataset_filter <- dataset %>%
  select(subject, activity, source, matches('mean\\(|std\\('))

###########################################################################################
# Using descriptive activity names to name the activities in the data set.
###########################################################################################

dataset_filter$activity <- factor(dataset_filter$activity, 
                                  levels = c(1, 2, 3, 4, 5, 6), 
                                  labels = c('walking', 
                                             'walking_upstairs', 
                                             'walking_downstairs', 
                                             'sitting', 
                                             'standing', 
                                             'laying'))

dataset_filter$source <- as.factor(dataset_filter$source)
table(dataset_filter$source)
################################################################################
# Appropriately labels the data set with descriptive variable names. 
################################################################################]

# Mean varabiables
attributes(dataset_filter$`tBodyAcc-mean()-X`)$label <- 'time Body Accelerometer X-axial signal - mean'
attributes(dataset_filter$`tBodyAcc-mean()-Y`)$label <- 'time Body Accelerometer Y-axial signal - mean'
attributes(dataset_filter$`tBodyAcc-mean()-Z`)$label <- 'time Body Accelerometer Z-axial signal - mean'
attributes(dataset_filter$`tGravityAcc-mean()-X`)$label <- 'time Gravity Accelerometer X-axial signal - mean'
attributes(dataset_filter$`tGravityAcc-mean()-Y`)$label <- 'time Gravity Accelerometer Y-axial signal - mean'
attributes(dataset_filter$`tGravityAcc-mean()-Z`)$label <- 'time Gravity Accelerometer Z-axial signal - mean'
attributes(dataset_filter$`tBodyAccJerk-mean()-X`)$label <- 'time Body Accelerometer Jerk X-axial signal - mean'
attributes(dataset_filter$`tBodyAccJerk-mean()-Y`)$label <- 'time Body Accelerometer Jerk Y-axial signal - mean'
attributes(dataset_filter$`tBodyAccJerk-mean()-Z`)$label <- 'time Body Accelerometer Jerk Z-axial signal - mean'
attributes(dataset_filter$`tBodyGyro-mean()-X`)$label <- 'time Body Gyroscope X-axial signal - mean'
attributes(dataset_filter$`tBodyGyro-mean()-Y`)$label <- 'time Body Gyroscope Y-axial signal - mean'
attributes(dataset_filter$`tBodyGyro-mean()-Z`)$label <- 'time Body Gyroscope Z-axial signal - mean'
attributes(dataset_filter$`tBodyGyroJerk-mean()-X`)$label <- 'time Body Gyroscope Jerk X-axial signal - mean'
attributes(dataset_filter$`tBodyGyroJerk-mean()-Y`)$label <- 'time Body Gyroscope Jerk Y-axial signal - mean'
attributes(dataset_filter$`tBodyGyroJerk-mean()-Z`)$label <- 'time Body Gyroscope Jerk Z-axial signal - mean'
attributes(dataset_filter$`tBodyAccMag-mean()`)$label 		 <- 'time Body Accelerometer magnitude signal - mean'
attributes(dataset_filter$`tGravityAccMag-mean()`)$label 		 <- 'time Gravity Accelerometer magnitude signal - mean'
attributes(dataset_filter$`tBodyAccJerkMag-mean()`)$label 		 <- 'time Body Accelerometer Jerk magnitude signal - mean'
attributes(dataset_filter$`tBodyGyroMag-mean()`)$label 		 <- 'time Body Gyroscope magnitude signal - mean'
attributes(dataset_filter$`tBodyGyroJerkMag-mean()`)$label 		 <- 'frecuency Body Gyroscope Jerk magnitude signal - mean'
attributes(dataset_filter$`fBodyAcc-mean()-X`)$label <- 'frecuency Body Accelerometer X-axial signal - mean'
attributes(dataset_filter$`fBodyAcc-mean()-Y`)$label <- 'frecuency Body Accelerometer Y-axial signal - mean'
attributes(dataset_filter$`fBodyAcc-mean()-Z`)$label <- 'frecuency Body Accelerometer Z-axial signal - mean'
attributes(dataset_filter$`fBodyAccJerk-mean()-X`)$label <- 'frecuency Body Accelerometer Jerk X-axial signal - mean'
attributes(dataset_filter$`fBodyAccJerk-mean()-Y`)$label <- 'frecuency Body Accelerometer Jerk Y-axial signal - mean'
attributes(dataset_filter$`fBodyAccJerk-mean()-Z`)$label <- 'frecuency Body Accelerometer Jerk Z-axial signal - mean'
attributes(dataset_filter$`fBodyGyro-mean()-X`)$label <- 'frecuency Body Gyroscope X-axial signal - mean'
attributes(dataset_filter$`fBodyGyro-mean()-Y`)$label <- 'frecuency Body Gyroscope Y-axial signal - mean'
attributes(dataset_filter$`fBodyGyro-mean()-Z`)$label <- 'frecuency Body Gyroscope Z-axial signal - mean'
attributes(dataset_filter$`fBodyAccMag-mean()`)$label 		 <- 'frecuency Body Accelerometer magnitude signal - mean'
attributes(dataset_filter$`fBodyBodyAccJerkMag-mean()`)$label 		 <- 'frecuency Body Accelerometer Jerk magnitude signal - mean'
attributes(dataset_filter$`fBodyBodyGyroMag-mean()`)$label 		 <- 'frecuency Body Gyroscope magnitude signal - mean'
attributes(dataset_filter$`fBodyBodyGyroJerkMag-mean()`)$label 		 <- 'frecuency Body Gyroscope Jerk magnitude signal - mean'

# SD varabiables
attributes(dataset_filter$`tBodyAcc-std()-X`)$label <- 'time Body Accelerometer X-axial signal - Standard deviation'
attributes(dataset_filter$`tBodyAcc-std()-Y`)$label <- 'time Body Accelerometer Y-axial signal - Standard deviation'
attributes(dataset_filter$`tBodyAcc-std()-Z`)$label <- 'time Body Accelerometer Z-axial signal - Standard deviation'
attributes(dataset_filter$`tGravityAcc-std()-X`)$label <- 'time Gravity Accelerometer X-axial signal - Standard deviation'
attributes(dataset_filter$`tGravityAcc-std()-Y`)$label <- 'time Gravity Accelerometer Y-axial signal - Standard deviation'
attributes(dataset_filter$`tGravityAcc-std()-Z`)$label <- 'time Gravity Accelerometer Z-axial signal - Standard deviation'
attributes(dataset_filter$`tBodyAccJerk-std()-X`)$label <- 'time Body Accelerometer Jerk X-axial signal - Standard deviation'
attributes(dataset_filter$`tBodyAccJerk-std()-Y`)$label <- 'time Body Accelerometer Jerk Y-axial signal - Standard deviation'
attributes(dataset_filter$`tBodyAccJerk-std()-Z`)$label <- 'time Body Accelerometer Jerk Z-axial signal - Standard deviation'
attributes(dataset_filter$`tBodyGyro-std()-X`)$label <- 'time Body Gyroscope X-axial signal - Standard deviation'
attributes(dataset_filter$`tBodyGyro-std()-Y`)$label <- 'time Body Gyroscope Y-axial signal - Standard deviation'
attributes(dataset_filter$`tBodyGyro-std()-Z`)$label <- 'time Body Gyroscope Z-axial signal - Standard deviation'
attributes(dataset_filter$`tBodyGyroJerk-std()-X`)$label <- 'time Body Gyroscope Jerk X-axial signal - Standard deviation'
attributes(dataset_filter$`tBodyGyroJerk-std()-Y`)$label <- 'time Body Gyroscope Jerk Y-axial signal - Standard deviation'
attributes(dataset_filter$`tBodyGyroJerk-std()-Z`)$label <- 'time Body Gyroscope Jerk Z-axial signal - Standard deviation'
attributes(dataset_filter$`tBodyAccMag-std()`)$label 		 <- 'time Body Accelerometer magnitude signal - Standard deviation'
attributes(dataset_filter$`tGravityAccMag-std()`)$label 		 <- 'time Gravity Accelerometer magnitude signal - Standard deviation'
attributes(dataset_filter$`tBodyAccJerkMag-std()`)$label 		 <- 'time Body Accelerometer Jerk magnitude signal - Standard deviation'
attributes(dataset_filter$`tBodyGyroMag-std()`)$label 		 <- 'time Body Gyroscope magnitude signal - Standard deviation'
attributes(dataset_filter$`tBodyGyroJerkMag-std()`)$label 		 <- 'frecuency Body Gyroscope Jerk magnitude signal - Standard deviation'
attributes(dataset_filter$`fBodyAcc-std()-X`)$label <- 'frecuency Body Accelerometer X-axial signal - Standard deviation'
attributes(dataset_filter$`fBodyAcc-std()-Y`)$label <- 'frecuency Body Accelerometer Y-axial signal - Standard deviation'
attributes(dataset_filter$`fBodyAcc-std()-Z`)$label <- 'frecuency Body Accelerometer Z-axial signal - Standard deviation'
attributes(dataset_filter$`fBodyAccJerk-std()-X`)$label <- 'frecuency Body Accelerometer Jerk X-axial signal - Standard deviation'
attributes(dataset_filter$`fBodyAccJerk-std()-Y`)$label <- 'frecuency Body Accelerometer Jerk Y-axial signal - Standard deviation'
attributes(dataset_filter$`fBodyAccJerk-std()-Z`)$label <- 'frecuency Body Accelerometer Jerk Z-axial signal - Standard deviation'
attributes(dataset_filter$`fBodyGyro-std()-X`)$label <- 'frecuency Body Gyroscope X-axial signal - Standard deviation'
attributes(dataset_filter$`fBodyGyro-std()-Y`)$label <- 'frecuency Body Gyroscope Y-axial signal - Standard deviation'
attributes(dataset_filter$`fBodyGyro-std()-Z`)$label <- 'frecuency Body Gyroscope Z-axial signal - Standard deviation'
attributes(dataset_filter$`fBodyAccMag-std()`)$label 		 <- 'frecuency Body Accelerometer magnitude signal - Standard deviation'
attributes(dataset_filter$`fBodyBodyAccJerkMag-std()`)$label 		 <- 'frecuency Body Accelerometer Jerk magnitude signal - Standard deviation'
attributes(dataset_filter$`fBodyBodyGyroMag-std()`)$label 		 <- 'frecuency Body Gyroscope magnitude signal - Standard deviation'
attributes(dataset_filter$`fBodyBodyGyroJerkMag-std()`)$label 		 <- 'frecuency Body Gyroscope Jerk magnitude signal - Standard deviation'

############################################################################################
# the average of each variable for each activity and each subject.
############################################################################################

final_data <- dataset_filter %>%
  group_by(subject, activity, source) %>%
  summarise_each('mean')

write.table(dataset_mean, file = 'final_data.txt', row.name=FALSE)

