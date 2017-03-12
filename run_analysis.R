url_file<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url_file, "file_proj.zip")
unzip("file_proj.zip",list = TRUE)

library(dplyr)

dts_al <- tbl_df(read.table(paste0("./UCI HAR Dataset/", "activity_labels.txt"), header  = FALSE, stringsAsFactors = FALSE))
dts_fe <- tbl_df(read.table(paste0("./UCI HAR Dataset/","features.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_fe <- mutate(dts_fe, all = paste(V1,"-",V2))
dts_x_train <- tbl_df(read.table(paste0("./UCI HAR Dataset/train/","X_train.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_y_train <- tbl_df(read.table(paste0("./UCI HAR Dataset/train/","Y_train.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_s_train <- tbl_df(read.table(paste0("./UCI HAR Dataset/train/","subject_train.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_bax_train<- tbl_df(read.table(paste0("./UCI HAR Dataset/train/Inertial Signals/","body_acc_x_train.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_bay_train<- tbl_df(read.table(paste0("./UCI HAR Dataset/train/Inertial Signals/","body_acc_y_train.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_baz_train<- tbl_df(read.table(paste0("./UCI HAR Dataset/train/Inertial Signals/","body_acc_z_train.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_bgx_train<- tbl_df(read.table(paste0("./UCI HAR Dataset/train/Inertial Signals/","body_gyro_x_train.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_bgy_train<- tbl_df(read.table(paste0("./UCI HAR Dataset/train/Inertial Signals/","body_gyro_y_train.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_bgz_train<- tbl_df(read.table(paste0("./UCI HAR Dataset/train/Inertial Signals/","body_gyro_z_train.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_tax_train<- tbl_df(read.table(paste0("./UCI HAR Dataset/train/Inertial Signals/","total_acc_x_train.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_tay_train<- tbl_df(read.table(paste0("./UCI HAR Dataset/train/Inertial Signals/","total_acc_y_train.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_taz_train<- tbl_df(read.table(paste0("./UCI HAR Dataset/train/Inertial Signals/","total_acc_z_train.txt") ,header = FALSE, stringsAsFactors = FALSE))

dts_x_test <- tbl_df(read.table(paste0("./UCI HAR Dataset/test/","X_test.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_y_test <- tbl_df(read.table(paste0("./UCI HAR Dataset/test/","Y_test.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_s_test <- tbl_df(read.table(paste0("./UCI HAR Dataset/test/","subject_test.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_bax_test <- tbl_df(read.table(paste0("./UCI HAR Dataset/test/Inertial Signals/","body_acc_x_test.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_bay_test <- tbl_df(read.table(paste0("./UCI HAR Dataset/test/Inertial Signals/","body_acc_y_test.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_baz_test <- tbl_df(read.table(paste0("./UCI HAR Dataset/test/Inertial Signals/","body_acc_z_test.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_bgx_test <- tbl_df(read.table(paste0("./UCI HAR Dataset/test/Inertial Signals/","body_gyro_x_test.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_bgy_test <- tbl_df(read.table(paste0("./UCI HAR Dataset/test/Inertial Signals/","body_gyro_y_test.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_bgz_test <- tbl_df(read.table(paste0("./UCI HAR Dataset/test/Inertial Signals/","body_gyro_z_test.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_tax_test <- tbl_df(read.table(paste0("./UCI HAR Dataset/test/Inertial Signals/","total_acc_x_test.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_tay_test <- tbl_df(read.table(paste0("./UCI HAR Dataset/test/Inertial Signals/","total_acc_y_test.txt") ,header = FALSE, stringsAsFactors = FALSE))
dts_taz_test <- tbl_df(read.table(paste0("./UCI HAR Dataset/test/Inertial Signals/","total_acc_z_test.txt") ,header = FALSE, stringsAsFactors = FALSE))

names(dts_x_train) <- dts_fe$all
names(dts_x_test) <- dts_fe$all
names(dts_y_train) <- "id_activity"
names(dts_y_test) <- "id_activity"
names(dts_s_train) <- "Subject"
names(dts_s_test) <- "Subject"

dts_s_y_x_train=cbind(dts_s_train,dts_y_train,dts_x_train)
dts_s_y_x_test=cbind(dts_s_test,dts_y_test,dts_x_test)

dts_s_y_x = rbind(dts_s_y_x_train, dts_s_y_x_test)
  
dts_mean_std <- dts_s_y_x %>% select(matches("mean|std"))

names(dts_al)<- c("id_activity", "Activity")
dts_s_y_x <- inner_join(dts_s_y_x, dts_al, by = "id_activity")

names(dts_s_y_x)<-gsub("tBodyAcc-","Body acceleration signal in time domain (from the accelerometer)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("tBodyAccMag-","Body acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("tBodyAccJerk-","Body acceleration jerk signal in time domain (from the accelerometer)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("tBodyAccJerkMag-","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform (from the accelerometer)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("tGravityAcc-","Gravity acceleration signal in time domain (from the accelerometer)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("tGravityAccMag-","Gravity acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("tBodyGyro-","Body acceleration signal in time domain (from the gyroscope)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("tBodyGyroMag-","Body acceleration signal in time domain applied to Fast Fourrier Transform(from the gyroscope)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("tBodyGyroJerk-","Body acceleration jerk signal in time domain (from the gyroscope)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("tBodyGyroJerkMag-","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform(from the gyroscope)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("fBodyAcc-","Body acceleration signal in frequence domain (from the accelerometer)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("fBodyAccMag-","Body acceleration signal in frequence domain applied to Fast Fourier Transform(from the accelerometer)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("fBodyAccJerk-","Body acceleration jerk signal in frequence domain (from the accelerometer)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("fBodyGyro-","Body acceleration signal in frequence domain (from the gyroscope)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("fBodyAccJerkMag-","Body acceleration jerk signal in frequence domain applied to Fast Fourrier Transform (from the accelerometer)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("fBodyGyroMag-","Body acceleration signal in frequence domain applied to Fast Fourier Transform (from the gyroscope)",names(dts_s_y_x))
names(dts_s_y_x)<-gsub("mean()", "Mean", names(dts_s_y_x))
names(dts_s_y_x)<-gsub("std()", "Standard Deviation", names(dts_s_y_x))

dts_s_y_x_mean <- dts_s_y_x %>% group_by(Subject, id_activity) %>% summarise_all(mean)

write.table(dts_s_y_x_mean, "TidyData.txt", col.names = FALSE, row.names = FALSE)
