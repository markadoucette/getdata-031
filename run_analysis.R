# ==========    Read files in

#Load Test files
        files_X <- "data/UCI HAR Dataset/test/X_test.txt"
        files_Y <- "data/UCI HAR Dataset/test/y_test.txt"
        files_Subject <- "data/UCI HAR Dataset/test/subject_test.txt"
#Load Train files
        files_X <- c(files_X, "data/UCI HAR Dataset/train/X_train.txt" )
        files_Y <- c(files_Y,"data/UCI HAR Dataset/train/y_train.txt" )
        files_Subject <- c(files_Subject, "data/UCI HAR Dataset/train/subject_train.txt") 
#Load Featurs
        Features <- read.table("data/UCI HAR Dataset/features.txt")
#Load Activitys
        Activity_lables <- read.table("data/UCI HAR Dataset/activity_labels.txt")

# ==========    Merge files

#Merge X
        X <- data.frame()
        for (i in 1:length(files_X) ) {
                X <- rbind(X, read.table(files_X[i]) )
        }
#Merge Y
        Y <- data.frame()
        for (i in 1:length(files_Y) ) {
                Y <- rbind(Y, read.table(files_Y[i]) )
        }
#Merge Subject
        Subject <- data.frame()
        for (i in 1:length(files_Subject) ) {
                Subject <- rbind(Subject, read.table(files_Subject[i]) )
        }
# ========== Get columns that have Mean or Standard Dievation

#Label X
        colnames(X) <- c(as.character(Features[,2]))
#Mean and Standard Dievation columns in X
        mean_sd<-grep("-mean|-std",colnames(X))
        means <-X[,mean_sd]
# ========== Aggragate tables for Activities
        
#Create Activities Dataframe
        act1 <- cbind(Y,means)
#Label Activity column
        colnames(act1)[1] <- "Activity"
#Label Activities on Activities Dataframe
        Activity_lables[,2]<-as.character(Activity_lables[,2])
        for(i in 1:length(act1[,1])){
                act1[i,1]<-Activity_lables[act1[i,1],2]
        }
# ==========    Second dataset

#Combine subjects and activites
        act2 <-cbind(Subject,act1)
#Label Subject column
        colnames(act2)[1] <- "Subject"
#Create new dataframe for final tidy data
        data <- aggregate( act2[,3] ~ Subject+Activity, data = act2, FUN= "mean" )
#Add rest of the columns to the datas
        for(i in 4:ncol(act2)){
                data[,i] <- aggregate( act2[,i] ~ Subject+Activity, data = act2, FUN= "mean" )[,3]
        }
#Label columns in data
        colnames(data)[3:ncol(data)] <- colnames(means)
#Write table to file
        #write.table(data, file = "getdata-031_tidy_data.txt", row.names = FALSE)
data        
