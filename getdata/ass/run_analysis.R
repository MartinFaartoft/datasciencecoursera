run_analysis <- function() {
    #1. Merge training and test data
    
    #1.1 Read training data
    training <- read.table("train/X_train.txt")
    
    #1.2 Read test data
    test <- read.table("test/X_test.txt")
    
    #1.3 Read column names from features.txt
    features <- read.table("features.txt")
    cols <- features[,2]
    
    #1.4 Attach column names to training and test data
    colnames(training) <- cols
    colnames(test) <- cols
    
    #1.5 Add Subject column
    training <- cbind(training, subject = read.table("train/subject_train.txt")[,1])
    test <- cbind(test, subject = read.table("test/subject_test.txt")[,1])
    
    #1.6 Read Activity column
    activity_labels <- read.table("activity_labels.txt", stringsAsFactors=FALSE)
    training_activity <- read.table("train/y_train.txt")
    test_activity <- read.table("test/y_test.txt")
    
    #1.7 Replace activity number with label
    for (index in 1:dim(training_activity)[1]) {
        activity_type <- as.numeric(training_activity[index, 1])
        activity_label <- activity_labels[activity_type, 2]
        training_activity[index, 1] = activity_label
    }
    
    for (index in 1:dim(test_activity)[1]) {
        activity_type <- as.numeric(test_activity[index, 1])
        activity_label <- activity_labels[activity_type, 2]
        test_activity[index, 1] = activity_label
    }
    
    #1.8 Attach the activity column
    training <- cbind(training, activity = training_activity[,1])
    test <- cbind(test, activity = test_activity[,1])
    
    #1.9 Actually merge
    merged <- rbind(training, test)
    
    #2 Extracts only the measurements on the mean and standard deviation for each measurement.
    ix_relevant <- grepl("mean()|std()|subject|activity", colnames(merged), )
    subset <- merged[, ix_relevant]
    
    #2.1 calculate mean grouped by subject and activity
    means <- aggregate(. ~ subject + activity, data = subset, FUN = mean)
    
    #2.2 ordering
    means <- means[order(means$subject, means$activity), ]
    
    #2.3 remove rownames
    rownames(means) <- NULL
 
    #write results
    write.table(means, file = "output.txt", row.name=FALSE)
}