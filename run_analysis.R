
# Returns one data set by reading and merging all component files.
# Data set comprises of the X values, Y values and Subject IDs.
# The path_prefix indicates the path where the data files can be found.
# The fname_suffix indicates the file name suffix to be used to create the complete file name.
#
# This also subsets the data to extract only the measurements on the mean and standard deviation for each measurement.
# The required columns in the subset is determined by selecting only those columns that have either "mean()" or "std()" in their names.
# Subsetting is done early on to help reduce memory requirements.
readData <- function(fname_suffix, path_prefix) {
    fpath <- file.path(path_prefix, paste0("y_", fname_suffix, ".txt"))
    y_data <- read.table(fpath, header=F, col.names=c("ActivityID"))
    
    fpath <- file.path(path_prefix, paste0("subject_", fname_suffix, ".txt"))
    subject_data <- read.table(fpath, header=F, col.names=c("SubjectID"))
    
    # read the column names
    data_cols <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
    
    # read the X data file
    fpath <- file.path(path_prefix, paste0("X_", fname_suffix, ".txt"))
    data <- read.table(fpath, header=F, col.names=data_cols$MeasureName)
    
    # names of subset columns required
    subset_data_cols <- grep(".*mean\\(\\)|.*std\\(\\)", data_cols$MeasureName)
    
    # subset the data (done early to save memory)
    data <- data[,subset_data_cols]
    
    # append the activity id and subject id columns
    data$ActivityID <- y_data$ActivityID
    data$SubjectID <- subject_data$SubjectID
    
    # return the data
    data
}


    # read test data set, in a folder named "test", and data file names suffixed with "test"
    test <- readData("test", "test")

    # read test data set, in a folder named "train", and data file names suffixed with "train"
    train <- readData("train", "train")

    # Merge both train and test data sets
    data <- rbind(test, train)

    # Also make the column names nicer
    cnames <- colnames(data)
    cnames <- gsub("\\.+mean\\.+", cnames, replacement="Mean")
    cnames <- gsub("\\.+std\\.+",  cnames, replacement="Std")
    colnames(data) <- cnames

    # Add the activity names as another column
    activity_labels <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
    activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)
    data_labeled <- merge(data, activity_labels)

    # Create a tidy data set that has the average of each variable for each activity and each subject.
    library(reshape2)
    
    # melt the dataset
    id_vars = c("ActivityID", "ActivityName", "SubjectID")
    measure_vars = setdiff(colnames(data_labeled), id_vars)
    melted_data <- melt(data_labeled, id=id_vars, measure.vars=measure_vars)
    
    # recast 
    tidy_data <- dcast(melted_data, ActivityName + SubjectID ~ variable, mean)    

    # Create the tidy data set and save it on to the named file
    write.table(tidy_data, "tidy.txt")

    print("Done.")