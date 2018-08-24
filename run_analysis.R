library(readr)

# Download file into a temporary folder and unzip it, then delete the unused download
temp <- tempfile(tmpdir = "./data/")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
              temp)
unzipped <- unzip(temp)
unlink(temp)

# Define a function to read in the .txt files and format & label the resulting DFs
extract_dataset <- function(x="train") {
        
        xtrain <- read_table(grep(paste(x, "/X_", x, ".txt$", sep = ""), unzipped, value = TRUE),
                             col_names = FALSE)
        
        features <- read.delim("./UCI HAR Dataset/features.txt", header = FALSE)
        
        activity <- read.delim(grep(paste(x, "/y_", x, ".txt$", sep = ""), unzipped, value = TRUE), 
                               header = FALSE, col.names = "activitylabel")
        
        subject <- read.delim(grep(paste(x, "/subject_", x, ".txt$", sep = ""), unzipped, value = TRUE), 
                              header = FALSE, col.names = "subjectid")
        
        # Name observation variables of xtrain with the corresponding feature names of features.txt
        fnames <- strsplit(as.character(features$V1), "^[0-9]{1,3} ")
        fnames <- sapply(fnames, function(x) x[2])
        colnames(xtrain) <- fnames
        
        # Add subject ids and labeled activities to xtrain observation set
        traindata <- do.call(bind_cols, c(xtrain, subject, activity))
        
        # Add variable indicating whether it is a training or test observation
        result <- mutate(traindata, purpose = x)
        
}

# Apply the "extract_dataset" function to create a tidy DF for training and test observations
traindata <- extract_dataset("train")
testdata <- extract_dataset("test")

# Merge both DFs for one date set
onedataset <- bind_rows(traindata, testdata)

# Select on variables that represent a mean or a standard deviation (std)
meanstdvar <- onedataset %>% select(c(grep("mean", names(merged),value = TRUE),
                                grep("std", names(merged), value = TRUE), subjectid, activitylabel))

# Rename activities into descriptive names
tidydata <- meanstdvar %>% mutate(activitylabel = case_when(
        activitylabel == 1 ~ "walking", 
        activitylabel == 2 ~ "walkingup",
        activitylabel == 3 ~ "walkingdown",
        activitylabel == 4 ~ "sitting",
        activitylabel == 5 ~ "standing",
        activitylabel == 6 ~ "laying"
        )
)

# Create a copy of the final tidy data set
tidydata2 <- tidydata

# Summarise the data set by subect & activity - display the mean of each variable
tidydata2 <- tidydata2 %>% group_by(subjectid, activitylabel) %>%
        summarise_all(mean)

# Delete temporary unzipped file
unlink(unzipped)

