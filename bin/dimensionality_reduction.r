setwd(".")
options(stringsAsFactors = FALSE)

list.of.packages <- c("easypackages", "PRROC", "e1071", "randomForest","class", "gmodels", "formula.tools", "RSpectra", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")

# args = commandArgs(trailingOnly=TRUE)
# thisNtree <- as.integer(args[1])

# thisNtree <- 5000

num_to_return <- 1
exe_num <- sample(1:as.numeric(Sys.time()), num_to_return)

datasetFileName <- "/home/davide/projects/heart-failure-gene-expression-analysis/temp/STEMI_patients_data_heart_failure_1052379918.csv"
targetName <- "added_diagnosis"
patients_dataset <- read.csv(file=datasetFileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("datasetFileName = ", datasetFileName, "\n", sep="")


cat("[Randomizing the rows]\n")
patients_dataset <- patients_dataset[sample(nrow(patients_dataset)),] # shuffle the rows
patients_dataset <- patients_dataset%>%select(-targetName,targetName)

patients_dataset_original <- patients_dataset

totalElements <- dim(patients_dataset)[1]
target_index <- dim(patients_dataset)[2]

dataset_dim_retriever(patients_dataset)
imbalance_retriever(patients_dataset[,target_index])

trunc <- 5

cat("[Performing dimensionality reduction]\n")

# Dimensionality reduction  - tSVD 
dataset_without_labels <- patients_dataset[, 1:(target_index-1)]
dataset_labels <- patients_dataset[, target_index]
svds_outcome <- svds(as.matrix(dataset_without_labels), k=trunc)
dataset_without_labels_dim_red <- svds_outcome$u
dataset_without_labels_dim_red_with_labels <- (cbind(dataset_without_labels_dim_red, dataset_labels))
        
dim_red_patients_dataset <- as.data.frame(dataset_without_labels_dim_red_with_labels)
    

# target_index changed now
NEW_target_index <- dim(dim_red_patients_dataset)[2]
colnames(dim_red_patients_dataset)[NEW_target_index] <- targetName

dataset_dim_retriever(dim_red_patients_dataset)

datasetFileNameWithoutExtension <- strsplit(datasetFileName, ".csv")[[1]]
dimRedDatasetFile <- paste0(datasetFileNameWithoutExtension, "_dimRed_", exe_num, ".csv")
cat("The dimensionality reduction dataset will be saved in the ", dimRedDatasetFile, " file\n", sep="")
write.csv(dim_red_patients_dataset, file=dimRedDatasetFile, row.names=FALSE)

