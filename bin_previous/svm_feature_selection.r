setwd(".")
options(stringsAsFactors = FALSE)

list.of.packages <- c("e1071","PRROC", "caret", "fastAdaboost")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")


fileName <- "../data/dataset_edited_without_time_NORM.csv"
patients_data_norm <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

patients_data_norm <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
patients_data_norm <- patients_data_norm[sample(nrow(patients_data_norm)),] # shuffle the rows

totalElements <- dim(patients_data_norm)[1]

subsets_size <- totalElements

if (subsets_size != totalElements) {
    cat("!!! ATTENTION: We are running the method on a subset of the original dataset, \n", sep="")
    cat("!!! containing only ", subsets_size, " elements \n", sep="")
    cat("!!! instead of ", totalElements, " elements \n", sep="")
}

patients_data_norm <- patients_data_norm[1:subsets_size, ]

dataset_dim_retriever(patients_data_norm)
target_index <- dim(patients_data_norm)[2]
# imbalance_retriever(patients_data_norm[,target_index])

patients_norm_data_labels <-  (patients_data_norm[, target_index])

num_folds <- 20
num_feature <- c(ncol(patients_data_norm))

# svm for feature selection
svmProfile <- rfe(patients_data_norm, patients_norm_data_labels,
                  sizes = num_feature,
                  rfeControl = rfeControl(functions = caretFuncs, number = num_folds),
                  method = "svmRadial")
                  
                  
featureImportance <- varImp(svmProfile, scale=FALSE)
featureImportance$feature <- rownames(featureImportance)
print(featureImportance)



