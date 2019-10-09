setwd(".")
options(stringsAsFactors = FALSE)

list.of.packages <- c("PRROC", "e1071", "clusterSim","rpart", "caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("clusterSim")
library("PRROC")
library("e1071")
library("rpart")
library("caret")

source("./confusion_matrix_rates.r")
source("./utils.r")


fileNameDataNorm <- "../data/dataset_edited_without_time_NORM.csv"
patients_data_norm <- read.csv(fileNameDataNorm, header = TRUE, sep =",");
cat("Read data from file ", fileNameDataNorm, "\n", sep="")


target_index <- dim(patients_data_norm)[2]
original_patients_data_norm <- patients_data_norm

# shuffle the rows
patients_data_norm <- original_patients_data_norm[sample(nrow(original_patients_data_norm)),] 

# select formula based on feature
allFeaturesFormula <- as.formula(paste(colnames(patients_data_norm)[target_index], '.', sep=' ~ ' ))
selectedFormula <- allFeaturesFormula

cart_model <- rpart(selectedFormula, method="class", data=patients_data_norm);
ptree <- prune(cart_model, cp=cart_model$cptable[which.min(cart_model$cptable[,"xerror"]),"CP"])
importanceFrame <- (varImp(ptree))
importanceFrame$feature <- rownames(importanceFrame)
importanceFrameSorted <- importanceFrame[order(-importanceFrame$"Overall"), ]

print(importanceFrameSorted)
