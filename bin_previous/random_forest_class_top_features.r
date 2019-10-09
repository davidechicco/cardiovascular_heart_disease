setwd(".")
options(stringsAsFactors = FALSE)


# function that returns the top features
selectTopFeatures <- function(rfModel, numFeatures) 
{

    dd <- as.data.frame(rfModel$importance);

    dd_sorted_MSE <- dd[order(-dd$"%IncMSE"), ]
    # print(dd_sorted_MSE);

    dd_sorted_IncNodePurity <- dd[order(-dd$"IncNodePurity"), ]

    # varImpPlot(rfModel)

    dd_sorted_IncNodePurity_only <- dd_sorted_IncNodePurity
    dd_sorted_IncNodePurity_only$"%IncMSE" <- NULL
    dd_sorted_IncNodePurity_only$purityPos <- c(1:dim(dd_sorted_IncNodePurity_only)[1])
    dd_sorted_MSE_only <- dd_sorted_MSE
    colnames(dd_sorted_MSE_only)[1] <- c("%IncMSE")


    dd_sorted_MSE_only$IncNodePurity <- NULL
    dd_sorted_MSE_only$msePos <- c(1:dim(dd_sorted_IncNodePurity_only)[1])

    dd_sorted_MSE_only$features <- rownames(dd_sorted_MSE_only)
    dd_sorted_IncNodePurity_only$features <- rownames(dd_sorted_IncNodePurity_only)

    #print(dd_sorted_MSE_only)
    #print(dd_sorted_IncNodePurity_only)

    mergedRanking <- cbind(dd_sorted_MSE_only, dd_sorted_IncNodePurity_only)

    mergedRankingAlphaBeta <- mergedRanking[order(mergedRanking$"features"), ]
    mergedRankingAlphaBeta$posSum <- mergedRankingAlphaBeta$purityPos + mergedRankingAlphaBeta$msePos

    mergedRankingGeneralRank <- mergedRankingAlphaBeta[order(mergedRankingAlphaBeta$"posSum"), ]
    mergedRankingGeneralRank$finalPos <- c(1:dim(mergedRankingGeneralRank)[1])
    lastCol <- dim(mergedRankingGeneralRank)[2]

    featuresCol <- 6

    # cat("\n\n\n\n")
    # print(mergedRankingGeneralRank[,c(lastCol), drop=FALSE])

    return (c((mergedRankingGeneralRank[1:numFeatures,])$features))

}



list.of.packages <- c("PRROC", "e1071", "randomForest","class", "gmodels", "formula.tools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("PRROC")
library("e1071")
library("randomForest")
library("class")
library("gmodels")
library("formula.tools")

source("./confusion_matrix_rates.r")
source("./utils.r")

# args = commandArgs(trailingOnly=TRUE)
# thisNtree <- as.integer(args[1])

# thisNtree <- 5000

threshold <- 0.5
fileName <- "../data/dataset_edited_without_time.csv"
patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

cat("[Randomizing the rows]\n")
patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows

totalElements <- dim(patients_data)[1]

subsets_size <- totalElements

target_index <- dim(patients_data)[2]

target_label <- colnames(patients_data[target_index])
cat("target_label = ", target_label, "\n", sep="")

if (subsets_size != totalElements) {
    cat("ATTENTION: We are running the method on a subset of the original dataset, \n", sep="")
    cat(" containing only ", subsets_size, " elements \n", sep="")
    cat(" instead of ", totalElements, " elements \n", sep="")
}

patients_data <- patients_data[1:subsets_size, ]

dataset_dim_retriever(patients_data)
imbalance_retriever(patients_data$death_event)

feature_selection_set_perc <- 10
training_set_perc <- 70

cat("[feature selection set = ", feature_selection_set_perc,"%]\n", sep="")
cat("[training set = ", training_set_perc,"%]\n", sep="")
cat("[test set = ", (100-training_set_perc-feature_selection_set_perc),"%]\n", sep="")


# the training set is the first % of the whole dataset
feature_selection_set_first_index <- 1 # NEW
feature_selection_set_last_index <- round(dim(patients_data)[1]*feature_selection_set_perc/100) # NEW


training_set_first_index <- feature_selection_set_last_index + 1 # NEW
training_set_last_index <- training_set_first_index + round(dim(patients_data)[1]*training_set_perc/100) # NEW

# the test set is the last 40% of the whole dataset
test_set_first_index <- training_set_last_index+1 # NEW
test_set_last_index <- dim(patients_data)[1] # NEW

cat("[Creating the training set and test set for the values]\n")
patients_data_feature_selection <- patients_data[feature_selection_set_first_index:feature_selection_set_last_index, 1:(target_index)] # NEW
patients_data_train <- patients_data[training_set_first_index:training_set_last_index, 1:(target_index)] # NEW
patients_data_test <- patients_data[test_set_first_index:test_set_last_index, 1:(target_index)] # NEW

cat("[feature selection set dimensions: ", dim(patients_data_feature_selection)[1], " patients]\n")
cat("[training set dimensions: ", dim(patients_data_train)[1], " patients]\n")
cat("[test set dimensions: ", dim(patients_data_test)[1], " patients]\n")

cat("[Creating the training set and test set for the labels \"1\"-\"0\"]\n")
patients_data_feature_selection_labels <- patients_data[feature_selection_set_first_index:feature_selection_set_last_index, target_index] # NEW
patients_data_train_labels <- patients_data[training_set_first_index:training_set_last_index, target_index] # NEW
patients_data_test_labels <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW


cat("patients_data_feature_selection:\n ")
dataset_dim_retriever(patients_data_feature_selection)
imbalance_retriever(patients_data_feature_selection$death_event)

cat("patients_data_train:\n ")
dataset_dim_retriever(patients_data_train)
imbalance_retriever(patients_data_train$death_event)

cat("patients_data_test:\n ")
dataset_dim_retriever(patients_data_test)
imbalance_retriever(patients_data_test$death_event)

cat("\n[Training the random forest classifier on the training set]\n")

# rf_new <- randomForest(death_event ~ ., data=patients_data_train, importance=TRUE, proximity=TRUE, ntree=thisNtree)
allFeaturesFormula <- death_event ~ .
selectedFormula <- allFeaturesFormula

rf_new <- randomForest(selectedFormula, data=patients_data_feature_selection, importance=TRUE, proximity=TRUE)
cat("\nFeatures used in this prediction: \t", as.character(selectedFormula), "\n\n", sep="")

TOP_FEATURE_NUM <- 2
thisTopFeatures <- selectTopFeatures(rf_new, TOP_FEATURE_NUM)

cat("thisTopFeatures: \n")
print(thisTopFeatures)

thisFormulaTop2features <- as.formula(paste("death_event ~ ", toString(thisTopFeatures[1]), " + ", toString(thisTopFeatures[2]), sep=""))
rf_new2features <- randomForest(thisFormulaTop2features, data=patients_data_train, importance=TRUE, proximity=TRUE)
cat("\nFeatures used in this prediction: \t", as.character(thisFormulaTop2features), "\n\n", sep="")

cat("\n[Applying the trained random forest classifier on the test set]\n")
patients_data_test_PRED <- predict(rf_new2features, patients_data_test, type="response")

confusion_matrix_rates(patients_data_test_labels, patients_data_test_PRED, "@@@ Test set @@@")

# mcc_outcome <- mcc(patients_data_test_labels, patients_data_test_PRED_binary)



