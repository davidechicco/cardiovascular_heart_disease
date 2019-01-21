setwd(".")
options(stringsAsFactors = FALSE)
# library("clusterSim")

library("OneR");
library(class)
library(gmodels)
source("./confusion_matrix_rates.r")

threshold <- 0.5

fileName <- "../data/dataset_edited_without_time_NORM.csv"
patients_data_norm <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

patients_data_norm <- patients_data_norm[sample(nrow(patients_data_norm)),] # shuffle the rows

target_index <- dim(patients_data_norm)[2]

training_set_perce = 80
cat("training_set_perce = ", training_set_perce, "\n", sep="")

# the training set is the first 60% of the whole dataset
training_set_first_index <- 1 # NEW
training_set_last_index <- round(dim(patients_data_norm)[1]*training_set_perce/100) # NEW

# the test set is the last 40% of the whole dataset
test_set_first_index <- training_set_last_index+1 # NEW
test_set_last_index <- dim(patients_data_norm)[1] # NEW

cat("[Creating the subsets for the values]\n")
patients_data_train <- patients_data_norm[training_set_first_index:training_set_last_index, 1:(target_index)] # NEW
patients_data_test <- patients_data_norm[test_set_first_index:test_set_last_index, 1:(target_index)] # NEW

patients_data_test_labels  <- patients_data_norm[test_set_first_index:test_set_last_index, target_index]   # NEW


print("dim(patients_data_train)")
print(dim(patients_data_train))

print("dim(patients_data_test)")
print(dim(patients_data_test))


# #rf_new <- randomForest(Metastasis ~ ., data=patients_data_train, importance=TRUE, proximity=TRUE)


# Original application of One Rule with all the dataset
prc_model_train <- OneR(patients_data_train, verbose = TRUE)

# Generation of the CART model
# prc_model_train <- OneR(Metastasis ~ keep.side + platelet.count..PLT., method="class", data=patients_data_train);

summary(prc_model_train)
prediction <- predict(prc_model_train, patients_data_test)
# eval_model(prediction, patients_data_test)

prediction_binary <- as.numeric(prediction) -1
patients_data_test_PRED_binary <- data.frame(prediction)

confusion_matrix_rates(patients_data_test_labels, prediction_binary, "@@@ Test set @@@")



