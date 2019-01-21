setwd(".")
options(stringsAsFactors = FALSE)
# library("clusterSim")
# library("PRROC")
library("e1071")

source("./confusion_matrix_rates.r")

threshold <- 0.5

cat("threshold = ", threshold, "\n", sep="")

fileNameDataNorm <- "../data/dataset_edited_without_time_NORM.csv"
patients_data_norm <- read.csv(fileNameDataNorm, header = TRUE, sep =",");
cat("Read data from file ", fileNameDataNorm, "\n", sep="")

patients_data_norm <- patients_data_norm[sample(nrow(patients_data_norm)),] # shuffle the rows

target_index <- dim(patients_data_norm)[2]

training_set_perce = 80
cat("training_set_perce = ", training_set_perce, "%\n", sep="")

# the training set is the first 60% of the whole dataset
training_set_first_index <- 1 # NEW
training_set_last_index <- round(dim(patients_data_norm)[1]*training_set_perce/100) # NEW

# the test set is the last 40% of the whole dataset
test_set_first_index <- training_set_last_index+1 # NEW
test_set_last_index <- dim(patients_data_norm)[1] # NEW

cat("[Creating the subsets for the values]\n")
patients_data_train <- patients_data_norm[training_set_first_index:training_set_last_index, 1:(target_index)] # NEW
patients_data_test <- patients_data_norm[test_set_first_index:test_set_last_index, 1:(target_index)] # NEW

cat("[Creating the subsets for the labels \"1\"-\"0\"]\n")
patients_data_train_labels <- patients_data_norm[training_set_first_index:training_set_last_index, target_index] # NEW
patients_data_test_labels <- patients_data_norm[test_set_first_index:test_set_last_index, target_index]   # NEW


print("dim(patients_data_train)")
print(dim(patients_data_train))

print("dim(patients_data_test)")
print(dim(patients_data_test))


library(class)
library(gmodels)

naive_bayes_model <-  naiveBayes(as.factor(death_event) ~ . , data=patients_data_train)

patients_data_test_PRED <- predict((naive_bayes_model), patients_data_test)
patients_data_test_PRED_binary <- as.numeric(patients_data_test_PRED)-1

patients_data_test_PRED_binary[patients_data_test_PRED_binary>=threshold]=1
patients_data_test_PRED_binary[patients_data_test_PRED_binary<threshold]=0

# print("predictions:")
# print(patients_data_test_PRED_binary)
# 
# 
# print("labels:")
# print(patients_data_test$death_event)


confusion_matrix_rates(patients_data_test_labels, patients_data_test_PRED_binary, "@@@ Test set @@@")



