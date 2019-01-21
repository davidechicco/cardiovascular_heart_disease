setwd(".")
options(stringsAsFactors = FALSE)

list.of.packages <- c("PRROC", "e1071", "clusterSim","rpart")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("clusterSim")
library("PRROC")
library("e1071")
library("rpart")

source("./confusion_matrix_rates.r")
source("./utils.r")



threshold <- 0.5

fileNameDataNorm <- "../data/dataset_edited_without_time_NORM.csv"
patients_data_norm <- read.csv(fileNameDataNorm, header = TRUE, sep =",");
cat("Read data from file ", fileNameDataNorm, "\n", sep="")


target_index <- dim(patients_data_norm)[2]


original_patients_data_norm <- patients_data_norm

# shuffle the rows
patients_data_norm <- original_patients_data_norm[sample(nrow(original_patients_data_norm)),] 

# Allocation of the size of the training set
perce_training_set <- 80
size_training_set <- round(dim(patients_data_norm)[1]*(perce_training_set/100))

cat("perce_training_set = ",perce_training_set,"%", sep="")

# Allocation of the training set and of the test set
training_set <- (patients_data_norm[1:size_training_set,])
test_set_index_start <- size_training_set+1
test_set_index_end <- dim(patients_data_norm)[1]
test_set  <- patients_data_norm[test_set_index_start:test_set_index_end,]

test_labels <- patients_data_norm[test_set_index_start:test_set_index_end, target_index]   # NEW


print("dim(training_set)")
print(dim(training_set))

print("dim(test_set)")
print(dim(test_set))


# Generation of the CART model
# cart_model <- rpart(class.of.diagnosis ~ keep.side + platelet.count..PLT., method="class", data=training_set);
cart_model <- rpart(death_event ~ ., method="class", data=training_set);

pred_test_predictions <- as.numeric(predict(cart_model, test_set, typ="class"))-1
pred_test_set_labels <- as.numeric(test_set$death_event)

patients_data_test_PRED_binary <- as.numeric(pred_test_predictions)

patients_data_test_PRED_binary[patients_data_test_PRED_binary>=threshold]=1
patients_data_test_PRED_binary[patients_data_test_PRED_binary<threshold]=0
# mcc_outcome <- mcc(pred_test_set_labels, patients_data_test_PRED_binary)
# confusion_matrix_rates(pred_test_set_labels, patients_data_test_PRED_binary)

confusion_matrix_rates(test_labels, pred_test_predictions, "@@@ Test set @@@")

