setwd(".")
options(stringsAsFactors = FALSE)

# Survival analysis and the stratified sample:
# https://towardsdatascience.com/survival-analysis-and-the-stratified-sample-2c2582aa9805

list.of.packages <- c("easypackages", "survival",  "dplyr", "survminer", "stats", "PRROC", "formula.tools", "dplyr", "pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")

NUM_METRICS <- 7
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1 score", "accuracy", "TP_rate", "TN_rate", "PR_AUC", "ROC_AUC")


fileNameData<- "../data/S1Data_EDITED2.csv"
targetName <- "DEATH_EVENT"
patients_data <- read.csv(fileNameData, header = TRUE, sep =",")
cat("Read data from file ", fileNameData, "\n", sep="")

# let's put the target label last on the right 
patients_data <- patients_data%>%select(-targetName,targetName)
target_index <- dim(patients_data)[2]


execution_number <- 100
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

  cat("exe = ", exe_i, "\n", sep="")

  # shuffle the rows
  patients_data <- patients_data[sample(nrow(patients_data)),] 

  num_to_return <- 1
  upper_num_limit <- 10000000
  exe_num <- sample(1:upper_num_limit, num_to_return)


  cat("nrow(patients_data) = ", nrow(patients_data), "\n", sep="")
  cat("ncol(patients_data) = ", ncol(patients_data), "\n", sep="")


  # split training set and test set
  training_set_perce <- 80
  cat("training_set_perce = ", training_set_perce, "%\n", sep="")
  # validation_set_perce <- 40
  # cat("validation_set_perce = ", validation_set_perce, "%\n", sep="")

  # the training set is the first training_set_perce% of the whole dataset
  training_set_first_index <- 1 # NEW
  training_set_last_index <- round(dim(patients_data)[1]*training_set_perce/100) 

  # validation_set_first_index <- training_set_last_index + 1
  # validation_set_last_index <- round(dim(patients_data)[1]*validation_set_perce/100) 

  # the test set is the last 20% of the whole dataset
  test_set_first_index <- training_set_last_index + 1
  test_set_last_index <- dim(patients_data)[1] # NEW

  cat("[Creating the subsets for the values]\n")
  prc_data_train <- patients_data[training_set_first_index:training_set_last_index, ] 
  prc_data_test <- patients_data[test_set_first_index:test_set_last_index, ] 

  # prc_data_validation <- patients_data[validation_set_first_index:validation_set_last_index, ] 

  prc_data_train_labels <- prc_data_train[, target_index] 
  # prc_data_validation_labels <- prc_data_validation[, target_index] 
  prc_data_test_labels <- prc_data_test[, target_index] 

  cat("creating glm model\n")
  glm_model <- glm(DEATH_EVENT ~ sex + smoking + diabetes + high_blood_pressure + anaemia + age + ejection_fraction + serum_sodium + serum_creatinine + platelets + creatinine_phosphokinase + factor(TIME_MONTH), data = prc_data_train, family = "binomial")


  cat("creating feature ranking\n")
  # clinical ranking on the training set
  featureImportance <-  varImp(glm_model, scale = FALSE)
  featureImportance$clinical_feature <- row.names(featureImportance)
  clinical_feature_ranking <- featureImportance[order(-featureImportance$"Overall"),]


  cat("selecting the top features\n")
  # select the features that do not contain the word "factor"
  grep("^[^factor*]", row.names(clinical_feature_ranking), value=TRUE)

  # select the top N features
  N <- 2
  selectedFeatures <- nonTimeFeatures[1:N]

  cat("training the 2 feature model on the training set\n")

  if( selectedFeatures[1] == "ejection_fraction" && selectedFeatures[2]=="serum_creatinine")
  {

    cat("creating the glm model on the 2 features\n")
    glm_model2features <- glm(DEATH_EVENT ~ ejection_fraction + serum_creatinine + factor(TIME_MONTH), data = prc_data_train[,c(selectedFeatures, "DEATH_EVENT", "TIME_MONTH")], family = "binomial")

    # thisFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), paste(selectedFeatures[1], selectedFeatures[2], factor(TIME_MONTH), sep='+'), sep=' ~ '))
    # glm_model2features <- glm(thisFormula, data = prc_data_train, family = "binomial")

    cat("testing the 2 features glm model on the test set\n")
    prc_data_test_pred <- predict(glm_model2features, prc_data_test[,c(selectedFeatures, "TIME_MONTH")], type = "response")
    threshold <- 0.5
    prc_data_test_pred_bin <- as.numeric(prc_data_test_pred)

    prc_data_test_pred_bin[prc_data_test_pred_bin>=threshold]<-1
    prc_data_test_pred_bin[prc_data_test_pred_bin<threshold]<-0

    thisConfMat <- confusion_matrix_rates(prc_data_test_pred_bin, prc_data_test_labels, "@@@ Test set @@@")
    
    if (exe_i == 1)  confMatDataFrame <-  thisConfMat
    else  confMatDataFrame <- rbind(confMatDataFrame, thisConfMat)

  }

}


cat("\n\n\n=== final results ===\n")
 
cat("Number of executions = ", execution_number, "\n", sep="")
 # statistics on the dataframe of confusion matrices
statDescConfMatr <- stat.desc(confMatDataFrame)
meanRowResults <- (statDescConfMatr)[c("mean"),]
print(dec_three(meanRowResults))
cat("\n\n=== === === ===\n")