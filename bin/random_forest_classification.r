setwd(".")
options(stringsAsFactors = FALSE)

EXP_ARG_NUM <- 2


TRAIN_SET_OVERSAMPLING_SYNTHETIC <- FALSE

# args = commandArgs(trailingOnly=TRUE)
# if (length(args)<EXP_ARG_NUM) {
#   stop("At least two argument must be supplied (input files)", call.=FALSE)
# } else {
#   # default output file
#   fileName <- args[1]
#   targetName <- args[2]
# }

fileName <- "../data/dataset_edited_without_time.csv"
targetName <- "death_event"

# fileName <- "../data/dataset_edited_without_time.csv"
# targetName <- "death_event"

# fileName <- "../../../projects/sepsis_severity_ICU/data/sepsis_severity_dataset_edited_2019-02-11.csv"
# targetName <- "ADDED.survival"

cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")

list.of.packages <- c("easypackages", "PRROC", "e1071", "randomForest","class", "gmodels", "formula.tools", "dplyr", "pastecs", "ROSE")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)


source("./confusion_matrix_rates.r")
source("./utils.r")

NUM_METRICS <- 7
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1 score", "accuracy", "TP_rate", "TN_rate", "PR_AUC", "ROC_AUC")

threshold <- 0.5

patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

# let's put the target label last on the right 
patients_data <- patients_data%>%select(-targetName,targetName)
target_index <- dim(patients_data)[2]    
patients_data_original <- patients_data

# formula
allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))

# cycle of executions

execution_number <- 100
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat("[Randomizing the rows]\n")
    patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows

    totalElements <- dim(patients_data)[1]

    subsets_size <- totalElements

    target_label <- colnames(patients_data[target_index])
    cat("target_label = ", target_label, "\n", sep="")

    if (subsets_size != totalElements) {
        cat("ATTENTION: We are running the method on a subset of the original dataset, \n", sep="")
        cat(" containing only ", subsets_size, " elements \n", sep="")
        cat(" instead of ", totalElements, " elements \n", sep="")
    }

    patients_data <- patients_data[1:subsets_size, ]

    dataset_dim_retriever(patients_data)
    imbalance_retriever(patients_data[,target_index])

    training_set_perc <- 80
    INPUT_PERC_POS <- 50
    cat("[training set = ", training_set_perc,"%]\n", sep="")
    cat("[test set = ", (100-training_set_perc),"%]\n", sep="")

    artificialBalance <- FALSE
    balancedFlag <- FALSE # flag that sets everything to 50% 50% ratio

    if (artificialBalance == TRUE) {


        train_data_balancer_output <- train_data_balancer(patients_data, target_index, training_set_perc, INPUT_PERC_POS, balancedFlag)

        patients_data_train <- train_data_balancer_output[[1]]
        patients_data_test <- train_data_balancer_output[[2]]
        
        # Creating the subsets for the targets
        patients_data_train_labels <- patients_data_train[, target_index] # NEW
        patients_data_test_labels <- patients_data_test[, target_index]   # NEW

    } else {


        # the training set is the first 60% of the whole dataset
        training_set_first_index <- 1 # NEW
        training_set_last_index <- round(dim(patients_data)[1]*training_set_perc/100) # NEW

        # the test set is the last 40% of the whole dataset
        test_set_first_index <- training_set_last_index+1 # NEW
        test_set_last_index <- dim(patients_data)[1] # NEW

        cat("[Creating the training set and test set for the values]\n")
        patients_data_train <- patients_data[training_set_first_index:training_set_last_index, 1:(target_index)] # NEW
        patients_data_test <- patients_data[test_set_first_index:test_set_last_index, 1:(target_index)] # NEW
        
        # train_balanced_over <- ovun.sample(allFeaturesFormula, data = patients_data_train, method = "both",  p=0.5, N = (nrow(patients_data_train)*2))$data
        # patients_data_train <- train_balanced_over
         
         # https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
         
         if(TRAIN_SET_OVERSAMPLING_SYNTHETIC == TRUE)
         {
            thisP <- 0.5
         
            data.rose <- ROSE(allFeaturesFormula, data = patients_data_train, p=thisP, seed = 1)$data
            patients_data_train <- data.rose
         }
        
        cat("[training set dimensions: ", dim(patients_data_train)[1], " patients]\n")

        cat("[test set dimensions: ", dim(patients_data_test)[1], " patients]\n")

        cat("[Creating the training set and test set for the labels \"1\"-\"0\"]\n")
        patients_data_train_labels <- patients_data_train[, target_index] # NEW
        patients_data_test_labels <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW

    }


    dataset_dim_retriever(patients_data_train)
    imbalance_retriever(patients_data_train[, targetName])    

    cat("\n[Training the random forest classifier on the training set]\n")

    rf_new <- NULL
    rf_new <- randomForest(allFeaturesFormula, data=patients_data_train, importance=TRUE, proximity=TRUE)
    
    cat("\n[Applying the trained random forest classifier on the test set]\n")
    patients_data_test_PRED <- predict(rf_new, patients_data_test, type="response")

    thisConfMat <- confusion_matrix_rates(patients_data_test_labels, patients_data_test_PRED, "@@@ Test set @@@")
    
    if (exe_i == 1)  confMatDataFrame <-  thisConfMat
    else  confMatDataFrame <- rbind(confMatDataFrame, thisConfMat)
    
 }
 
 cat("\n\n\n=== final results ===\n")
 
 cat("Number of executions = ", execution_number, "\n", sep="")
 # statistics on the dataframe of confusion matrices
 statDescConfMatr <- stat.desc(confMatDataFrame)
medianAndMeanRowResults <- (statDescConfMatr)[c("median", "mean"),]
print(dec_three(medianAndMeanRowResults))
cat("\n\n=== === === ===\n")
