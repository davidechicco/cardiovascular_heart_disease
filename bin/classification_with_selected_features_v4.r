
# Script originally mainly written by Giuseppe Jurman <jurman@fbk.eu> on 29th March 2019.


setwd(".")
options(stringsAsFactors = FALSE)

# package loading
list.of.packages <- c("easypackages", "FSelector", "caret", "MXM", "e1071", "rpart", "xgboost", "randomForest", "kernlab", "mltools", "boot", "PRROC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./utils.r")
# source("/home/davide/GitHub_general_project/general_project/bin/confusion_matrix.r")
datasetFile <- "../data/dataset_edited_without_time.csv"
targetName <- "death_event"

# "death_event" is the target here

# # # # # # # # # # # # # # # # # # 

# confusion matrix tau threshold
CF_TAU <- 0.5

# aggregate results
results_by_method <- function(thisSplits, algorithm) 
{
    # algorithm must be "rf" or "svm" or "xgb"
    if(algorithm != "rf" & algorithm != "svm" & algorithm != "xgb")   
    {
        cat("error: the algorithm must be rf or svm or xgb. The program will exit\n")
        sys.exit()    
    }

    algorithm_MCCs <- unlist(lapply(thisSplits, FUN = function(x){x[["top_feats"]][[algorithm]][["TS"]][["MCC"]]}))
    algorithm_accuracies<- unlist(lapply(thisSplits, FUN = function(x){x[["top_feats"]][[algorithm]][["TS"]][["accuracy"]]}))
    algorithm_f1_scores<- unlist(lapply(thisSplits, FUN = function(x){x[["top_feats"]][[algorithm]][["TS"]][["F1_score"]]}))
    algorithm_TP_rates <- unlist(lapply(thisSplits, FUN = function(x){x[["top_feats"]][[algorithm]][["TS"]][["TP_rate"]]}))
    algorithm_TN_rates <- unlist(lapply(thisSplits, FUN = function(x){x[["top_feats"]][[algorithm]][["TS"]][["TN_rate"]]}))
    algorithm_PR_AUCs <- unlist(lapply(thisSplits, FUN = function(x){x[["top_feats"]][[algorithm]][["TS"]][["PR_AUC"]]}))
    algorithm_ROC_AUCs <- unlist(lapply(thisSplits, FUN = function(x){x[["top_feats"]][[algorithm]][["TS"]][["ROC_AUC"]]}))
   
   algorithmCompleteName <- NULL
   if(algorithm == "rf") algorithmCompleteName <- "Random Forests"
   if(algorithm == "svm") algorithmCompleteName <- "Support Vector Machine with radial Gaussian kernel"
   if(algorithm == "xgb") algorithmCompleteName <- "Gradient boosting (XGBoost)"
   
   
    cat("\nconfidence_intervals mean results for ", algorithmCompleteName," \n", sep="")
    CI_algorithm_MCCs <- confidence_intervals(algorithm_MCCs)
    CI_algorithm_accuracies <- confidence_intervals(algorithm_accuracies)
    CI_algorithm_f1_scores <- confidence_intervals(algorithm_f1_scores)
    CI_algorithm_TP_rates <- confidence_intervals(algorithm_TP_rates)
    CI_algorithm_TN_rates <- confidence_intervals(algorithm_TN_rates)
    CI_algorithm_PR_AUCs <- confidence_intervals(algorithm_PR_AUCs)
    CI_algorithm_ROC_AUCs <- confidence_intervals(algorithm_ROC_AUCs)
    
    cat("MCC  accuracy  F1_score  TP_rate  TN_rate  PR_AUC   ROC_AUC\n")
    meanIndex <- 2
    cat(dec_three(CI_algorithm_MCCs[meanIndex]), "\t", dec_three(CI_algorithm_accuracies[meanIndex]), "\t", dec_three(CI_algorithm_f1_scores[meanIndex]), "   ", dec_three(CI_algorithm_TP_rates[meanIndex]), "  ", dec_three(CI_algorithm_TN_rates[meanIndex]),  " ", dec_three(CI_algorithm_PR_AUCs[meanIndex]),  " ", dec_three(CI_algorithm_ROC_AUCs[meanIndex]), "\n")

}

# mean_fun
mean.fun <- function(d, i)
{    
    m <- mean(d$data[i])
    n <- length(i)
    v <- (n-1)*var(d$data[i])/n^2
    c(m, v)
}

# ci function
confidence_intervals <- function(data){
  
  thisDataFrame <- as.data.frame( cbind(seq(length(data)),data)  )
  VALUE_COL <- 2
  FIRST_ELE <- 1
    
  # if all the elements have the same value
  if(all(thisDataFrame[,VALUE_COL]==thisDataFrame[,VALUE_COL][FIRST_ELE]))
  {
    output_result  <- c(thisDataFrame[,VALUE_COL][FIRST_ELE], thisDataFrame[,VALUE_COL][FIRST_ELE], thisDataFrame[,VALUE_COL][FIRST_ELE])
  }
  else
  {
    names(thisDataFrame) <- c("id","data")
    bootsrap_reps <- 999
    boot <- boot(thisDataFrame, mean.fun, R=bootsrap_reps )
    # cat("boot()\n")
    # print(boot)
    boot.ci <- boot.ci(boot, type = c("stud"))
    # cat("boot.ci()\n")
    # print(boot.ci)
    output_result <- c(boot.ci$student[4], boot.ci$t0, boot.ci$student[5])
  }
  
 # cat("output_result\n")
 # print(output_result)
  
  # cat("confidence intervals (ci)\n")
  names(output_result) <- c("lower","mean","upper")
  return(output_result)
}

# feature ranking function:
# takes in input the whole dataset and the list of sample indices for the training set
ranker <- function(data, train_indices, method="relief")
{
    x <- data[train_indices,,drop=FALSE]
    xx <- x[,which(names(x) != "death_event", arr.ind = TRUE)]
    
    if(method=="relief"){
        tmp<-relief(death_event~.,x)
        ranked_list <- rownames(tmp[order(tmp$attr_importance,decreasing = TRUE),,drop=FALSE])
    }
    
    if(method=="MMPC"){
        tmp <- MMPC(target=as.vector(x$death_event),dataset = xx)
        ranked_list <- names(xx)[order(tmp@stats, decreasing = TRUE)]
    }
    
    if(method=="RF"){
        tmp <- random.forest.importance(death_event~.,x,importance.type = 1)
        ranked_list <- rownames(tmp[order(tmp$attr_importance,decreasing = TRUE),,drop=FALSE])
    }
    
    if(method=="oneR"){
        tmp <- oneR(death_event~.,x)
        
#         cat("\noneR\n")
#         cat("str(tmp): \n")
#         str(tmp)
#         print(tmp)
       
        ranked_list <- rownames(tmp[order(tmp$attr_importance,decreasing = TRUE),,drop=FALSE])

    }
    
    if(method=="rpart"){
        tmp <- rpart(death_event~.,x)
        tmp2 <- rep(0,length(names(xx)))
        names(tmp2) <- names(xx)
        for(nn in names(tmp$variable.importance)) tmp2[nn] <- tmp$variable.importance[nn]
        ranked_list <- names(sort(tmp2,decreasing = TRUE))
    }
    
    if(method=="lsvm"){
        fit <- svm(death_event~.,data=x)
        w <- t(fit$coefs) %*% fit$SV                 
        w <- apply(w, 2, function(v){sqrt(sum(v^2))})  
        ranked_list<- names(sort(w, decreasing = TRUE))
    }
    
    if(method=="xgboost"){
        train <- list(data=xx,label=x$death_event)
        tmp <- xgboost(data = as.matrix(train$data), label = train$label, nrounds = 5, 
                                        objective = "binary:logistic", verbose=0)
        ranked_list<- xgb.importance(model = tmp)$Feature
        for(i in names(xx)) if (i %in% ranked_list==FALSE) ranked_list[length(ranked_list)+1] <- i
    }
  
  return(ranked_list)
  
}

  
# classifier
classif <- function(method, the_train, the_test, feats)
{
  res <- list()
  
  predicted_values <- NULL
  actual_values <- NULL
  
  if(method=="rf"){
    dd<-randomForest(as.factor(death_event)~ ., data=the_train,ntree=1000)
    cf_tr <- dd$confusion[1:2,1:2]
    cf_ts <- confusionMatrix(as.factor(predict(dd,the_test)),as.factor(the_test$death_event))$table
    
    predicted_values <- predict(dd,the_test)
    actual_values <- the_test$death_event
    }
  
  if(method=="svm"){
    aa <- ksvm(death_event~.,data=the_train,type='C-svc',C=100000,kernel="rbfdot")
    cf_tr <- confusionMatrix(as.factor(aa@fitted),as.factor(aa@ymatrix))$table
    cf_ts <- confusionMatrix(as.factor(predict(aa,the_test)),as.factor(the_test$death_event))$table
    
    predicted_values <- predict(aa,the_test)
    actual_values <- the_test$death_event
    }
  
  if(method=="xgb"){
    ee <- xgboost(data = as.matrix(the_train[,feats]), label = the_train$death_event,nrounds = 25, silent=0,objective = "binary:logistic", verbose=0)
    cf_tr <- confusionMatrix(as.factor(as.numeric(predict(ee,as.matrix(the_train[,feats]))>CF_TAU)),as.factor(the_train$death_event))$table
    cf_ts <- confusionMatrix(as.factor(as.numeric(predict(ee,as.matrix(the_test[,feats]))>CF_TAU)),as.factor(the_test$death_event))$table
    
#     print("confusionMatrix(as.factor(as.numeric(predict(ee,as.matrix(the_test[,feats]))>CF_TAU)),as.factor(the_test$death_event))")
#     print(confusionMatrix(as.factor(as.numeric(predict(ee,as.matrix(the_test[,feats]))>CF_TAU)),as.factor(the_test$death_event)))
#     
    predicted_values <- predict(ee,as.matrix(the_test[,feats]))
    actual_values <- the_test$death_event
  }
  
  fg_test <- predicted_values[actual_values==1]
  bg_test <- predicted_values[actual_values==0]
  
  pr_curve_test <- pr.curve(scores.class0 = fg_test, scores.class1 = bg_test, curve = F)
  prc_auc <- pr_curve_test$auc.integral
  roc_curve_test <- PRROC::roc.curve(scores.class0 = fg_test, scores.class1 = bg_test, curve = F)
  roc_auc <- roc_curve_test$auc
 
  row.names(cf_tr) <- NULL
  colnames(cf_tr) <- NULL
  cf_tr <- matrix(cf_tr, nrow = 2)
  res[["TR"]] <- list()
  res[["TR"]][["ConfMat"]] <- cf_tr
  res[["TR"]][["MCC"]] <- mcc(confusionM = cf_tr)
  
  # What does TS mean?!?!?!
  
  row.names(cf_ts) <- NULL
  colnames(cf_ts) <- NULL
  dimnames(cf_ts) <- NULL
  cf_ts <- matrix(cf_ts,nrow = 2)
  res[["TS"]] <- list()
  res[["TS"]][["ConfMat"]] <- cf_ts
  res[["TS"]][["MCC"]] <- mcc(confusionM = cf_ts)
  
  TN <- cf_ts[1,1]
  FP <- cf_ts[2,1]
  
  TP <- cf_ts[2,2]
  FN <- cf_ts[1,2]
  
#   cat("cf_ts")
#   print(cf_ts)
  
  this_f1_score <- 2*TP / (2*TP + FP + FN)
  this_accuracy <- (TN+TP) / (TN + TP + FP + FN)
  this_TP_rate <- TP / (TP + FN)
  this_TN_rate <- TN / (TN + FP)
  
  res[["TS"]][["F1_score"]] <- this_f1_score
  res[["TS"]][["accuracy"]] <- this_accuracy
  res[["TS"]][["TP_rate"]] <- this_TP_rate
  res[["TS"]][["TN_rate"]] <- this_TN_rate
  res[["TS"]][["PR_AUC"]] <- prc_auc
  res[["TS"]][["ROC_AUC"]] <- roc_auc
  
  return(res)
}




# load data
all_data <- read.table(datasetFile, sep=",", header=TRUE)
N_data <- dim(all_data)[1]
names(all_data)
all_features <- names(all_data)[!names(all_data) %in% "death_event"]

methods=c("relief", "MMPC", "RF", "oneR", "rpart", "lsvm","xgboost")

# generate 10 splits 80%/20% stratified
attach(all_data)
idx_class0 <- which(death_event==0, arr.ind = TRUE)
idx_class1 <- which(death_event==1, arr.ind = TRUE)


trts_ratio <- 0.7
els_tr_cl0 <- round(trts_ratio*length(idx_class0))
els_tr_cl1 <- round(trts_ratio*length(idx_class1))



n_sets <- 10  # 50

if(n_sets != 100) cat("reset n_sets must be set to 100\n\n\n")


splits <- list()
cat("ranker() loop  ", sep="")

for(i in 1:n_sets)
{
  # cat("i=", i, ") out of ", n_sets, " n_sets\n", sep="")
  cat("\n (i=", i, ") ", sep="")
  
  splits[[i]] <- list()
  this_idx_class0 <- sample(idx_class0)
  this_idx_class1 <- sample(idx_class1)
  splits[[i]][["TR"]] <- c(this_idx_class0[1:els_tr_cl0], this_idx_class1[1:els_tr_cl1])
  splits[[i]][["TS"]] <- c(this_idx_class0[(els_tr_cl0+1):length(idx_class0)], this_idx_class1[(els_tr_cl1+1):length(idx_class1)])
  splits[[i]][["ranked_list"]] <- list()
  for(nn in methods)  
  { 
        cat(nn, " ", sep="")
        splits[[i]][["ranked_list"]][[nn]] <- ranker(data=all_data, train_indices=splits[[i]][["TR"]], method=nn) 
   }
}

M <- as.data.frame(matrix(NA,n_sets*length(methods),length(all_features)+2))
colnames(M) <- c("set","method",all_features)
j <- 0

cat("\nmatch() loop ")
for(i in 1:n_sets)
{
  cat("\n(i=", i, ") ", sep="")

  for(nn in methods)
  { 
  
    cat(nn, " ", sep="")
  
    j <- j+1
    M[j,"set"] <- i
    M[j,"method"] <- nn
    M[j,3:(length(all_features)+2)] <- match(all_features,splits[[i]][["ranked_list"]][[nn]])
  }
}

borda <- rep(NA,length(all_features))
names(borda) <- all_features
for(feat in all_features) borda[feat]=mean(M[,feat])
borda <- sort(borda,decreasing = FALSE)

# So pick up the two best features: serum_creatinine and ejection_fraction
n_top_feats <- 2
top_feats <- names(borda)[1:n_top_feats]

cat("\n\nHere are the top ", n_top_feats, " features:\n", sep="")
print(top_feats)

# Now I build a few models

cat("\ntrain and test loop ", sep="")
all_methods <- c("rf","svm","xgb")
for(i in 1:n_sets){

     cat("\n(i=", i, ") ", sep="")
  
    splits[[i]][["top_feats"]] <- list()
    splits[[i]][["top_feats"]][["feats"]] <- top_feats

    this_train <- all_data[splits[[i]][["TR"]], c(top_feats, "death_event"), drop=FALSE]
    this_test <- all_data[splits[[i]][["TS"]], c(top_feats, "death_event"), drop=FALSE]

    for(m in all_methods)
    {
    cat(m, " ", sep="")
    
        outputClassif <- classif(method=m, the_train=this_train, the_test=this_test, feats=top_feats)
        splits[[i]][["top_feats"]][[m]] <- outputClassif
    }
}

cat("\n\n~ ~ ~ results on ", n_sets, " executions ~ ~ ~ \n", sep="")
results_by_method(splits, "rf")
results_by_method(splits, "svm")
results_by_method(splits, "xgb")
cat("~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ \n")
