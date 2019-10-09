setwd(".")
options(stringsAsFactors = FALSE)

fileNameData <-  "/home/davide/projects/breast_cancer_Coimbra/data/dataR2_EDITED.csv"
targetName <- "DIAGNOSIS"

# args = commandArgs(trailingOnly=TRUE)
# if (length(args)<EXP_ARG_NUM) {
#   stop("At least two argument must be supplied (input files)", call.=FALSE)
# } else {
#   # default output file
#   fileName <- args[1]
#   targetName <- args[2]
# }

list.of.packages <- c("easypackages", "e1071","PRROC", "caret", "fastAdaboost", "dplyr", "pastecs", "kernlab", "FSelector")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")

thisMethod = "One rule feature selection"

patients_data <- read.csv(file=fileNameData,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileNameData = ", fileNameData, "\n", sep="")

# rename target
names(patients_data)[names(patients_data) == targetName] <- "target"

# put the target on the last right position
patients_data <- patients_data%>%dplyr::select(-target, target)


patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows

totalElements <- dim(patients_data)[1]

subsets_size <- totalElements

if (subsets_size != totalElements) {
    cat("!!! ATTENTION: We are running the method on a subset of the original dataset, \n", sep="")
    cat("!!! containing only ", subsets_size, " elements \n", sep="")
    cat("!!! instead of ", totalElements, " elements \n", sep="")
}

patients_data <- patients_data[1:subsets_size, ]

dataset_dim_retriever(patients_data)
target_index <- dim(patients_data)[2]
# imbalance_retriever(patients_data[,target_index])

allExecutionsFinalRanking <- data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)
                 
execution_number <-  100
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat("\n\n[Execution number ", exe_i," of the ",thisMethod,"]\n")

    patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows again
 
#     # svm for feature selection
#     svmProfile <- rfe(patients_data[,1:(target_index-1)], patients_data$target,
#                     sizes = num_feature,
#                     rfeControl = rfeControl(functions = caretFuncs, number = num_folds),
#                     method = thisMethod)
     
    tmp <- oneR((target)~., patients_data)
                    
    # featureImportance <- rownames(tmp[order(tmp$attr_importance,decreasing = TRUE),,drop=FALSE])
    featureImportance <- tmp
    featureImportance$feature <- rownames(featureImportance)

    rownames(featureImportance) <- removeUnderscoreAndDot(rownames(featureImportance))
    featureImportance$feature <- removeUnderscoreAndDot(featureImportance$feature)

    cat("== temporary ranking == \n")
    print(head(featureImportance[c("attr_importance")]))
    
    
    if (exe_i == 1) {
        allExecutionsFinalRanking <- featureImportance[c("attr_importance")]
    } else {
        allExecutionsFinalRanking$"attr_importance" <- allExecutionsFinalRanking$"attr_importance" + featureImportance$"attr_importance"
    } 
}

cat("\n\n\n == final ranking after ", execution_number, " executions\n", sep="")

allExecutionsFinalRanking$"finalOverall" <- allExecutionsFinalRanking$"attr_importance" / execution_number
allExecutionsFinalRanking <- allExecutionsFinalRanking[order(-allExecutionsFinalRanking$"finalOverall"), ]
allExecutionsFinalRanking$"finalPos" <- c(1:dim(allExecutionsFinalRanking)[1])

print(allExecutionsFinalRanking[, c("finalOverall", "finalPos")])
