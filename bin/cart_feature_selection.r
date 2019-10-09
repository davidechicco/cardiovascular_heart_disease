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

list.of.packages <- c("easypackages", "PRROC", "e1071", "clusterSim","rpart", "caret", "dplyr", "pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")


patients_data <- read.csv(fileNameData, header = TRUE, sep =",");
cat("Read data from file ", fileNameData, "\n", sep="")

# rename target
names(patients_data)[names(patients_data) == targetName] <- "target"

# put the target on the last right position
patients_data <- patients_data%>%dplyr::select(-target, target)

target_index <- dim(patients_data)[2]

allExecutionsFinalRanking <- data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)

execution_number <- 100
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat("[Execution number ", exe_i," of the CART decision tree]\n")

    # shuffle the rows
    patients_data <- patients_data[sample(nrow(patients_data)),] 

    # select formula based on feature
    allFeaturesFormula <- as.formula(paste(colnames(patients_data)[target_index], '.', sep=' ~ ' ))
    selectedFormula <- allFeaturesFormula

    cart_model <- rpart(selectedFormula, method="class", data=patients_data);
    ptree <- prune(cart_model, cp=cart_model$cptable[which.min(cart_model$cptable[,"xerror"]),"CP"])


    importanceFrame <- (varImp(ptree))
    importanceFrame$feature <- rownames(importanceFrame)
    importanceFrameSorted <- importanceFrame[order(-importanceFrame$"Overall"), ]

    rownames(importanceFrameSorted) <- removeUnderscoreAndDot(rownames(importanceFrameSorted))
    importanceFrameSorted$feature <- removeUnderscoreAndDot(importanceFrameSorted$feature)


    if (exe_i == 1) {
        allExecutionsFinalRanking <- importanceFrameSorted[c("Overall")]
    } else {
        allExecutionsFinalRanking$"Overall" <- allExecutionsFinalRanking$"Overall" + importanceFrameSorted$"Overall"
    } 
}

cat("\n\n\n == final ranking after ", execution_number, " executions\n", sep="")

allExecutionsFinalRanking$"finalOverall" <- allExecutionsFinalRanking$"Overall" / execution_number
allExecutionsFinalRanking <- allExecutionsFinalRanking[order(-allExecutionsFinalRanking$"finalOverall"), ]
allExecutionsFinalRanking$"finalPos" <- c(1:dim(allExecutionsFinalRanking)[1])

print(allExecutionsFinalRanking[, c("finalOverall", "finalPos")])
