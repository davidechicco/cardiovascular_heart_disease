setwd(".")
options(stringsAsFactors = FALSE)

# Survival analysis and the stratified sample:
# https://towardsdatascience.com/survival-analysis-and-the-stratified-sample-2c2582aa9805

list.of.packages <- c("easypackages", "survival",  "dplyr", "survminer", "stats", "PRROC", "caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")


fileNameData<- "../data/S1Data_EDITED.csv"
targetName <- "DEATH_EVENT"
patients_data <- read.csv(fileNameData, header = TRUE, sep =",")
cat("Read data from file ", fileNameData, "\n", sep="")


patients_data$"ptid" <- NULL
patients_data$"transplant_date" <- NULL
patients_data$"blood_collection_date" <- NULL

# let's put the target label last on the right 
patients_data <- patients_data%>%dplyr::select(-targetName,targetName)

target_index <- dim(patients_data)[2]

# shuffle the rows
patients_data <- patients_data[sample(nrow(patients_data)),] 

num_to_return <- 1
upper_num_limit <- 10000000
exe_num <- sample(1:upper_num_limit, num_to_return)


cat("nrow(patients_data) = ", nrow(patients_data), "\n", sep="")
cat("ncol(patients_data) = ", ncol(patients_data), "\n", sep="")

covariates <- c("Gender",    "Smoking",    "Diabetes",   "BP",    "Anaemia",    "Age",        "Ejection.Fraction",  "Sodium",    "Creatinine" ,   "Pletelets",  "CPK")

allExecutionsFinalRanking <- data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)

thisMethod <- "logistic regression"

execution_number <-  100
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

        cat("\n\n[Execution number ", exe_i," of the ",thisMethod,"]\n", sep="")

        # shuffle the rows
        patients_data <- patients_data[sample(nrow(patients_data)),] 


        glm_model <- glm(DEATH_EVENT ~ Gender +   Smoking +   Diabetes +  BP +   Anaemia +   Age +       Ejection.Fraction + Sodium +   Creatinine  +  Pletelets + CPK  + factor(TIME_MONTH), data = patients_data, family = "binomial")

        # Does varImp select the top features or the less important?

        featureImportance <-  varImp(glm_model, scale = FALSE)
        featureImportance$autoantibody <- row.names(featureImportance)
        autoantibody_ranking <- featureImportance[order(featureImportance$"autoantibody"),]
        
        # R Glm Coefficients: not defined because of singularities
        # "You're probably getting that error because two or more of your independent variables are perfectly collinear"
        # https://stats.stackexchange.com/a/22644
        # https://stats.stackexchange.com/a/213020

        cat("== temporary ranking == \n")
        cat("top autoantibodies: \n")
        cat("- - - - - - - - - - - - - - - - - - - - - \n")
        print(head(autoantibody_ranking))
        cat("- - - - - - - - - - - - - - - - - - - - - \n")

        if (exe_i == 1) {
                allExecutionsFinalRanking <- autoantibody_ranking
        } else {        
        
                allExecutionsFinalRanking$"Overall" <- allExecutionsFinalRanking$"Overall" + autoantibody_ranking$"Overall"
        }         
        

} # for

cat("\n\n\n == final ranking after ", execution_number, " executions\n", sep="")

allExecutionsFinalRanking$"finalOverall" <- allExecutionsFinalRanking$"Overall" / execution_number
allExecutionsFinalRanking <- allExecutionsFinalRanking[order(-allExecutionsFinalRanking$"finalOverall"), ]
allExecutionsFinalRanking$"finalPos" <- c(1:dim(allExecutionsFinalRanking)[1])

print((allExecutionsFinalRanking[, c("autoantibody", "finalOverall", "finalPos")]))

cat("The final ranking contains ", nrow(allExecutionsFinalRanking), " autoantibodies. The other ones were removed because of singularities\n", sep="")


# print file
# autoantibodiesRankingFile <- paste0("../results/autoantibodies_log_reg_ranking_rand", exe_num, ".csv")

# cat("\n\nThe autoantibodies ranking will be saved in the \n ", autoantibodiesRankingFile, " file\n", sep="")
# write.csv(allExecutionsFinalRanking[, c("finalPos", "autoantibody", "finalOverall")], file=autoantibodiesRankingFile, row.names=FALSE)
