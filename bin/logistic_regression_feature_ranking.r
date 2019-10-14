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


fileNameData<- "../data/S1Data_EDITED2.csv"
targetName <- "DEATH_EVENT"
patients_data <- read.csv(fileNameData, header = TRUE, sep =",")
cat("Read data from file ", fileNameData, "\n", sep="")


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

covariates <- c("Gender", "Smoking", "Diabetes", "BP",    "Anaemia", "Age", "Ejection.Fraction", "Sodium", "Creatinine",   "Pletelets", "CPK")

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


        glm_model <- glm(DEATH_EVENT ~ sex + smoking + diabetes + high_blood_pressure + anaemia + age + ejection_fraction + serum_sodium + serum_creatinine + platelets + creatinine_phosphokinase + factor(TIME_MONTH), data = patients_data, family = "binomial")

        # Does varImp select the top features or the less important?

        featureImportance <-  varImp(glm_model, scale = FALSE)
        featureImportance$clinical_feature <- row.names(featureImportance)
        clinical_feature_ranking <- featureImportance[order(featureImportance$"clinical_feature"),]
        
        
        
        
        # https://stats.stackexchange.com/a/213020

        cat("== temporary ranking == \n")
        cat("top clinical_features: \n")
        cat("- - - - - - - - - - - - - - - - - - - - - \n")
        print((clinical_feature_ranking))
        cat("- - - - - - - - - - - - - - - - - - - - - \n")

        if (exe_i == 1) {
                allExecutionsFinalRanking <- clinical_feature_ranking
        } else {        
        
                allExecutionsFinalRanking$"Overall" <- allExecutionsFinalRanking$"Overall" + clinical_feature_ranking$"Overall"
        }         
        

} # for

cat("\n\n\n == final ranking after ", execution_number, " executions\n", sep="")

allExecutionsFinalRanking$"finalOverall" <- allExecutionsFinalRanking$"Overall" / execution_number
allExecutionsFinalRanking <- allExecutionsFinalRanking[order(-allExecutionsFinalRanking$"finalOverall"), ]
allExecutionsFinalRanking$"finalPos" <- c(1:dim(allExecutionsFinalRanking)[1])

print((allExecutionsFinalRanking[, c("clinical_feature", "finalOverall", "finalPos")]))

# cat("The final ranking contains ", nrow(allExecutionsFinalRanking), " clinical_features. The other ones were removed because of singularities\n", sep="")


# print file
# clinical_featuresRankingFile <- paste0("../results/clinical_features_log_reg_ranking_rand", exe_num, ".csv")

# cat("\n\nThe clinical_features ranking will be saved in the \n ", clinical_featuresRankingFile, " file\n", sep="")
# write.csv(allExecutionsFinalRanking[, c("finalPos", "clinical_feature", "finalOverall")], file=clinical_featuresRankingFile, row.names=FALSE)
