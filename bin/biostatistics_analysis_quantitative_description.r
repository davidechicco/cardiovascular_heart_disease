setwd(".")
options(stringsAsFactors = FALSE)

source("./utils.r")

# tableAnalysis
tableAnalysis <- function(dataframe_patients) 
{
    cat("== binary features ==\n")
    cat("feature  ", SEP, "  \t value  ", SEP, "  #  ", SEP, "  %  ", END_OF_ROW, "  \n", sep="")
    for(i in 1:(ncol(dataframe_patients))) 
    { 
            if (dim(table(dataframe_patients[,i]))==TWO_DIM) 
            {                 
                thisFeatureName <- removeUnderscoreAndDot(colnames(dataframe_patients)[i])
            
                # 2-dimensional            
                firstComponentNum <- (table(dataframe_patients[,i]))[[FIRST_COMP]]
                firstComponentName <- rownames(table(dataframe_patients[,i]))[FIRST_COMP]
                firstComponentPerc <- firstComponentNum*100/numPatients
                
                secondComponentNum <- (table(dataframe_patients[,i]))[[SECOND_COMP]]
                secondComponentName <- rownames(table(dataframe_patients[,i]))[SECOND_COMP]
                secondComponentPerc <- secondComponentNum*100/numPatients
                
                # cat(colnames(dataframe_patients)[i], " ", sep="")             
                cat(thisFeatureName, " ",  sep="") 
                cat(SEP, "\t", firstComponentName, " ", SEP, " \t ", firstComponentNum," ", SEP," \t ", dec_two(firstComponentPerc), " ", END_OF_ROW, " \n", sep="")
                cat(thisFeatureName, " ", sep="") 
                cat(SEP, "\t", secondComponentName, " ", SEP, " \t ", secondComponentNum," ", SEP," \t ", dec_two(secondComponentPerc), " ", END_OF_ROW, " \n", sep="")
                }
    }

    cat("\n\n== numeric features ==\n")
    cat("feature ", SEP, "\t median ", SEP, " \t mean ", SEP, "\t range ", END_OF_ROW, "\n", sep="")
    for(i in 1:(ncol(dataframe_patients))) { 
    
        thisFeatureName <- removeUnderscoreAndDot(colnames(dataframe_patients)[i])

            if (dim(table(dataframe_patients[,i]))!=TWO_DIM) 
            {  
                # non  2-dimensional     
                thisMedian <- summary(dataframe_patients[,i])[[MEDIAN_INDEX]]
                thisMean <- summary(dataframe_patients[,i])[[MEAN_INDEX]]
                thisMin <- summary(dataframe_patients[,i])[[MIN_INDEX]]
                thisMax <- summary(dataframe_patients[,i])[[MAX_INDEX]]
                cat(thisFeatureName, " ",  SEP, "\t", dec_two(thisMedian), " ",  SEP, "\t", dec_two(thisMean), " ",  SEP, "\t[", dec_two(thisMin), ", ", dec_two(thisMax), "] ", END_OF_ROW, " \n", sep="")
            }
    }
}

# fileName <- "/home/davide/projects/cardiovascular_heart_disease/data/dataset_edited_without_time.csv"
# targetName <- "death_event"

## part to edit manually ##

fileName <-   "/home/davide/projects/breast_cancer_Coimbra/data/dataR2.csv"
targetName <- "Classification"
ABNORMAL_TARGET_CONDITION <- 1
NORMAL_TARGET_CONDITION <- 0

CORRELATIONS_PLOTS <- FALSE
INDEP_YES_NO_QUANT_DESCRIPTION <- FALSE

## ## ## ## ## ## ## ## ##

# indices for the summary
EXP_ARG_NUM <- 2
MEDIAN_INDEX <- 3
MEAN_INDEX <- 4
MIN_INDEX <- 1
MAX_INDEX <- 6

# args = commandArgs(trailingOnly=TRUE)
# if (length(args)<EXP_ARG_NUM) {
#   stop("At least two argument must be supplied (input files)", call.=FALSE)
# } else {
#   # default output file
#   fileName <- args[1]
#   targetName <- args[2]
# }

num_to_return <- 1
upper_num_limit <- 10000000
exe_num <- sample(1:upper_num_limit, num_to_return)

LATEX_MODE <- FALSE

LATEX_SEP <- "&"
LATEX_END_OF_ROW <- "\\\\"

EMPTY_SEP <- ""
EMPTY_END_OF_ROW <- ""

SEP <- EMPTY_SEP
END_OF_ROW <- EMPTY_END_OF_ROW

if (LATEX_MODE == TRUE ) {
    SEP <- LATEX_SEP
    END_OF_ROW <- LATEX_END_OF_ROW
}

cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")


patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName: ", fileName, "\n", sep="")

TWO_DIM <- 2
FIRST_COMP <- 1
SECOND_COMP <- 2

patients_data_original <- patients_data
numPatients <- nrow(patients_data)

# sort the columns alphabetically
patients_data <- patients_data[ , order(names(patients_data))]

cat("\n// all patients //\n", sep="")
# call to the main function
tableAnalysis(patients_data)

#
# let's split here patients from healthy controls
#
if(INDEP_YES_NO_QUANT_DESCRIPTION == TRUE)
{

    targetIndex <- which(colnames(patients_data)==targetName)

    cat("\n\nthe target feature is ", targetName, ", which is the column #", targetIndex, "\n", sep="")
    
    targetYesValue <- ABNORMAL_TARGET_CONDITION
    targetNoValue <- NORMAL_TARGET_CONDITION

    # patients YES

    patients_data_target_yes <- (patients_data[patients_data[, targetIndex]==targetYesValue,])
    patients_data_target_yes <- patients_data_target_yes[ , order(names(patients_data_target_yes))]

    numPatientsYes <- nrow(patients_data_target_yes)

    cat("\n\t\t\t>>> target YES patients \n", sep="")
    tableAnalysis(patients_data_target_yes)

    # patients NO

    patients_data_target_no <- (patients_data[patients_data[, targetIndex]==targetNoValue,])
    patients_data_target_no<- patients_data_target_no[ , order(names(patients_data_target_no))]
    numPatientsNo <- nrow(patients_data_target_no)

    cat("\n\t\t\t>>> target NO patients \n", sep="")
    tableAnalysis(patients_data_target_no)
}
