setwd(".")
options(stringsAsFactors = FALSE)

source("./utils.r")

fileName <- "/home/davide/projects/cardiovascular_heart_disease/data/dataset_edited_without_time.csv"
targetName <- "death_event"

# fileName <-  "/home/davide/projects/breast_cancer_Coimbra/data/dataR2.csv"
# targetName <- "Classification"

ALL_PATIENTS_CORRELATION <- TRUE
CORRELATIONS_PLOTS <- FALSE

EXP_ARG_NUM <- 2
MEDIAN_INDEX <- 3
MEAN_INDEX <- 4

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

patients_data_original <- patients_data
numPatients <- nrow(patients_data)

# sort the columns alphabetically
patients_data <- patients_data[ , order(names(patients_data))]

targetIndex <- which(colnames(patients_data)==targetName)

# All patients: p-value, t-value, and PCC

vectorPCC <- c()
vector_p_values <- c()
vectorStudentT <- c()

allTestsDataframe <- data.frame(Characters=character(), Doubles=double(),Doubles=double(),Doubles=double(),  stringsAsFactors=FALSE)
colnames(allTestsDataframe) <- c("feature", "abs_PCC", "p", "abs_t")
# rownames(allTestsDataframe) <- rownames(patients_data)


if (ALL_PATIENTS_CORRELATION == TRUE) {

    cat("\n// all patients correlations //\n\n", sep="")
    cat("[target==", targetName, "] ", SEP,"\t abs(t) ", SEP,"\ \t p-value ", SEP," \t abs(PCC) ", SEP," \t conf_int ", SEP,"\n\n", sep="")
    for(i in 1:(ncol(patients_data))) { 

        # cat("\n\ncorrelation between (target) ", colnames(patients_data)[targetIndex], " and ",  colnames(patients_data)[i], ": \n", sep="") 
        
        # cat("\n patients_data)[", i, "] ", colnames(patients_data)[i], " [versus]  ", colnames(patients_data)[targetIndex], "(target) : correlation\n", sep="")
        
        thisTtest <- t.test(patients_data[,i], patients_data[,targetIndex])
        tValue <- abs((thisTtest$statistic)[[1]])
        pValue <- thisTtest$p.value
        thisAbsPCC <- abs(cor(patients_data[,i], patients_data[,targetIndex], method=c("pearson")))
        conf_int_start <- dec_three((thisTtest$conf.int)[1])
        conf_int_end <- dec_three((thisTtest$conf.int)[2])
        
        # cat(colnames(patients_data)[i], "\t\t abs(t) \t p-value \t PCC \t conf_int\n", sep="")
        cat(colnames(patients_data)[i], " ", SEP,"\t", dec_three(tValue), " ", SEP,"\t", dec_three(pValue), " ", SEP,"\t", dec_three(thisAbsPCC), " ", SEP,"\t", conf_int_start, " ", SEP,"\t", conf_int_end, " ", END_OF_ROW,"\n", sep="")
        
        # cat("t = ", dec_three(tValue), "\n", sep="")
        # cat("p-value = ", dec_three(pValue), "\n", sep="")
        # cat("PCC(", colnames(patients_data)[targetIndex], ", ", colnames(patients_data)[i], ") = ",  dec_three(thisAbsPCC), "\n", sep="")
      
         allTestsDataframe[i,] <- data.frame(feature = colnames(patients_data)[i], abs_PCC = thisAbsPCC, p=pValue, abs_t=tValue)         
    }
    
    rownames(allTestsDataframe) <- allTestsDataframe$"feature"
    # allTestsDataframe$"feature" <- NULL
    
    # let's eliminate the target index from the rank
    targetRow <-  which(allTestsDataframe==targetName)
    allTestsDataframe <- allTestsDataframe[-c( which(allTestsDataframe==targetName)), ]    
    
    
    absPCCresultDataframe <- (allTestsDataframe[order(-allTestsDataframe$abs_PCC), c("abs_PCC"), drop=FALSE])
    absPCCresultDataframe$pos <- c(1:dim(absPCCresultDataframe)[1])
    absPCCresultDataframe$feature <- rownames(absPCCresultDataframe)
        
    p_values_resultDataframe <- (allTestsDataframe[order(allTestsDataframe$p), c("p"), drop=FALSE])
    p_values_resultDataframe$pos <- c(1:dim(p_values_resultDataframe)[1])
    p_values_resultDataframe$feature <- rownames(p_values_resultDataframe)
    
    abs_t_Dataframe <- (allTestsDataframe[order(-allTestsDataframe$abs_t), c("abs_t"), drop=FALSE])
    abs_t_Dataframe$pos <- c(1:dim(abs_t_Dataframe)[1])
    abs_t_Dataframe$feature <- rownames(abs_t_Dataframe)
    
    if(CORRELATIONS_PLOTS == TRUE) {    
    
        x_upper_lim <- 1
        barPlotOfRanking(absPCCresultDataframe, absPCCresultDataframe$abs_PCC, absPCCresultDataframe$feature, absPCCresultDataframe$pos, exe_num, "feature", "abs (PCC)", x_upper_lim)

        x_upper_lim <- 300
        barPlotOfRanking(p_values_resultDataframe, abs(log(p_values_resultDataframe$p)), p_values_resultDataframe$feature, (p_values_resultDataframe$pos), exe_num, "feature", "abs(log(p-value))", x_upper_lim)

        x_upper_lim <- 100
        barPlotOfRanking(abs_t_Dataframe, abs_t_Dataframe$abs_t, abs_t_Dataframe$feature, (abs_t_Dataframe$pos), exe_num, "feature", "Student's abs(t)", x_upper_lim)    
    }

}


# (t(summary(patients_data)))[,c(1,3,4,6)]
