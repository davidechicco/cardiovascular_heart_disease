setwd(".")

list.of.packages <- c("easypackages", "nnet", "pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

USELESS_VALUE <- -2

# retrieveMostSimilarRow
retrieveMostSimilarRow <- function(rowDataFrame, thisDataFrame) 
{
     nRows <- nrow(thisDataFrame)
     simiVector <- vector()

        for(j in 1:nRows) { 
    
            if(rownames(rowDataFrame) != rownames(thisDataFrame[j,])  && (sum(is.na(thisDataFrame[j,])) == 0))
            {
                 thisSimi <- similarityWithNas(rowDataFrame, thisDataFrame[j,]) 
                 simiVector[j] <- thisSimi
                 
                 # we stop if we find a cosine distance similar enough
                 if (thisSimi >= 0.99 ) 
                 {
                    break;
                 }
                 
            } else { 
            
              #  cat("(rownames:", rownames(rowDataFrame), " == ", rownames(thisDataFrame[j,]), ")\t", sep="")
              # cat("or thisDataFrame[", j,"] contains NAs)\n", sep="")
                simiVector[j] <- USELESS_VALUE   
            }
       
       # cat("simiVector[", j ,"] = ", simiVector[j], "\n", sep="")
        }
   
    # max similarity value among the rows without NAs
    maxSimiValueNonNAs <- max(simiVector)
    maxSimiValueNonNAsIndex <- which.is.max(simiVector)
    cat("maxSimiValueNonNAs = ", maxSimiValueNonNAs, "\n", sep="")
    cat("maxSimiValueNonNAsIndex = ", maxSimiValueNonNAsIndex, "\n", sep="")
    
    resultList <- list(simiVector=simiVector, maxSimiValueNonNAs=maxSimiValueNonNAs, maxSimiValueNonNAsIndex=maxSimiValueNonNAsIndex)

    return(resultList)
}

# replaceEachNAsRowWithMostSimilarRow
replaceEachNAsRowWithMostSimilarRow <- function(thisDataFrame)
{
     original_thisDataFrame <- thisDataFrame
     outputDataFrame <- thisDataFrame 
     nRows <- nrow(thisDataFrame)
     replacement_count <- 0
     for(j in 1:nRows) { 
     
        if (sum(is.na(thisDataFrame[j,])) > 0) { 
            mostSimiRowOutput <- retrieveMostSimilarRow(thisDataFrame[j,], thisDataFrame)
            thisIndex <- mostSimiRowOutput$maxSimiValueNonNAsIndex
            mostSimiRow <- thisDataFrame[thisIndex,]
            
            outputDataFrame[j,] <- replaceNAsRowWithValuesRow(thisDataFrame[j,], mostSimiRow)
            cat("\nwe replaced the NAs of row j=", j, " with the values of row ", thisIndex, "\n", sep="")
            replacement_count <- replacement_count + 1
            
            percCompletion <- round((j*100)/nRows, 2)
            cat("[", percCompletion, "% completed]\n", sep="")
        }     
     }
     
      cat("\n == data imputation finished ==\n")
      cat("The replaceEachNAsRowWithMostSimilarRow() function replaced NAs values in ", replacement_count, " rows\n", sep="")
      return(outputDataFrame)
}

# replaceNAsWithValues
replaceNAsRowWithValuesRow <- function(rowDataFrameWithNAs, rowDataFrameWithValues)
{
    # cat("[INPUT] rowDataFrameWithNAs: ")
    # print(rowDataFrameWithNAs)

    if (sum(is.na(rowDataFrameWithNAs)) > 0) { 
        NA_columns <- which(is.na(rowDataFrameWithNAs)) 
        rowDataFrameWithNAs[,c(NA_columns)] <- rowDataFrameWithValues[,c(NA_columns)]
    }

    # cat("[OUTPUT]  rowDataFrameWithNAs: ")
    # print(rowDataFrameWithNAs)
    
    return(rowDataFrameWithNAs)
}

# cosineSimilarity
# https://stats.stackexchange.com/a/31573
cosineSimilarity<- function(left_dataframe, right_dataframe) 
{
    return(sum(left_dataframe*right_dataframe)/sqrt(sum(left_dataframe^2)*sum(right_dataframe^2)) )
}

# similarityWithNas
# https://stats.stackexchange.com/a/31573
similarityWithNas <- function(dataFrameA, dataFrameB) 
{
    
    if (sum(is.na(dataFrameA)) > 0) { 
        NA_columnsA <- which(is.na(dataFrameA)) 
        dataFrameA[,c(NA_columnsA)] <- NULL # if we remove some columns from A, we remove the 
        dataFrameB[,c(NA_columnsA)] <- NULL # same columns from B
    }
    if (sum(is.na(dataFrameB)) > 0) { 
        NA_columnsB <- which(is.na(dataFrameB))  
        dataFrameB[,c(NA_columnsB)] <- NULL # if we remove some columns from B, we remove the 
        dataFrameA[,c(NA_columnsB)] <- NULL # same columns from A    
    }
    
    return(cosineSimilarity(dataFrameA, dataFrameB))

}   

list.of.packages <- c("easypackages", "clusterSim", "PRROC", "e1071", "rpart",  "dplyr", "pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

datasetFileName <-  "/home/davide/projects/sepsis_neonatal_ICU/data/journal.pone.0212665.s007.csv"
cat("datasetFileName: ", datasetFileName, "\n", sep="")
patients_data <- read.csv(datasetFileName, header = TRUE, sep =",");
cat("Read data from file ", datasetFileName, "\n", sep="")

num_to_return <- 1
upper_num_limit <- 10000000
exe_num <- sample(1:upper_num_limit, num_to_return)

patients_data_new_imputed <- replaceEachNAsRowWithMostSimilarRow(patients_data)

datasetFileNameWithoutExtension <- strsplit(datasetFileName, ".csv")[[1]]
dataImputationFile <- paste0(datasetFileNameWithoutExtension, "_cosineDist_data_imputed_", exe_num, ".csv")

cat("The imputed dataset will be saved in the ", dataImputationFile, " file\n", sep="")
write.csv(patients_data_new_imputed, file=dataImputationFile)


