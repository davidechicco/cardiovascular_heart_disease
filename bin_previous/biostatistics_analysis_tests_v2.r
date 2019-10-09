setwd(".")

# Software originally developed by Giuseppe Jurman <jurman@fbk.eu> on 1st March 2019
# Edited by Davide Chicco <davide.chicco@gmail.com> on 4th March 2019


# Data load

list.of.packages <- c("readr", "easypackages")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("easypackages")
libraries(list.of.packages)

path_to_data <- "../data/"
filename <- paste(path_to_data,"dataset_edited_without_time.csv",sep="")
dataset_edited_without_time <- as.data.frame(read_csv(filename,))
# print(head(dataset_edited_without_time))


ROUND_NUM <- 6

dataset_edited_without_time$death_event <-
as.factor(dataset_edited_without_time$death_event)

DEATH_LABEL <- 1
SURVIVAL_LABEL <- 0
dead_patients_data <-  dataset_edited_without_time[dataset_edited_without_time$death_event==DEATH_LABEL, ]
survived_patients_data <-  dataset_edited_without_time[dataset_edited_without_time$death_event==SURVIVAL_LABEL, ]

# All the outputs of the tests are stored on the alltests lists, that we print at the end of the discussion.

TARGET_LABEL <- "death_event"

mycols <- names(dataset_edited_without_time)
mycols <- mycols[mycols!=TARGET_LABEL]
alltests <- list()
alltests[["Wilcoxon_rank"]] <- alltests[["Kruskal"]] <- alltests[["Chi"]] <- alltests[["Shapiro"]] <- list()
for(thecol in mycols){
    alltests[["Wilcoxon_rank"]][[thecol]] <-     wilcox.test(as.formula(paste(thecol,TARGET_LABEL,sep="~")),     data=dataset_edited_without_time)
    alltests[["Kruskal"]][[thecol]] <-     kruskal.test(as.formula(paste(thecol,TARGET_LABEL,sep="~")),     data=dataset_edited_without_time)
    alltests[["Chi"]][[thecol]] <-     chisq.test(x=as.factor(dataset_edited_without_time[,thecol]),     y=dataset_edited_without_time$death_event,    simulate.p.value = TRUE)   
    alltests[["Shapiro"]][[thecol]] <-     shapiro.test(dataset_edited_without_time[,thecol])
}
alltests[["Shapiro"]][[TARGET_LABEL]]<- shapiro.test(as.numeric(dataset_edited_without_time$death_event))

# As a rule of thumb, the validity of the tests is assessed by looking at the resulting p-values.
# We start with the Shapiro test of normality,
#cat("\n\t\t == Shapiro ==\n")
vectorShapiro <- c()
for(thecol in c(mycols,TARGET_LABEL)) { 
    vectorShapiro[thecol] <- alltests[["Shapiro"]][[thecol]]$p.value 
    
   # cat(names((vectorShapiro)[thecol]), "\t \t \t", ((vectorShapiro)[[thecol]]), "\n")
}
# print(vectorShapiro)

# cat("\n\n\t\t == Wilcoxon_rank ==\n")
vectorWilcoxon <- c()
for(thecol in mycols) { 
    vectorWilcoxon[thecol] <- round(alltests[["Wilcoxon_rank"]][[thecol]]$p.value, ROUND_NUM) 
    
  #  cat(names((vectorWilcoxon)[thecol]), "\t \t \t", ((vectorWilcoxon)[[thecol]]), "\n")
}
# print(vectorWilcoxon)

# cat("\n\t\t == Kruskal ==\n")
vectorKruskal <- c()
for(thecol in mycols) { 
    vectorKruskal[thecol] <- round(alltests[["Kruskal"]][[thecol]]$p.value, ROUND_NUM) 
    
    # cat(names((vectorKruskal)[thecol]), "\t \t \t", ((vectorKruskal)[[thecol]]), "\n")
}
# print(vectorKruskal)

# cat("\n\t\t == Chi ==\n")
vectorChi <- c()
for(thecol in mycols) { 
    vectorChi[thecol] <- round(alltests[["Chi"]][[thecol]]$p.value, ROUND_NUM)
    
    # cat(names((vectorChi)[thecol]), "\t \t \t", ((vectorChi)[[thecol]]), "\n")
}
# print(vectorChi)


sortedVectorShapiro <- sort(vectorShapiro)
sortedVectorWilcoxon <- sort(vectorWilcoxon)
sortedVectorKruskal <- sort(vectorKruskal)
sortedVectorChi <- sort(vectorChi)

LATEX_SEP <- "&"
LATEX_END_OF_ROW <- "\\\\"

EMPTY_SEP <- ""
EMPTY_END_OF_ROW <- ""

SEP <- EMPTY_SEP
END_OF_ROW <- EMPTY_END_OF_ROW

index <- 1
cat("\n\t\t == Shapiro ==\n")
for(thecol in names(sortedVectorShapiro)) {    
    
    cat(index, " ", SEP," \t",  names((sortedVectorShapiro)[thecol]), " ", SEP," \t ", ((sortedVectorShapiro)[[thecol]]), " ", END_OF_ROW," \n", sep="")
    index <- index + 1
}

index <- 1
cat("\n\t\t == Wilcoxon_rank ==\n")
for(thecol in names(sortedVectorWilcoxon)) { 
    
    cat(index, " ", SEP," \t",  names((sortedVectorWilcoxon)[thecol]), " ", SEP," \t ", ((sortedVectorWilcoxon)[[thecol]]), " ", END_OF_ROW," \n", sep="")
    index <- index + 1
}

index <- 1
cat("\n\t\t == Kruskal ==\n")
for(thecol in names(sortedVectorKruskal)) { 
    
    cat(index, " ", SEP," \t",  names((sortedVectorKruskal)[thecol]), " ", SEP," \t ", ((sortedVectorKruskal)[[thecol]]), " ", END_OF_ROW," \n", sep="")
    index <- index + 1
}

index <- 1
cat("\n\t\t == Chi Squared ==\n")
for(thecol in names(sortedVectorChi)) { 
    
     cat(index, " ", SEP," \t",  names((sortedVectorChi)[thecol]), " ", SEP," \t ", ((sortedVectorChi)[[thecol]]), " ", END_OF_ROW," \n", sep="")
     index <- index + 1
}



# print(alltests)
