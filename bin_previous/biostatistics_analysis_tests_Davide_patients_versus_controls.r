setwd(".")


# Data load

list.of.packages <- c("readr", "easypackages", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("easypackages")
libraries(list.of.packages)

path_to_data <- "../data/"
filename <- paste(path_to_data,"dataset_edited_without_time.csv",sep="")
dataset_edited_without_time <- as.data.frame(read_csv(filename,))
# print(head(dataset_edited_without_time))

source("./utils.r")


# dataset_edited_without_time$death_event <- as.factor(dataset_edited_without_time$death_event)

DEATH_LABEL <- 1
SURVIVAL_LABEL <- 0
dead_patients_data <-  dataset_edited_without_time[dataset_edited_without_time$death_event==DEATH_LABEL, ]
survived_patients_data <-  dataset_edited_without_time[dataset_edited_without_time$death_event==SURVIVAL_LABEL, ]

SEPARATE_DEAD_AND_SURVIVED <- TRUE

# All the outputs of the tests are stored on the alltests lists, that we print at the end of the discussion.

TARGET_LABEL<-"death_event"

mycols <- names(dataset_edited_without_time)
mycols <- mycols[mycols!=TARGET_LABEL]
alltests <- list()
alltests_dead <- list()
alltests_survived <- list()
alltests[["Wilcoxon_rank"]] <- alltests[["Kruskal"]] <- alltests[["Chi"]] <- alltests[["Shapiro"]] <- list()
alltests_dead[["Wilcoxon_rank"]] <- alltests[["Kruskal"]] <- alltests[["Chi"]] <- alltests[["Shapiro"]] <- list()
alltests_survived[["Wilcoxon_rank"]] <- alltests[["Kruskal"]] <- alltests[["Chi"]] <- alltests[["Shapiro"]] <- list()

for(thecol in mycols){

    alltests[["Wilcoxon_rank"]][[thecol]] <- wilcox.test(as.formula(paste(thecol,TARGET_LABEL,sep="~")), data=dataset_edited_without_time)    
    alltests[["Kruskal"]][[thecol]] <- kruskal.test(as.formula(paste(thecol,TARGET_LABEL,sep="~")), data=dataset_edited_without_time)    
    alltests[["Chi"]][[thecol]] <- chisq.test(x=as.factor(dataset_edited_without_time[,thecol]),  y=dataset_edited_without_time$death_event,  simulate.p.value = TRUE)    
    alltests[["Shapiro"]][[thecol]] <-  shapiro.test(dataset_edited_without_time[,thecol])
    
    if (SEPARATE_DEAD_AND_SURVIVED == TRUE ) {
    
        NUM_INDEX <- 1
        target_dead <- as.numeric(select(dead_patients_data, TARGET_LABEL)[,NUM_INDEX])
        col_dead <- as.numeric(select(dead_patients_data, thecol)[,NUM_INDEX])
            
        alltests_dead[["Wilcoxon_rank"]][[thecol]] <- wilcox.test(target_dead,col_dead)    
        alltests_dead[["Kruskal"]][[thecol]] <- kruskal.test(target_dead,col_dead)    
       # alltests_dead[["Chi"]][[thecol]] <- chisq.test(x=as.factor(target_dead), y=col_dead)    
        alltests_dead[["Shapiro"]][[thecol]] <-  shapiro.test(col_dead)
        
        target_survived <- as.numeric(select(survived_patients_data, TARGET_LABEL)[,NUM_INDEX])
        col_survived <- as.numeric(select(survived_patients_data, thecol)[,NUM_INDEX])
        
        alltests_survived[["Wilcoxon_rank"]][[thecol]] <- wilcox.test(target_survived, col_survived)    
        alltests_survived[["Kruskal"]][[thecol]] <- kruskal.test(target_survived, col_survived)    
        # alltests_survived[["Chi"]][[thecol]] <- chisq.test(x=as.factor(target_survived), y=col_survived)    
        alltests_survived[["Shapiro"]][[thecol]] <-  shapiro.test(col_survived)    
    
    }
    
}

alltests[["Shapiro"]][[TARGET_LABEL]]<- shapiro.test(as.numeric(dataset_edited_without_time$death_event))

# As a rule of thumb, the validity of the tests is assessed by looking at the resulting p-values.
# We start with the Shapiro test of normality,
#cat("\n\t\t == Shapiro ==\n")
vectorShapiro <- c()
vectorShapiroDead <- c()
vectorShapiroSurvived <- c()
for(thecol in c(mycols,TARGET_LABEL)) { 
    vectorShapiro[thecol] <- alltests[["Shapiro"]][[thecol]]$p.value 
    
        if (SEPARATE_DEAD_AND_SURVIVED == TRUE ) {
            vectorShapiroDead[thecol] <- alltests_dead[["Shapiro"]][[thecol]]$p.value 
            vectorShapiroSurvived[thecol] <- alltests_survived[["Shapiro"]][[thecol]]$p.value     
        }
    
   # cat(names((vectorShapiro)[thecol]), "\t \t \t", ((vectorShapiro)[[thecol]]), "\n")
}
# print(vectorShapiro)

# cat("\n\n\t\t == Wilcoxon_rank ==\n")
vectorWilcoxon <- c()
vectorWilcoxonDead <- c()
vectorWilcoxonSurvived<- c()
for(thecol in mycols) { 
    vectorWilcoxon[thecol] <- round(alltests[["Wilcoxon_rank"]][[thecol]]$p.value,6) 
    
    if (SEPARATE_DEAD_AND_SURVIVED == TRUE ) {
        vectorWilcoxonDead[thecol] <- round(alltests_dead[["Wilcoxon_rank"]][[thecol]]$p.value,6) 
        vectorWilcoxonSurvived[thecol] <- round(alltests_survived[["Wilcoxon_rank"]][[thecol]]$p.value,6) 
    }
    
  #  cat(names((vectorWilcoxon)[thecol]), "\t \t \t", ((vectorWilcoxon)[[thecol]]), "\n")
}
# print(vectorWilcoxon)

# cat("\n\t\t == Kruskal ==\n")
vectorKruskal <- c()
vectorKruskalDead <- c()
vectorKruskalSurvived <- c()
for(thecol in mycols) { 
    vectorKruskal[thecol] <- round(alltests[["Kruskal"]][[thecol]]$p.value,6) 
    
        if (SEPARATE_DEAD_AND_SURVIVED == TRUE ) {
            vectorKruskalDead[thecol] <- round(alltests_dead[["Kruskal"]][[thecol]]$p.value,6) 
            vectorKruskalSurvived[thecol] <- round(alltests_survived[["Kruskal"]][[thecol]]$p.value,6) 
        }
    
    # cat(names((vectorKruskal)[thecol]), "\t \t \t", ((vectorKruskal)[[thecol]]), "\n")
}
# print(vectorKruskal)

# cat("\n\t\t == Chi ==\n")
vectorChi <- c()
vectorChiDead <- c()
vectorChiSurvived <- c()
for(thecol in mycols) { 
    vectorChi[thecol] <- round(alltests[["Chi"]][[thecol]]$p.value,6)
    
        if (SEPARATE_DEAD_AND_SURVIVED == TRUE ) {
            vectorChiDead[thecol] <- round(alltests_dead[["Chi"]][[thecol]]$p.value,6)
            vectorChiSurvived[thecol] <- round(alltests_survived[["Chi"]][[thecol]]$p.value,6)
        }
    
    # cat(names((vectorChi)[thecol]), "\t \t \t", ((vectorChi)[[thecol]]), "\n")
}
# print(vectorChi)


sortedVectorShapiro <- sort(vectorShapiro)
sortedVectorWilcoxon <- sort(vectorWilcoxon)
sortedVectorKruskal <- sort(vectorKruskal)
sortedVectorChi <- sort(vectorChi)

    if (SEPARATE_DEAD_AND_SURVIVED == TRUE ) {
        sortedVectorShapiroDead <- sort(vectorShapiroDead)
        sortedVectorWilcoxonDead <- sort(vectorWilcoxonDead)
        sortedVectorKruskalDead <- sort(vectorKruskalDead)
        sortedVectorChiDead <- sort(vectorChiDead)

        sortedVectorShapiroSurvived <- sort(vectorShapiroSurvived)
        sortedVectorWilcoxonSurvived <- sort(vectorWilcoxonSurvived)
        sortedVectorKruskalSurvived <- sort(vectorKruskalSurvived)
        sortedVectorChiSurvived <- sort(vectorChiSurvived)
        }

index <- 1
cat("\n\t\t == Shapiro ==\n")
cat("t\t full sample \t dead patients \t survived patients \n")
for(thecol in names(sortedVectorShapiro)) {    
    
    cat(index, " & \t",  names((sortedVectorShapiro)[thecol]), " & \t ", ((sortedVectorShapiro)[[thecol]]), " & \t ", sep="")
    
    if (SEPARATE_DEAD_AND_SURVIVED == TRUE ) {
        cat(index, " & \t",  names((sortedVectorShapiroDead)[thecol]), " & \t ", ((sortedVectorShapiroDead)[[thecol]]), " & \t ", sep="")
        cat(index, " & \t",  names((sortedVectorShapiroSurvived)[thecol]), " & \t ", ((sortedVectorShapiroSurvived)[[thecol]]), " \\\\ ", sep="")
    }
    
    cat("\n")
    index <- index + 1
}

index <- 1
cat("\n\t\t == Wilcoxon_rank ==\n")
for(thecol in names(sortedVectorWilcoxon)) { 
    
    cat(index, " & \t",  names((sortedVectorWilcoxon)[thecol]), " & \t ", ((sortedVectorWilcoxon)[[thecol]]), " & \t ", sep="")
    
     if (SEPARATE_DEAD_AND_SURVIVED == TRUE ) {
            cat(index, " & \t",  names((sortedVectorWilcoxonDead)[thecol]), " & \t ", ((sortedVectorWilcoxonDead)[[thecol]]), " & \t ", sep="")
            cat(index, " & \t",  names((sortedVectorWilcoxonSurvived)[thecol]), " & \t ", ((sortedVectorWilcoxonSurvived)[[thecol]]), " \\\\ ", sep="")
        }
    
    cat("\n")
    index <- index + 1
}

index <- 1
cat("\n\t\t == Kruskal ==\n")
for(thecol in names(sortedVectorKruskal)) { 
    
    cat(index, " & \t",  names((sortedVectorKruskal)[thecol]), " & \t ", ((sortedVectorKruskal)[[thecol]]), "  & \t ", sep="")
    
    if (SEPARATE_DEAD_AND_SURVIVED == TRUE ) {
        cat(index, " & \t",  names((sortedVectorKruskalDead)[thecol]), " & \t ", ((sortedVectorKruskalDead)[[thecol]]), "  & \t ", sep="")
        cat(index, " & \t",  names((sortedVectorKruskalSurvived)[thecol]), " & \t ", ((sortedVectorKruskalSurvived)[[thecol]]), " \\\\ \n", sep="")
    }
    
    cat("\n")
    index <- index + 1
}

index <- 1
cat("\n\t\t == Chi Square ==\n")
for(thecol in names(sortedVectorChi)) { 
    
     cat(index, " & \t",  names((sortedVectorChi)[thecol]), " & \t ", ((sortedVectorChi)[[thecol]]), " & \t ", sep="")
     
     if (SEPARATE_DEAD_AND_SURVIVED == TRUE ) {
        cat(index, " & \t",  names((sortedVectorChiDead)[thecol]), " & \t ", ((sortedVectorChiDead)[[thecol]]), "  & \t ", sep="")
        cat(index, " & \t",  names((sortedVectorChiSurvived)[thecol]), " & \t ", ((sortedVectorChiSurvived)[[thecol]]), " \\\\ \n", sep="")
     }
     
     cat("\n")
     index <- index + 1
}



# print(alltests)
