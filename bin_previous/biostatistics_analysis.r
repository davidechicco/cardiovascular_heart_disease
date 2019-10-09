setwd(".")
options(stringsAsFactors = FALSE)

source("./utils.r")

fileName <- "../data/dataset_edited_without_time.csv"
patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName: ", fileName, "\n", sep="")

ALL_PATIENTS_CORRELATION <- TRUE

TWO_DIM <- 2
FIRST_COMP <- 1
SECOND_COMP <- 2

patients_data_original <- patients_data
numPatients <- nrow(patients_data)

# let's change some feature name
# names(patients_data)[names(patients_data) == "Chronic.Kidney.Disease.with.dialysis..CKD.with.dialysis."] <- "ChrKD.with.dialysis"
# names(patients_data)[names(patients_data) == "Chronic.Kidney.Disease.without.dialysis..CKD.w.o.dialysis."] <- "ChrKD.withOUT.dialysis"
# names(patients_data)[names(patients_data) == "Chronic.Obstructive.Pulmonary.Disease..COPD."] <- "ChrOPD"
# names(patients_data)[names(patients_data) == "ADDED.survival"] <- "survival"

# sort the columns alphabetically
patients_data <- patients_data[ , order(names(patients_data))]

cat("\n// all patients //\n", sep="")
for(i in 1:(ncol(patients_data))) { 

    cat("\n\n", colnames(patients_data)[i], ": \n", sep="") 
        if (dim(table(patients_data[,i]))==TWO_DIM) { # 2-dimensional
        
        firstComponentNum <- (table(patients_data[,i]))[[FIRST_COMP]]
        firstComponentName <- rownames(table(patients_data[,i]))[FIRST_COMP]
        firstComponentPerc <- firstComponentNum*100/numPatients
        
        secondComponentNum <- (table(patients_data[,i]))[[SECOND_COMP]]
        secondComponentName <- rownames(table(patients_data[,i]))[SECOND_COMP]
        secondComponentPerc <- secondComponentNum*100/numPatients
        
        # cat(firstComponentName, "\t #\t %\n", sep="")
        cat("\t #\t %\n", sep="")
        cat(firstComponentName, "\t ", firstComponentNum," & \t ", dec_two(firstComponentPerc),"\n", sep="")
        # cat(secondComponentName, "\t #\t %\n", sep="")
        cat(secondComponentName, "\t ", secondComponentNum," & \t ", dec_two(secondComponentPerc),"\n;", sep="")
        
    } else { 
    
        print(table(patients_data[,i])) 
    
    }
    
    
    print(summary(patients_data[,i])) 

}


# Vaspopressors as target

targetYesValue <- 1
targetNoValue <- 0
targetName <- "death_event"

targetIndex <- which(colnames(patients_data)==targetName)


# patients YES

patients_data_target_yes <- (patients_data[patients_data[, targetIndex]==targetYesValue,])
patients_data_target_yes <- patients_data_target_yes[ , order(names(patients_data_target_yes))]

numPatientsYes <- nrow(patients_data_target_yes)

cat("\n// target YES patients //\n", sep="")
for(i in 1:(ncol(patients_data_target_yes))) { 

    cat("\n\n", colnames(patients_data_target_yes)[i], ": \n", sep=""); 
    
    if (dim(table(patients_data_target_yes[,i]))==TWO_DIM) { # 2-dimensional
        
        firstComponentNum <- (table(patients_data_target_yes[,i]))[[FIRST_COMP]]
        firstComponentName <- rownames(table(patients_data_target_yes[,i]))[FIRST_COMP]
        firstComponentPerc <- firstComponentNum*100/numPatientsYes
        
        secondComponentNum <- (table(patients_data_target_yes[,i]))[[SECOND_COMP]]
        secondComponentName <- rownames(table(patients_data_target_yes[,i]))[SECOND_COMP]
        secondComponentPerc <- secondComponentNum*100/numPatientsYes
        
        # cat(firstComponentName, "\t #\t %\n", sep="")
        cat("\t #\t %\n", sep="")
        cat(firstComponentName, "\t ", firstComponentNum," & \t ", dec_two(firstComponentPerc),"\n", sep="")
        # cat(secondComponentName, "\t #\t %\n", sep="")
        cat(secondComponentName, "\t ", secondComponentNum," & \t ", dec_two(secondComponentPerc),"\n", sep="")
        
    } else { 
    
        print(table(patients_data_target_yes[,i])) 
    
    }
    
    print(summary(patients_data_target_yes[,i]))    
}

# patients NO

patients_data_target_no <- (patients_data[patients_data[, targetIndex]==targetNoValue,])
patients_data_target_no<- patients_data_target_no[ , order(names(patients_data_target_no))]
numPatientsNo <- nrow(patients_data_target_no)

cat("\n// target NO patients //\n", sep="")
for(i in 1:(ncol(patients_data_target_no))) { 

    cat("\n\n", colnames(patients_data_target_no)[i], ": \n", sep=""); 
    if (dim(table(patients_data_target_no[,i]))==TWO_DIM) { # 2-dimensional
        
        firstComponentNum <- (table(patients_data_target_no[,i]))[[FIRST_COMP]]
        firstComponentName <- rownames(table(patients_data_target_no[,i]))[FIRST_COMP]
        firstComponentPerc <- firstComponentNum*100/numPatientsNo
        
        secondComponentNum <- (table(patients_data_target_no[,i]))[[SECOND_COMP]]
        secondComponentName <- rownames(table(patients_data_target_no[,i]))[SECOND_COMP]
        secondComponentPerc <- secondComponentNum*100/numPatientsNo
        
        # cat(firstComponentName, "\t #\t %\n", sep="")
        cat("\t #\t %\n", sep="")
        cat(firstComponentName, "\t ", firstComponentNum," & \t ", dec_two(firstComponentPerc),"\n", sep="")
        # cat(secondComponentName, "\t #\t %\n", sep="")
        cat(secondComponentName, "\t ", secondComponentNum," & \t ", dec_two(secondComponentPerc),"\n;", sep="")
        
    } else { 
    
        print(table(patients_data_target_no[,i])) 
    
    }
    
    print(summary(patients_data_target_no[,i])) 
}

# All patients: p-value, t-value, and PCC

if (ALL_PATIENTS_CORRELATION == TRUE) {

    cat("\n// all patients correlations //\n\n", sep="")
    cat(targetName, ";\t abs(t); \t p-value; \t PCC; \t conf_int;\n\n", sep="")
    for(i in 1:(ncol(patients_data))) { 

        # cat("\n\ncorrelation between (target) ", colnames(patients_data)[targetIndex], " and ",  colnames(patients_data)[i], ": \n", sep="") 
        
        # cat("\n patients_data)[", i, "] ", colnames(patients_data)[i], " [versus]  ", colnames(patients_data)[targetIndex], "(target) : correlation\n", sep="")
        
        thisTtest <- t.test(patients_data[,i], patients_data[,targetIndex])
        tValue <- abs((thisTtest$statistic)[[1]])
        pValue <- (thisTtest$p.value)
        thisPCC <- cor(patients_data[,i], patients_data[,targetIndex], method=c("pearson"))
        conf_int_start <- dec_two((thisTtest$conf.int)[1])
        conf_int_end <- dec_two((thisTtest$conf.int)[2])
        
        # cat(colnames(patients_data)[i], "\t\t abs(t) \t p-value \t PCC \t conf_int\n", sep="")
        cat(colnames(patients_data)[i], ";\t", dec_two(tValue), ";\t", pValue, ";\t", dec_two(thisPCC), ";\t", conf_int_start, ";\t", conf_int_end, ";\n", sep="")
        
        # cat("t = ", dec_two(tValue), "\n", sep="")
        # cat("p-value = ", dec_two(pValue), "\n", sep="")
        # cat("PCC(", colnames(patients_data)[targetIndex], ", ", colnames(patients_data)[i], ") = ",  dec_two(thisPCC), "\n", sep="")

    }

}
