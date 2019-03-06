

list.of.packages <- c("randomForest","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("randomForest")
library("ggplot2")

source("./confusion_matrix_rates.r")
source("./utils.r")

PLOT_DEPICTION <- FALSE


fileNameData<- "../data/dataset_edited_without_time.csv"
patients_data <- read.csv(fileNameData, header = TRUE, sep =",");
cat("Read data from file ", fileNameData, "\n", sep="")

fileNameDataNorm <- "../data/dataset_edited_without_time_NORM.csv"
patients_data_norm <- read.csv(fileNameDataNorm, header = TRUE, sep =",");
cat("Read data from file ", fileNameDataNorm, "\n", sep="")

num_to_return <- 1
exe_num <- sample(1:as.numeric(Sys.time()), num_to_return)


rf_output <- randomForest(as.factor(death_event) ~ ., data=patients_data_norm, importance=TRUE, proximity=TRUE)

dd <- as.data.frame(rf_output$importance);

dd_sorted_MSE <- dd[order(-dd$"MeanDecreaseAccuracy"), ]
# print(dd_sorted_MSE);

dd_sorted_IncNodePurity <- dd[order(-dd$"MeanDecreaseGini"), ]
# print(dd_sorted_IncNodePurity);

# feature sum_of_positions
# 
# serum_creatinine         2
# ejection_fraction	     4
# age	                                 6
# creatinine_phosphokinase	10
# serum_sodium	        10
# gender	                        13
# platelets	                    13
# smoking	                    16
# blood_pressure	        17
# diabetes	                    20
# anaemia	                    21


varImpPlot(rf_output)

dd_sorted_IncNodePurity_only <- dd_sorted_IncNodePurity
dd_sorted_IncNodePurity_only$"MeanDecreaseAccuracy" <- NULL
dd_sorted_IncNodePurity_only$purityPos <- c(1:dim(dd_sorted_IncNodePurity_only)[1])
dd_sorted_MSE_only <- dd_sorted_MSE
colnames(dd_sorted_MSE_only)[1] <- c("MeanDecreaseAccuracy")


dd_sorted_MSE_only$IncNodePurity <- NULL
dd_sorted_MSE_only$msePos <- c(1:dim(dd_sorted_IncNodePurity_only)[1])

dd_sorted_MSE_only$features <- rownames(dd_sorted_MSE_only)
dd_sorted_IncNodePurity_only$features <- rownames(dd_sorted_IncNodePurity_only)

#print(dd_sorted_MSE_only)
#print(dd_sorted_IncNodePurity_only)

mergedRanking <- cbind(dd_sorted_MSE_only, dd_sorted_IncNodePurity_only)

mergedRankingAlphaBeta <- mergedRanking[order(mergedRanking$"features"), ]
mergedRankingAlphaBeta$posSum <- mergedRankingAlphaBeta$purityPos + mergedRankingAlphaBeta$msePos

mergedRankingGeneralRank <- mergedRankingAlphaBeta[order(mergedRankingAlphaBeta$"posSum"), ]
mergedRankingGeneralRank$finalPos <- c(1:dim(mergedRankingGeneralRank)[1])
lastCol <- dim(mergedRankingGeneralRank)[2]

featuresCol <- 6

cat("\n\n\n\n")
print(mergedRankingGeneralRank[,c(lastCol), drop=FALSE])

if (PLOT_DEPICTION == TRUE) {

        library("ggplot2")
        # Minimal theme + blue fill color

        pdfFileNameMSE <-  paste("../results/MSE_features_", exe_num, ".pdf", sep="")
        pdf(pdfFileNameMSE)
        p <- ggplot(data=dd_sorted_MSE_only, aes(x=reorder(features, -pos), y=IncMSE)) +  geom_bar(stat="identity", fill="steelblue")  + labs(title = "Feature importance on accuracy reduction", y = "accuracy reduction", x = "features")
        p <- p + coord_flip()
        plot(p)
        dev.off()


        pdfFileNameGini <-  paste("../results/Gini_features_", exe_num, ".pdf", sep="")
        pdf(pdfFileNameGini)
        p <- ggplot(data=dd_sorted_IncNodePurity_only, aes(x=reorder(features, -pos), y=IncNodePurity)) +  geom_bar(stat="identity", fill="steelblue")  + labs(title = "Feature importance on Gini impurity", y = "Gini impurity", x = "features")
        p <- p + coord_flip()
        plot(p)
        dev.off()


        pdfFile_plot_death_serum_creatinine <- paste("../results/plot_death_VS_serum_creatinine_NORM_", exe_num, ".pdf", sep="")
        pdf(pdfFile_plot_death_serum_creatinine)
        plot_death_serum_creatinine <-  cdplot(factor(death_event, labels=c("survived", "dead")) ~ serum_creatinine, data=patients_data_norm, ylab = NA, xlab="serum creatinine")
        # plot(plot_death_serum_creatinine)
        dev.off()

        pdfFile_plot_death_ejection_fraction <- paste("../results/plot_death_VS_ejection_fraction_NORM_", exe_num, ".pdf", sep="")
        pdf(pdfFile_plot_death_ejection_fraction)
        plot_death_ejection_fraction <- cdplot(factor(death_event, labels=c("survived", "dead")) ~ ejection_fraction, data=patients_data_norm, ylab = NA, xlab="ejection fraction")
        #plot(plot_death_ejection_fraction)
        dev.off()

        # pdfFile_plot_death_age <- paste("../results/plot_death_VS_age_", exe_num, ".pdf", sep="")
        # pdf(pdfFile_plot_death_age)
        # plot_death_age <- cdplot(factor(death_event, labels=c("survived", "dead")) ~ age,, data=patients_data, ylab = NA)
        # dev.off()

        num_of_patients <- dim(patients_data_norm)[1]
        num_of_features <- dim(patients_data_norm)[2] - 1

        #
        # Pearson correlation coefficient
        #

        i <- 1
        for (i in 1:num_of_features) {

            thisPCC <- cor(patients_data_norm$death_event, patients_data_norm[,i], method = c("pearson"))

            cat("pearson(death_event, ", colnames(patients_data_norm)[i],")  = ", dec_two(thisPCC), "\n", sep="")
        }

        # 
        # Kendall
        # 
        i <- 1
        for (i in 1:num_of_features) {

            thisKendall <- cor(patients_data_norm$death_event, patients_data_norm[,i], method = c("kendall"))

            cat("kendall(death_event, ", colnames(patients_data_norm)[i],")  = ", dec_two(thisKendall), "\n", sep="")
        }


        # Pearson correlation coefficients
        # 0.37
        # -0.26
        # 0.24
        # 0.03
        # -0.20
        # -0.00
        # -0.04
        # -0.01
        # 0.08
        # -0.00
        # 0.07


        # Print the model tree
        # reprtree:::plot.getTree(rf_new)

        dotSize <- 3
        pdfHeight <- 10 # inches
        pdfWidth <- 20 # inches
        textSize <- 30

        pdfFile_dfFile_plot_death_age <- paste("../results/scatterplot_serum_creatinine_VS_ejection_fraction_", exe_num, ".pdf", sep="")
        pdf(pdfFile_dfFile_plot_death_age, height=pdfHeight, width=pdfWidth)
        p <- ggplot(patients_data, aes(x=serum_creatinine, y=ejection_fraction, color=factor(death_event, labels = c("survived", "dead")) )) + geom_point(size = dotSize)  + xlab("serum creatinine")   + ylab("ejection fraction") +  labs(colour="patient status", size="") + theme(text = element_text(size=textSize))
        plot(p)
        dev.off()


        pdfFile_dfFile_plot_death_age_withLine <- paste("../results/scatterplot_serum_creatinine_VS_ejection_fraction_withLine_", exe_num, ".pdf", sep="")
        pdf(pdfFile_dfFile_plot_death_age_withLine, height=pdfHeight, width=pdfWidth)
        p <- ggplot(patients_data, aes(x=serum_creatinine, y=ejection_fraction, color=factor(death_event, labels = c("survived", "dead")), shape=factor(death_event, labels = c("survived", "dead")) )) + geom_point(size = dotSize)  + xlab("serum creatinine")   + ylab("ejection fraction") +  labs(colour="patient status", size="", shape="") + geom_abline(intercept = 0.01, slope = 15, linetype="dashed") + theme(text = element_text(size=textSize)) + scale_shape(guide = 'none')
        plot(p)
        dev.off()

}
