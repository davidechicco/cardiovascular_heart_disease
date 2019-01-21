

list.of.packages <- c("randomForest","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("randomForest")
library("ggplot2")


fileNameData<- "../data/dataset_edited_without_time.csv"
patients_data <- read.csv(fileNameData, header = TRUE, sep =",");
cat("Read data from file ", fileNameData, "\n", sep="")

fileNameDataNorm <- "../data/dataset_edited_without_time_NORM.csv"
patients_data_norm <- read.csv(fileNameDataNorm, header = TRUE, sep =",");
cat("Read data from file ", fileNameDataNorm, "\n", sep="")

num_to_return <- 1
exe_num <- sample(1:as.numeric(Sys.time()), num_to_return)


rf_output <- randomForest(death_event ~ ., data=patients_data_norm, importance=TRUE, proximity=TRUE)

dd <- as.data.frame(rf_output$importance);

dd_sorted_MSE <- dd[order(-dd$"%IncMSE"), ]
print(dd_sorted_MSE);

dd_sorted_IncNodePurity <- dd[order(-dd$"IncNodePurity"), ]
print(dd_sorted_IncNodePurity);

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
dd_sorted_IncNodePurity_only$"%IncMSE" <- NULL
dd_sorted_IncNodePurity_only$pos <- c(1:dim(dd_sorted_IncNodePurity_only)[1])
colnames(dd_sorted_MSE_only)[1] <- c("IncMSE")

dd_sorted_MSE_only <- dd_sorted_MSE
dd_sorted_MSE_only$IncNodePurity <- NULL
dd_sorted_MSE_only$pos <- c(1:dim(dd_sorted_IncNodePurity_only)[1])

dd_sorted_MSE_only$features <- rownames(dd_sorted_MSE_only)
dd_sorted_IncNodePurity_only$features <- rownames(dd_sorted_IncNodePurity_only)


library("ggplot2")
# Minimal theme + blue fill color

pdfFileNameMSE <-  paste("../results/barplots/MSE_features_", exe_num, ".pdf", sep="")
pdf(pdfFileNameMSE)
p <- ggplot(data=dd_sorted_MSE_only, aes(x=reorder(features, -pos), y=IncMSE)) +  geom_bar(stat="identity", fill="steelblue")  + labs(title = "Feature importance on accuracy reduction", y = "accuracy reduction", x = "features")
p + coord_flip()
dev.off()


pdfFileNameGini <-  paste("../results/barplots/Gini_features_", exe_num, ".pdf", sep="")
pdf(pdfFileNameGini)
p <- ggplot(data=dd_sorted_IncNodePurity_only, aes(x=reorder(features, -pos), y=IncNodePurity)) +  geom_bar(stat="identity", fill="steelblue")  + labs(title = "Feature importance on Gini impurity", y = "Gini impurity", x = "features")
p + coord_flip()
p
dev.off()


pdfFile_plot_death_serum_creatinine <- paste("../results/plot_death_VS_serum_creatinine_", exe_num, ".pdf", sep="")
pdf(pdfFile_plot_death_serum_creatinine)
plot_death_serum_creatinine <-  cdplot(factor(death_event, labels=c("alive", "dead")) ~ serum_creatinine, data=patients_data_norm, ylab = NA)
dev.off()

pdfFile_plot_death_ejection_fraction <- paste("../results/plot_death_VS_ejection_fraction_", exe_num, ".pdf", sep="")
pdf(pdfFile_plot_death_ejection_fraction)
plot_death_ejection_fraction <- cdplot(factor(death_event, labels=c("alive", "dead")) ~ ejection_fraction, data=patients_data_norm, ylab = NA)
dev.off()

pdfFile_plot_death_age <- paste("../results/plot_death_VS_age_", exe_num, ".pdf", sep="")
pdf(pdfFile_plot_death_age)
plot_death_age <- cdplot(factor(death_event, labels=c("alive", "dead")) ~ age,, data=patients_data_norm, ylab = NA)
dev.off()

pearson_death_serum_creatinine <- cor(patients_data_norm$death_event, patients_data_norm$serum_creatinine, method = c("pearson"))
pearson_death_ejection_fraction <- cor(patients_data_norm$death_event, patients_data_norm$ejection_fraction, method = c("pearson"))
pearson_death_age <- cor(patients_data_norm$death_event, patients_data_norm$age, method = c("pearson"))


# Print the model tree
# reprtree:::plot.getTree(rf_new)

dotSize <- 3
pdfHeight <- 10 # inches
pdfWidth <- 12 # inches

pdfFile_dfFile_plot_death_age <- paste("../results/scatterplot_serum_creatinine_VS_ejection_fraction_", exe_num, ".pdf", sep="")
pdf(pdfFile_dfFile_plot_death_age, height=pdfHeight, width=pdfWidth)
ggplot(patients_data, aes(x=serum_creatinine, y=ejection_fraction, color=factor(death_event, labels = c("alive", "dead")) )) + geom_point(size = dotSize)  +  labs(colour="patient status", size="") + theme(text = element_text(size=30))
dev.off()


pdfFile_dfFile_plot_death_age_withLine <- paste("../results/scatterplot_serum_creatinine_VS_ejection_fraction_withLine_", exe_num, ".pdf", sep="")
pdf(pdfFile_dfFile_plot_death_age_withLine, height=pdfHeight, width=pdfWidth)
ggplot(patients_data, aes(x=serum_creatinine, y=ejection_fraction, color=factor(death_event, labels = c("alive", "dead")) )) + geom_point(size = dotSize)  +  labs(colour="patient status", size="") + geom_abline(intercept = 0.01, slope = 15, linetype="dashed") + theme(text = element_text(size=30))
dev.off()
