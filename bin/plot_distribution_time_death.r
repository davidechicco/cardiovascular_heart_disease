
# patients_data <- read.csv(fileNameData, header = TRUE, sep =",");
# targetName <- "DEATH_EVENT"
# 
# library("ggplot2")
# 
# p <- ggplot(patients_data, aes(x=TIME_DAYS, y=TIME_DAYS, color=factor(patients_data[, targetName], labels = c("survived", "dead")) )) + geom_point(size = dotSize)  + xlab("TIME_DAYS")   + ylab("TIME_DAYS") +  labs(colour="patient status", size="") + theme(text = element_text(size=textSize))
#         plot(p)

library("ggplot2")

list.of.packages <- c("easypackages", "ggplot2", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("easypackages")
libraries(list.of.packages)

num_to_return <- 1
upper_num_limit <- 10000000
exe_num <- sample(1:upper_num_limit, num_to_return)

dataFile <- "../data/S1Data_months_and_deaths.csv"

        
monthDeathsData <- read.csv(dataFile, header = TRUE, sep =",")

dotSize <- 3
pdfHeight <- 20 # inches
pdfWidth <- 20 # inches
textSize <- 30
        

pdfFile_plot <- paste("../results/barplot_months_VS_survivals_", exe_num, ".pdf", sep="")
pdf(pdfFile_plot, height=pdfHeight, width=pdfWidth)

p <- ggplot(data=monthDeathsData, aes(x=TIME_MONTH, y=survived_percent)) + geom_bar(stat="identity", fill="steelblue", width=0.5) + xlab("month")   + ylab("survived percentage %") + scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))  + theme(text = element_text(size=textSize)) 

plot(p)
dev.off()

cat("The file ", pdfFile_plot, " has been saved\n", sep="")
  
# dev.off()