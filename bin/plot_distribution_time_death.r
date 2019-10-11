
patients_data <- read.csv(fileNameData, header = TRUE, sep =",");
targetName <- "DEATH_EVENT"

library("ggplot2")

p <- ggplot(patients_data, aes(x=TIME_DAYS, y=TIME_DAYS, color=factor(patients_data[, targetName], labels = c("survived", "dead")) )) + geom_point(size = dotSize)  + xlab("TIME_DAYS")   + ylab("TIME_DAYS") +  labs(colour="patient status", size="") + theme(text = element_text(size=textSize))
        plot(p)


        dev.off()
