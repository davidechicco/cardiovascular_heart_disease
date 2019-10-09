setwd(".")

# Software originally developed by Giuseppe Jurman <jurman@fbk.eu> on 1st March 2019
# Edited by Davide Chicco <davide.chicco@gmail.com> on 4th March 2019


# Data load

list.of.packages <- c("survival", "ranger", "ggplot2", "dplyr", "ggfortify", "readr", "easypackages")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("easypackages")
libraries(list.of.packages)

path_to_data <- "../data/"
filename <- paste(path_to_data,"dataset_edited.csv",sep="")
dataset_edited <- as.data.frame(read_csv(filename,))
print(head(dataset_edited))

# tutorial here: https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/

kaplanMeierAnalysis <- with(dataset_edited, Surv(time, death_event))

km_fit <- survfit(Surv(time, death_event) ~ 1, data=dataset_edited)

summary(km_fit)

autoplot(km_fit)
