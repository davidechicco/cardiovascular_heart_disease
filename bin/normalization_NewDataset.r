# install.packages("class")
# install.packages("gmodels")

# function that normalizes
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}
  

# function that converts a string 
# https://stats.stackexchange.com/a/17995
fromStringToNumeric <- function(x_array) {

   new_x <- as.factor(x_array)
   levels(new_x) <- 1:length(levels(new_x))
   new_x_num <- as.numeric(new_x)

   return (new_x_num)
}


cat("[Reading the data file]\n")
patients_data <- read.csv("../data/dataset_edited_without_time.csv", stringsAsFactors = FALSE) 


num_of_columns_original <- dim(patients_data)[2]
num_of_instances <- dim(patients_data)[1]
num_of_features_original <- num_of_columns_original - 1

patients_data_original <- patients_data

colnames(patients_data)

patients_data_num <- patients_data

num_of_columns <- dim(patients_data_num)[2]
num_of_features <- num_of_columns - 1

target_column_index <- grep("death_event", colnames(patients_data_num))

cat("num_of_features = ", num_of_features, "\n")
cat("the target is patients_data_num$death_event, column index =", target_column_index, "\n")

for(i in 1:(num_of_features))
{
  patients_data_num[,i] <- fromStringToNumeric(patients_data_num[,i])
}
patients_data_num$death_event <- patients_data$death_event
# patients_data_num <- patients_data_num[sample(nrow(patients_data_num)),] # shuffle the rows


round(prop.table(table(patients_data_num$death_event)) * 100, digits = 1)  # it gives the result in the percentage form rounded of to 1 decimal place( and so it’s digits = 1)

cat("[Normalizing the values of the data file (except the death_event target column)]\n")
patients_data_norm <- as.data.frame(lapply(patients_data_num[1:num_of_features], normalize))
patients_data_norm$death_event <- patients_data_num$death_event

colnames(patients_data_norm)

write.table(patients_data_norm, file = "../data/dataset_edited_without_time_NORM.csv", row.names=FALSE, na="", col.names=TRUE, sep=",")

