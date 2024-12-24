##correlation matrices for Dame and Linne cleaned files, using demographic and stress data


#Split data by Subgroup
grouped_data <- split(LinneDemographicsAndAnswers, LinneDemographicsAndAnswers$Trainingsversion)

#Compute correlation matrices for each group
cor_matrices <- lapply(grouped_data, function(group) {
  numeric_data <- select_if(group, is.numeric)
  round(cor(numeric_data, use = "complete.obs"), 2)
})

