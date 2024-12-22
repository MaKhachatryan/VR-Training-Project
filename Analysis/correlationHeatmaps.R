##correlation matrices for Dame and Linne cleaned files, using demographic and stress data
DameDemographicsAndAnswers_numeric <- select_if(DameDemographicsAndAnswers, is.numeric)
cor(DameDemographicsAndAnswers_numeric)

DameDemographicsAndAnswers


#Split data by Subgroup
grouped_data <- split(DameDemographicsAndAnswers, DameDemographicsAndAnswers$Trainingsversion)

# Step 2: Compute correlation matrices for each group
cor_matrices <- lapply(grouped_data, function(group) {
  numeric_data <- select_if(group, is.numeric)
  cor(numeric_data, use = "complete.obs")
})

# Step 3: Print results
print(cor_matrices)
