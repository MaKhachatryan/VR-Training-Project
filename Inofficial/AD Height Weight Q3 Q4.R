## AD Question 3 Question 4

## -------- LOADING --------
## load library
library(readr)
library(dplyr)
library(ggplot2)

## load data sets
DameDemographicsAndAnswers <- read_csv("Data/processedData/DameDemographicsAndAnswers.csv")
LinneDemographicsAndAnswers <- read_csv("Data/processedData/LinneDemographicsAndAnswers.csv")
combinedCohorts <- read_csv("Data/processedData/combinedCohorts.csv")


## ------------- WRITE GENERAL FUNCTION -------------
heightWeightSubgroup <- function(dataSet, heightWeight, answerQ1Q2, groupByVar = NA) {
  selectedCols <- c(heightWeight, answerQ1Q2)
  if (!is.na(groupByVar)) {
    selectedCols <- c(selectedCols, groupByVar)
  }
  
  result <- dataSet |> 
    select(all_of(selectedCols))
  
  if (!is.na(groupByVar)) {
    result <- result |> 
      group_by(across(all_of(groupByVar))) |> 
      mutate(
        groupMean = mean(.data[[heightWeight]], na.rm = TRUE),
        comparison = if_else(
          .data[[heightWeight]] > groupMean,
          sprintf("above mean (%s)", round(groupMean, 2)),
          sprintf("below mean (%s)", round(groupMean, 2))
        )
      ) |> 
      ungroup()
  } else {
    result <- result |> 
      mutate(
        groupMean = mean(.data[[heightWeight]], na.rm = TRUE),
        comparison = if_else(
          .data[[heightWeight]] > groupMean,
          sprintf("above mean (%s)", round(groupMean, 2)),
          sprintf("below mean (%s)", round(groupMean, 2))
        )
      )
  }
  result
}

## ----- draw plot
firstplotHeightWeight <- function(heightWeightSubgroupDf) {
  if (ncol(heightWeightSubgroupDf) == 4) {
    resultPlot <- ggplot(heightWeightSubgroupDf, aes(
      x = comparison, 
      y = .data[[colnames(heightWeightSubgroupDf)[2]]]
    )) +
      geom_boxplot() +
      scale_y_continuous(breaks = seq(1, 6, by = 1), limits = c(1, 6)) +
      labs(
        title = sprintf(
          "%s Answer Distribution by %s Group",
          colnames(heightWeightSubgroupDf)[2],
          colnames(heightWeightSubgroupDf)[1]
        ),
        x = sprintf("%s Group", colnames(heightWeightSubgroupDf)[1]),
        y = sprintf("%s", colnames(heightWeightSubgroupDf)[2]),
        subtitle = deparse(substitute(heightWeightSubgroupDf))
      ) +
      theme_minimal()
  } else {
    resultPlot <- ggplot(heightWeightSubgroupDf, aes(
      x = comparison, 
      y = .data[[colnames(heightWeightSubgroupDf)[2]]],
      fill = .data[[colnames(heightWeightSubgroupDf)[3]]]
    )) +
      geom_boxplot() +
      scale_y_continuous(breaks = seq(1, 6, by = 1), limits = c(1, 6)) +
      labs(
        title = sprintf(
          "%s Answer Distribution by %s Group",
          colnames(heightWeightSubgroupDf)[2],
          colnames(heightWeightSubgroupDf)[1]
        ),
        subtitle = deparse(substitute(heightWeightSubgroupDf)),
        x = sprintf("%s Group", colnames(heightWeightSubgroupDf)[1]),
        y = sprintf("%s", colnames(heightWeightSubgroupDf)[2])
      ) +
      theme_minimal()
  }
  
  return(resultPlot)
}

plotHeightWeight <- function(heightWeightSubgroupDf) {
  if (ncol(heightWeightSubgroupDf) == 4) {
    resultPlot <- ggplot(heightWeightSubgroupDf, aes(
      x = comparison, 
      y = .data[[colnames(heightWeightSubgroupDf)[2]]]
    )) +
      geom_boxplot() +
      scale_y_continuous(breaks = seq(1, 6, by = 1), limits = c(1, 6)) +
      labs(
        title = sprintf(
          "%s Answer Distribution by %s Group",
          colnames(heightWeightSubgroupDf)[2],
          colnames(heightWeightSubgroupDf)[1]
        ),
        x = sprintf("%s Group", colnames(heightWeightSubgroupDf)[1]),
        y = sprintf("%s", colnames(heightWeightSubgroupDf)[2]),
        subtitle = deparse(substitute(heightWeightSubgroupDf))
      ) +
      theme_minimal()
  } else {
    resultPlot <- ggplot(heightWeightSubgroupDf, aes(
      x = .data[[colnames(heightWeightSubgroupDf)[3]]], 
      y = .data[[colnames(heightWeightSubgroupDf)[2]]],
      fill = comparison
    )) +
      geom_boxplot() +
      scale_y_continuous(breaks = seq(1, 6, by = 1), limits = c(1, 6)) +
      labs(
        title = sprintf(
          "%s Answer Distribution by %s Group",
          colnames(heightWeightSubgroupDf)[2],
          colnames(heightWeightSubgroupDf)[1]
        ),
        subtitle = deparse(substitute(heightWeightSubgroupDf)),
        x = sprintf("%s Group", colnames(heightWeightSubgroupDf)[1]),
        y = sprintf("%s", colnames(heightWeightSubgroupDf)[2])
      ) +
      theme_minimal()
  }
  
  return(resultPlot)
}

## -------------- ANALYSIS --------------
### ----- DAME ------
# no group
heightDameQ1 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Height", "Mean_Answer_Q1")
plotHeightWeight(heightDameQ1)

heightDameQ2 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Height", "Mean_Answer_Q2")
plotHeightWeight(heightDameQ2)

weightDameQ1 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Weight", "Mean_Answer_Q1")
plotHeightWeight(weightDameQ1)

weightDameQ2 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Weight", "Mean_Answer_Q2")
plotHeightWeight(weightDameQ2)

# group with gender
heightGenderDameQ1 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Height", "Mean_Answer_Q1", "Gender")
plotHeightWeight(heightGenderDameQ1)

heightGenderDameQ2 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Height", "Mean_Answer_Q2", "Gender")
plotHeightWeight(heightGenderDameQ2)

weightGenderDameQ1 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Weight", "Mean_Answer_Q1", "Gender")
plotHeightWeight(weightGenderDameQ1)

weightGenderDameQ2 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Weight", "Mean_Answer_Q2", "Gender")
plotHeightWeight(weightGenderDameQ2)

# group with trainingsversion
heightGroupDameQ1 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Height", "Mean_Answer_Q1", "Trainingsversion")
plotHeightWeight(heightGroupDameQ1)

heightGroupDameQ2 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Height", "Mean_Answer_Q2", "Trainingsversion")
plotHeightWeight(heightGroupDameQ2)  # THIS ONE SHOWS CLEAR DIFFERENCES IN HEIGHT WITHIN TRAININGSVERSION

weightGroupDameQ1 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Weight", "Mean_Answer_Q1", "Trainingsversion")
plotHeightWeight(weightGroupDameQ1)

weightGroupDameQ2 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Weight", "Mean_Answer_Q2", "Trainingsversion")
plotHeightWeight(weightGroupDameQ2)


### ----- Linne ------
# no group
heightLinneQ1 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Height", "Mean_Answer_Q1")
plotHeightWeight(heightLinneQ1)

heightLinneQ2 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Height", "Mean_Answer_Q2")
plotHeightWeight(heightLinneQ2)

weightLinneQ1 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Weight", "Mean_Answer_Q1")
plotHeightWeight(weightLinneQ1)

weightLinneQ2 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Weight", "Mean_Answer_Q2")
plotHeightWeight(weightLinneQ2)

# group with gender
heightGenderLinneQ1 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Height", "Mean_Answer_Q1", "Gender")
plotHeightWeight(heightGenderLinneQ1)  # THIS ONE SHOWS CLEAR DIFFERENCES IN HEIGHT WITHIN GENDER

heightGenderLinneQ2 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Height", "Mean_Answer_Q2", "Gender")
plotHeightWeight(heightGenderLinneQ2)  # THIS ONE SHOWS CLEAR DIFFERENCES IN HEIGHT WITHIN GENDER

weightGenderLinneQ1 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Weight", "Mean_Answer_Q1", "Gender")
plotHeightWeight(weightGenderLinneQ1)  # THIS ONE SHOWS CLEAR DIFFERENCES IN WEIGHT WITHIN GENDER

weightGenderLinneQ2 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Weight", "Mean_Answer_Q2", "Gender")
plotHeightWeight(weightGenderLinneQ2)

# group with trainingsversion
heightGroupLinneQ1 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Height", "Mean_Answer_Q1", "Trainingsversion")
plotHeightWeight(heightGroupLinneQ1) # THIS ONE SHOWS CLEAR DIFFERENCES IN HEIGHT WITHIN TRAININGSVERSION

heightGroupLinneQ2 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Height", "Mean_Answer_Q2", "Trainingsversion")
plotHeightWeight(heightGroupLinneQ2) # THIS ONE SHOWS CLEAR DIFFERENCES IN HEIGHT WITHIN TRAININGSVERSION

weightGroupLinneQ1 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Weight", "Mean_Answer_Q1", "Trainingsversion")
plotHeightWeight(weightGroupLinneQ1)  # THIS ONE SHOWS CLEAR DIFFERENCES IN WEIGHT WITHIN TRAININGSVERSION

weightGroupLinneQ2 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Weight", "Mean_Answer_Q2", "Trainingsversion")
plotHeightWeight(weightGroupLinneQ2) # THIS ONE SHOWS CLEAR DIFFERENCES IN WEIGHT WITHIN TRAININGSVERSION

### ----- Combined ------
# no group
heightCombinedQ1 <- heightWeightSubgroup(combinedCohorts, "Height", "Mean_Answer_Q1")
plotHeightWeight(heightCombinedQ1)

heightCombinedQ2 <- heightWeightSubgroup(combinedCohorts, "Height", "Mean_Answer_Q2")
plotHeightWeight(heightCombinedQ2)

weightCombinedQ1 <- heightWeightSubgroup(combinedCohorts, "Weight", "Mean_Answer_Q1")
plotHeightWeight(weightCombinedQ1)

weightCombinedQ2 <- heightWeightSubgroup(combinedCohorts, "Weight", "Mean_Answer_Q2")
plotHeightWeight(weightCombinedQ2)

# group with gender
heightGenderCombinedQ1 <- heightWeightSubgroup(combinedCohorts, "Height", "Mean_Answer_Q1", "Gender")
plotHeightWeight(heightGenderCombinedQ1)

heightGenderCombinedQ2 <- heightWeightSubgroup(combinedCohorts, "Height", "Mean_Answer_Q2", "Gender")
plotHeightWeight(heightGenderCombinedQ2)

weightGenderCombinedQ1 <- heightWeightSubgroup(combinedCohorts, "Weight", "Mean_Answer_Q1", "Gender")
plotHeightWeight(weightGenderCombinedQ1)

weightGenderCombinedQ2 <- heightWeightSubgroup(combinedCohorts, "Weight", "Mean_Answer_Q2", "Gender")
plotHeightWeight(weightGenderCombinedQ2)

# group with trainingsversion
heightGroupCombinedQ1 <- heightWeightSubgroup(combinedCohorts, "Height", "Mean_Answer_Q1", "Trainingsversion")
plotHeightWeight(heightGroupCombinedQ1)

heightGroupCombinedQ2 <- heightWeightSubgroup(combinedCohorts, "Height", "Mean_Answer_Q2", "Trainingsversion")
plotHeightWeight(heightGroupCombinedQ2)

weightGroupCombinedQ1 <- heightWeightSubgroup(combinedCohorts, "Weight", "Mean_Answer_Q1", "Trainingsversion")
plotHeightWeight(weightGroupCombinedQ1)

weightGroupCombinedQ2 <- heightWeightSubgroup(combinedCohorts, "Weight", "Mean_Answer_Q2", "Trainingsversion")
plotHeightWeight(weightGroupCombinedQ2)


## -------- CONCLUSION - CHOOSING PLOT ----------
# copy the plot shows clearly a different behaviour of a subgroup

# group with trainingsversion
heightGroupDameQ2 <- heightWeightSubgroup(DameDemographicsAndAnswers, "Height", "Mean_Answer_Q2", "Trainingsversion")
plotHeightWeight(heightGroupDameQ2)  # THIS ONE SHOWS CLEAR DIFFERENCES IN HEIGHT WITHIN TRAININGSVERSION

# group with gender
heightGenderLinneQ1 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Height", "Mean_Answer_Q1", "Gender")
plotHeightWeight(heightGenderLinneQ1)  # THIS ONE SHOWS CLEAR DIFFERENCES IN HEIGHT WITHIN GENDER

heightGenderLinneQ2 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Height", "Mean_Answer_Q2", "Gender")
plotHeightWeight(heightGenderLinneQ2)  # THIS ONE SHOWS CLEAR DIFFERENCES IN HEIGHT WITHIN GENDER

weightGenderLinneQ1 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Weight", "Mean_Answer_Q1", "Gender")
plotHeightWeight(weightGenderLinneQ1)  # THIS ONE SHOWS CLEAR DIFFERENCES IN WEIGHT WITHIN GENDER

# group with trainingsversion
heightGroupLinneQ1 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Height", "Mean_Answer_Q1", "Trainingsversion")
plotHeightWeight(heightGroupLinneQ1) # THIS ONE SHOWS CLEAR DIFFERENCES IN HEIGHT WITHIN TRAININGSVERSION

heightGroupLinneQ2 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Height", "Mean_Answer_Q2", "Trainingsversion")
plotHeightWeight(heightGroupLinneQ2) # THIS ONE SHOWS CLEAR DIFFERENCES IN HEIGHT WITHIN TRAININGSVERSION

weightGroupLinneQ1 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Weight", "Mean_Answer_Q1", "Trainingsversion")
plotHeightWeight(weightGroupLinneQ1)  # THIS ONE SHOWS CLEAR DIFFERENCES IN WEIGHT WITHIN TRAININGSVERSION

weightGroupLinneQ2 <- heightWeightSubgroup(LinneDemographicsAndAnswers, "Weight", "Mean_Answer_Q2", "Trainingsversion")
plotHeightWeight(weightGroupLinneQ2) # THIS ONE SHOWS CLEAR DIFFERENCES IN WEIGHT WITHIN TRAININGSVERSION













