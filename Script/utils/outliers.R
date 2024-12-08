# Combine data for MANOVA, but keep groups separate
#MANOVA tests whether there are significant differences between groups 
#across multiple dependent variables simultaneously. In our case, the dependent variables
#are likely age, height, and weight, and the independent variable is Group (Dame vs. Linne).
#Df (Degrees of Freedom)
#Pillai (Pillai's Trace)
#approx F (F-statistic)
#num Df and den Df
#Pr(>F) (p-value)
#There is no strong evidence to suggest that Dame and Linne differ significantly
#across the variables tested (e.g., age, height, and weight).

combined_data <- bind_rows(
  dame_user_demographics %>% mutate(Group = "Dame"),
  linne_user_demographics %>% mutate(Group = "Linne")
)

manova_results <- manova(cbind(Age, Height, Weight) ~ Group, data = combined_data)
summary(manova_results)

