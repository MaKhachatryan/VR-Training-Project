#calculations for correlation
# Select numeric columns (exclude Answer_Q1 and Answer_Q2 temporarily, also round number)
measurement_columns <- dame_linne_combined %>%
  select(where(is.numeric)) %>%
  select(-c(Answer_Q1, Answer_Q2, Round_number)) %>% 
  colnames()
print(measurement_columns)

# Compute correlations between Answer_Q1 and each PMD column
cor_Q1_PMD <- dame_linne_combined %>%
  summarise(across(all_of(measurement_columns), ~ cor(Answer_Q1, .x, use = "complete.obs", method = "spearman")))
#now with Answer_Q2
cor_Q2_PMD <- dame_linne_combined %>%
  summarise(across(all_of(measurement_columns), ~ cor(Answer_Q2, .x, use = "complete.obs", method = "spearman")))

#making a tale to showcase the correlation results
combined_tibble <- bind_rows(
  cor_Q1_PMD %>% mutate(row_name = "Answer_Q1"),  # Add row name for tibble1
  cor_Q2_PMD %>% mutate(row_name = "Answer_Q2")   # Add row name for tibble2
)%>%
  select(row_name, everything())


#save it as a csv file
write.csv(combined_tibble, "tibble_output.csv", row.names = FALSE)  # Base R

#not final just trying some stuff, turned out it was pretty stupid

#for the 2nd question "over the rounds"
# Group by round_number and calculate correlation
correlation_per_round <- dame_linne_combined %>%
  group_by(Round_number) %>%
  summarise(
    across(all_of(measurement_columns), ~ cor(Answer_Q1, .x, use = "complete.obs", method = "spearman"))
  )

print(correlation_per_round)





# Reshape data to long format
data_long <- correlation_per_round %>%
  pivot_longer(
    cols = -Round_number,             # Keep Round_number as is
    names_to = "Measurement",         # Create a new column for measurement names
    values_to = "Value"               # Create a column for measurement values
  )

# Create the line plot
ggplot(data_long, aes(x = Round_number, y = Value, color = Measurement)) +
  geom_line(size = 1) +
  labs(
    title = "Measurement Trends Over Rounds",
    x = "Round Number",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = 1:9) +
  scale_color_brewer(palette = "Set1") 