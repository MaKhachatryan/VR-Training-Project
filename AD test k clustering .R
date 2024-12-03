## AD
## test run k-clustering for Q3Q4

## ---------- age - height ----------------
### k cluster age - height dame !!! only male
library(readxl)
dame_raw <- read_excel("Data/rawData/SimTLXAnswers_Dame.xlsx")
no.na_dame <- dame_raw[complete.cases(dame_raw), ]
no.na_dame <- no.na_dame[no.na_dame$Gender == "M", ]

cluster_dame <- kmeans(no.na_dame[, c("Weight", "Height")], centers = 2)
height_age <- plot(y = no.na_dame$Weight, x = no.na_dame$Height)

cluster_dame_df <- data.frame(no.na_dame$Weight, no.na_dame$Height, cluster_dame$cluster)
colnames(cluster_dame_df) <- c("weight", "height", "cluster")

library(ggplot2)
ggplot(cluster_dame_df, aes(x = height, y = weight, color = as.factor(cluster))) + 
  geom_point()

## ----------- check stress level or physical load
# idea : probably we dont need very complicated subsets
# maybe only compare one dimensional supgroup (gender, group adaptive/non, ...)
# 2 dimensional subgroups that worth-trying : weight-height (might indicate ppl 
## ...with unhealthy figures have more stress or physical load)

### AD comment: i already run 2 dimensions kclustering between weight and height...
### ...we might merge and compare between those 2 clusters
