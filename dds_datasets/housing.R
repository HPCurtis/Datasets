library(tidymodels)
library(tidyr)
library(ggplot2)




url <- "https://raw.githubusercontent.com/ageron/handson-ml2/master/datasets/housing/housing.csv"
df <- read.csv(url)
df_oceanprox_non <- subset(df, select = -ocean_proximity)
# Look at the first few rows.
print(head(df))
pritn(str(df))

# Get numebr nas
na_counts <- colSums(is.na(df))
print(na_counts)

table(df$ocean_proximity)

plots <- list()

# Loop through each column in the dataframe
for (col in names(subset(df, select = -ocean_proximity))) {
  # Create histogram plot for each column
  plot <- ggplot(df, aes(x = .data[[col]])) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", col),
         x = col,
         y = "Frequency")
  
  # Add plot to the list
  plots[[col]] <- plot
}

init <- initial_split(df, prop = 0.80)
train_split <- training(init)
test_split <- testing(init)
test_split

# create income categories.
bins <- c(0, 1.5, 3.0, 4.5, 6.0, Inf)
labels <- c(1, 2, 3, 4, 5)

# Create the new column 'income_cat' using cut()
df$income_cat <- cut(df$median_income, breaks = bins, labels = labels, right = FALSE)

strat_init <- initial_split(df, prop = 0.80, strata = income_cat)
strat_train_split <- training(strat_init)
strat_test_split <- testing(strat_init)
strat_test_split

atts <- c("median_house_value", "median_income", "total_rooms",
              "housing_median_age")

# correltions
x <- pairs(df[,atts])


ggplot(df, aes(x = median_income, y = median_house_value)) +
  geom_point()
