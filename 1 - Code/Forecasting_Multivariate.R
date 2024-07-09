library(tidyverse)
library(xgboost)

# Load the dataset
duomenys <- read.csv("Y1.csv")
duomenys$date <- as.Date(paste0(duomenys$date, "01"), format = "%YM%m%d")

# Set the Y variable
y <- duomenys$keleiviu_sk

# Set the X variables
x <- select(duomenys, -keleiviu_sk, -date)

# Split the data into training and testing sets
set.seed(123) # For reproducibility
train_indices <- sample(nrow(duomenys), 0.8 * nrow(duomenys))
train_set <- duomenys[train_indices, ]
test_set <- duomenys[-train_indices, ]

# Function to calculate prediction error
prognozavimo_klaida <- function(y_tikrasis, y_prognozuojamas) {
  rmse <- sqrt(mean((y_tikrasis - y_prognozuojamas)^2))
  mape <- mean(abs((y_tikrasis - y_prognozuojamas) / y_tikrasis))
  return(c(rmse = rmse, mape = mape))
}

# Search for the best Y and X pairs
geriausios_poros <- vector("list", length(x))
for (i in 1:length(x)) {
  # Train the XGBoost model
  modelis <- xgboost(data = as.matrix(train_set[, i]), label = train_set$keleiviu_sk, nrounds = 100)
  
  # Predict the number of passengers in the test set
  y_prognozuojamas <- predict(modelis, as.matrix(test_set[, i]))
  
  # Calculate prediction error
  klaida <- prognozavimo_klaida(test_set$keleiviu_sk, y_prognozuojamas)
  
  # Save the best pair
  geriausios_poros[[i]] <- list(x = names(x)[i], rmse = klaida[1], mape = klaida[2])
}

# Convert to data frame
geriausios_poros_df <- bind_rows(geriausios_poros)

# Order geriausios_poros by rmse
geriausios_poros_df <- geriausios_poros_df[order(geriausios_poros_df$rmse), ]

# Print the best Y and X pairs
print(geriausios_poros_df)

# Train the final model with the best pair
best_x <- geriausios_poros_df$x[1]
final_model <- xgboost(data = as.matrix(duomenys[, best_x]), label = duomenys$keleiviu_sk, nrounds = 100)

# Predict for the entire year
all_dates <- seq(min(duomenys$date), max(duomenys$date), by = "month")
predicted_values <- predict(final_model, as.matrix(select(duomenys, best_x)))

# Plot the graph
plot_data <- data.frame(date = duomenys$date, actual = y, predicted = predicted_values)

ggplot(plot_data, aes(x = date)) +
  geom_line(aes(y = actual), color = "blue", linetype = "solid", size = 1) +
  geom_line(aes(y = predicted), color = "red", linetype = "dashed", size = 1) +
  labs(x = "Date", y = "Passenger Count", title = "Actual vs. Predicted Passenger Count") +
  theme_minimal()
