# Split the data into training and testing sets
set.seed(123)
data_split <- initial_split(clean_penguin, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

###########################
#### Linear regression ####
###########################
# Fit the linear regression model to the training data
lin_reg_fit <- linear_reg() %>% set_engine("lm") %>%
  fit(
    Delta.15.N..o.oo. ~  
      Sex +
      Body.Mass..g. +
      Culmen.Length..mm. +
      Culmen.Depth..mm. +
      Flipper.Length..mm.,
    data = train_data)
tidy(lin_reg_fit)

# Make predictions on the test data using the trained linear regression model
lin_reg_pred <- predict(lin_reg_fit, new_data = test_data)

# Evaluate the linear regression model performance on the test data
rmse_vec(test_data$Delta.15.N..o.oo., lin_reg_pred$.pred)
rsq_vec(test_data$Delta.15.N..o.oo., lin_reg_pred$.pred)

###############
#### Lasso ####
###############
# The penalty parameter controls the amount of shrinkage applied to the model coefficients
# We'll tune this parameter later
lasso_model <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

# Define the parameter grid to tune the LASSO model
# We'll try 20 different values of the penalty parameter
lasso_grid <- grid_regular(penalty(range = c(-4, -1)), levels = 20)

#### 15N Lasso Model ####
# Tune the LASSO model using 5-fold cross-validation
# This will help us find the optimal value of the penalty parameter
lasso_tune_15N <- tune_grid(
  lasso_model,
  Delta.15.N..o.oo. ~  
    Sex +
    Body.Mass..g. +
    Culmen.Length..mm. +
    Culmen.Depth..mm. +
    Flipper.Length..mm.,
  resamples = vfold_cv(train_data, v = 5),
  grid = lasso_grid
)

# Select the best LASSO model based on the lowest RMSE during tuning
best_lasso_15N <- select_best(lasso_tune_15N, metric = "rmse")

# Fit the final LASSO model on the full training data
# Using the optimal penalty parameter found during tuning
final_lasso_15N <- finalize_model(lasso_model, best_lasso_15N) %>%
  fit(Delta.15.N..o.oo. ~
        Sex +
        Body.Mass..g. +
        Culmen.Length..mm. +
        Culmen.Depth..mm. +
        Flipper.Length..mm., 
      data = train_data)

# Print the variable names and coefficients
lasso_coefs_15N <- coef(final_lasso_15N$fit, s = best_lasso_15N$penalty)
lasso_coefs_15N

# Make predictions on the test data using the trained LASSO model
# Get the predicted probabilities instead of just the predictions
lasso_pred_15N <- predict(final_lasso_15N, new_data = test_data)

# Evaluate the LASSO model performance on the test data
rmse_vec(test_data$Delta.15.N..o.oo., lasso_pred_15N$.pred)
rsq_vec(test_data$Delta.15.N..o.oo., lasso_pred_15N$.pred)

# Calculate residuals
residuals_15N <- test_data$Delta.15.N..o.oo. - lasso_pred_15N$.pred

# Create a data frame for plotting with the Species column
residual_df <- data.frame(
  Fitted = lasso_pred_15N$.pred, 
  Residuals = residuals_15N,
  Species = test_data$Species
)

# Residuals vs. Fitted Values with color by Species
ggplot(residual_df, aes(x = Fitted, y = Residuals, color = Species)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = expression(
    bold(
      paste("Residuals vs. Fitted Values - ",delta,"15N")
      )
    ),
     x = "Fitted Values",
     y = "Residuals") +
  scale_color_manual(
       values = c(adelie_col, chinstrap_col, gentoo_col),
       labels = c('Adelie','Chinstrap','Gentoo')
       ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Create a data frame for plotting with the Species column
pred_obs_df <- data.frame(
  Observed = test_data$Delta.15.N..o.oo.,
  Predicted = lasso_pred_15N$.pred,
  Species = test_data$Species
)

# Predicted vs Observed with color by Species
ggplot(pred_obs_df, aes(x = Observed, y = Predicted, color = Species)) +
  geom_point(size=2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  # Add the identity line
  labs(title =expression(
    bold(
      paste("Predicted vs. Observed - ",delta,"15N")
      )
    ),
     x = expression(paste("Observed ",delta,"15N")),
     y = expression(paste("Predicted ",delta,"15N"))) +
  scale_color_manual(
    values = c(adelie_col, chinstrap_col, gentoo_col),
    labels = c('Adelie','Chinstrap','Gentoo')
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))





#### 13C Lasso Model ####
# The penalty parameter controls the amount of shrinkage applied to the model coefficients
# We'll tune this parameter later
lasso_model <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

# Define the parameter grid to tune the LASSO model
# We'll try 20 different values of the penalty parameter
lasso_grid <- grid_regular(penalty(range = c(-4, -1)), levels = 20)

# Tune the LASSO model using 5-fold cross-validation
# This will help us find the optimal value of the penalty parameter
lasso_tune_13C <- tune_grid(
  lasso_model,
  Delta.13.C..o.oo. ~  Species + Island + Culmen.Length..mm. + Culmen.Depth..mm. + Flipper.Length..mm.,
  resamples = vfold_cv(train_data, v = 5),
  grid = lasso_grid
  )

# Select the best LASSO model based on the lowest RMSE during tuning
best_lasso_13C <- select_best(lasso_tune_13C, metric = "rmse")

# Fit the final LASSO model on the full training data
# Using the optimal penalty parameter found during tuning
final_lasso_13C <- finalize_model(lasso_model, best_lasso_13C) %>%
  fit(Delta.13.C..o.oo. ~  Species + Island + Culmen.Length..mm. + Culmen.Depth..mm. + Flipper.Length..mm., data = train_data)

# Print the variable names and coefficients
lasso_coefs_13C <- coef(final_lasso_13C$fit, s = best_lasso_13C$penalty)
lasso_coefs_13C

# Make predictions on the test data using the trained LASSO model
# Get the predicted probabilities instead of just the predictions
lasso_pred_13C <- predict(final_lasso_13C, new_data = test_data)

# Evaluate the LASSO model performance on the test data
rmse_vec(test_data$Delta.13.C..o.oo., lasso_pred_13C$.pred)
rsq_vec(test_data$Delta.13.C..o.oo., lasso_pred_13C$.pred)

plot(lasso_pred_13C$.pred, lasso_pred_13C$.pred - test_data$Delta.13.C..o.oo., main = "Residuals vs Fitted - LASSO Regression")
qqnorm(lasso_pred_13C$.pred - test_data$Delta.13.C..o.oo., main = "QQ Plot - LASSO Regression")
qqline(lasso_pred_13C$.pred - test_data$Delta.13.C..o.oo.)
plot(lasso_pred_13C$.pred, sqrt(abs(lasso_pred_13C$.pred - test_data$Delta.13.C..o.oo.)), main = "Scale-Location Plot - LASSO Regression")


# Calculate residuals
residuals <- test_data$Delta.15.N..o.oo. - lasso_pred_15N

ggplot(residuals, aes(x = fitted.values(model), y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals",
       title = "Residuals vs. Fitted Values")


library(ggplot2)

# Extract non-zero coefficients
nonzero_coefs <- lasso_coefs_15N[lasso_coefs_15N != 0]
nonzero_coefs
# Create a data frame for plotting
coef_df <- data.frame(
  Variable = names(nonzero_coefs),
  Coefficient = as.numeric(nonzero_coefs)
)

# Plot the coefficients
ggplot(coef_df, aes(x = Variable, y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "LASSO Model Variable Importance",
       x = "Predictor Variables",
       y = "Coefficient Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Extract non-zero coefficients (corrected)
nonzero_coefs <- lasso_coefs_15N@x  # Access the non-zero values directly

# Get the corresponding variable names
variable_names <- rownames(lasso_coefs_15N)[lasso_coefs_15N@i + 1]

# Create a data frame for plotting
coef_df <- data.frame(
  Variable = variable_names,
  Coefficient = as.numeric(nonzero_coefs)
)

coef_df <- coef_df[coef_df$Variable != "(Intercept)", ]

# Now you can use coef_df to create your variable importance plot
ggplot(coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "LASSO Model Variable Importance",
       x = "Predictor Variables",
       y = "Coefficient Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate residuals
residuals_15N <- test_data$Delta.15.N..o.oo. - lasso_pred_15N$.pred

# Residuals vs. Fitted Values
ggplot(data.frame(Fitted = lasso_pred_15N$.pred, Residuals = residuals_15N),
       aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

# Residuals vs. Specific Predictor (example: Species)
ggplot(data.frame(Species = test_data$Island, Residuals = residuals_15N),
       aes(x = Species, y = Residuals)) +
  geom_boxplot() +
  labs(title = "Residuals vs. Species",
       x = "Species",
       y = "Residuals")







#### KNN ####
# K-Nearest Neighbors
#####################
# Define the k-nearest neighbors regression model
knn_model <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

# Tune the k-nearest neighbors model using 5-fold cross-validation
knn_tune <- tune_grid(
  knn_model,
  Delta.15.N..o.oo. ~  
    Species +
    Island + 
    Sex +
    Body.Mass..g. +
    Culmen.Length..mm. +
    Culmen.Depth..mm. +
    Flipper.Length..mm.,
  resamples = vfold_cv(train_data, v = 5),
  grid = grid_regular(neighbors(range = c(1, 15)), levels = 5)
)

# Select the best k-nearest neighbors model based on the lowest RMSE during tuning
best_knn <- select_best(knn_tune, metric = "rmse")

# Fit the final k-nearest neighbors model on the full training data
final_knn <- finalize_model(knn_model, best_knn) %>%
  fit(Delta.15.N..o.oo. ~  
        Sex +
        Body.Mass..g. +
        Culmen.Length..mm. +
        Culmen.Depth..mm. +
        Flipper.Length..mm., data = train_data)

# Make predictions on the test data using the trained k-nearest neighbors model
knn_pred <- predict(final_knn, new_data = test_data)

# Evaluate the k-nearest neighbors model performance on the test data
rmse_vec(test_data$Delta.15.N..o.oo., knn_pred$.pred)
rsq_vec(test_data$Delta.15.N..o.oo., knn_pred$.pred)

# Calculate residuals
residuals_15N <- test_data$Delta.15.N..o.oo. - knn_pred$.pred

# Create a data frame for plotting with the Species column
residual_df <- data.frame(
  Fitted = knn_pred$.pred, 
  Residuals = residuals_15N,
  Species = test_data$Species
)

# Residuals vs. Fitted Values with color by Species
ggplot(residual_df, aes(x = Fitted, y = Residuals, color = Species)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values - Delta 15N",
       x = "Fitted Values",
       y = "Residuals") +
  theme_bw() 

# Create a data frame for plotting with the Species column
pred_obs_df <- data.frame(
  Observed = test_data$Delta.15.N..o.oo.,
  Predicted = lasso_pred_15N$.pred,
  Species = test_data$Species
)

# Predicted vs Observed with color by Species
ggplot(pred_obs_df, aes(x = Observed, y = Predicted, color = Species)) +
  geom_point(size=2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  # Add the identity line
  labs(title = "Predicted vs. Observed - Delta 15N",
       x = "Observed Delta 15N",
       y = "Predicted Delta 15N") +
  scale_color_manual(
    values = c(adelie_col, chinstrap_col, gentoo_col),
    labels = c('Adelie','Chinstrap','Gentoo')
  ) +
  theme_bw()



# Define the random forest model
rf_model <- rand_forest() %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# Fit the random forest model to the training data
rf_fit <- rf_model %>%
  fit(Delta.15.N..o.oo. ~  
        Sex +
        Body.Mass..g. +
        Culmen.Length..mm. +
        Culmen.Depth..mm. +
        Flipper.Length..mm.,
      data = train_data)

# Make predictions on the test data using the trained random forest model
rf_pred <- predict(rf_fit, new_data = test_data)

# Print the variable importance plot
vip(rf_fit)

# Evaluate the random forest model performance on the test data
rmse_vec(test_data$Delta.15.N..o.oo., rf_pred$.pred)
rsq_vec(test_data$Delta.15.N..o.oo., rf_pred$.pred)



lin_reg_fit <- linear_reg() %>% set_engine("lm") %>%
  fit(Delta.15.N..o.oo. ~  
        Sex +
        Body.Mass..g. +
        Culmen.Length..mm. +
        Culmen.Depth..mm. +
        Flipper.Length..mm.,
      data = train_data)
tidy(lin_reg_fit)

# Make predictions on the test data using the trained linear regression model
lin_reg_pred <- predict(lin_reg_fit, new_data = test_data)

# Evaluate the linear regression model performance on the test data
rmse_vec(test_data$Delta.15.N..o.oo., lin_reg_pred$.pred)
rsq_vec(test_data$Delta.15.N..o.oo., lin_reg_pred$.pred)


library(tidymodels)
library(broom)

# ... (Your code for different regression models) ...

# Assuming you have these models:
# - lasso_model (LASSO)
# - lm_model (Linear Regression)
# - ... (other models)

# Create a list to store model evaluations
model_results <- list()

# Function to calculate RMSE and R-squared for a model
evaluate_model <- function(model, test_data) {
  predictions <- predict(model, new_data = test_data)
  rmse <- rmse_vec(test_data$Delta.15.N..o.oo., predictions$.pred)
  rsq <- rsq_vec(test_data$Delta.15.N..o.oo., predictions$.pred)
  return(c(RMSE = rmse, R_squared = rsq))
}

# Evaluate each model
model_results[["LASSO"]] <- evaluate_model(final_lasso_15N, test_data)
model_results[["Linear Regression"]] <- evaluate_model(lin_reg_fit, test_data)
model_results[["Linear Regression"]] <- evaluate_model(lin_reg_fit, test_data)
# ... (Add evaluations for other models) ...

# Create an R table for results
results_table <- do.call(rbind, model_results) %>% 
  t() %>% 
  as.data.frame()

# Print the table
print(results_table)


library(formattable)
library(tidymodels)
library(broom)

# ... (Your code for different regression models) ...

# Assuming you have these models:
# - lasso_model (LASSO)
# - lm_model (Linear Regression)
# - ... (other models)

# Create a list to store model evaluations
model_results <- list()

# Function to calculate RMSE and R-squared for a model
evaluate_model <- function(model, test_data) {
  predictions <- predict(model, new_data = test_data)
  rmse <- rmse_vec(test_data$Delta.15.N..o.oo., predictions$.pred)
  rsq <- rsq_vec(test_data$Delta.15.N..o.oo., predictions$.pred)
  return(c(RMSE = rmse, R_squared = rsq))
}

# Evaluate each model
model_results[["LASSO"]] <- evaluate_model(final_lasso_15N, test_data)
model_results[["Linear Regression"]] <- evaluate_model(lin_reg_fit, test_data)
# ... (Add evaluations for other models) ...

# Create a data frame from the model results
results_df <- data.frame(do.call(rbind, model_results))
rownames(results_df) <- names(model_results)

# Format the data frame using formattable
results_table <- formattable(
  results_df, 
  list(
    RMSE = color_tile("lightpink", "lightgreen"),
    R_squared = color_bar("lightblue")
  )
)


# Format the data frame using formattable
results_table <- formattable(
  results_df,
  list(
    RMSE = formatter("span", style = x ~ style(
      "background-color" = ifelse(x == min(results_df$RMSE), "lightgreen", "white")
    )),
    R_squared = formatter("span", style = x ~ style(
      "background-color" = ifelse(x == max(results_df$R_squared), "lightblue", "white")
    ))
  )
)

# Print the formatted table
print(results_table)
 #### K means ####
k5 <- kmeans(iris[, 1:4], centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = iris[, 1:4]) + ggtitle("k = 2")

k <- kmeans(clean_penguin[,c(8:11, 13:14)], centers = 3, nstart = 25)

# plots to compare
fviz_cluster(k, geom = "point", data = clean_penguin[, c(8:11, 13:14)]) + ggtitle("k = 2")


