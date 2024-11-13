##                                  ##
#### delta_13C_models_and_plots.R ####
##                                  ##
# Description: This script contains the code to train and evaluate 
# machine learning models to predict the delta 13C values of penguins.

# need these to function independent of previous plots
adelie_col = "#c99b38"
chinstrap_col = "#595959"
gentoo_col = "#1b485e"

# Split the data into training and testing sets
set.seed(123)
data_split = initial_split(clean_penguin, prop = 0.7)
train_data = training(data_split)
test_data = testing(data_split)

# model formula for delta 13C
model_formula_13C = Delta.13.C..o.oo. ~
  Sex +
  Body.Mass..g. +
  Culmen.Length..mm. +
  Culmen.Depth..mm. +
  Flipper.Length..mm.

##                       ##
#### Linear regression ####
##                       ##
# Fit the linear regression model to the training data
lin_reg_fit_13C = linear_reg() %>% set_engine("lm") %>%
  fit( model_formula_13C,
       data = train_data)
tidy(lin_reg_fit_13C)

##           ##
#### Lasso ####
##           ##
# Define the LASSO regression model
lasso_model_13C = linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

# Define the parameter grid to tune the LASSO model
lasso_grid_13C = grid_regular(penalty(range = c(-4, -1)), levels = 20)

# Tuning the LASSO model
lasso_tune_13C = tune_grid(
  lasso_model_13C,
  model_formula_13C,
  resamples = vfold_cv(train_data, v = 5), # 5-fold cross-validation
  grid = lasso_grid_13C
  )

# Select the best LASSO model based on the lowest RMSE during tuning
best_lasso_13C = select_best(lasso_tune_13C, metric = "rmse")

# Fit the final LASSO model on the training data
final_lasso_13C = finalize_model(lasso_model_13C, best_lasso_13C) %>%
  fit(model_formula_13C, 
      data = train_data)

##                         ##
#### K-Nearest Neighbors ####
##                         ##
# Define the k-nearest neighbors regression model
knn_model_13C = nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

# Tune the k-nearest neighbors model
knn_tune_13C = tune_grid(
  knn_model_13C,
  model_formula_13C,
  resamples = vfold_cv(train_data, v = 5), # 5-fold cross-validation
  grid = grid_regular(neighbors(range = c(1, 15)), levels = 5)
  )

# Select the best k-nearest neighbors model based on the lowest RMSE during tuning
best_knn_13C = select_best(knn_tune_13C, metric = "rmse")

# Fit the KNN model on the training data
final_knn_13C = finalize_model(knn_model_13C, best_knn_13C) %>%
  fit(model_formula_13C,
      data = train_data)

##                   ##
#### Random Forest ####
##                   ##
# Define the random forest model
rf_model_13C = rand_forest() %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# Fit the random forest model to the training data
rf_fit_13C = rf_model_13C %>%
  fit(model_formula_13C,
      data = train_data)

##                                ##
#### Table of Model Evaluations ####
##                                ##
# Empty list to store model results
model_results_13C = list()

# Function to calculate RMSE, R-squared, MAE, and MAPE for each model
evaluate_model = function(model, test_data) {
  predictions = predict(model, new_data = test_data)
  
  # Calculate each metric
  rmse = rmse_vec(test_data$Delta.13.C..o.oo., predictions$.pred)
  rsq = rsq_vec(test_data$Delta.13.C..o.oo., predictions$.pred)
  mae = mae_vec(test_data$Delta.13.C..o.oo., predictions$.pred)
  mape = mape_vec(test_data$Delta.13.C..o.oo., predictions$.pred)
  
  return(c(RMSE = rmse, R_squared = rsq, MAE = mae, MAPE = mape))
}

# Evaluate each model
model_results_13C[["LASSO"]] = evaluate_model(final_lasso_13C, test_data)
model_results_13C[["Linear Regression"]] = evaluate_model(lin_reg_fit_13C, test_data)
model_results_13C[["KNN"]] = evaluate_model(final_knn_13C, test_data)
model_results_13C[["Random Forest"]] = evaluate_model(rf_fit_13C, test_data)

# Create a data frame from the model results
results_df_13C = data.frame(do.call(rbind, model_results_13C))
rownames(results_df_13C) = names(model_results_13C)

# Format the data frame using formattable into a table
# Highlights best value for each metric in green
results_table_13C = formattable(
  results_df_13C,
  list(
    RMSE = formatter("span", style = x ~ style(
      "background-color" = ifelse(x == min(results_df_13C$RMSE), "#4caf50", "white")
    )),
    R_squared = formatter("span", style = x ~ style(
      "background-color" = ifelse(x == max(results_df_13C$R_squared), "#4caf50", "white")
    )),
    MAE = formatter("span", style = x ~ style(
      "background-color" = ifelse(x == min(results_df_13C$MAE), "#4caf50", "white")
    )),
    MAPE = formatter("span", style = x ~ style(
      "background-color" = ifelse(x == min(results_df_13C$MAPE), "#4caf50", "white")
    ))
    )
  )

# Save table as an image for formatting
save_html(as.htmlwidget(results_table_13C), "plots/individual_plots/results_table_13C.html")
webshot(
  "plots/individual_plots/results_table_13C.html",
  "plots/individual_plots/results_table_13C.png",
  vwidth = 700,
  vheight = 80,
  zoom = 2)

# Load the image as a raster object
table_img_13C <- png::readPNG("plots/individual_plots/results_table_13C.png")
table_grob_13C <- grid::rasterGrob(table_img_13C, interpolate = TRUE)

##                                           ##
#### Calculate additional dfs for plotting ####
##                                           ##
# Calculate residuals
knn_pred_13C <- predict(final_knn_13C, new_data = test_data)
knn_residuals_13C <- test_data$Delta.13.C..o.oo. - knn_pred_13C$.pred

# Create a data frame for plotting with the Species column
residual_df_13C <- data.frame(
  Fitted = knn_pred_13C$.pred, 
  Residuals = knn_residuals_13C,
  Species = test_data$Species
  )

# Create a data frame for plotting with the Species column
pred_obs_df_13C <- data.frame(
  Observed = test_data$Delta.13.C..o.oo.,
  Predicted = knn_pred_13C$.pred,
  Species = test_data$Species
  )

##                                              ##
#### Extract Feature Importance for KNN Model ####
##                                              ##
# Extract the predictors (right-hand side of the formula)
predictors <- all.vars(model_formula_13C)[-1]  # Remove the response variable

# Subset data with the predictors used in the model
data_subset <- train_data[, predictors]

# Wrap the KNN model in Predictor
knn_predictor <- Predictor$new(final_knn_13C, data = data_subset, y = train_data$Delta.13.C..o.oo.)

# Calculate permutation importance
knn_importance <- FeatureImp$new(knn_predictor, loss = "rmse")

# Extract the feature importance data
importance_data <- knn_importance$results

# Rename the columns for plot
importance_data$feature <- c(
  "Culmen.Length..mm." = "Culmen Length (mm)",
  "Culmen.Depth..mm." = "Culmen Depth (mm)",
  "Flipper.Length..mm." = "Flipper Length (mm)",
  "Body.Mass..g." = "Body Mass (g)",
  "Sex" = "Sex"
  )

##           ##
#### Plots ####
##           ##
# Create the ggplot
knn_coef_13C = ggplot(
  data = importance_data,
  aes(
    x = reorder(feature, importance),
    y = importance)) +
  geom_bar(stat = "identity", fill = "#133337") +
  coord_flip() +
  labs(
    title = "",
    x = "Predictor",
    y = "Importance"
  ) +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Residuals vs. Fitted Values with color by Species
residuals_plot_13C <- ggplot(
  data = residual_df_13C,
  aes(
    x = Fitted,
    y = Residuals,
    color = Species
    )
  ) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
    ) +
  labs(
    x = "Fitted Values",
    y = "Residuals"
    ) +
  scale_color_manual(
    values = c(adelie_col,chinstrap_col,gentoo_col),
    labels = c('Adelie','Chinstrap','Gentoo')
    ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.background = element_blank())

# Predicted vs Observed with color by Species
pred_obs_plot_13C <- ggplot(
  data = pred_obs_df_13C,
  aes(
    x = Observed,
    y = Predicted,
    color = Species
    )
  ) +
  geom_point(size = 4, alpha = 0.8) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "gray") +  
  labs(
    title =expression(bold(paste("Predicted vs. Observed - ",delta,"13C"))),
    x = expression(paste("Observed ",delta,"13C")),
    y = expression(paste("Predicted ",delta,"13C"))) +
  scale_color_manual(
    values = c(adelie_col, chinstrap_col, gentoo_col),
    labels = c('Adelie','Chinstrap','Gentoo')
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(    
    legend.position = "right",
    legend.key.size = unit(3, "cm"), #change legend key size
    legend.key.height = unit(1, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=14), #change legend title font size
    legend.text = element_text(size=15) #change legend text font size
    )

# Combine the two plots
inlayed_plot_13C <- ggdraw(pred_obs_plot_13C) +
  draw_plot(residuals_plot_13C, x = 0.058, y = 0.7, width = 0.44, height = 0.23)

##                           ##
#### Save individual plots ####
##                           ##
ggsave("plots/individual_plots/knn_coef_13C.png", plot = knn_coef_13C, width = 8, height = 6, dpi = 800)
ggsave("plots/individual_plots/residuals_plot_13C.png", plot = residuals_plot_13C, width = 8, height = 6, dpi = 800)
ggsave("plots/individual_plots/pred_obs_plot_13C.png", plot = pred_obs_plot_13C, width = 8, height = 6, dpi = 800)
ggsave("plots/individual_plots/inlayed_plot_13C.png", plot = inlayed_plot_13C, width = 8, height = 6, dpi = 800)

##                       ##
#### Arrange all plots ####
##                       ##
model_plot_13C = ggarrange(
  inlayed_plot_13C,
  ggarrange(
    table_grob_13C,
    knn_coef_13C,
    ncol = 2,
    nrow = 1,
    labels = c("B", "C"),
    widths = c(9, 6)),
  #legend.grob = legend,
  #legend = "right",
  nrow = 2,
  ncol = 1,
  labels = "A",
  heights = c(4,1.5),  # Adjust width ratio to make space for the legend
  common.legend = FALSE
  )

##                   ##
#### Saving Figure ####
##                   ##
ggsave("plots/peng_delta_13C_plt.png", width = 12, height = 10, dpi = 800, bg = "white")