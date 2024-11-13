##                                  ##
#### delta_15N_models_and_plots.R ####
##                                  ##
# Description: This script contains the code to build and evaluate 
# the predictive models for the delta 15N data.

# need these to function independent of previous plots
adelie_col = "#c99b38"
chinstrap_col = "#595959"
gentoo_col = "#1b485e"

# Split the data into training and testing sets
set.seed(123)
data_split = initial_split(clean_penguin, prop = 0.7)
train_data = training(data_split)
test_data = testing(data_split)

# Define the model formula for predicting delta 15N
model_formula_15N = Delta.15.N..o.oo. ~
  Sex +
  Body.Mass..g. +
  Culmen.Length..mm. +
  Culmen.Depth..mm. +
  Flipper.Length..mm.

##                       ##
#### Linear regression ####
##                       ##
# Fit the linear regression model to the training data
lin_reg_fit_15N = linear_reg() %>% set_engine("lm") %>%
  fit( model_formula_15N,
    data = train_data)
tidy(lin_reg_fit_15N)

##           ##
#### Lasso ####
##           ##
# Define lasso model
lasso_model_15N = linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

# define the grid of penalty parameters to search over
lasso_grid_15N = grid_regular(penalty(range = c(-4, -1)), levels = 20)

# tune the lasso model
lasso_tune_15N = tune_grid(
  lasso_model_15N,
  model_formula_15N,
  resamples = vfold_cv(train_data, v = 5), # 5-fold cross-validation
  grid = lasso_grid_15N
  )

# Select the best LASSO model based on the lowest RMSE during tuning
best_lasso_15N = select_best(lasso_tune_15N, metric = "rmse")

# Fitting the final LASSO model on the training data
final_lasso_15N = finalize_model(lasso_model_15N, best_lasso_15N) %>%
  fit(model_formula_15N, 
      data = train_data)

##                         ##
#### K-Nearest Neighbors ####
##                         ##
# Define the k-nearest neighbors regression model
knn_model_15N = nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

# Tune the k-nearest neighbors model 
knn_tune_15N = tune_grid(
  knn_model_15N,
  model_formula_15N,
  resamples = vfold_cv(train_data, v = 5), # 5-fold cross-validation
  grid = grid_regular(neighbors(range = c(1, 15)), levels = 5)
  )

# Select the best k-nearest neighbors model based on the lowest RMSE during tuning
best_knn_15N = select_best(knn_tune_15N, metric = "rmse")

# Fit the final knn model on the training data
final_knn_15N = finalize_model(knn_model_15N, best_knn_15N) %>%
  fit(model_formula_15N,
      data = train_data)

##                   ##
#### Random Forest ####
##                   ##
# Define the random forest model
rf_model_15N = rand_forest() %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# Fit the random forest model to the training data
rf_fit_15N = rf_model_15N %>%
  fit(model_formula_15N,
      data = train_data)

##                                ##
#### Table of Model Evaluations ####
##                                ##
# make a list to store the results of each model
model_results_15N = list()

# Function to calculate RMSE, R-squared, MAE, and MAPE for a model
evaluate_model = function(model, test_data) {
  predictions = predict(model, new_data = test_data)
  
  # Calculate each metric
  rmse = rmse_vec(test_data$Delta.15.N..o.oo., predictions$.pred)
  rsq = rsq_vec(test_data$Delta.15.N..o.oo., predictions$.pred)
  mae = mae_vec(test_data$Delta.15.N..o.oo., predictions$.pred)
  mape = mape_vec(test_data$Delta.15.N..o.oo., predictions$.pred)
  
  return(c(RMSE = rmse, R_squared = rsq, MAE = mae, MAPE = mape))
}

# Evaluate each model
model_results_15N[["LASSO"]] = evaluate_model(final_lasso_15N, test_data)
model_results_15N[["Linear Regression"]] = evaluate_model(lin_reg_fit_15N, test_data)
model_results_15N[["KNN"]] = evaluate_model(final_knn_15N, test_data)
model_results_15N[["Random Forest"]] = evaluate_model(rf_fit_15N, test_data)

# Create a data frame from the model results
results_df_15N = data.frame(do.call(rbind, model_results_15N))
rownames(results_df_15N) = names(model_results_15N)

# Format the data frame using formattable
# each best metric is highlighted in green
results_table_15N = formattable(
  results_df_15N,
  list(
    RMSE = formatter("span", style = x ~ style(
      "background-color" = ifelse(x == min(results_df_15N$RMSE), "#4caf50", "white")
    )),
    R_squared = formatter("span", style = x ~ style(
      "background-color" = ifelse(x == max(results_df_15N$R_squared), "#4caf50", "white")
    )),
    MAE = formatter("span", style = x ~ style(
      "background-color" = ifelse(x == min(results_df_15N$MAE), "#4caf50", "white")
    )),
    MAPE = formatter("span", style = x ~ style(
      "background-color" = ifelse(x == min(results_df_15N$MAPE), "#4caf50", "white")
    ))
    )
  )

# Save table as an image for formatting
save_html(as.htmlwidget(results_table_15N), "plots/individual_plots/results_table_15N.html")
webshot("plots/individual_plots/results_table_15N.html", "plots/individual_plots/results_table_15N.png", vwidth = 700, vheight = 80, zoom = 2)

# Load the image as a raster object
table_img_15N <- png::readPNG("plots/individual_plots/results_table_15N.png")
table_grob_15N <- grid::rasterGrob(table_img_15N, interpolate = TRUE)

##                                           ##
#### Calculate additional dfs for plotting ####
##                                           ##
rf_pred_15N <- predict(rf_fit_15N, new_data = test_data)
# Calculate residuals
rf_residuals_15N <- test_data$Delta.15.N..o.oo. - rf_pred_15N$.pred

# Create a data frame for plotting with the Species column
residual_df_15N <- data.frame(
  Fitted = rf_pred_15N$.pred, 
  Residuals = rf_residuals_15N,
  Species = test_data$Species
  )

# Create a data frame for plotting with the Species column
pred_obs_df_15N <- data.frame(
  Observed = test_data$Delta.15.N..o.oo.,
  Predicted = rf_pred_15N$.pred,
  Species = test_data$Species
  )

# Extract variable importance
importance_values <- rf_fit_15N$fit$variable.importance
importance_df <- data.frame(
  Predictor = names(importance_values),
  Importance = importance_values
  )

# Sort by importance
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

importance_df$Predictor <- recode(
  importance_df$Predictor,
  "Sex" = "Sex",
  "Body.Mass..g." = "Body Mass (g)",
  "Culmen.Length..mm." = "Culmen Length (mm)",
  "Culmen.Depth..mm." = "Culmen Depth (mm)",
  "Flipper.Length..mm." = "Flipper Length (mm)"
  )

##           ##
#### Plots ####
##           ##
coef_plot_15N = ggplot(
  data = importance_df[1:5, ],
  aes(
    x = reorder(Predictor, Importance),
    y = Importance)
    ) +
  geom_bar(stat = "identity", fill = "#133337") +
  coord_flip() +
  labs(
    title = "", 
    x = "Predictor",
    y = "Importance") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Residuals vs. Fitted Values with color by Species
residuals_plot_15N <- ggplot(
  data = residual_df_15N,
  aes(
    x = Fitted,
    y = Residuals,
    color = Species
    )
  ) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Fitted Values",
    y = "Residuals") +
  scale_color_manual(
    values = c(adelie_col, chinstrap_col, gentoo_col),
    labels = c('Adelie','Chinstrap','Gentoo')
    ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 8),  # Change axis text size
        axis.title = element_text(size = 8),
        panel.background = element_blank())

# Predicted vs Observed with color by Species
pred_obs_plot_15N <- ggplot(
  data = pred_obs_df_15N,
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
    color = "gray"
    ) +  
  labs(
    title =expression(bold(paste("Predicted vs. Observed - ",delta,"15N"))),
    x = expression(paste("Observed ",delta,"15N")),
    y = expression(paste("Predicted ",delta,"15N"))
    ) +
  scale_color_manual(
    values = c(adelie_col, chinstrap_col, gentoo_col),
    labels = c('Adelie','Chinstrap','Gentoo')
    ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(    
    legend.position = "right",
    legend.key.size = unit(5, "cm"), #change legend key size
    legend.key.height = unit(1, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=14), #change legend title font size
    legend.text = element_text(size=15) #change legend text font size
  )

# inlay plots
inlayed_plot_15N <- ggdraw(pred_obs_plot_15N) +
  draw_plot(residuals_plot_15N, x = 0.41, y = 0.07, width = 0.44, height = 0.23)

##                    ##
#### Save the plots ####
##                    ##
# Save coefficient importance plot
ggsave("plots/individual_plots/coef_plot_15N.png", plot = coef_plot_15N, width = 8, height = 6, dpi = 800)

# Save residuals vs. fitted values plot
ggsave("plots/individual_plots/residuals_plot_15N.png", plot = residuals_plot_15N, width = 8, height = 6, dpi = 800)

# Save predicted vs. observed values plot
ggsave("plots/individual_plots/pred_obs_plot_15N.png", plot = pred_obs_plot_15N, width = 8, height = 6, dpi = 800)

# Save the inlayed plot combining predicted vs. observed and residuals plot
ggsave("plots/individual_plots/inlayed_plot_15N.png", plot = inlayed_plot_15N, width = 8, height = 6, dpi = 800)

##                       ##
#### Arrange all plots ####
##                       ##
model_plot = ggarrange(
  inlayed_plot_15N,
  ggarrange(
    table_grob_15N,
    coef_plot_15N,
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
ggsave("plots/peng_delta_15N_plot.png", width = 12, height = 10, dpi = 800, bg = "white")
