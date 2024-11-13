
# Extract variable importance
importance_values <- rf_fit$fit$variable.importance
importance_df <- data.frame(
  Predictor = names(importance_values),
  Importance = importance_values
)

# Sort by importance
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

ggplot(importance_df[1:5, ], aes(x = reorder(Predictor, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#") +
  coord_flip() +
  labs(title = "Predictors by Importance in Random Forest Model", x = "Predictor", y = "Importance") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Residuals vs. Fitted Values with color by Species
residuals_plot <- ggplot(residual_df, aes(x = Fitted, y = Residuals, color = Species)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  scale_color_manual(values = c(adelie_col, chinstrap_col, gentoo_col), labels = c('Adelie','Chinstrap','Gentoo')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 8),  # Change axis text size
        axis.title = element_text(size = 8),
        panel.background = element_blank())

# Predicted vs Observed with color by Species
pred_obs_plot <- ggplot(pred_obs_df, aes(x = Observed, y = Predicted, color = Species)) +
  geom_point(size=2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  
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

# Use 'ggdraw' and 'insert_ggplot' to create the inset plot
combined_plot <- ggdraw(pred_obs_plot) +
  draw_plot(residuals_plot, x = 0.4, y = 0.088, width = 0.44, height = 0.23)  # Adjust position and size as needed

print(combined_plot)
