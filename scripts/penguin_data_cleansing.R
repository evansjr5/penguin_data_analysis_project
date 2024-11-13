#### File: penguin_data_cleaning.R ####
### Description: 
## This file produces clean penguin data by removing NAs and empty strings present in some variables

# Display Missing data
plot_missing(penguins_lter)
ggsave("plots/individual_plots/missing_data_plot.png")

# Subset of data that does not contain comments, NAs, or empty strings
clean_penguin <- penguins_lter %>%
  subset(select = c(Species:Delta.13.C..o.oo.)) %>%
  drop_na() %>%
  filter(Sex %in% c("MALE", "FEMALE") & Sex != "")

# Visualize data after Filters
plot_missing(clean_penguin)
ggsave("plots/individual_plots/missing_data_after_filter_plot.png")
# Summary
capture.output(summary(clean_penguin), file = "plots/individual_plots/summary_clean_penguin.txt")

# Write clean data to a CSV file
write_csv(clean_penguin,"clean_penguin_data.csv")

# Basic exploratory data analysis plots
png("plots/individual_plots/body_mass_histogram.png")
hist(clean_penguin$Body.Mass..g., main = "Body Mass Distribution", xlab = "Body Mass (g)")
dev.off()

png("plots/individual_plots/delta_13_C_histogram.png")
hist(clean_penguin$Delta.13.C..o.oo., main = "Delta 13 C Distribution", xlab = "Delta 13 C (o/oo)")
dev.off()

png("plots/individual_plots/delta_15_N_histogram.png")
hist(clean_penguin$Delta.15.N..o.oo., main = "Delta 15 N Distribution", xlab = "Delta 15 N (o/oo)")
dev.off()

plot_bar(clean_penguin)
ggsave("plots/individual_plots/bar_plot.png")

plot_histogram(clean_penguin)
ggsave("plots/individual_plots/histogram_plot.png")

plot_correlation(clean_penguin)
ggsave("plots/individual_plots/correlation_plot.png")

plot_scatterplot(split_columns(clean_penguin)$continuous, by = "Culmen.Length..mm.", sampled_rows = 1000L)
ggsave("plots/individual_plots/scatterplot_culmen_length.png")
