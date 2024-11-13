##                        ##
#### descritive_plots.R ####
##                        ##
# Description:
# This file produces 

##          ##
#### Prep ####
##          ##
# Setting global theme for ggarange to work and color sets
theme_set(theme_pubr())

adelie_col = "#c99b38"
chinstrap_col = "#595959"
gentoo_col = "#1b485e"

#Compiling info for antarctic map pulled from Wikepedia on island location
antarctica <- map_data("world", region = "Antarctica")

df_penguinloc <-
  tibble(
    island = c("Dream", "Biscoe", "Torgersen"),
    lat_y = c(-64.7333, -65.4333, -64.7666636),
    long_x = c(-64.2333, -65.5000, -64.083333)
    )

##                             ##
#### Panel A: Antarctica Map ####
##                             ##
map = ggplot(
  antarctica,
  aes(
    x = long,
    y= lat,
    group = group
    )
  ) +
  geom_polygon(
    fill = "#506B8E",
    alpha = .5
    ) +
  coord_map(
    "ortho",
    orientation = c(-90, 0, 0),
    xlim = c(-62, -59.8),
    ylim = c(-71, -63)
    ) +
  geom_point(
    df_penguinloc,
    mapping=aes(
      long_x,
      lat_y,
      group = 1,
      shape = island
      ),
    size = 10,
    alpha =.8
    ) +
  scale_shape_manual(values = c(15, 16, 17)) +
  labs() +
  theme_map() +
  theme(legend.position = "none")

##                                      ##
#### Panel B: Species Count by Island ####
##                                      ##
isl_specie_cnt = ggplot(
  clean_penguin,
  aes(
    x=Island,
    fill = Species
    )
  ) + 
  geom_bar(width = 0.6) +
  theme_cowplot() +
  xlab("Island") +
  ylab("Counts") +
  scale_fill_manual(
    values = c(adelie_col, chinstrap_col, gentoo_col),
    labels = c('Adelie','Chinstrap','Gentoo')) +
  theme(    
    legend.position = "none"
    ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
    ) +
  scale_x_discrete(name = "")

##                                        ##
#### Panel C: Body Mass vs Culmen Depth ####
##                                        ##
bg_cm_depth = ggplot(
  clean_penguin,
  aes(
    x = Body.Mass..g.,
    y = Culmen.Depth..mm.,
    color = Species,
    shape = Island)
    ) + 
  geom_point(size = 2) +
  theme_classic() +
  xlab("Body Mass (g)") +
  ylab("Culmen Depth (mm)") +
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_manual(
    values = c(adelie_col, chinstrap_col, gentoo_col),
    labels = c('Adelie','Chinstrap','Gentoo')
    )+
  guides(
    color = guide_legend(override.aes = list(size = 8)),
    shape = guide_legend(override.aes = list(size = 8))
    ) +
  theme(    
    legend.position = "right",
    legend.key.size = unit(2, "cm"), #change legend key size
    legend.key.height = unit(1, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=14), #change legend title font size
    legend.text = element_text(size=15) #change legend text font size
    )

##                                       ##
#### Legend work and finishing Panel C ####
##                                       ##
# Extract the legend
legend = get_legend(bg_cm_depth)

# Define a function to calculate R-squared for each species
calculate_r_squared <- function(data, species) {
  species_data <- data %>% filter(Species == species)
  model <- lm(Culmen.Depth..mm. ~ Body.Mass..g., data = species_data)
  summary(model)$r.squared
  }

# Calculate R-squared for each species
r_squared_values <- clean_penguin %>%
  distinct(Species) %>%
  rowwise() %>%
  mutate(R_squared = calculate_r_squared(clean_penguin, Species))

r_squared_labels <- r_squared_values %>%
  mutate(
    color = case_when(
      Species == "Adelie" ~ adelie_col,
      Species == "Chinstrap" ~ chinstrap_col,
      Species == "Gentoo" ~ gentoo_col
      ),
    label = paste0("R² = ", round(R_squared, 2))
    )

# Remove legend from plot to avoid duplicity
int = bg_cm_depth + theme(legend.position = "none") + 
  geom_mark_ellipse(
    aes(
      color = Species,
      filter = Species == 'Gentoo penguin (Pygoscelis papua)'
    ),
    expand = unit(1.5,"mm")
  ) +
  annotate("text", x = 5750, y = 22, label = r_squared_labels$label[1], color = adelie_col, size = 5, hjust = 0) +
  annotate("text", x = 5750, y = 21.25, label = r_squared_labels$label[2], color = chinstrap_col, size = 5, hjust = 0) +
  annotate("text", x = 5750, y = 20.5, label = r_squared_labels$label[3], color = gentoo_col, size = 5, hjust = 0)

##                                    ##
#### Panel D: Delta 13C Violin Plot ####
##                                    ##
# Perform ANOVA for Delta.13.C
aov_13C <- aov(Delta.13.C..o.oo. ~ Species, data = clean_penguin)

# Tukey HSD post-hoc test
tukey_results_13C <- TukeyHSD(aov_13C)$Species

delta_13C_aov_plt = ggplot(
  data = clean_penguin,
  aes(
    x = Species,
    y = Delta.13.C..o.oo.,
    color = Species,
    fill = Species
    )
  ) +
  geom_violin() + 
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +  # Add boxplots
  theme_classic() +
  xlab(list("Adelie","Chinstrap","Gentoo"))+
  xlab("Species") +
  ylab(expression(paste(delta,"13C"))) +
  theme(axis.title.y = element_text(size = 20)) +
  scale_color_manual(
    values = c(adelie_col, chinstrap_col, gentoo_col),
    labels = c('Adelie','Chinstrap','Gentoo')
  ) +
  scale_fill_manual(values = c(adelie_col, chinstrap_col, gentoo_col),
                    labels = c('Adelie','Chinstrap','Gentoo')) +
  scale_x_discrete(labels=c(
    "Adelie Penguin (Pygoscelis adeliae)" = "Adelie",
    "Chinstrap penguin (Pygoscelis antarctica)" = "Chinstrap",
    "Gentoo penguin (Pygoscelis papua)" = "Gentoo"),
    name = "") +
  theme(axis.text.x = element_text(size = 10)) +
  theme(legend.position = "none") +
  annotate("text",label = "****", x = 1, y = -23.7, size = 5, color = "black") +
  annotate("text",label = "****", x = 2, y = -23.7, size = 5, color = "black") +
  annotate("text",label = "****", x = 3, y = -24.7, size = 5, color = "black")

##                                    ##
#### Panel E: Delta 15N Violin Plot ####
##                                    ##
# Perform ANOVA for Delta.15.N
aov_15N <- aov(Delta.15.N..o.oo. ~ Species, data = clean_penguin)

# Tukey HSD post-hoc test
tukey_results_15N <- TukeyHSD(aov_15N)$Species

delta_15N_aov_plt = ggplot(
  data = clean_penguin,
  aes(
    x = Species,
    y = Delta.15.N..o.oo.,
    color = Species,
    fill = Species
    )
  ) +
  geom_violin() + 
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +  # Add boxplots
  theme_classic() +
  xlab(list("Adelie","Chinstrap","Gentoo"))+
  xlab("Species") +
  ylab(expression(paste(delta,"15N"))) +
  theme(axis.title.y = element_text(size = 20)) +
  scale_color_manual(
    values = c(adelie_col, chinstrap_col, gentoo_col),
    labels = c('Adelie','Chinstrap','Gentoo')
    ) +
  scale_fill_manual(values = c(adelie_col, chinstrap_col, gentoo_col),
                    labels = c('Adelie','Chinstrap','Gentoo')) +
  scale_x_discrete(labels=c(
    "Adelie Penguin (Pygoscelis adeliae)" = "Adelie",
    "Chinstrap penguin (Pygoscelis antarctica)" = "Chinstrap",
    "Gentoo penguin (Pygoscelis papua)" = "Gentoo"),
    name = "") +
  theme(axis.text.x = element_text(size = 10)) +
  theme(legend.position = "none") +
  annotate("text",label = "****", x = 1, y = 10, size = 5, color = "black") +
  annotate("text",label = "****", x = 2, y = 10.25, size = 5, color = "black") +
  annotate("text",label = "****", x = 3, y = 9, size = 5, color = "black")

##                             ##
#### saving individual plots ####
##                             ##
ggsave("plots/individual_plots/map.png", plot = map, width = 10, height = 8, dpi = 800)
ggsave("plots/individual_plots/isl_specie_cnt.png", plot = isl_specie_cnt, width = 8, height = 6, dpi = 800)
ggsave("plots/individual_plots/bg_cm_depth.png", plot = bg_cm_depth, width = 8, height = 6, dpi = 800)
ggsave("plots/individual_plots/delta_13C_aov_plt.png", plot = delta_13C_aov_plt, width = 8, height = 6, dpi = 800)
ggsave("plots/individual_plots/delta_15N_aov_plt.png", plot = delta_15N_aov_plt, width = 8, height = 6, dpi = 800)

##                       ##
#### Arrange all plots ####
##                       ##
final_plot = ggarrange(
  map,
  ggarrange(isl_specie_cnt, int, delta_13C_aov_plt, delta_15N_aov_plt, ncol = 2,nrow = 2, labels = c("B", "C","D","E")),
  legend.grob = legend,
  legend = "right",
  nrow = 2,
  labels = "A",
  widths = c(3, 0.5),  # Adjust width ratio to make space for the legend
  common.legend = FALSE
  )

annotate_figure(
  final_plot,
  top = text_grob("Palmer Archipelago Penguins", size = 25, face = "bold")
  )

##                   ##
#### Saving Figure ####
##                   ##
ggsave("plots/peng_descriptive_figure.png", width = 15, height = 15, dpi = 800, bg = "white")