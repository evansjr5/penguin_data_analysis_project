ggplot(
  data = clean_penguin,
  aes(
    x = Species,
    y = Flipper.Length..mm.,
    color = Species
    )
  ) +
  geom_boxplot()


ggplot(
  data = clean_penguin,
  aes(
    x = Body.Mass..g.,
    y = Flipper.Length..mm.,
    color = Species,
    shape = Island
  )
) +
  geom_point()

ggplot(
  data = clean_penguin,
  aes(
    x = Species,
    y = Delta.15.N..o.oo.,
    color = Species
  )
) +
  geom_violin()

ggplot(
  data = clean_penguin,
  aes(
    x = Species,
    y = Delta.13.C..o.oo.,
    color = Species,
    fill = Species
  )
) +
  geom_violin() +
  theme_classic() +
  xlab(list("Adelie","Chinstrap","Gentoo"))+
  xlab("Species") +
  ylab(expression(paste(delta,"13C"))) +
  scale_color_manual(
    values = c("#5e4c5f", "#999999", "#ffbb6f"),
    labels = c('Adelie','Chinstrap','Gentoo')
  ) +
  scale_fill_manual(values = c("#5e4c5f", "#999999", "#ffbb6f"),
                    labels = c('Adelie','Chinstrap','Gentoo')) +
  scale_x_discrete(labels=c(
    "Adelie Penguin (Pygoscelis adeliae)" = "Adelie",
    "Chinstrap penguin (Pygoscelis antarctica)" = "Chinstrap",
    "Gentoo penguin (Pygoscelis papua)" = "Gentoo")) +
  theme(legend.position = "none")


one.way <- aov(clean_penguin$Delta.13.C..o.oo.~clean_penguin$Species)
one.way <- aov(clean_penguin$Delta.15.N..o.oo.~clean_penguin$Species)

post_hoc = TukeyHSD(one.way)

emmeans(one.way, specs = "Species")
cld = cld(emmeans(one.way, specs = "Species"), Letters = letters, adjust = "tukey")

aov_res = aov(data = clean_data,formula = Culmen.Depth..mm. ~ Body.Mass..g.)
aov(data = clean_data,formula = Culmen.Depth..mm. ~ Body.Mass..g.)
TukeyHSD(aov(model2)) # Performs Tukey's Honest Significant Differences (HSD) test on a dataset.
aov(model2) # Performs analysis of variance (ANOVA) on a statistical model.


# Perform ANOVA for Delta.15.N
aov_15N <- aov(Delta.15.N..o.oo. ~ Species, data = clean_penguin)

# Tukey HSD post-hoc test
tukey_results <- TukeyHSD(aov_15N)$Species

delta_15N_aov = ggplot(
  data = clean_penguin,
  aes(
    x = Species,
    y = Delta.15.N..o.oo.,
    color = Species,
    fill = Species
  )
) +
  geom_violin() +
  theme_classic() +
  xlab(list("Adelie","Chinstrap","Gentoo"))+
  xlab("Species") +
  ylab(expression(paste(delta,"15N"))) +
  scale_color_manual(
    values = c("#5e4c5f", "#999999", "#ffbb6f"),
    labels = c('Adelie','Chinstrap','Gentoo')
  ) +
  scale_fill_manual(values = c("#5e4c5f", "#999999", "#ffbb6f"),
                    labels = c('Adelie','Chinstrap','Gentoo')) +
  scale_x_discrete(labels=c(
    "Adelie Penguin (Pygoscelis adeliae)" = "Adelie",
    "Chinstrap penguin (Pygoscelis antarctica)" = "Chinstrap",
    "Gentoo penguin (Pygoscelis papua)" = "Gentoo")) +
  theme(legend.position = "none") +
  annotate("text",label = "****", x = 1, y = 10, size = 5, color = "black") +
  annotate("text",label = "****", x = 2, y = 10.25, size = 5, color = "black") +
  annotate("text",label = "****", x = 3, y = 9, size = 5, color = "black")



# Perform ANOVA for Delta.13.C
aov_13C <- aov(Delta.13.C..o.oo. ~ Species, data = clean_penguin)

# Tukey HSD post-hoc test
tukey_results_13C <- TukeyHSD(aov_13C)$Species

delta_13C_aov = ggplot(
  data = clean_penguin,
  aes(
    x = Species,
    y = Delta.13.C..o.oo.,
    color = Species,
    fill = Species
  )
) +
  geom_violin() +
  theme_classic() +
  xlab(list("Adelie","Chinstrap","Gentoo"))+
  xlab("Species") +
  ylab(expression(paste(delta,"13C"))) +
  scale_color_manual(
    values = c("#5e4c5f", "#999999", "#ffbb6f"),
    labels = c('Adelie','Chinstrap','Gentoo')
  ) +
  scale_fill_manual(values = c("#5e4c5f", "#999999", "#ffbb6f"),
                    labels = c('Adelie','Chinstrap','Gentoo')) +
  scale_x_discrete(labels=c(
    "Adelie Penguin (Pygoscelis adeliae)" = "Adelie",
    "Chinstrap penguin (Pygoscelis antarctica)" = "Chinstrap",
    "Gentoo penguin (Pygoscelis papua)" = "Gentoo"),name = "") +
  theme(legend.position = "none") +
  annotate("text",label = "****", x = 1, y = -23.7, size = 5, color = "black") +
  annotate("text",label = "****", x = 2, y = -23.7, size = 5, color = "black") +
  annotate("text",label = "****", x = 3, y = -24.7, size = 5, color = "black")
