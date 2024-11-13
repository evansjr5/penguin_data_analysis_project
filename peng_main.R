##                 ##
#### peng_main.R ####
##                 ##
# Description:
# Set wd, load packages, open data, and run scripts to produce clean data, 
# descriptive figure and ML figure for predicting blood contents of penguins

# Set Working Directly
#setwd(dir = "/Users/jamesevans/Documents/GitHub/Vanderbilt/Penguin_Data_Analysis")

# Load packages
pacman::p_load(
  broom,
  caret,
  cluster,
  cowplot,
  DataExplorer,
  datasets,
  dbplyr,
  dbscan,
  emmeans,
  factoextra,
  formattable,
  ggdendro,
  ggforce,
  ggplot2,
  ggpubr,
  ggrepel,
  ggthemes,
  glmnet,
  htmltools,
  iml,
  kknn,
  mclust,
  multcomp,
  naniar,
  patchwork,
  pROC,
  randomForest,
  tibble,
  tidymodels,
  tidyverse,
  vip,
  webshot,
  yardstick
  )

# open file holding penguin data
penguins_lter = read.csv("penguins_lter.csv")

##                     ##
#### Running scripts ####
##                     ##
# run script for data to be cleaned
source("scripts/penguin_data_cleansing.R")

# run script for descriptive plots
source("scripts/descriptive_plots.R")

# run script for delta 15N models and plots
source("scripts/delta_15N_models_and_plots.R")

# run script for delta 13C models and plots
source("scripts/delta_13C_models_and_plots.R")
