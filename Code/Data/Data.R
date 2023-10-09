# Libraries #

library(dplyr)
library(tidyr)
library(Amelia)

# Data Exploration #

df <- iris
str(df)
any(is.na(df))