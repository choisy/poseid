# Prerequisite -----------------------------------------------------------------
splits <- readRDS("data-raw/splits.RDS")
ah_splits <- readRDS("data-raw/ah_splits.RDS")

# Save data --------------------------------------------------------------------
devtools::use_data(splits, ah_splits, overwrite = TRUE, internal = TRUE)

# Erase everything -------------------------------------------------------------
rm(list = ls())
