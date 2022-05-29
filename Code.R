# Title:    Computing correlations


# INSTALL AND LOAD PACKAGES ################################


# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(corrplot, magrittr, pacman, rio, tidyverse)
# corrplot: for visualizing correlation matrices
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Save Google Correlate variables
df <- import("fileC.csv") %>%
  as_tibble() %>%
  select(Age, Income, NofCases, Children, Services, Days) %>% 
  print()

# CORRELATION MATRIX #######################################

# Correlation matrix for data frame
df %>% cor()

# Fewer decimal places
df %>%
  cor() %>%     # Compute correlations
  round(2) %>%  # Round to 2 decimals
  print()

# Visualize correlation matrix with corrplot() from
# corrplot package
df %>%
  cor() %>%
  corrplot(
    type   = "upper",     # Matrix: full, upper, or lower
    diag   = F,           # Remove diagonal
    order  = "original",  # Order for labels
    tl.col = "black",     # Font color
    tl.srt = 45           # Label angle
  )

# SINGLE CORRELATION #######################################

# Use cor.test() to test one pair of variables at a time.


df %$% cor.test(NofCases, Children)


