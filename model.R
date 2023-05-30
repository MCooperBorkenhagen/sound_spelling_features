
require(tidyverse)
require(tidymodels)
require(keras)


# data
Y = read_csv('data/phon.csv', col_names = F)
X = read_csv('data/orth.csv', col_names = F)
sound_spellings = read_csv('data/elp_3let_3phon_gpcs.csv')

# random seeds
set.seed(345)
fit_seeds <- sample.int(10^4, size = 3)

EPOCHS = 500
HIDDEN = length(unique(sound_spellings$gpc)) # unique rules in our set.

# model
model <- mlp(epochs = EPOCHS, hidden_units = HIDDEN) %>% 
  set_mode("regression") %>% 
  set_engine("keras", verbose = 1, validation_split = 0.1, seeds = fit_seeds) %>% 
  fit_xy(x = X, y = Y)

