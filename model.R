
require(tidyverse)
require(tidymodels)
require(keras)


# data
Y = read_csv('data/phon.csv', col_names = F)
X = read_csv('data/orth.csv', col_names = F)
sound_spellings = read_csv('data/elp_3let_3phon_gpcs.csv')
words = read_csv('data/elp_3let_3phon_letphononly.csv')

# random seeds
set.seed(345)
fit_seeds <- sample.int(10^4, size = 3)

EPOCHS = 500
HIDDEN = length(unique(sound_spellings$gpc)) # unique rules in our set.

# Question 1 Model (35 features)
model <- mlp(epochs = EPOCHS, hidden_units = HIDDEN) %>% 
  set_mode("regression") %>% 
  set_engine("keras", verbose = 1, validation_split = 0.1, seeds = fit_seeds) %>% 
  fit_xy(x = X, y = Y)


## Get layer activations
layer <- 'dense' # just the name of the target layer

model_hidden_layer <- keras_model(inputs = model$fit$input,
                                  outputs = get_layer(model$fit, layer)$output)

hidden_layer_output <- model_hidden_layer$predict_on_batch(X) %>% 
  as_tibble() %>%
  set_names(~str_replace(., "V", "unit_")) %>% 
  mutate(word = words$word) %>% 
  select(word, everything())

write_csv(hidden_layer_output, 'data/hidden_layer_output.csv')

# pairwise distance matrix to inspect word-by-word distances
dist_ = dist(hidden_layer_output[,2:ncol(hidden_layer_output)])

dist_long = distance_matrix_to_long(dist_, words = hidden_layer_output$word)

dist_long %>% 
  dplyr::filter(word_1 == "ben") %>% 
  arrange(desc(distance)) %>% 
  view()

  


# words with <b> for example, should reliably load on the same unit 
# and this should be the case across this set of items - one unit should
# spike to 1. Maybe start with vowels.



