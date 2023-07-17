
pacman::p_load(tidyverse, word2vec, tidytext)

# Define function to train new word2vec models for each institution/year corpus
word2vec_percorpus <- function(data = NULL, input_suffix = NULL, years = 2017:2021, type = "cbow", dim = 50, words = NULL) {
 
  for(i in years) {
    
   data_filt <- data %>% filter(year==i)
   
   assign(paste("word2vec_", input_suffix, "_", i, sep = ""),
          word2vec(x = data_filt$text, type = type, dim = dim, iter = 20),
          envir = .GlobalEnv)
   
   assign(paste("lookslike_", input_suffix, "_", i, sep = ""),
          predict(get(paste("word2vec_", input_suffix, "_", i, sep = "")), 
                  words, type = "nearest", top_n = 20),
          envir = .GlobalEnv)
 } 
}

word2vec_percorpus(data = docs_n, input_suffix = "n", words = c("biodiversity", "safeguard", "hope", "landscape"))
word2vec_percorpus(data = docs_a, input_suffix = "a", words = c("biodiversity", "safeguard", "hope", "landscape"))
word2vec_percorpus(data = docs_m_nyt_filt, input_suffix = "m", words = c("biodiversity", "safeguard", "hope", "landscape"))

# HOPE across years
lookslike_n_hope <- 
  lookslike_n_2017[["hope"]] %>% mutate(year = 2017) %>%
  bind_rows(lookslike_n_2018[["hope"]] %>% mutate(year = 2018)) %>%
  bind_rows(lookslike_n_2019[["hope"]] %>% mutate(year = 2019)) %>%
  bind_rows(lookslike_n_2020[["hope"]] %>% mutate(year = 2020)) %>%
  bind_rows(lookslike_n_2021[["hope"]] %>% mutate(year = 2021)) %>%
  pivot_wider(id_cols = c("term1", "term2"), names_from = "year", values_from = c("similarity", "rank")) %>%
  mutate(years_topn = rowSums(!is.na(.[,c("rank_2017", "rank_2018", "rank_2019", "rank_2020", "rank_2021")])))

lookslike_a_hope <- 
  lookslike_a_2017[["hope"]] %>% mutate(year = 2017) %>%
  bind_rows(lookslike_a_2018[["hope"]] %>% mutate(year = 2018)) %>%
  bind_rows(lookslike_a_2019[["hope"]] %>% mutate(year = 2019)) %>%
  bind_rows(lookslike_a_2020[["hope"]] %>% mutate(year = 2020)) %>%
  bind_rows(lookslike_a_2021[["hope"]] %>% mutate(year = 2021)) %>%
  pivot_wider(id_cols = c("term1", "term2"), names_from = "year", values_from = c("similarity", "rank"))

# BIODIVERSITY across years
lookslike_n_biodiversity <- 
  lookslike_n_2017[["biodiversity"]] %>% mutate(year = 2017) %>%
  bind_rows(lookslike_n_2018[["biodiversity"]] %>% mutate(year = 2018)) %>%
  bind_rows(lookslike_n_2019[["biodiversity"]] %>% mutate(year = 2019)) %>%
  bind_rows(lookslike_n_2020[["biodiversity"]] %>% mutate(year = 2020)) %>%
  bind_rows(lookslike_n_2021[["biodiversity"]] %>% mutate(year = 2021)) %>%
  pivot_wider(id_cols = c("term1", "term2"), names_from = "year", values_from = c("similarity", "rank")) %>%
  mutate(years_topn = rowSums(!is.na(.[,c("rank_2017", "rank_2018", "rank_2019", "rank_2020", "rank_2021")])))

lookslike_a_biodiversity <- 
  lookslike_a_2017[["biodiversity"]] %>% mutate(year = 2017) %>%
  bind_rows(lookslike_a_2018[["biodiversity"]] %>% mutate(year = 2018)) %>%
  bind_rows(lookslike_a_2019[["biodiversity"]] %>% mutate(year = 2019)) %>%
  bind_rows(lookslike_a_2020[["biodiversity"]] %>% mutate(year = 2020)) %>%
  bind_rows(lookslike_a_2021[["biodiversity"]] %>% mutate(year = 2021)) %>%
  pivot_wider(id_cols = c("term1", "term2"), names_from = "year", values_from = c("similarity", "rank"))

