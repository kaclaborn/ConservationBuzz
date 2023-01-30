corp_a <- corpus(docs_a)

tokens_a <- corp_a %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(thesaurus$token, thesaurus$lemma, valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords(), padding = T)

collocations_a <- textstat_collocations(tokens_a, min_count = 5)
collocations_a <- collocations_a[1:500, ]

tokens_a <- tokens_compound(tokens_a, collocations_a)

docs_a_test <- docs_a %>% mutate(doc = paste("text", row_number(), sep = ""))

data_unnest_a <- 
  docs_a_test %>% 
  unnest_tokens(word, text)  %>%
  count(doc, word, sort = TRUE)

docs_a_test[1:100,] %>% unnest_tokens(word, text, token = "ngrams", n = 2)

tf_idf_a <- 
  data_unnest_a %>% 
  bind_tf_idf(word, doc, n)

avg_tf_idf_a <- tf_idf_a %>%
  group_by(word) %>%
  summarise(ndocs = length(doc),
            mean_tf_idf = mean(tf_idf),
            median_tf_idf = median(tf_idf))
