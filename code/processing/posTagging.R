
# POS TAGGING

pacman::p_load(tidyverse, quanteda, quanteda.textstats, udpipe,
               tidytext, lexicon, stopwords, parallel, igraph)


# ---- generate lists of random numbers ranging from length of each corpus ----

rand_200_a <- sample(1:length(docs_a$text), 200)
rand_200_n <- sample(1:length(docs_n$text), 200)
rand_200_m <- sample(1:length(docs_m$text), 200)


# ---- english language model for tagging ----

# run the below once to download model
# eng_model <- udpipe_download_model(language = "english-ewt", model_dir = "data/") 

# load model
eng_model <- udpipe_load_model("data/english-ewt-ud-2.5-191206.udpipe")


# ---- run tagging algorithm ----

docs_a_pos <- udpipe_annotate(eng_model, docs_a$text[rand_200_a]) %>% as.data.frame()
docs_n_pos <- udpipe_annotate(eng_model, docs_n$text[rand_200_n]) %>% as.data.frame()
docs_m_pos <- udpipe_annotate(eng_model, docs_m$text[rand_200_n]) %>% as.data.frame()


# ---- identify words to add by filtering by types of speech that should be removed ----

# NOTE: we do not filter on "IN" (prepositions), because this seems to be where the language model 
# most often messes up and categorizes nouns as prepositions (and the standard stopword lists should 
# be able to account for these)

# -- academic corpus sample
words_toadd_a <-
  docs_a_pos %>% 
  dplyr::filter(xpos%in%c("CC", "CD", "DT", "PDT", "PRP", "PRP$", 
                         "RB", "RBR", "RBS", "TO", "VB", "VBD", "VBG", "VBN", "VBZ")) %>%
  # NOTE: we do not filter on "IN" (prepositions), because this seems to be where the language model 
  # most often messes up and categorizes nouns as prepositions (and the standard stopword lists should 
  # be able to account for these)
  mutate(token = tolower(token)) %>%
  select(token, lemma) %>%
  unique(.)

# -- media corpus sample
words_toadd_m <-
  docs_m_pos %>% 
  dplyr::filter(xpos%in%c("DT", "PDT", "PRP", "PRP$", 
                          "RB", "RBR", "RBS", "TO", "VB", "VBD", "VBG", "VBN", "VBZ")) %>%
  
  mutate(token = tolower(token)) %>%
  select(token, lemma) %>%
  unique(.)

# -- NGO corpus sample (WE CURRENTLY DO NOT USE THIS ONE BECAUSE OF CHALLENGES IN TAGGING, FROM ODD SENTENCE STRUCTURE)
words_toadd_n <-
  docs_n_pos %>% 
  dplyr::filter(xpos%in%c("DT", "PDT", "PRP", "PRP$", 
                          "RB", "RBR", "RBS", "TO", "VB", "VBD", "VBG", "VBN", "VBZ")) %>%

  mutate(token = tolower(token)) %>%
  select(token, lemma) %>%
  unique(.)


# -- add words to standard SMART stopword list
stopwords_pos_extend <-
  c(stopwords(source = "smart"), words_toadd_a$token) %>% unique(.)

stopwords_pos_extend_m <-
  c(stopwords(source = "smart"), words_toadd_m$token) %>% unique(.)
