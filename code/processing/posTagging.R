
# POS TAGGING

pacman::p_load(tidyverse, quanteda, quanteda.textstats, udpipe,
               tidytext, lexicon, stopwords, parallel, igraph)

set.seed(12345)

# ---- import docs csvs for academic and media corpora (which will be tagged here) ----

docs_a <- read_csv("data/corpora/processed/docs_a.csv", locale = readr::locale(encoding = "UTF-8"))
docs_m <- read_csv("data/corpora/processed/docs_m_filtered.csv", locale = readr::locale(encoding = "UTF-8"))


# ---- generate lists of random numbers ranging from length of each corpus ----

rand_200_a <- sample(1:length(docs_a$text), 200)
rand_200_m <- sample(1:length(docs_m$text), 200)


# ---- english language model for tagging ----

# run the below once to download model
# eng_model <- udpipe_download_model(language = "english-ewt", model_dir = "data/") 

# load model
eng_model <- udpipe_load_model("data/english-ewt-ud-2.5-191206.udpipe")


# ---- run tagging algorithm ----

docs_a_pos <- udpipe_annotate(eng_model, docs_a$text[rand_200_a]) %>% as.data.frame()
docs_m_pos <- udpipe_annotate(eng_model, docs_m$text[rand_200_m]) %>% as.data.frame()


# ---- identify words to add by filtering by types of speech that should be removed ----

# NOTE: we do not filter on "IN" (prepositions), because this seems to be where the language model 
# most often messes up and categorizes nouns as prepositions (and the standard stopword lists should 
# be able to account for these)

# -- academic corpus sample
words_toadd_a <-
  docs_a_pos %>% 
  mutate(token = tolower(token)) %>%
  group_by(token, lemma) %>%
  summarise(n = length(token),
            any_noun = ifelse("NN"%in%unique(xpos) |
                                "NNP"%in%unique(xpos) |
                                "NNPS"%in%unique(xpos) |
                                "NNS"%in%unique(xpos), 1, 0),
            any_adj = ifelse("JJ"%in%unique(xpos) |
                               "JJR"%in%unique(xpos) |
                               "JJS"%in%unique(xpos), 1, 0),
            unique_xpos = list(unique(xpos)),
            xpos = ifelse(any_noun==1 | any_adj==1, "NN/JJ", xpos)) %>%
  dplyr::filter(xpos%in%c("CD", "RB", "RBR", "RBS", "VB", "VBD", "VBG", "VBN", "VBZ")) %>%
  select(token, lemma, xpos) %>%
  unique(.)

# -- media corpus sample
words_toadd_m <-
  docs_m_pos %>% 
  mutate(token = tolower(token)) %>%
  group_by(token, lemma) %>%
  summarise(n = length(token),
            any_noun = ifelse("NN"%in%unique(xpos) |
                                "NNP"%in%unique(xpos) |
                                "NNPS"%in%unique(xpos) |
                                "NNS"%in%unique(xpos), 1, 0),
            any_adj = ifelse("JJ"%in%unique(xpos) |
                               "JJR"%in%unique(xpos) |
                               "JJS"%in%unique(xpos), 1, 0),
            unique_xpos = list(unique(xpos)),
            xpos = ifelse(any_noun==1 | any_adj==1, "NN/JJ", xpos)) %>%
  dplyr::filter(xpos%in%c("CD", "RB", "RBR", "RBS", "VB", "VBD", "VBG", "VBN", "VBZ")) %>%
  

  select(token, lemma, xpos) %>%
  unique(.)


# -- add words to standard SMART stopword list
stopwords_pos_extend <-
  c(stopwords(source = "smart"), 
    words_toadd_a$token, words_toadd_a$lemma, 
    words_toadd_m$token, words_toadd_m$lemma) %>% 
  unique(.)


# -- export tagged sample, extended stopwords list

write.csv(docs_a_pos, "data/corpora/processed/posTagged/docs_a_sample_pos.csv", row.names = F)
write.csv(docs_m_pos, "data/corpora/processed/posTagged/docs_m_sample_pos.csv", row.names = F)

write.table(stopwords_pos_extend, "code/source/stopwords_extended_pos.txt", row.names = F)
