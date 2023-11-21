
# code: semantic drift of focal buzzwords

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES, DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


pacman::p_load(tidyverse, word2vec, tidytext, PsychWordVec, ggcorrplot)


# turn stopwords lists into data frames for cleanTexts function

source("code/source/stopwordsThesaurus.R")


# bring in docs
docs_a <- read_csv("data/corpora/processed/docs_a.csv", locale = readr::locale(encoding = "UTF-8"))
docs_n <- read_csv("data/corpora/processed/docs_n.csv", locale = readr::locale(encoding = "UTF-8"))
docs_m <- read_csv("data/corpora/processed/docs_m.csv", locale = readr::locale(encoding = "UTF-8"))
docs_p <- read_csv("data/corpora/processed/docs_p.csv", locale = readr::locale(encoding = "UTF-8"))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: DEFINE FUNCTIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# define text cleaning function that does same function as quanteda for semantic network generation 
# e.g., uses same stopwords, thesaurus, punctuation & numeric removal, lower casing

cleanTexts <- function(data, years = 2017:2021, input_suffix, stopwords_list) {
  
  org_col <- ifelse(input_suffix=="n", "org_id", 
                    ifelse(input_suffix=="a", "journal", 
                           ifelse(input_suffix=="m", "publication",
                                  ifelse(input_suffix=="p", "org"))))
  
  stopwords_suffix <- ifelse(stopwords_list=="stopwords_extended", "noverbs", 
                             ifelse(stopwords_list=="stopwords_basic", "verbs", NA))
  
  data <- data %>% mutate(org = as.character(get(org_col)))
  
  assign(paste0("texts_", input_suffix, "_", stopwords_suffix, sep = ""),
         data.frame(doc_id = numeric(), org = character(), year = numeric(), text = character()),
         envir = .GlobalEnv)
  
  for(i in years) {
    
    assign(paste0("texts_", input_suffix, "_", stopwords_suffix, sep = ""),
           bind_rows(get(paste0("texts_", input_suffix, "_", stopwords_suffix, sep = "")),
                     data %>% filter(year==i) %>% 
                       mutate(doc_id = row_number(),
                              textClean = gsub('[[:punct:]]+', '',
                                               gsub('\\\\n|\\.|\\,|\\;', ' ', tolower(text))),
                              textClean = stringr::str_replace_all(textClean, '[0-9]+|[[:punct:]]|\\(.*\\)', '')) %>%
                       select(year, org, doc_id, textClean) %>%
                       unnest_tokens(word, textClean)  %>%
                       left_join(get(stopwords_list), by = "word", relationship = "many-to-many") %>%
                       left_join(thesaurus, by = c("word" = "token"), relationship = "many-to-many") %>%
                       mutate(word = case_when(!is.na(lemma) ~ lemma,
                                               is.na(lemma) ~ word)) %>%
                       filter(is.na(stopword)) %>%
                       select(-stopword, -lemma) %>%
                       group_by(doc_id, org, year) %>% 
                       summarise(text = paste(word, collapse = ' '))),
           envir = .GlobalEnv)
    
  }
  
  
}


# define function to train new word2vec models for each institution/year corpus
word2vec_percorpus <- function(data = NULL, input_suffix = NULL, stopwords = "noverbs", years = 2017:2021, 
                               type = "cbow", dim = 50, window = 5) {
 
  for(i in years) {
    
   data_filt <- data %>% filter(year==i)
   
   assign(paste("word2vec_", input_suffix, "_", i, "_", stopwords, sep = ""),
          word2vec(x = data_filt$text, type = type, dim = dim, window = window, iter = 20),
          envir = .GlobalEnv)

 } 
}


# define function to do nearest neighbor analysis from a pre-trained word2vec model for a given word, per year
looksLike_byWord <- function(word, input_suffix, stopwords = "noverbs", years = 2017:2021, top_n = 20){
  
  assign("x", NULL, envir = .GlobalEnv)
  
  for(i in years) {
    
    assign("x",
           bind_rows(x,
                     predict(get(paste("word2vec_", input_suffix, "_", i, "_", stopwords, sep = "")),
                             word, type = "nearest", top_n = top_n)[[word]] %>% 
                       mutate(year = i)),
           envir = .GlobalEnv)
    
  }

  return(x)
}

# define function to gather & transform word embeddings from pre-trained word2vec model for a given word, per year
semanticDisplacement <- function(word, input_suffix, stopwords = "noverbs", ref_year = 2021, compare_years = 2017:2020) {
  
  ref <- predict(get(paste("word2vec_", input_suffix, "_", ref_year, "_", stopwords, sep = "")), word, type = "embedding")
  
  embeddings <- data.frame(t(ref))
  
  for(i in compare_years) {
    
    x <- predict(get(paste("word2vec_", input_suffix, "_", i, "_", stopwords, sep = "")), word, type = "embedding")
    trans <- orth_procrustes(ref, x)
    
    embeddings <- bind_cols(embeddings,
                            trans)
    
  }
  
  colnames(embeddings) <- c(as.character(ref_year), as.character(compare_years))
  
  
  # prepare matrix for calculating pairwise cosine similarities, by year
  sim <- t(as.matrix(embeddings)) / sqrt(rowSums(t(as.matrix(embeddings)) * t(as.matrix(embeddings))))
  
  similarities <- sim %*% t(sim)
  similarities <- as.data.frame(similarities)
  
  return(similarities)
  
}


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: CLEAN TEXTS, TRAIN MODELS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# clean texts, examine freq distributions of tokens
cleanTexts(data = docs_n, years = 2017:2021, input_suffix = "n", stopwords_list = "stopwords_extended")
cleanTexts(data = docs_a, years = 2017:2021, input_suffix = "a", stopwords_list = "stopwords_extended")
cleanTexts(data = docs_m, years = 2017:2021, input_suffix = "m", stopwords_list = "stopwords_extended")
cleanTexts(data = docs_p, years = c(2019, 2022), input_suffix = "p", stopwords_list = "stopwords_extended")

cleanTexts(data = docs_n, years = 2017:2021, input_suffix = "n", stopwords_list = "stopwords_basic")
cleanTexts(data = docs_a, years = 2000:2021, input_suffix = "a", stopwords_list = "stopwords_basic")
cleanTexts(data = docs_m, years = 2017:2021, input_suffix = "m", stopwords_list = "stopwords_basic")
cleanTexts(data = docs_p, years = c(2019, 2022), input_suffix = "p", stopwords_list = "stopwords_basic")

# bigrams <- tokens_n_2021 %>%
#   group_by(doc_id)  %>% 
#   summarize(textClean = paste(word, collapse=' ')) %>%
#   unnest_tokens(bigram, token = "ngrams", n = 2, textClean) %>%
#   mutate(bigram = stringr::str_replace_all(bigram, ' ', '_'))
# trigrams <- tokens_n_2021 %>%
#   group_by(doc_id)  %>% 
#   summarize(textClean = paste(word, collapse=' ')) %>%
#   unnest_tokens(bigram, token = "ngrams", n = 3, textClean) %>%
#   mutate(bigram = stringr::str_replace_all(bigram, ' ', '_'))
# quadgrams <- tokens_n_2021 %>%
#   group_by(doc_id)  %>% 
#   summarize(textClean = paste(word, collapse=' ')) %>%
#   unnest_tokens(bigram, token = "ngrams", n = 4, textClean) %>%
#   mutate(bigram = stringr::str_replace_all(bigram, ' ', '_'))
# 
# collocates <- bigrams %>% bind_rows(trigrams) %>% bind_rows(quadgrams) %>%
#   left_join(collocates_stopwords, by = "bigram") %>%
#   filter(is.na(stopword)) %>%
#   select(-stopword)
# 
# text_n_2021 <- tokens_n_2021 %>% group_by(doc_id, org_id, year) %>% 
#   summarise(text = paste(word, collapse = ' ')) %>%
#   left_join(collocates %>% group_by(doc_id) %>% summarise(collocates = paste(bigram, collapse = ' ')), 
#             by = "doc_id")
# 
# tokens_n_2021 %>% 
#   group_by(doc_id) %>% summarise(n_tokens = n()) %>% filter(n_tokens>1000)
#   mutate(n_tokens_binned = cut(n_tokens, breaks = c(0,seq(50,1000,50),Inf))) %>% 
#   group_by(n_tokens_binned) %>% summarise(n = n()) %>% 
#   ggplot(aes(x=n_tokens_binned,y=n)) + 
#   geom_bar(stat='identity',fill='blue') + theme_minimal() 


# train word2vec models with different corpora, per year
word2vec_percorpus(data = texts_n_noverbs, input_suffix = "n", stopwords = "noverbs", type = "skip-gram", dim = 50)
word2vec_percorpus(data = texts_a_noverbs, input_suffix = "a", stopwords = "noverbs", years = 2017:2021, type = "skip-gram", dim = 50)
word2vec_percorpus(data = texts_m_noverbs, input_suffix = "m", stopwords = "noverbs", type = "skip-gram", dim = 50)
word2vec_percorpus(data = texts_p_noverbs, input_suffix = "p", stopwords = "noverbs", years = c(2019, 2022), type = "skip-gram", dim = 50)

for(i in 2017:2021) {
  object <- paste("word2vec_m_", i, sep = "")
  
  saveRDS(get(paste(object, "_noverbs", sep = "")), paste0("data/outputs/word2vec/", object, ".rds", sep = ""))
}

word2vec_percorpus(data = texts_n_verbs, input_suffix = "n", stopwords = "verbs", type = "skip-gram", dim = 50)
word2vec_percorpus(data = texts_a_verbs, input_suffix = "a", stopwords = "verbs", years = 2000:2021, type = "skip-gram", dim = 50)
word2vec_percorpus(data = texts_m_verbs, input_suffix = "m", stopwords = "verbs", type = "skip-gram", dim = 50)
word2vec_percorpus(data = texts_p_verbs, input_suffix = "p", stopwords = "verbs", years = c(2019, 2022), type = "skip-gram", dim = 50)


# embedding data matrices & principal components

# ACADEMIC 2021
word2vec_a_2021_matrix <- scale(as.matrix(word2vec_a_2021))
word2vec_a_2021_cov <- cov(word2vec_a_2021_matrix)
pca_a_2021 <- prcomp(word2vec_a_2021_cov, center = TRUE, scale. = TRUE, retx = TRUE)
summary(pca_a_2021)

word_positions_pca_a_2021 <- data.frame(PC1 = numeric(0), PC2 = numeric(0),
                                        PC3 = numeric(0), PC4 = numeric(0),
                                        PC5 = numeric(0), PC6 = numeric(0),
                                        PC7 = numeric(0), PC8 = numeric(0),
                                        PC9 = numeric(0), PC10 = numeric(0),
                                        PC11 = numeric(0), PC12 = numeric(0))

for(i in rownames(word2vec_a_2021_matrix)) {
    word_positions_pca_a_2021[i, ] <- t(word2vec_a_2021_matrix)[, i] %*% pca_a_2021$x[, 1:12]
}


word_positions_pca_a_2021_filt <-
  word_positions_pca_a_2021 %>%
  mutate(node = rownames(.)) %>%
  left_join(node_freq_a %>% filter(year==2021), by = "node") %>%
  full_join(node_attributes_a %>% 
              filter(year==2021 & percentile_threshold==0.5 & consensus_threshold==0.25) %>% 
              mutate(node = str_replace(node, "-", "")) %>%
              select(node, conductivity, consensus, degree, symbol_type), by = "node") %>%
  mutate(symbol_buzzword = case_when(symbol_type=="placeholder" & !is.na(symbol_type) ~ "buzzword", 
                                     !is.na(symbol_type) ~ "not buzzword", 
                                     TRUE ~ "not classified")) %>%
  filter(!is.na(year))

PC1PC2_a_2021_plot <- 
  ggplot(word_positions_pca_a_2021_filt %>% filter(freq>50 & PC1>0 & PC2>0)) +
  geom_text(aes(x = PC1, y = PC2, label = node, colour = symbol_buzzword)) 
  
PC2PC3_a_2021_plot <- 
  ggplot(word_positions_pca_a_2021_filt %>% filter(freq>50 & PC2>0)) +
  geom_text(aes(x = PC2, y = PC3, label = node, colour = symbol_buzzword)) 


topwords_perPC_a_2021 <- 
  word_positions_pca_a_2021_filt %>%
  pivot_longer(cols = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "PC11", "PC12"), 
               names_to = "PC", values_to = "PC_score") %>%
  mutate(PC = factor(PC, unique(PC), ordered = T)) %>%
  group_by(PC) %>%
  mutate(rank = rank(desc(PC_score), ties.method = "first")) %>%
  filter(rank<=30) %>%
  arrange(PC) %>%
  pivot_wider(id_cols = "rank", values_from = c("node", "freq", "symbol_buzzword", "PC_score"), names_from = "PC")



# NGO 2021
word2vec_n_2021_matrix <- scale(as.matrix(word2vec_n_2021_noverbs))
word2vec_n_2021_cov <- cov(word2vec_n_2021_matrix)
pca_n_2021 <- prcomp(word2vec_n_2021_cov, center = TRUE, scale. = TRUE, retx = TRUE)
summary(pca_n_2021)

word_positions_pca_n_2021 <- data.frame(PC1 = numeric(0), PC2 = numeric(0),
                                        PC3 = numeric(0), PC4 = numeric(0),
                                        PC5 = numeric(0), PC6 = numeric(0),
                                        PC7 = numeric(0), PC8 = numeric(0),
                                        PC9 = numeric(0), PC10 = numeric(0),
                                        PC11 = numeric(0), PC12 = numeric(0))

for(i in rownames(word2vec_n_2021_matrix)) {
  word_positions_pca_n_2021[i, ] <- t(word2vec_n_2021_matrix)[, i] %*% pca_n_2021$x[, 1:12]
}

word_positions_pca_n_2021_filt <-
  word_positions_pca_n_2021 %>%
  mutate(node = rownames(.)) %>%
  left_join(node_freq_n %>% filter(year==2021), by = "node") %>%
  full_join(node_attributes_n %>% 
              filter(year==2021 & percentile_threshold==0.5 & consensus_threshold==0.5) %>% 
              mutate(node = str_replace(node, "-", "")) %>%
              select(node, conductivity, consensus, degree, symbol_type), by = "node") %>%
  mutate(symbol_buzzword = case_when(symbol_type=="placeholder" & !is.na(symbol_type) ~ "buzzword", 
                                     !is.na(symbol_type) ~ "not buzzword", 
                                     TRUE ~ "not classified")) %>%
  filter(!is.na(year))

word_positions_buzzwords_pca_n_2021 <- word_positions_pca_n_2021_filt %>% filter(symbol_buzzword=="buzzword")


PC1PC2_n_2021_plot <- 
  ggplot(word_positions_pca_n_2021_filt %>% filter(freq>10 & PC1>0 & PC2>0)) +
  geom_text(aes(x = PC1, y = PC2, label = node, colour = symbol_buzzword), 
            position = position_jitter()) 

PC2PC3_n_2021_plot <- 
  ggplot(word_positions_pca_n_2021_filt %>% filter(freq>10 & PC2>0 & PC3>0)) +
  geom_text(aes(x = PC2, y = PC3, label = node, colour = symbol_buzzword), 
            position = position_jitter()) 


topwords_perPC_n_2021 <- 
  word_positions_pca_n_2021_filt %>%
  pivot_longer(cols = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", 
                        "PC7", "PC8", "PC9", "PC10", "PC11", "PC12"), names_to = "PC", values_to = "PC_score") %>%
  group_by(PC) %>%
  mutate(rank = rank(desc(PC_score), ties.method = "first")) %>%
  filter(rank<=20) %>%
  pivot_wider(id_cols = "rank", values_from = c("node", "freq", "symbol_buzzword", "PC_score"), names_from = "PC")



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: FOCAL WORD DISPLACEMENT AND NEAREST NEIGHBOR ANALYSES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- BIODIVERSITY ----

# Calculate semantic displacement for BIODIVERSITY in different corpora
semanticdisplace_a_biodiversity <- semanticDisplacement(word = "biodiversity", input_suffix = "a",
                                                        ref_year = 2000, compare_years = 2001:2021)

semanticdisplace_n_biodiversity <- semanticDisplacement(word = "biodiversity", input_suffix = "n",
                                                        ref_year = 2017, compare_years = 2018:2021)

semanticdisplace_n_biodiversity_verbs <- semanticDisplacement(word = "biodiversity", input_suffix = "n", stopwords = "verbs",
                                                              ref_year = 2017, compare_years = 2018:2021)

semanticdisplace_m_biodiversity <- semanticDisplacement(word = "biodiversity", input_suffix = "m",
                                                        ref_year = 2017, compare_years = 2018:2021)


biodiversity_a_similarity_corrplot <- 
  semanticdisplace_a_biodiversity %>% 
  ggcorrplot(type = "upper", outline.col = "white") + 
  labs(title = "Biodiversity", subtitle = "Academic")

biodiversity_n_similarity_corrplot <- 
  semanticdisplace_n_biodiversity %>% 
  ggcorrplot(type = "upper", outline.col = "white") + 
  labs(title = "Biodiversity", subtitle = "NGOs")

biodiversity_m_similarity_corrplot <- 
  semanticdisplace_m_biodiversity %>% 
  ggcorrplot(type = "upper", outline.col = "white") + 
  labs(title = "Biodiversity", subtitle = "Media")



# Calculate pairwise word similarities for BIODIVERSITY in different corpora
lookslike_a_biodiversity <- looksLike_byWord("biodiversity", "a", years = 2017:2021, top_n = 50)
lookslike_n_biodiversity <- looksLike_byWord("biodiversity", "n", years = 2017:2021, top_n = 50)
lookslike_m_biodiversity <- looksLike_byWord("biodiversity", "m", years = 2017:2021, top_n = 50)
lookslike_p_biodiversity <- looksLike_byWord("biodiversity", "p", years = c(2019, 2022), top_n = 50)

toprank_n_biodiversity <- 
  lookslike_n_biodiversity %>%
  left_join(looksLike_byWord("biodiversity", "n", stopwords = "noverbs", years = 2017:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))


similarity_a_biodiversity <- 
  ggplot(lookslike_a_biodiversity) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Biodiversity'", subtitle = "Academic")

similarity_n_biodiversity <- 
  ggplot(lookslike_n_biodiversity) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Biodiversity'", subtitle = "NGOs")

similarity_m_biodiversity <- 
  ggplot(lookslike_m_biodiversity) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Biodiversity'", subtitle = "Media")


overlapping_n_biodiversity_toprank <-
  ggplot(toprank_n_biodiversity %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "biodiversity", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Biodiversity", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))


toprank_biodiversity_allinstitutions <- 
  lookslike_a_biodiversity %>% mutate(corpus = "academic") %>% filter(year==2021) %>%
  bind_rows(lookslike_n_biodiversity %>% mutate(corpus = "ngo") %>% filter(year==2021)) %>%
  bind_rows(lookslike_m_biodiversity %>% mutate(corpus = "media") %>% filter(year==2021)) %>%
  bind_rows(lookslike_p_biodiversity %>% mutate(corpus = "IPBES") %>% filter(year==2019)) %>%
  bind_rows(lookslike_p_biodiversity %>% mutate(corpus = "UNCBD") %>% filter(year==2022)) %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(min(rank)<=10, 1, NA),
         topsimilarity = ifelse(max(similarity)>=0.8, 1, NA),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES", "UNCBD"), ordered = T)) %>%
  ungroup()


overlapping_allinstitutions_biodiversity <-
  ggplot(toprank_biodiversity_allinstitutions %>%
           filter(!is.na(toprank)) %>%
           arrange(corpus, desc(rank), desc(term2)) %>% 
           mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = corpus, fill = similarity)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = paste("Similarity with\n'", "biodiversity", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", 
                              "IPBES\n(2019)", "UNCBD\n(2022)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Biodiversity", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))


# ---- INCLUSIVE ----

# Calculate semantic displacement for INCLUSIVE in different corpora

semanticdisplace_n_inclusive <- semanticDisplacement(word = "inclusive", input_suffix = "n",
                                                     ref_year = 2017, compare_years = 2018:2021)

semanticdisplace_m_inclusive <- semanticDisplacement(word = "inclusive", input_suffix = "m",
                                                     ref_year = 2017, compare_years = 2018:2021)


inclusive_n_similarity_corrplot <- 
  semanticdisplace_n_inclusive %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Inclusive", subtitle = "NGOs")

inclusive_m_similarity_corrplot <- 
  semanticdisplace_m_inclusive %>% 
  ggcorrplot(type = "upper", outline.col = "white") + 
  labs(title = "Inclusive", subtitle = "Media")


lookslike_n_inclusive <- looksLike_byWord("inclusive", "n", years = 2017:2021, top_n = 50)

toprank_n_inclusive <- 
  lookslike_n_inclusive %>%
  left_join(looksLike_byWord("inclusive", "n", stopwords = "noverbs", years = 2017:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))


overlapping_n_inclusive_toprank <-
  ggplot(toprank_n_inclusive %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "inclusive", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Inclusive", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))


# ---- COMMUNITY ----

# Calculate semantic displacement for COMMUNITY in different corpora
semanticdisplace_a_community <- semanticDisplacement(word = "community", input_suffix = "a",
                                                        ref_year = 2017, compare_years = 2018:2021)

semanticdisplace_n_community <- semanticDisplacement(word = "community", input_suffix = "n",
                                                        ref_year = 2017, compare_years = 2018:2021)

semanticdisplace_m_community <- semanticDisplacement(word = "community", input_suffix = "m",
                                                     ref_year = 2017, compare_years = 2018:2021)



community_a_similarity_corrplot <- 
  semanticdisplace_a_community %>% 
  ggcorrplot(type = "upper", outline.col = "white") + 
  labs(title = "Community", subtitle = "Academic")

community_n_similarity_corrplot <- 
  semanticdisplace_n_community %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Community", subtitle = "NGOs")

community_m_similarity_corrplot <- 
  semanticdisplace_m_community %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Community", subtitle = "Media")


# Calculate pairwise word similarities for COMMUNITY in different corpora
lookslike_a_community <- looksLike_byWord("community", "a", years = 2017:2021, top_n = 50)
lookslike_n_community <- looksLike_byWord("community", "n", years = 2017:2021, top_n = 50)
lookslike_m_community <- looksLike_byWord("community", "m", years = 2017:2021, top_n = 50)
lookslike_p_community <- looksLike_byWord("community", "p", years = c(2019, 2022), top_n = 50)


toprank_community_allinstitutions <- 
  lookslike_a_community %>% mutate(corpus = "academic") %>% filter(year==2021) %>%
  bind_rows(lookslike_n_community %>% mutate(corpus = "ngo") %>% filter(year==2021)) %>%
  bind_rows(lookslike_m_community %>% mutate(corpus = "media") %>% filter(year==2021)) %>%
  bind_rows(lookslike_p_community %>% mutate(corpus = "IPBES") %>% filter(year==2019)) %>%
  bind_rows(lookslike_p_community %>% mutate(corpus = "UNCBD") %>% filter(year==2022)) %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(min(rank)<=10, 1, NA),
         topsimilarity = ifelse(max(similarity)>=0.8, 1, NA),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES", "UNCBD"), ordered = T)) %>%
  ungroup()


overlapping_allinstitutions_community <-
  ggplot(toprank_community_allinstitutions %>%
           filter(!is.na(toprank)) %>%
           arrange(corpus, desc(rank), desc(term2)) %>% 
           mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = corpus, fill = similarity)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = paste("Similarity with\n'", "community", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", 
                              "IPBES\n(2019)", "UNCBD\n(2022)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Community", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))


# ---- LOCAL ----

# Calculate semantic displacement for LOCAL in different corpora
semanticdisplace_a_local <- semanticDisplacement(word = "local", input_suffix = "a",
                                                     ref_year = 2017, compare_years = 2018:2021)

semanticdisplace_n_local <- semanticDisplacement(word = "local", input_suffix = "n",
                                                     ref_year = 2017, compare_years = 2018:2021)

semanticdisplace_m_local <- semanticDisplacement(word = "local", input_suffix = "m",
                                                     ref_year = 2017, compare_years = 2018:2021)



local_a_similarity_corrplot <- 
  semanticdisplace_a_local %>% 
  ggcorrplot(type = "upper", outline.col = "white") + 
  labs(title = "Local", subtitle = "Academic")

local_n_similarity_corrplot <- 
  semanticdisplace_n_local %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Local", subtitle = "NGOs")

local_m_similarity_corrplot <- 
  semanticdisplace_m_local %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Local", subtitle = "Media")


# Calculate pairwise word similarities for LOCAL in different corpora
lookslike_a_local <- looksLike_byWord("local", "a", years = 2017:2021, top_n = 50)
lookslike_n_local <- looksLike_byWord("local", "n", years = 2017:2021, top_n = 50)
lookslike_m_local <- looksLike_byWord("local", "m", years = 2017:2021, top_n = 50)
lookslike_p_local <- looksLike_byWord("local", "p", years = c(2019, 2022), top_n = 50)


toprank_local_allinstitutions <- 
  lookslike_a_local %>% mutate(corpus = "academic") %>% filter(year==2021) %>%
  bind_rows(lookslike_n_local %>% mutate(corpus = "ngo") %>% filter(year==2021)) %>%
  bind_rows(lookslike_m_local %>% mutate(corpus = "media") %>% filter(year==2021)) %>%
  bind_rows(lookslike_p_local %>% mutate(corpus = "IPBES") %>% filter(year==2019)) %>%
  bind_rows(lookslike_p_local %>% mutate(corpus = "UNCBD") %>% filter(year==2022)) %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(min(rank)<=10, 1, NA),
         topsimilarity = ifelse(max(similarity)>=0.8, 1, NA),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES", "UNCBD"), ordered = T)) %>%
  ungroup()


overlapping_allinstitutions_local <-
  ggplot(toprank_local_allinstitutions %>%
           filter(!is.na(toprank)) %>%
           arrange(corpus, desc(rank), desc(term2)) %>% 
           mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = corpus, fill = similarity)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = paste("Similarity with\n'", "local", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", 
                              "IPBES\n(2019)", "UNCBD\n(2022)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Local", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))


# ---- SUSTAINABLE ----

# Calculate semantic displacement for SUSTAINABLE in different corpora
semanticdisplace_a_sustainable <- semanticDisplacement(word = "sustainable", input_suffix = "a",
                                                 ref_year = 2017, compare_years = 2018:2021)

semanticdisplace_n_sustainable <- semanticDisplacement(word = "sustainable", input_suffix = "n",
                                                 ref_year = 2017, compare_years = 2018:2021)

semanticdisplace_m_sustainable <- semanticDisplacement(word = "sustainable", input_suffix = "m",
                                                 ref_year = 2017, compare_years = 2018:2021)



sustainable_a_similarity_corrplot <- 
  semanticdisplace_a_sustainable %>% 
  ggcorrplot(type = "upper", outline.col = "white") + 
  labs(title = "Sustainable", subtitle = "Academic")

sustainable_n_similarity_corrplot <- 
  semanticdisplace_n_sustainable %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Sustainable", subtitle = "NGOs")

sustainable_m_similarity_corrplot <- 
  semanticdisplace_m_sustainable %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Sustainable", subtitle = "Media")


# Calculate pairwise word similarities for SUSTAINABLE in different corpora
lookslike_a_sustainable <- looksLike_byWord("sustainable", "a", years = 2017:2021, top_n = 50)
lookslike_n_sustainable <- looksLike_byWord("sustainable", "n", years = 2017:2021, top_n = 50)
lookslike_m_sustainable <- looksLike_byWord("sustainable", "m", years = 2017:2021, top_n = 50)
lookslike_p_sustainable <- looksLike_byWord("sustainable", "p", years = c(2019, 2022), top_n = 50)


toprank_sustainable_allinstitutions <- 
  lookslike_a_sustainable %>% mutate(corpus = "academic") %>% filter(year==2021) %>%
  bind_rows(lookslike_n_sustainable %>% mutate(corpus = "ngo") %>% filter(year==2021)) %>%
  bind_rows(lookslike_m_sustainable %>% mutate(corpus = "media") %>% filter(year==2021)) %>%
  bind_rows(lookslike_p_sustainable %>% mutate(corpus = "IPBES") %>% filter(year==2019)) %>%
  bind_rows(lookslike_p_sustainable %>% mutate(corpus = "UNCBD") %>% filter(year==2022)) %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(min(rank)<=10, 1, NA),
         topsimilarity = ifelse(max(similarity)>=0.8, 1, NA),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES", "UNCBD"), ordered = T)) %>%
  ungroup()


overlapping_allinstitutions_sustainable <-
  ggplot(toprank_sustainable_allinstitutions %>%
           filter(!is.na(toprank)) %>%
           arrange(corpus, desc(rank), desc(term2)) %>% 
           mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = corpus, fill = similarity)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = paste("Similarity with\n'", "sustainable", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", 
                              "IPBES\n(2019)", "UNCBD\n(2022)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Sustainable", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))




# ---- NATURE-BASED SOLUTIONS ----

# Calculate semantic displacement for NATURE-BASED in different corpora
semanticdisplace_n_naturebased<- semanticDisplacement(word = "naturebased", input_suffix = "n", stopwords = "noverbs",
                                                     ref_year = 2017, compare_years = 2018:2021)

naturebased_n_similarity_corrplot <- 
  semanticdisplace_n_naturebased %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Nature-Based", subtitle = "NGOs")


lookslike_n_naturebased <- looksLike_byWord("naturebased", "n", years = 2017:2021, top_n = 50)

toprank_n_naturebased <- 
  lookslike_n_naturebased %>%
  left_join(looksLike_byWord("naturebased", "n", stopwords = "noverbs", years = 2017:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))

topsimilarity_n_naturebased <- 
  lookslike_n_naturebased %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(max(similarity>=0.76), 1, NA)) %>%
  ungroup()


similarity_n_naturebased <- 
  ggplot(lookslike_n_naturebased) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Nature-based'", subtitle = "NGOs")


overlapping_n_naturebased_toprank <-
  ggplot(topsimilarity_n_naturebased %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "nature-based", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Nature-Based", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))



# ---- INVESTMENT ----

lookslike_n_investment <- looksLike_byWord("investment", "n", years = 2017:2021, top_n = 50)
lookslike_a_investment <- looksLike_byWord("investment", "a", years = 2017:2021, top_n = 50)
lookslike_m_investment <- looksLike_byWord("investment", "m", years = 2017:2021, top_n = 50)
lookslike_p_investment <- looksLike_byWord("investment", "p", years = 2019, top_n = 50)

toprank_n_investment <- 
  lookslike_n_investment %>%
  left_join(looksLike_byWord("investment", "n", stopwords = "noverbs", years = 2017:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))

topsimilarity_n_investment <- 
  lookslike_n_investment %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(max(similarity>=0.76), 1, NA)) %>%
  ungroup()

overlapping_n_investment_toprank <-
  ggplot(topsimilarity_n_investment %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "investment", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Investment", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))

toprank_investment_allinstitutions <- 
  lookslike_a_investment %>% mutate(corpus = "academic") %>% filter(year==2021) %>%
  bind_rows(lookslike_n_investment %>% mutate(corpus = "ngo") %>% filter(year==2021)) %>%
  bind_rows(lookslike_m_investment %>% mutate(corpus = "media") %>% filter(year==2021)) %>%
  bind_rows(lookslike_p_investment %>% mutate(corpus = "IPBES") %>% filter(year==2019)) %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(min(rank)<=10, 1, NA),
         topsimilarity = ifelse(max(similarity)>=0.8, 1, NA),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES"), ordered = T)) %>%
  ungroup()


overlapping_allinstitutions_investment <-
  ggplot(toprank_investment_allinstitutions %>%
           filter(!is.na(toprank)) %>%
           arrange(corpus, desc(rank), desc(term2)) %>% 
           mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = corpus, fill = similarity)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = paste("Similarity with\n'", "investment", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", 
                              "IPBES\n(2019)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Investment", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))


# ---- STAKEHOLDER ----

lookslike_n_stakeholder <- looksLike_byWord("stakeholder", "n", years = 2017:2021, top_n = 50)
lookslike_a_stakeholder <- looksLike_byWord("stakeholder", "a", years = 2017:2021, top_n = 50)
lookslike_m_stakeholder <- looksLike_byWord("stakeholder", "m", years = 2017:2021, top_n = 50)
lookslike_p_stakeholder <- looksLike_byWord("stakeholder", "p", years = 2019, top_n = 50)

toprank_n_stakeholder <- 
  lookslike_n_stakeholder %>%
  left_join(looksLike_byWord("stakeholder", "n", stopwords = "noverbs", years = 2017:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))

topsimilarity_n_stakeholder <- 
  lookslike_n_stakeholder %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(max(similarity>=0.76), 1, NA)) %>%
  ungroup()

overlapping_n_stakeholder_toprank <-
  ggplot(topsimilarity_n_stakeholder %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "stakeholder", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Stakeholder", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))

toprank_stakeholder_allinstitutions <- 
  lookslike_a_stakeholder %>% mutate(corpus = "academic") %>% filter(year==2021) %>%
  bind_rows(lookslike_n_stakeholder %>% mutate(corpus = "ngo") %>% filter(year==2021)) %>%
  bind_rows(lookslike_m_stakeholder %>% mutate(corpus = "media") %>% filter(year==2021)) %>%
  bind_rows(lookslike_p_stakeholder %>% mutate(corpus = "IPBES") %>% filter(year==2019)) %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(min(rank)<=10, 1, NA),
         topsimilarity = ifelse(max(similarity)>=0.8, 1, NA),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES"), ordered = T)) %>%
  ungroup()


overlapping_allinstitutions_stakeholder <-
  ggplot(toprank_stakeholder_allinstitutions %>%
           filter(!is.na(toprank)) %>%
           arrange(corpus, desc(rank), desc(term2)) %>% 
           mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = corpus, fill = similarity)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = paste("Similarity with\n'", "stakeholder", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", 
                              "IPBES\n(2019)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Stakeholder", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))


# ---- HOPE ----

# Calculate semantic displacement for HOPE in different corpora
semanticdisplace_n_hope <- semanticDisplacement(word = "hope", input_suffix = "n",
                                                       ref_year = 2017, compare_years = 2018:2021)

semanticdisplace_m_hope <- semanticDisplacement(word = "hope", input_suffix = "m",
                                                ref_year = 2017, compare_years = 2018:2021)

hope_n_similarity_corrplot <- 
  semanticdisplace_n_hope %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Hope", subtitle = "NGOs")

hope_m_similarity_corrplot <- 
  semanticdisplace_m_hope %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Hope", subtitle = "Media")


lookslike_n_hope <- looksLike_byWord("hope", "n", years = 2017:2021, top_n = 50)
lookslike_m_hope <- looksLike_byWord("hope", "m", years = 2017:2021, top_n = 50)

similarity_n_hope <- 
  ggplot(lookslike_n_hope) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Hope'", subtitle = "NGOs")

similarity_m_hope <- 
  ggplot(lookslike_m_hope) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Hope'", subtitle = "Media")


toprank_hope_allinstitutions <- 
  lookslike_a_hope %>% mutate(corpus = "academic") %>% filter(year==2021) %>%
  bind_rows(lookslike_n_hope %>% mutate(corpus = "ngo") %>% filter(year==2021)) %>%
  bind_rows(lookslike_m_hope %>% mutate(corpus = "media") %>% filter(year==2021)) %>%
  bind_rows(lookslike_p_hope %>% mutate(corpus = "IPBES") %>% filter(year==2019)) %>%
  bind_rows(lookslike_p_hope %>% mutate(corpus = "UNCBD") %>% filter(year==2022)) %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(min(rank)<=10, 1, NA),
         topsimilarity = ifelse(max(similarity)>=0.8, 1, NA),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES", "UNCBD"), ordered = T)) %>%
  ungroup()


overlapping_allinstitutions_hope <-
  ggplot(toprank_hope_allinstitutions %>%
           filter(!is.na(toprank)) %>%
           arrange(corpus, desc(rank), desc(term2)) %>% 
           mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = corpus, fill = similarity)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = paste("Similarity with\n'", "hope", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", 
                              "IPBES\n(2019)", "UNCBD\n(2022)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Hope", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))


# ---- CONSERVATION ----

# Calculate semantic displacement for CONSERVATION in different corpora
semanticdisplace_a_conservation <- semanticDisplacement(word = "conservation", input_suffix = "a",
                                                ref_year = 2000, compare_years = 2001:2021)

semanticdisplace_n_conservation <- semanticDisplacement(word = "conservation", input_suffix = "n",
                                                ref_year = 2017, compare_years = 2018:2021)

semanticdisplace_m_conservation <- semanticDisplacement(word = "conservation", input_suffix = "m",
                                                ref_year = 2017, compare_years = 2018:2021)

conservation_a_similarity_corrplot <- 
  semanticdisplace_a_conservation %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Conservation", subtitle = "Academic")

conservation_n_similarity_corrplot <- 
  semanticdisplace_n_conservation %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Conservation", subtitle = "NGOs")

conservation_m_similarity_corrplot <- 
  semanticdisplace_m_conservation %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Conservation", subtitle = "Media")


lookslike_a_conservation <- looksLike_byWord("conservation", "a", years = 2017:2021, top_n = 50)
lookslike_n_conservation <- looksLike_byWord("conservation", "n", years = 2017:2021, top_n = 50)
lookslike_m_conservation <- looksLike_byWord("conservation", "m", years = 2017:2021, top_n = 50)
lookslike_p_conservation <- looksLike_byWord("conservation", "p", years = c(2019, 2022), top_n = 50)


toprank_conservation_allinstitutions <- 
  lookslike_a_conservation %>% mutate(corpus = "academic") %>% filter(year==2021) %>%
  bind_rows(lookslike_n_conservation %>% mutate(corpus = "ngo") %>% filter(year==2021)) %>%
  bind_rows(lookslike_m_conservation %>% mutate(corpus = "media") %>% filter(year==2021)) %>%
  bind_rows(lookslike_p_conservation %>% mutate(corpus = "IPBES") %>% filter(year==2019)) %>%
  bind_rows(lookslike_p_conservation %>% mutate(corpus = "UNCBD") %>% filter(year==2022)) %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(min(rank)<=10, 1, NA),
         topsimilarity = ifelse(max(similarity)>=0.8, 1, NA),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES", "UNCBD"), ordered = T)) %>%
  ungroup()


overlapping_allinstitutions_conservation <-
  ggplot(toprank_conservation_allinstitutions %>%
           filter(!is.na(toprank)) %>%
           arrange(corpus, desc(rank), desc(term2)) %>% 
           mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = corpus, fill = similarity)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = paste("Similarity with\n'", "conservation", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", 
                              "IPBES\n(2019)", "UNCBD\n(2022)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Conservation", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))



# ---- ADAPTATION ----

lookslike_a_adaptation <- looksLike_byWord("adaptation", "a", years = 2017:2021, top_n = 50)
lookslike_n_adaptation <- looksLike_byWord("adaptation", "n", years = 2017:2021, top_n = 50)
lookslike_m_adaptation <- looksLike_byWord("adaptation", "m", years = 2017:2021, top_n = 50)
lookslike_p_adaptation <- looksLike_byWord("adaptation", "p", years = 2019, top_n = 50)


toprank_n_adaptation <- 
  lookslike_n_adaptation %>%
  left_join(looksLike_byWord("adaptation", "n", stopwords = "noverbs", years = 2017:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))

overlapping_n_adaptation_toprank <-
  ggplot(toprank_n_adaptation %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "adaptation", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Adaptation", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))



toprank_adaptation_allinstitutions <- 
  lookslike_a_adaptation %>% mutate(corpus = "academic") %>% filter(year==2021) %>%
  bind_rows(lookslike_n_adaptation %>% mutate(corpus = "ngo") %>% filter(year==2021)) %>%
  bind_rows(lookslike_m_adaptation %>% mutate(corpus = "media") %>% filter(year==2021)) %>%
  bind_rows(lookslike_p_adaptation %>% mutate(corpus = "IPBES") %>% filter(year==2019)) %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(min(rank)<=10, 1, NA),
         topsimilarity = ifelse(max(similarity)>=0.8, 1, NA),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES"), ordered = T)) %>%
  ungroup()


overlapping_allinstitutions_adaptation <-
  ggplot(toprank_adaptation_allinstitutions %>%
           filter(!is.na(toprank)) %>%
           arrange(corpus, desc(rank), desc(term2)) %>% 
           mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = corpus, fill = similarity)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = paste("Similarity with\n'", "adaptation", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", 
                              "IPBES\n(2019)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Adaptation", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))


# ---- VULNERABLE ----

lookslike_a_vulnerable <- looksLike_byWord("vulnerable", "a", years = 2017:2021, top_n = 50)
lookslike_n_vulnerable <- looksLike_byWord("vulnerable", "n", years = 2017:2021, top_n = 50)
lookslike_m_vulnerable <- looksLike_byWord("vulnerable", "m", years = 2017:2021, top_n = 50)
lookslike_p_vulnerable <- looksLike_byWord("vulnerable", "p", years = 2019, top_n = 50)


toprank_n_vulnerable <- 
  lookslike_n_vulnerable %>%
  left_join(looksLike_byWord("vulnerable", "n", stopwords = "noverbs", years = 2017:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))

overlapping_n_vulnerable_toprank <-
  ggplot(toprank_n_vulnerable %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "vulnerable", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Vulnerable", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))



toprank_vulnerable_allinstitutions <- 
  lookslike_a_vulnerable %>% mutate(corpus = "academic") %>% filter(year==2021) %>%
  bind_rows(lookslike_n_vulnerable %>% mutate(corpus = "ngo") %>% filter(year==2021)) %>%
  bind_rows(lookslike_m_vulnerable %>% mutate(corpus = "media") %>% filter(year==2021)) %>%
  bind_rows(lookslike_p_vulnerable %>% mutate(corpus = "IPBES") %>% filter(year==2019)) %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(min(rank)<=10, 1, NA),
         topsimilarity = ifelse(max(similarity)>=0.8, 1, NA),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES"), ordered = T)) %>%
  ungroup()


overlapping_allinstitutions_vulnerable <-
  ggplot(toprank_vulnerable_allinstitutions %>%
           filter(!is.na(toprank)) %>%
           arrange(corpus, desc(rank), desc(term2)) %>% 
           mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = corpus, fill = similarity)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = paste("Similarity with\n'", "vulnerable", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", 
                              "IPBES\n(2019)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Vulnerable", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))


# ---- INDIGENOUS ----

lookslike_a_indigenous <- looksLike_byWord("indigenous", "a", years = 2017:2021, top_n = 50)
lookslike_n_indigenous <- looksLike_byWord("indigenous", "n", years = 2017:2021, top_n = 50)
lookslike_m_indigenous <- looksLike_byWord("indigenous", "m", years = 2017:2021, top_n = 50)
lookslike_p_indigenous <- looksLike_byWord("indigenous", "p", years = c(2019, 2022), top_n = 50)


toprank_n_indigenous <- 
  lookslike_n_indigenous %>%
  left_join(looksLike_byWord("indigenous", "n", stopwords = "noverbs", years = 2017:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))

overlapping_n_indigenous_toprank <-
  ggplot(toprank_n_indigenous %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "indigenous", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Indigenous", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))



toprank_indigenous_allinstitutions <- 
  lookslike_a_indigenous %>% mutate(corpus = "academic") %>% filter(year==2021) %>%
  bind_rows(lookslike_n_indigenous %>% mutate(corpus = "ngo") %>% filter(year==2021)) %>%
  bind_rows(lookslike_m_indigenous %>% mutate(corpus = "media") %>% filter(year==2021)) %>%
  bind_rows(lookslike_p_indigenous %>% mutate(corpus = "IPBES") %>% filter(year==2019)) %>%
  bind_rows(lookslike_p_indigenous %>% mutate(corpus = "UNCBD") %>% filter(year==2022)) %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(min(rank)<=10, 1, NA),
         topsimilarity = ifelse(max(similarity)>=0.8, 1, NA),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES", "UNCBD"), ordered = T)) %>%
  ungroup()


overlapping_allinstitutions_indigenous <-
  ggplot(toprank_indigenous_allinstitutions %>%
           filter(!is.na(toprank)) %>%
           arrange(corpus, desc(rank), desc(term2)) %>% 
           mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = corpus, fill = similarity)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = paste("Similarity with\n'", "indigenous", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", 
                              "IPBES\n(2019)", "UNCBD\n(2022)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Indigenous", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))


# ---- MOVEMENT ----

# Calculate semantic displacement for MOVEMENT in different corpora
semanticdisplace_n_movement_noverbs <- semanticDisplacement(word = "movement", input_suffix = "n", stopwords = "noverbs",
                                                               ref_year = 2018, compare_years = 2019:2021)


movement_n_similarity_corrplot <- 
  semanticdisplace_n_movement_noverbs %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Movement", subtitle = "NGOs")


lookslike_n_movement <- looksLike_byWord("movement", "n", years = 2018:2021, top_n = 50)

toprank_n_movement <- 
  lookslike_n_movement %>%
  left_join(looksLike_byWord("movement", "n", stopwords = "noverbs", years = 2018:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))

topsimilarity_n_movement <- 
  lookslike_n_movement %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(max(similarity>=0.75), 1, NA)) %>%
  ungroup()


similarity_n_movement <- 
  ggplot(lookslike_n_movement) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Movement'", subtitle = "NGOs")


overlapping_n_movement_toprank <-
  ggplot(topsimilarity_n_movement %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "movement", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Movement", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))


# ---- YOUTH ----

# Calculate semantic displacement for YOUTH in different corpora
semanticdisplace_n_youth <- semanticDisplacement(word = "youth", input_suffix = "n", stopwords = "noverbs",
                                                            ref_year = 2017, compare_years = 2018:2021)


youth_n_similarity_corrplot <- 
  semanticdisplace_n_youth_noverbs %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Youth", subtitle = "NGOs")


lookslike_n_youth <- looksLike_byWord("youth", "n", years = 2017:2021, top_n = 50)

toprank_n_youth <- 
  lookslike_n_youth %>%
  left_join(looksLike_byWord("youth", "n", stopwords = "noverbs", years = 2017:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))

topsimilarity_n_youth <- 
  lookslike_n_youth %>%
  group_by(term2) %>%
  mutate(toprank = ifelse(max(similarity>=0.78), 1, NA)) %>%
  ungroup()


similarity_n_youth <- 
  ggplot(lookslike_n_youth) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Youth'", subtitle = "NGOs")


overlapping_n_youth_toprank <-
  ggplot(topsimilarity_n_youth %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "youth", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Youth", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))



# ---- EXPORT ----

output_dir <- "data/outputs/figures/heatmaps/"
  
exportOverlapPlots <- function(plot, filename, height = 6, width = 8) {
  png(paste(output_dir, "/", filename, ".png", sep = ""),
      units = "in", height = height, width = width, res = 400)
  grid.newpage()
  grid.draw(plot)
  dev.off()
}


exportOverlapPlots(overlapping_allinstitutions_community, "community_allinstitutions", height = 9)
exportOverlapPlots(overlapping_allinstitutions_local, "local_allinstitutions", height = 9)
exportOverlapPlots(overlapping_allinstitutions_sustainable, "sustainable_allinstitutions", height = 9)
exportOverlapPlots(overlapping_allinstitutions_biodiversity, "biodiversity_allinstitutions", height = 9)
exportOverlapPlots(overlapping_allinstitutions_adaptation, "adaptation_allinstitutions", height = 9)
exportOverlapPlots(overlapping_allinstitutions_vulnerable, "vulnerable_allinstitutions", height = 9)
exportOverlapPlots(overlapping_allinstitutions_indigenous, "indigenous_allinstitutions", height = 9)
exportOverlapPlots(overlapping_allinstitutions_stakeholder, "stakeholder_allinstitutions", height = 9)
exportOverlapPlots(overlapping_n_youth_toprank, "youth_n", height = 9)
exportOverlapPlots(overlapping_n_movement_toprank, "movement_n", height = 9)
exportOverlapPlots(overlapping_n_naturebased_toprank, "naturebased_n", height = 9)
exportOverlapPlots(overlapping_n_inclusive_toprank, "inclusive_n", height = 9)
exportOverlapPlots(overlapping_n_adaptation_toprank, "adaptation_n", height = 9)
exportOverlapPlots(overlapping_n_vulnerable_toprank, "vulnerable_n", height = 9)
exportOverlapPlots(overlapping_n_indigenous_toprank, "indigenous_n", height = 9)
exportOverlapPlots(overlapping_n_investment_toprank, "investment_n", height = 9)
