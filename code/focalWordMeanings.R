
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

stopwords_extended <- data.frame(word = stopwords_extended, stopword = 1)
stopwords_basic <- data.frame(word = stopwords::stopwords(source = "smart"), stopword = 1)

collocates_stopwords <- data.frame(bigram = c("birdlife international", "birdlife partner", "conservation international",
                                              "nature conservancy", "david shepherd wildlife foundation", "david shepherd", 
                                              "world wildlife fund", "wetlands international", "unite state"),
                                   stopword = 1)

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
cleanTexts(data = docs_a, years = 2000:2021, input_suffix = "a", stopwords_list = "stopwords_extended")
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
word2vec_percorpus(data = texts_n, input_suffix = "n", stopwords = "noverbs", type = "skip-gram", dim = 50)
word2vec_percorpus(data = texts_a, input_suffix = "a", stopwords = "noverbs", years = 2000:2021, type = "skip-gram", dim = 50)
word2vec_percorpus(data = texts_m, input_suffix = "m", stopwords = "noverbs", type = "skip-gram", dim = 50)
word2vec_percorpus(data = texts_p, input_suffix = "p", stopwords = "noverbs", years = c(2019, 2022), type = "skip-gram", dim = 50)

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
word2vec_n_2021_matrix <- scale(as.matrix(word2vec_n_2021))
word2vec_n_2021_cov <- cov(word2vec_n_2021_matrix)
pca_n_2021 <- prcomp(word2vec_n_2021_cov, center = TRUE, scale. = TRUE, retx = TRUE)
summary(pca_n_2021)

word_positions_pca_n_2021 <- data.frame(PC1 = numeric(0), PC2 = numeric(0),
                                        PC3 = numeric(0), PC4 = numeric(0),
                                        PC5 = numeric(0))

for(i in rownames(word2vec_n_2021_matrix)) {
  word_positions_pca_n_2021[i, ] <- t(word2vec_n_2021_matrix)[, i] %*% pca_n_2021$x[, 1:5]
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
  pivot_longer(cols = c("PC1", "PC2", "PC3", "PC4", "PC5"), names_to = "PC", values_to = "PC_score") %>%
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
lookslike_a_biodiversity <- looksLike_byWord("biodiversity", "a", years = 2000:2021, top_n = 50)
lookslike_a_biodiversity_wide <- pivot_wider(lookslike_a_biodiversity, 
                                             values_from = c(similarity, rank), names_from = year)


lookslike_n_biodiversity <- looksLike_byWord("biodiversity", "n", years = 2017:2021, top_n = 50)
lookslike_n_biodiversity_wide <- pivot_wider(lookslike_n_biodiversity, 
                                             values_from = c(similarity, rank), names_from = year)

lookslike_n_biodiversity_verbs <- looksLike_byWord("biodiversity", "n", stopwords = "verbs", years = 2017:2021, top_n = 50)
lookslike_n_biodiversity_verbs_wide <- pivot_wider(lookslike_n_biodiversity_verbs, 
                                             values_from = c(similarity, rank), names_from = year)

lookslike_m_biodiversity <- looksLike_byWord("biodiversity", "m", years = 2017:2021, top_n = 50)
lookslike_m_biodiversity_wide <- pivot_wider(lookslike_m_biodiversity, 
                                             values_from = c(similarity, rank), names_from = year)

lookslike_p_biodiversity <- looksLike_byWord("biodiversity", "p", years = c(2019, 2022), top_n = 50)
lookslike_p_biodiversity_wide <- pivot_wider(lookslike_p_biodiversity, 
                                             values_from = c(similarity, rank), names_from = year)

toprank_n_biodiversity_noverbs <- 
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



overlapping_n_biodiversity <-
  ggplot(lookslike_n_biodiversity %>% filter(similarity>=0.85) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "biodiversity", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Biodiversity similarity", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))

overlapping_n_biodiversity_verbs <-
  ggplot(lookslike_n_biodiversity_verbs %>% filter(similarity>=0.75) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "biodiversity", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Biodiversity similarity (verbs included)", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))


overlapping_n_biodiversity_toprank_noverbs <-
  ggplot(toprank_n_biodiversity_noverbs %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "biodiversity", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Biodiversity similarity (verbs excluded)", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))


overlapping_a_biodiversity <-
  ggplot(lookslike_a_biodiversity %>% filter(similarity>=0.85 & year%in%c(2017:2021)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "biodiversity", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Biodiversity similarity", subtitle = "Academic", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))

overlapping_m_biodiversity <-
  ggplot(lookslike_m_biodiversity %>% filter(similarity>=0.87) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "biodiversity", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Biodiversity similarity", subtitle = "Media", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))

overlapping_p_biodiversity <-
  ggplot(lookslike_p_biodiversity %>% filter(similarity>=0.85) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "biodiversity", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Biodiversity similarity", subtitle = "Policy", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))


overlapping_allinstitutions_biodiversity <-
  ggplot(lookslike_a_biodiversity %>% mutate(corpus="a") %>% filter(year==2021) %>%
           bind_rows(lookslike_n_biodiversity %>% mutate(corpus="n") %>% filter(year==2021)) %>% 
           bind_rows(lookslike_m_biodiversity %>% mutate(corpus="m") %>% filter(year==2021)) %>%
           filter(similarity>=0.85) %>%
           arrange(corpus, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = corpus, fill = similarity)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = paste("Similarity with\n'", "biodiversity", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)"),
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

node_freq_n %>% filter(node=="inclusive")

lookslike_n_inclusive <- looksLike_byWord("inclusive", "n", years = 2017:2021, top_n = 20)
lookslike_n_inclusive_wide <- pivot_wider(lookslike_n_inclusive, 
                                          values_from = c(similarity, rank), names_from = year)

toprank_n_inclusive_noverbs <- 
  lookslike_n_inclusive %>%
  left_join(looksLike_byWord("inclusive", "n", stopwords = "noverbs", years = 2017:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))


overlapping_n_inclusive <-
  ggplot(lookslike_n_inclusive %>% filter(similarity>=0.85) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "inclusive", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Inclusive similarity", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))

overlapping_n_inclusive_toprank_noverbs <-
  ggplot(toprank_n_inclusive_noverbs %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "inclusive", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Inclusive similarity (verbs excluded)", subtitle = "NGOs", y = "", x = "") +
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
lookslike_a_community <- looksLike_byWord("community", "a", years = 2000:2021, top_n = 20)
lookslike_a_community_wide <- pivot_wider(lookslike_a_community, 
                                             values_from = c(similarity, rank), names_from = year)


lookslike_n_community <- looksLike_byWord("community", "n", years = 2017:2021, top_n = 20)
lookslike_n_community_wide <- pivot_wider(lookslike_n_community, 
                                             values_from = c(similarity, rank), names_from = year)

lookslike_m_community <- looksLike_byWord("community", "m", years = 2017:2021, top_n = 20)
lookslike_m_community_wide <- pivot_wider(lookslike_m_community, 
                                          values_from = c(similarity, rank), names_from = year)


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



# ---- NATURE-BASED SOLUTIONS ----

# Calculate semantic displacement for NATURE-BASED in different corpora
semanticdisplace_n_naturebased_noverbs <- semanticDisplacement(word = "naturebased", input_suffix = "n", stopwords = "noverbs",
                                                     ref_year = 2017, compare_years = 2018:2021)

semanticdisplace_n_naturebased_verbs <- semanticDisplacement(word = "naturebased", input_suffix = "n", stopwords = "verbs",
                                                       ref_year = 2017, compare_years = 2018:2021)

naturebased_n_similarity_corrplot <- 
  semanticdisplace_n_naturebased_noverbs %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Nature-Based", subtitle = "NGOs")

naturebased_n_similarity_corrplot_verbs <- 
  semanticdisplace_n_naturebased_verbs %>% 
  ggcorrplot(type = "upper", outline.col = "white") +
  labs(title = "Nature-Based (with verbs)", subtitle = "NGOs")


lookslike_n_naturebased_noverbs <- looksLike_byWord("naturebased", "n", stopwords = "noverbs", years = 2017:2021, top_n = 50)
lookslike_n_naturebased_noverbs_wide <- pivot_wider(lookslike_n_naturebased_noverbs, 
                                          values_from = c(similarity, rank), names_from = year)

lookslike_n_naturebased_verbs <- looksLike_byWord("naturebased", "n", stopwords = "verbs", years = 2017:2021, top_n = 50)
lookslike_n_naturebased_verbs_wide <- pivot_wider(lookslike_n_naturebased_verbs, 
                                            values_from = c(similarity, rank), names_from = year)

toprank_n_naturebased_verbs <- 
  lookslike_n_naturebased_verbs %>%
  left_join(looksLike_byWord("naturebased", "n", stopwords = "verbs", years = 2017:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))

toprank_n_naturebased_noverbs <- 
  lookslike_n_naturebased_noverbs %>%
  left_join(looksLike_byWord("naturebased", "n", stopwords = "noverbs", years = 2017:2021, top_n = 10) %>% 
              group_by(term2) %>% summarise(toprank = 1))


similarity_n_naturebased <- 
  ggplot(lookslike_n_naturebased) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Nature-based'", subtitle = "NGOs")


overlapping_n_naturebased <-
  ggplot(lookslike_n_naturebased %>% filter(similarity>=0.7) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "nature-based", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Nature-Based similarity", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))


overlapping_n_naturebased_verbs <-
  ggplot(lookslike_n_naturebased_verbs %>% filter(similarity>=0.75) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "nature-based", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Nature-Based similarity (verbs included)", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))


overlapping_n_naturebased_toprank_verbs <-
  ggplot(toprank_n_naturebased_verbs %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "nature-based", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Nature-Based similarity (verbs included)", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))


overlapping_n_naturebased_toprank_noverbs <-
  ggplot(toprank_n_naturebased_noverbs %>% filter(!is.na(toprank)) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "nature-based", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Nature-Based similarity (verbs excluded)", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))


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


lookslike_n_hope <- looksLike_byWord("hope", "n", years = 2017:2021, top_n = 20)
lookslike_n_hope_wide <- pivot_wider(lookslike_n_hope, 
                                            values_from = c(similarity, rank), names_from = year)

lookslike_m_hope <- looksLike_byWord("hope", "m", years = 2017:2021, top_n = 20)
lookslike_m_hope_wide <- pivot_wider(lookslike_m_hope, 
                                     values_from = c(similarity, rank), names_from = year)



similarity_n_hope <- 
  ggplot(lookslike_n_hope) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Hope'", subtitle = "NGOs")

similarity_m_hope <- 
  ggplot(lookslike_m_hope) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Hope'", subtitle = "Media")


overlapping_n_hope <-
  ggplot(lookslike_n_hope %>% filter(rank<=10) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "hope", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Hope similarity", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))

overlapping_m_hope <-
  ggplot(lookslike_m_hope %>% filter(rank<=10) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "hope", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Hope similarity", subtitle = "Media", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))


# ---- CONSERVATION (non-buzzword) ----

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


lookslike_a_conservation <- looksLike_byWord("conservation", "a", years = 2017:2021, top_n = 20)
lookslike_a_conservation_wide <- pivot_wider(lookslike_a_conservation, 
                                             values_from = c(similarity, rank), names_from = year)

lookslike_n_conservation <- looksLike_byWord("conservation", "n", years = 2017:2021, top_n = 20)
lookslike_n_conservation_wide <- pivot_wider(lookslike_n_conservation, 
                                     values_from = c(similarity, rank), names_from = year)

lookslike_m_conservation <- looksLike_byWord("conservation", "m", years = 2017:2021, top_n = 20)
lookslike_m_conservation_wide <- pivot_wider(lookslike_m_conservation, 
                                     values_from = c(similarity, rank), names_from = year)



similarity_n_conservation <- 
  ggplot(lookslike_n_conservation) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Hope'", subtitle = "NGOs")

similarity_m_conservation <- 
  ggplot(lookslike_m_conservation) + 
  geom_boxplot(aes(x = year, y = similarity, group = year), fill = "#44AA99") + 
  theme_minimal() + labs(title = "'Hope'", subtitle = "Media")


overlapping_n_conservation <-
  ggplot(lookslike_n_conservation %>% filter(rank<=10) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "conservation", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Conservation similarity", subtitle = "NGOs", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))

overlapping_m_conservation <-
  ggplot(lookslike_m_conservation %>% filter(rank<=10) %>% 
           arrange(year, desc(rank), desc(term2)) %>% mutate(term2 = factor(term2, levels = unique(term2))), 
         aes(y = term2, x = year, fill = similarity)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = paste("Similarity with\n'", "conservation", "'", sep = ""),
                       palette = "Blues",
                       direction = 1) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Conservation similarity", subtitle = "Media", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"))


