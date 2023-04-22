#
# code: Co-occurrence network analysis
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: March 2023
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE LIBRARIES, FUNCTIONS, INPUT PARAMETERS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

options(dplyr.summarise.inform = FALSE)
options(spam.force64 = TRUE)


# ---- 1.1 Import libraries ----

pacman::p_load(spam, spam64, tidyverse, quanteda, quanteda.textstats, tidytext, 
               lexicon, stopwords, parallel, igraph)


# ---- 1.2 Source processing & analysis functions ----

source('code/source/processingAnalysisFunctions.R')
source('code/source/calculateCoocStatistics.R') # this is a dependency of coocGraph3Tier() in "processingAnalysisFunctions.R"


# ---- 1.3 Source reference lists ----

source('code/source/stopwordsThesaurus.R')


# ---- 1.4 Identify parameters for analysis ----

input_suffix <- "n"
# "n" for NGO texts, "a" for academic texts, "p" for policy texts, "m" for media texts

years <- 2017:2021
# NGO texts can be between 2017:2021, academic texts between 2000:2021, policy texts either 2018 or 2022, media texts between 2017:2021

consensus_thresholds <- c(0.25, 0.33, 0.5)
# what percent of documents need to contain the link (i.e., the co-occurrence) for it to be considered as having "consensus"?
# we classify words using different consensus thresholds, with a lower consensus threshold a lower bar for being considered as having "consensus"

percentile_thresholds <- c(0.3, 0.45, 0.5)
# what should be the cut-off percentile for the network measures to be considered "low"? All in increments of 0.05.
# (note that the "high" cut-off would be 1 minus the indicated threshold)



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: IMPORT DATA, RUN PROCESSING FUNCTIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 Import corpus documents ----

docs_a <- read_csv("data/corpora/processed/docs_a.csv", locale = readr::locale(encoding = "UTF-8"))
docs_n <- read_csv("data/corpora/processed/docs_n.csv", locale = readr::locale(encoding = "UTF-8"))
docs_m <- read_csv("data/corpora/processed/docs_m_filtered.csv", locale = readr::locale(encoding = "UTF-8"))

# ---- 2.2 Subset DTMs per year, create co-occurrence networks, and export to flat file ----
## COMPUTATIONALLY INTENSIVE -- ONLY RUN ONCE AND THEN SOURCE IN FLAT FILES TO DO ANALYSIS IN SECTION 3 AND BEYOND

coocGraphsPerYear(input_data = docs_m, input_suffix = "m", years = 2021, 
                  coocTerm = "conservation", sd_multiplier = 5, stopword_list = stopwords_pos_extend_m)


 # ---- 2.3 Define node attributes per co-occurrence network, and compare across years ----

findNodeAttributes(input_suffix = "n", 
                   years = 2017:2021, 
                   consensus_thresholds = c(0.25, 0.33, 0.5), 
                   percentile_thresholds = c(0.35, 0.4, 0.45, 0.5),
                   coocTerm = "conservation")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: TRACK WORDS THROUGH TIME ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# NOTE: frequency counts are counts of number of DOCUMENTS a term appears in, not a total word count.

compare_symbol_types_a <-
  node_attributes_a %>% 
  select(-freq) %>%
  left_join(graph_attr_a, by = "year") %>%
  left_join(node_freq_a, by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs) %>%
  group_by(node, consensus_threshold, percentile_threshold) %>%
  mutate(buzzword_years = length(year[symbol_type=="buzzword" & !is.na(symbol_type)]),
         placeholder_years = length(year[symbol_type=="placeholder" & !is.na(symbol_type)]),
         standard_years = length(year[symbol_type=="standard" & !is.na(symbol_type)])) %>%
  ungroup() %>%
  group_by(node, consensus_threshold, percentile_threshold, year) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(node, consensus_threshold, percentile_threshold, buzzword_years, placeholder_years, standard_years), 
              names_from = year, values_from = c(symbol_type, freq, rel_freq))

compare_symbol_types_n <-
  node_attributes_n %>% 
  select(-freq) %>%
  left_join(graph_attr_n, by = "year") %>%
  left_join(node_freq_n, by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs) %>%
  group_by(node, consensus_threshold, percentile_threshold) %>%
  mutate(buzzword_years = length(year[symbol_type=="buzzword" & !is.na(symbol_type)]),
         placeholder_years = length(year[symbol_type=="placeholder" & !is.na(symbol_type)]),
         standard_years = length(year[symbol_type=="standard" & is.na(symbol_type)])) %>%
  pivot_wider(id_cols = c(node, consensus_threshold, percentile_threshold, buzzword_years, placeholder_years, standard_years), 
              names_from = year, values_from = c(symbol_type, freq, rel_freq))

# placeholders_compare_a <-
#   node_attributes_a %>%
#   filter(symbol_type=="placeholder") %>%
#   left_join(graph_attr_a, by = "year") %>%
#   rename("total_nodes" = "nodes") %>%
#   mutate(rel_freq = freq / total_nodes) %>%
#   group_by(node, consensus_threshold, percentile_threshold) %>%
#   mutate(total_years = length(year)) %>%
#   pivot_wider(id_cols = c(node, consensus_threshold, percentile_threshold, total_years), names_from = year, values_from = c(freq, rel_freq))
# 


place_n_c0.5 <- placeholders_compare_n %>% filter(consensus_threshold==0.5)
place_n_c0.33 <- placeholders_compare_n %>% filter(consensus_threshold==0.33)





# ---- Explore ----

# EXPERIMENT
# Consider making the percentile thresholds based on network measures from similarly sized random networks
# Need to call in coocGraph per corpus to get a sense of number of nodes & links, then generate random network and calculate measures
coocGraph_n_2017 <-   read_csv('data/outputs/coocGraphs/20230418/coocGraph_n_2017.csv', 
                               locale = readr::locale(encoding = "UTF-8"))

DTM_n_2017 <- readRDS('data/outputs/DTMs/20230418/DTM_n_2017.rds')

graph_attr_n_2017 <- 
  coocGraph_n_2017 %>%
  summarise(links = length(from)) %>%
  cbind.data.frame(nodes = length(node_attributes_n_2017$node),
                   ndoc = length(docs_n$text[docs_n$year==2017]),
                   nwords_avg = round(mean(ntoken(DTM_n_2017))), #NOTE: these are number of words after stopwords removed
                   nwords_sd = sd(ntoken(DTM_n_2017)),
                   nwords_min = min(ntoken(DTM_n_2017)),
                   nwords_max = max(ntoken(DTM_n_2017)),
                   year = 2017,
                   corpus = "ngo")

g_n_2017 <- sample_gnm(graph_attr_n_2017$nodes, graph_attr_n_2017$links)
g_n_2017_betweenness <- g_n_2017 %>% betweenness(.)
g_n_2017_degree <- g_n_2017 %>% degree(.)


# bind_graphs <- data.frame(from = character(),
#                           to = character(),
#                           cooc = character())
# bind_nodes <- data.frame(node = character(),
#                          run = numeric(0))
# 
# i <- 1
# 
# while(i <= graph_attr_n_2017$ndoc){
#   
#   alt_graph <- 
#     make_full_graph(graph_attr_n_2017$nwords_avg)
#   
#   V(alt_graph)$name <- as.character(sample(V(g_n_2017), graph_attr_n_2017$nwords_avg))
#   
#   cooc_alt_graph <- as.data.frame(ends(alt_graph, E(alt_graph))) %>%
#     rename("from" = "V1",
#            "to" = "V2") %>%
#     mutate(cooc1 = paste(from, to, sep = "-"), 
#            cooc2 = paste(to, from, sep = "-")) %>%
#     pivot_longer(cols = c(cooc1, cooc2), values_to = "cooc") %>%
#     group_by(from, to) %>%
#     summarise(cooc = sort(cooc)[1]) %>%
#     ungroup()
#   
#   bind_graphs <- rbind(bind_graphs, cooc_alt_graph)
#   bind_nodes <- rbind.data.frame(bind_nodes, data.frame(node = V(alt_graph)$name,
#                                                         run = i))
# 
#   i <- i + 1
#     
# }
# 
# node_freq_bind_graphs <- bind_nodes %>% group_by(node) %>% summarise(freq = length(node))
# bind_graphs_n_2017 <- bind_graphs %>% group_by(cooc) %>% summarise(nCooc = length(cooc))
# 
# node_attributes_altgraphs_n_2017 <- 
#   bind_graphs %>%
#   left_join(bind_graphs_n_2017, by = "cooc") %>%
#   distinct() %>%
#   left_join(node_freq_bind_graphs, by = c("from" = "node")) %>%
#   left_join(node_freq_bind_graphs, by = c("to" = "node")) %>%
#   mutate(lower.freq = ifelse(freq.x<freq.y, freq.x, freq.y)) %>%
#   mutate(consensus = ifelse(nCooc/lower.freq>=0.5, 1, 0)) %>% # if co-occurrence exists at least XX% of the time the less frequent of the two nodes is used, counts as "consensus"
#   rename("node" = "from") %>%
#   group_by(node) %>%
#   summarise(year = 2017,
#             consensus = sum(consensus) / length(node),
#             num_links = length(node),
#             consensus_threshold = 0.5) %>%
#   left_join(node_freq_bind_graphs, by = "node")
  

n_2017_thresholds <- data.frame(conductivity = mean(g_n_2017_betweenness),
                                cond_sd = sd(g_n_2017_betweenness),
                                degree = mean(g_n_2017_degree),
                                deg_sd = sd(g_n_2017_degree))


conductivity_n_2017 <- data.frame(conductivity = betweenness(graph.data.frame(coocGraph_n_2017, directed = F)),
                           degree = degree(graph.data.frame(coocGraph_n_2017, directed = F))) %>%
  mutate(node = rownames(.))


nodeFreq_n_2017 <- data.frame(node = colnames(DTM_n_2017),
                       freq = diag(t(DTM_n_2017) %*% DTM_n_2017)) %>%
  mutate(node = stringr::str_replace_all(node, "_", " "))


# define node attributes for each consensus threshold j, for year i
  node_attributes_n_2017 <- 
    coocGraph_n_2017 %>%
    distinct() %>%
    left_join(nodeFreq_n_2017, by = c("from" = "node")) %>%
    left_join(nodeFreq_n_2017, by = c("to" = "node")) %>%
    mutate(lower.freq = ifelse(freq.x<freq.y, freq.x, freq.y)) %>%
    mutate(consensus = ifelse(coocFreq/lower.freq>=0.5, 1, 0)) %>% # if co-occurrence exists at least XX% of the time the less frequent of the two nodes is used, counts as "consensus"
    rename("node" = "from") %>%
    group_by(node, sd_multiplier) %>%
    summarise(year = 2017,
              consensus = sum(consensus) / length(node),
              num_links = length(node),
              consensus_threshold = 0.5) %>%
    left_join(nodeFreq_n_2017, by = "node") %>%
    left_join(conductivity_n_2017, by = "node")
  
  
  quantiles_n_2017 <- as.data.frame(cbind(quantile = seq(0, 1, by = 0.05),
                                          consensus = as.numeric(quantile(node_attributes_n_2017$consensus, 
                                                                          seq(0, 1, by = 0.05),
                                                                          na.rm = T)),
                                          degree = as.numeric(quantile(g_n_2017_degree, 
                                                                       seq(0, 1, by = 0.05))),
                                          conductivity = as.numeric(quantile(node_attributes_n_2017$conductivity, 
                                                                             seq(0, 1, by = 0.05)))))
  
  k <- 0.4
  
  node_attributes_percentile_n_2017 <- 
    node_attributes_n_2017 %>%
    mutate(symbol_type = 
             case_when(consensus<=as.numeric(quantiles_n_2017 %>% filter(quantile==k) %>% select(consensus)) &
                         degree<=as.numeric(quantiles_n_2017 %>% filter(quantile==k) %>% select(degree)) & 
                         conductivity<=as.numeric(quantiles_n_2017 %>% filter(quantile==k) %>% select(conductivity)) ~ "ordinary",
                       consensus>=as.numeric(quantiles_n_2017 %>% filter(quantile==(1-k)) %>% select(consensus)) &
                         degree<=as.numeric(quantiles_n_2017 %>% filter(quantile==k) %>% select(degree)) & 
                         conductivity<=as.numeric(quantiles_n_2017 %>% filter(quantile==k) %>% select(conductivity)) ~ "factoid",
                       consensus<=as.numeric(quantiles_n_2017 %>% filter(quantile==k) %>% select(consensus)) &
                         degree>=as.numeric(quantiles_n_2017 %>% filter(quantile==(1-k)) %>% select(degree)) & 
                         conductivity<=as.numeric(quantiles_n_2017 %>% filter(quantile==k) %>% select(conductivity)) ~ "allusion",
                       consensus<=as.numeric(quantiles_n_2017 %>% filter(quantile==k) %>% select(consensus)) &
                         degree<=as.numeric(quantiles_n_2017 %>% filter(quantile==k) %>% select(degree)) & 
                         conductivity>=as.numeric(quantiles_n_2017 %>% filter(quantile==(1-k)) %>% select(conductivity)) ~ "buzzword",
                       consensus>=as.numeric(quantiles_n_2017 %>% filter(quantile==(1-k)) %>% select(consensus)) &
                         degree>=as.numeric(quantiles_n_2017 %>% filter(quantile==(1-k)) %>% select(degree)) & 
                         conductivity<=as.numeric(quantiles_n_2017 %>% filter(quantile==k) %>% select(conductivity)) ~ "stereotype",
                       consensus>=as.numeric(quantiles_n_2017 %>% filter(quantile==(1-k)) %>% select(consensus)) &
                         degree<=as.numeric(quantiles_n_2017 %>% filter(quantile==k) %>% select(degree)) & 
                         conductivity>=as.numeric(quantiles_n_2017 %>% filter(quantile==(1-k)) %>% select(conductivity)) ~ "emblem",
                       consensus<=as.numeric(quantiles_n_2017 %>% filter(quantile==k) %>% select(consensus)) &
                         degree>=as.numeric(quantiles_n_2017 %>% filter(quantile==(1-k)) %>% select(degree)) & 
                         conductivity>=as.numeric(quantiles_n_2017 %>% filter(quantile==(1-k)) %>% select(conductivity)) ~ "placeholder",
                       consensus>=as.numeric(quantiles_n_2017 %>% filter(quantile==(1-k)) %>% select(consensus)) &
                         degree>=as.numeric(quantiles_n_2017 %>% filter(quantile==(1-k)) %>% select(degree)) & 
                         conductivity>=as.numeric(quantiles_n_2017 %>% filter(quantile==(1-k)) %>% select(conductivity)) ~ "standard"))
  
  place_n_2017_consensus50 <- 
    node_attributes_percentile_n_2017 %>% 
    filter(symbol_type=="placeholder" & consensus_threshold==0.5)
  
  buzz_n_2017_consensus50 <- 
    node_attributes_percentile_n_2017 %>% 
    filter(symbol_type=="buzzword" & consensus_threshold==0.5)
  
  
  
  # identify buzzwords + placeholders by varying parameters
place_n_2017_consensus50 <- 
  node_attributes_n_2017 %>% 
  filter(symbol_type=="placeholder" & consensus_threshold==0.5)
place_n_2018_consensus50 <- 
  node_attributes_n_2018 %>% 
  filter(symbol_type=="placeholder" & consensus_threshold==0.5)
place_n_2019_consensus50 <- 
  node_attributes_n_2019 %>% 
  filter(symbol_type=="placeholder" & consensus_threshold==0.5)
place_n_2020_consensus50 <- 
  node_attributes_n_2020 %>% 
  filter(symbol_type=="placeholder" & consensus_threshold==0.5)
place_n_2021_consensus50 <- 
  node_attributes_n_2021 %>% 
  filter(symbol_type=="placeholder" & consensus_threshold==0.5)

placeholders_compare_45percentile_consensus25_n <- 
  node_attributes_n %>%
  filter(symbol_type=="placeholder" & consensus_threshold==0.25 & percentile_threshold==0.45) %>%
  group_by(node) %>%
  summarise(total_freq = sum(freq),
            n_years = length(node),
            first_year = min(year),
            last_year = max(year))

buzzwords_compare_45percentile_consensus25_n <- 
  node_attributes_n %>%
  filter(symbol_type=="buzzword" & consensus_threshold==0.25 & percentile_threshold==0.45) %>%
  group_by(node) %>%
  summarise(total_freq = sum(freq),
            n_years = length(node),
            first_year = min(year),
            last_year = max(year))
