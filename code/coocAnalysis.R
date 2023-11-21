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

pacman::p_load(spam, spam64)

pacman::p_load(quanteda, quanteda.textstats, tidytext, 
               parallel, igraph)

library("tidyverse")


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

docs_m_unfilt <- read_csv("data/corpora/processed/docs_m_unfiltered.csv", locale = readr::locale(encoding = "UTF-8"))

docs_m <- docs_m_unfilt %>%
  filter(grepl("NewYorkTimes", publication)==T) %>%
  filter(grepl("conservation|sustainability|natural resource|climate change|global warming|environmental protection|species", 
               subject, ignore.case = T)==T)

# ---- 2.1 Import corpus documents ----

docs_a <- read_csv("data/corpora/processed/docs_a.csv", locale = readr::locale(encoding = "UTF-8"))
docs_n <- read_csv("data/corpora/processed/docs_n.csv", locale = readr::locale(encoding = "UTF-8"))
docs_m <- read_csv("data/corpora/processed/docs_m.csv", locale = readr::locale(encoding = "UTF-8"))
docs_p <- read_csv("data/corpora/processed/docs_p.csv", locale = readr::locale(encoding = "UTF-8"))


# ---- 2.2 Subset corpus docs as needed ----



docs_n %>% group_by(org_id) %>% summarise(doc_year = length(unique(year))) %>% ungroup() %>% summarise(sum = sum(doc_year))


# ---- 2.3 Subset DTMs per year, create co-occurrence networks, and export to flat file ----
## COMPUTATIONALLY INTENSIVE -- ONLY RUN ONCE AND THEN SOURCE IN FLAT FILES TO DO ANALYSIS IN SECTION 3 AND BEYOND

coocGraphsPerYear(input_data = docs_a, input_suffix = "a", years = 2000:2021, 
                  sd_multiplier = 3, stopword_list = stopwords_extended$word,
                  coocTerm = "conservation",
                  n_cores = 1)


 # ---- 2.4 Define node attributes per co-occurrence network, and compare across years ----

findNodeAttributes(input_suffix = "a", 
                   years = 2000:2021, 
                   consensus_thresholds = c(0.25, 0.3, 0.4, 0.5), 
                   percentile_thresholds = c(0.35, 0.4, 0.45, 0.5),
                   coocTerm = "conservation", 
                   export = T)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: COMPARE WORDS/NETWORKS THROUGH TIME ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# Can call in the most recently run versions of node_attributes, node_freq, & graph_attr, as they output and re-write
# to the outputs folder as generic "node_attributes"/"node_freq"/"graph_attr" csv files each time findNodeAttributes is run.

node_attributes_a <- read_csv("data/outputs/node_attributes_a.csv")
node_attributes_n <- read_csv("data/outputs/node_attributes_n.csv")
node_attributes_m_nyt_filt <- read_csv("data/outputs/node_attributes_m_nyt_filt.csv")
node_attributes_p <- read_csv("data/outputs/node_attributes_p.csv")

node_freq_a <- read_csv("data/outputs/node_freq_a.csv")
node_freq_n <- read_csv("data/outputs/node_freq_n.csv")
node_freq_m_nyt_filt <- read_csv("data/outputs/node_freq_m_nyt_filt.csv")
node_freq_p <- read_csv("data/outputs/node_freq_p.csv")

graph_attr_a <- read_csv("data/outputs/graph_attr_a.csv")
graph_attr_n <- read_csv("data/outputs/graph_attr_n.csv")
graph_attr_m_nyt_filt <- read_csv("data/outputs/graph_attr_m_nyt_filt.csv")
graph_attr_p <- read_csv("data/outputs/graph_attr_p.csv")



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
         buzzword_years_2017_2021 = length(year[symbol_type=="buzzword" & !is.na(symbol_type) & year%in%2017:2021]),
         placeholder_years = length(year[symbol_type=="placeholder" & !is.na(symbol_type)]),
         placeholder_years_2017_2021 = length(year[symbol_type=="placeholder" & !is.na(symbol_type) & year%in%2017:2021]),
         standard_years = length(year[symbol_type=="standard" & !is.na(symbol_type)]),
         buzzplace_years_2017_2021 = length(year[(symbol_type=="buzzword" | symbol_type=="placeholder") & 
                                         !is.na(symbol_type) & year%in%2017:2021])) %>%
  ungroup() %>%
  group_by(node, consensus_threshold, percentile_threshold, year) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(node, consensus_threshold, percentile_threshold, buzzword_years, buzzword_years_2017_2021,
                          placeholder_years, placeholder_years_2017_2021, standard_years, buzzplace_years_2017_2021), 
              names_from = year, values_from = c(symbol_type, freq, rel_freq, conductivity, consensus, degree, 
                                                 consensus_percentile, conductivity_percentile))

compare_symbol_types_n <-
  node_attributes_n %>% 
  select(-freq) %>%
  left_join(graph_attr_n, by = "year") %>%
  left_join(node_freq_n, by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs) %>%
  group_by(node, consensus_threshold, percentile_threshold) %>%
  mutate(buzzword_years_2017_2021 = length(year[symbol_type=="buzzword" & !is.na(symbol_type) & year%in%2017:2021]),
         placeholder_years_2017_2021 = length(year[symbol_type=="placeholder" & !is.na(symbol_type) & year%in%2017:2021]),
         standard_years = length(year[symbol_type=="standard" & !is.na(symbol_type)]),
         buzzplace_years_2017_2021 = length(year[(symbol_type=="buzzword" | symbol_type=="placeholder") & 
                                                   !is.na(symbol_type) & year%in%2017:2021])) %>%
  ungroup() %>%
  group_by(node, consensus_threshold, percentile_threshold, year) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(node, consensus_threshold, percentile_threshold, buzzword_years_2017_2021, 
                          placeholder_years_2017_2021, standard_years, buzzplace_years_2017_2021), 
              names_from = year, values_from = c(symbol_type, freq, rel_freq, conductivity, consensus, degree, 
                                                 consensus_percentile, conductivity_percentile))

compare_symbol_types_m_nyt <-
  node_attributes_m_nyt_filt %>% 
  select(-freq) %>%
  left_join(graph_attr_m_nyt_filt, by = "year") %>%
  left_join(node_freq_m_nyt_filt, by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs) %>%
  group_by(node, consensus_threshold, percentile_threshold) %>%
  mutate(buzzword_years_2017_2021 = length(year[symbol_type=="buzzword" & !is.na(symbol_type) & year%in%2017:2021]),
         placeholder_years_2017_2021 = length(year[symbol_type=="placeholder" & !is.na(symbol_type) & year%in%2017:2021]),
         standard_years = length(year[symbol_type=="standard" & !is.na(symbol_type)]),
         buzzplace_years_2017_2021 = length(year[(symbol_type=="buzzword" | symbol_type=="placeholder") & 
                                                   !is.na(symbol_type) & year%in%2017:2021])) %>%
  ungroup() %>%
  group_by(node, consensus_threshold, percentile_threshold, year) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(node, consensus_threshold, percentile_threshold, buzzword_years_2017_2021, 
                          placeholder_years_2017_2021, standard_years, buzzplace_years_2017_2021), 
              names_from = year, values_from = c(symbol_type, freq, rel_freq, conductivity, consensus, degree, 
                                                 consensus_percentile, conductivity_percentile))

compare_symbol_types_p <-
  node_attributes_p %>% 
  select(-freq) %>%
  left_join(graph_attr_p, by = "year") %>%
  left_join(node_freq_p, by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs) %>%
  group_by(node, consensus_threshold, percentile_threshold) %>%
  mutate(buzzword_years = length(year[symbol_type=="buzzword" & !is.na(symbol_type)]),
         placeholder_years = length(year[symbol_type=="placeholder" & !is.na(symbol_type)]),
         standard_years = length(year[symbol_type=="standard" & !is.na(symbol_type)]),
         buzzplace_years = length(year[(symbol_type=="buzzword" | symbol_type=="placeholder") & 
                                                   !is.na(symbol_type)])) %>%
  ungroup() %>%
  group_by(node, consensus_threshold, percentile_threshold, year) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(node, consensus_threshold, percentile_threshold, buzzword_years, 
                          placeholder_years, standard_years, buzzplace_years), 
              names_from = year, values_from = c(symbol_type, freq, rel_freq, conductivity, consensus, degree, 
                                                 consensus_percentile, conductivity_percentile))


buzzplace_p_c0.75_t0.5 <- 
  compare_symbol_types_p %>% 
  filter(buzzplace_years>0 &
           consensus_threshold==0.75 &
           percentile_threshold==0.5)
  



place_n_c0.5_t0.4 <- compare_symbol_types_n %>% filter(placeholder_years>0 &
                                                         consensus_threshold==0.5 & 
                                                         percentile_threshold==0.4)
place_n_c0.5_t0.5 <- compare_symbol_types_n %>% filter(placeholder_years>0 &
                                                         consensus_threshold==0.5 & 
                                                         percentile_threshold==0.5)

buzz_n_c0.5_t0.4 <- compare_symbol_types_n %>% filter(buzzword_years>0 &
                                                         consensus_threshold==0.5 & 
                                                         percentile_threshold==0.4)
buzz_n_c0.5_t0.5 <- compare_symbol_types_n %>% filter(buzzword_years>0 &
                                                         consensus_threshold==0.5 & 
                                                         percentile_threshold==0.5)


# ---- Academic corpus, top placeholders & buzzwords ----

place_a_c0.25_t0.5 <- compare_symbol_types_a %>% filter(placeholder_years_2017_2021>0 &
                                                          consensus_threshold==0.25 &
                                                          percentile_threshold==0.5)
buzz_a_c0.25_t0.5 <- compare_symbol_types_a %>% filter(buzzword_years_2017_2021>0 &
                                                         consensus_threshold==0.25 &
                                                         percentile_threshold==0.5)

# short-lists of words, based on relative frequency and years classified 
top_placeholders_a_by_relfreq <- 
  place_a_c0.25_t0.5 %>%
  arrange(desc(rel_freq_2021)) %>%
  slice_head(n = 30)

top_placeholders_a_by_years <- 
  place_a_c0.25_t0.5 %>%
  filter(placeholder_years_2017_2021==5)

top_buzzwords_a_by_relfreq <- 
  buzz_a_c0.25_t0.5 %>%
  arrange(desc(rel_freq_2021)) %>%
  slice_head(n = 30)

top_buzzwords_a_by_years <- 
  buzz_a_c0.25_t0.5 %>%
  filter(buzzword_years_2017_2021>1)

# look at top words that flip between buzzword and placeholder
place_and_buzz_a_c0.25_t0.5 <- 
  compare_symbol_types_a %>% 
  filter(buzzword_years_2017_2021>0 &
           placeholder_years_2017_2021>0 &
           consensus_threshold==0.25 &
           percentile_threshold==0.5) %>%
  rowwise() %>%
  mutate(place_buzz_years = sum(c_across(c(buzzword_years_2017_2021, placeholder_years_2017_2021)))) %>%
  arrange(desc(place_buzz_years))
  


# ---- NGO corpus, top placeholders & buzzwords ----

place_n_c0.5_t0.5 <- compare_symbol_types_n %>% filter(placeholder_years_2017_2021>0 &
                                                          consensus_threshold==0.5 &
                                                          percentile_threshold==0.5)
buzz_n_c0.5_t0.5 <- compare_symbol_types_n %>% filter(buzzword_years_2017_2021>0 &
                                                         consensus_threshold==0.5 &
                                                         percentile_threshold==0.5)

# short-lists of words, based on relative frequency and years classified 
top_placeholders_n_by_relfreq <- 
  place_n_c0.5_t0.5 %>%
  arrange(desc(rel_freq_2021)) %>%
  slice_head(n = 30)

top_placeholders_n_by_years <- 
  place_n_c0.5_t0.5 %>%
  filter(placeholder_years_2017_2021>1)

top_buzzwords_n_by_relfreq <- 
  buzz_n_c0.5_t0.5 %>%
  arrange(desc(rel_freq_2021)) %>%
  slice_head(n = 30)

top_buzzwords_n_by_years <- 
  buzz_n_c0.5_t0.5 %>%
  filter(buzzword_years_2017_2021>1)

# look at top words that flip between buzzword and placeholder
place_and_buzz_n_c0.5_t0.5 <- 
  compare_symbol_types_n %>% 
  filter(buzzword_years_2017_2021>0 &
           placeholder_years_2017_2021>0 &
           consensus_threshold==0.5 &
           percentile_threshold==0.5) %>%
  rowwise() %>%
  mutate(place_buzz_years = sum(c_across(c(buzzword_years_2017_2021, placeholder_years_2017_2021)))) %>%
  arrange(desc(place_buzz_years))


# institution-level averages (network measures, prop different word types, etc.)

network_avgs_a <-
  years_compare_a %>% 
  filter(percentile_threshold==0.5) %>% 
  ungroup() %>%
  summarise(avg_degree = mean(avg_degree),
            avg_conductivity = mean(avg_conductivity),
            avg_consensus = mean(avg_consensus),
            avg_nodes = mean(n_nodes),
            avg_prop_placeholders = mean(n_placeholders) / avg_nodes)

avg_placeholder_length_a <-
  node_attributes_a %>%
  filter(consensus_threshold==0.25 & percentile_threshold==0.5 & symbol_type=="placeholder" & year%in%c(2017:2021)) %>%
  group_by(node) %>%
  summarise(n_years_placeholder = length(node)) %>%
  ungroup() %>%
  summarise(avg_years_placeholder = mean(n_years_placeholder),
            n_placeholders = length(node))
  

network_avgs_n <-
  years_compare_n %>% 
  filter(percentile_threshold==0.5) %>% 
  ungroup() %>%
  summarise(avg_degree = mean(avg_degree),
            avg_conductivity = mean(avg_conductivity),
            avg_consensus = mean(avg_consensus),
            avg_nodes = mean(n_nodes),
            avg_prop_placeholders = mean(n_placeholders) / avg_nodes)

avg_placeholder_length_n <-
  node_attributes_n %>%
  filter(consensus_threshold==0.5 & percentile_threshold==0.5 & symbol_type=="placeholder") %>%
  group_by(node) %>%
  summarise(n_years_placeholder = length(node)) %>%
  ungroup() %>%
  summarise(avg_years_placeholder = mean(n_years_placeholder),
            n_placeholders = length(node))

network_avgs_m <-
  years_compare_m_nyt_filt %>% 
  filter(percentile_threshold==0.5 & !is.na(year)) %>% 
  ungroup() %>%
  summarise(avg_degree = mean(avg_degree),
            avg_conductivity = mean(avg_conductivity),
            avg_consensus = mean(avg_consensus),
            avg_nodes = mean(n_nodes),
            avg_prop_placeholders = mean(n_placeholders) / avg_nodes)

avg_placeholder_length_m <-
  node_attributes_m_nyt_filt %>%
  filter(consensus_threshold==0.5 & percentile_threshold==0.5 & symbol_type=="placeholder") %>%
  group_by(node) %>%
  summarise(n_years_placeholder = length(node)) %>%
  ungroup() %>%
  summarise(avg_years_placeholder = mean(n_years_placeholder),
            n_placeholders = length(node))

network_avgs_p <-
  years_compare_p %>% 
  filter(percentile_threshold==0.5) %>% 
  summarise(avg_degree = mean(avg_degree),
            avg_conductivity = mean(avg_conductivity),
            avg_consensus = mean(avg_consensus),
            avg_nodes = mean(n_nodes),
            avg_prop_placeholders = mean(n_placeholders) / avg_nodes)

n_placeholders_p <-
  node_attributes_p %>%
  filter(consensus_threshold==0.75 & percentile_threshold==0.5 & symbol_type=="placeholder") %>%
  group_by(year) %>%
  summarise(n_placeholders = length(node))

