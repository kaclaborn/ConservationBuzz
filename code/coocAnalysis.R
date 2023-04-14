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


# ---- 1.1 Import libraries ----

pacman::p_load(tidyverse, quanteda, quanteda.textstats, tidytext, qdap, 
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

docs_n <- read_csv("data/corpora/processed/docs_n.csv", locale = readr::locale(encoding = "UTF-8"))
docs_m <- read_csv("data/corpora/preprocessed/media/docs_m_filtered.csv", locale = readr::locale(encoding = "UTF-8"))

# ---- 2.2 Subset DTMs per year, create co-occurrence networks, and export to flat file ----
## COMPUTATIONALLY INTENSIVE -- ONLY RUN ONCE AND THEN SOURCE IN FLAT FILES TO DO ANALYSIS IN SECTION 3 AND BEYOND

coocGraphsPerYear(input_data = docs_m, input_suffix = "m", years = 2021, 
                  coocTerm = "conservation", sd_multiplier = 5, stopword_list = stopwords_pos_extend_m)


 # ---- 2.3 Define node attributes per co-occurrence network, and compare across years ----

findNodeAttributes(input_suffix = "m", 
                   years = 2021, 
                   consensus_thresholds = c(0.25, 0.33, 0.4, 0.45, 0.5), 
                   percentile_thresholds = 0.5,
                   coocTerm = "conservation")


# ---- Explore ----

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
