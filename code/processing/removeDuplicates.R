# 
# code: Remove document duplicates
# (after preliminary identification using Levenshtein string matching in code/processing folder)
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: December 2022
# 
# 

# ---- IMPORT LIBRARIES ----

pacman::p_load(rio, tidyverse)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#



# ---- 1.1 Import outputs from Levenshtein string distance matching ----

list_checkduplicates <- list.files("data/corpora/preprocessed/media/checkduplicates/")

# NYT
files_nyt_checkduplicates <- 
  list_checkduplicates[grep("nyt", list_checkduplicates)] %>%
  .[grep("a.csv", .)] %>%
  paste("data/corpora/preprocessed/media/checkduplicates/", ., sep = "")
  
for(i in 1:length(files_nyt_checkduplicates)) {
  
  assign(paste("check_", i, sep = ""), read.table(files_nyt_checkduplicates[i], sep = ",", fill = T,
                                                            col.names = c("docid", "duplicate1", "duplicate2", "duplicate3")))
  
}

checkduplicates_nyt <- do.call(rbind, lapply(ls()[grep("check_", ls())], get) ) %>% distinct()

# WSJ
files_wsj_checkduplicates <- 
  list_checkduplicates[grep("wsj", list_checkduplicates)] %>%
  .[grep("a.csv", .)] %>%
  paste("data/corpora/preprocessed/media/checkduplicates/", ., sep = "")

for(i in 1:length(files_wsj_checkduplicates)) {
  
  assign(paste("check_", i, sep = ""), read.table(files_wsj_checkduplicates[i], sep = ",", fill = T,
                                                  col.names = c("docid", "duplicate1", "duplicate2", "duplicate3")))
  
}

checkduplicates_wsj <- do.call(rbind, lapply(ls()[grep("check_", ls())], get) ) %>% distinct()

# Remove unnecessary objects
remove(check_1, check_2, check_3, check_4, check_5, check_6, check_7, check_8, check_9, check_10,
       check_11, check_12, check_13, check_14, check_15, check_16, check_17, check_18, check_19, check_20)


# ---- 1.2 Import pre-processed documents used in findDuplicates analysis ----

docs_m_nyt <- read.csv("data/corpora/preprocessed/media/docs_nyt_findDuplicates.csv")
docs_m_wsj <- read.csv("data/corpora/preprocessed/media/docs_wsj_findDuplicates.csv")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: COMPARE DUPLICATES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# Look for final edition or other identifier (most recent?) to keep one version over another

duplicates_nyt <- 
  checkduplicates_nyt %>% .[order(.$docid),] %>% filter(!is.na(duplicate1)) %>% mutate(groupid = row_number()) %>%
  pivot_longer(cols = c("docid", "duplicate1", "duplicate2", "duplicate3"), values_to = "docid") %>% 
  na.omit() %>%
  select(groupid, docid) %>%
  left_join(docs_m %>% select(date, author, publication, docid, text), by = "docid") %>%
  mutate(date = as.Date(date, format = "%B %e, %Y"))

duplicates_nyt_bygroup <-
  duplicates_nyt %>%
  group_by(groupid) %>%
  summarise(n = length(docid),
            docid = list(docid),
            date_range = case_when(n==2 ~ date[1] - date[2], 
                                   n==3 ~ date[1] - date[3], 
                                   n==4 ~ date[1] - date[4]),
            same_auth = case_when(n==2 & author[1]==author[2] ~ 1, 
                                  n==3 & author[1]==author[2] & author[1]==author[3] ~ 1,
                                  n==4 & author[1]==author[2] & author[1]==author[3] & author[1]==author[4] ~ 1))
  



duplicates_nyt$text[duplicates_nyt$docid==8707]
