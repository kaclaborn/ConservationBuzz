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


# ---- 1.1 Import outputs from findDuplicates.py analysis (identifying >=90% similar texts) ----

# NOTE: First need to move the .csv files named "similar_texts_nyt" and "similar_texts_wsj" from code/processing to data/corpora/preprocessed/media

suppressWarnings(
  
checkduplicates_nyt <- read.csv("data/corpora/preprocessed/media/similar_texts_nyt.csv") %>%
  select(docid, similar_docs_90) %>%
  separate(similar_docs_90, into = c("duplicate1", "duplicate2"), sep = ",") %>%
  mutate_if(is.character, as.numeric)

)

suppressWarnings(
  
  checkduplicates_wsj <- read.csv("data/corpora/preprocessed/media/similar_texts_wsj.csv") %>%
    select(docid, similar_docs_90) %>%
    separate(similar_docs_90, into = c("duplicate1", "duplicate2"), sep = ",") %>%
    mutate_if(is.character, as.numeric)
  
)

# ---- 1.2 Import docs_m from flat file ----

docs_m <- read.csv("data/corpora/preprocessed/media/docs_m.csv", fileEncoding = "UTF-8")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: COMPARE DUPLICATES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 NYT duplicates ----

# Look at list of duplicates, verify that they are true duplicates


# ---- 2.1.1 Fix date / "Not available" text errors in NYT articles ----

docs_m_nyt <- 
  docs_m %>%
  filter(grepl("NewYorkTimes", publication)) %>%
  mutate(year = sub(" ", "", year),
         date = paste(sub("\\,.*", "", date), year, sep = ", "),
         date = as.Date(date, format = "%B %e, %Y")) %>%
  filter(text!="Not available.") %>%
  filter(!is.na(year))


# ---- 2.1.2 create groups of duplicates, join to full text data for visual comparison ----

duplicates_nyt <-
  checkduplicates_nyt %>% .[order(.$docid),] %>% filter(!is.na(duplicate1)) %>%
  mutate(min_docid = ifelse(docid < duplicate1, docid, duplicate1),
         min_docid = ifelse(is.na(duplicate2), 
                            min_docid,
                            ifelse(min_docid < duplicate2, 
                                   min_docid, 
                                   duplicate2)),
         med_docid = ifelse(docid > duplicate1, docid, duplicate1),
         med_docid = ifelse(is.na(duplicate2), 
                            med_docid, 
                            ifelse((duplicate2!=min_docid) & (med_docid < duplicate2), 
                                   med_docid, 
                                   ifelse(duplicate2!=min_docid & med_docid > duplicate2,
                                          duplicate2,
                                          med_docid))),
         max_docid = ifelse(is.na(duplicate2), 
                            duplicate2,
                            ifelse(duplicate2 > med_docid,
                                   duplicate2,
                                   ifelse(docid > med_docid, 
                                          docid, 
                                          duplicate1))),
         doc_list = paste(min_docid, med_docid, max_docid, sep = ",")) %>%
  group_by(doc_list) %>%
  summarise(docid = docid[1],
            duplicate1 = duplicate1[1],
            duplicate2 = duplicate2[1]) %>%
  select(-doc_list) %>%
  mutate(groupid = row_number()) %>%
  pivot_longer(cols = c("docid", "duplicate1", "duplicate2"), values_to = "docid") %>% 
  na.omit() %>%
  select(groupid, docid) %>%
  left_join(docs_m_nyt %>% select(date, author, publication, docid, text), by = "docid")


# ---- 2.1.3 group duplicates by groupid and identify authorship and date range ----

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
                                  n==4 & author[1]==author[2] & author[1]==author[3] & author[1]==author[4] ~ 1)) %>%
  left_join(duplicates_nyt, by = "groupid")
  

# ---- 2.1.4 look at duplicate with day range greater than 1 for closer visual inspection ----

duplicates_nyt_bygroup_grt1day <-
  duplicates_nyt_bygroup %>%
  filter(date_range!=0 & date_range!=1 & date_range!=-1)


# ---- 2.2 WSJ duplicates ----

# Look at list of duplicates, verify that they are true duplicates


# ---- 2.2.1 Fix date / "Not available" text errors in WSJ articles ----

docs_m_wsj <- 
  docs_m %>%
  filter(grepl("WallStreetJournal", publication)) %>%
  mutate(year = sub(" ", "", year),
         date = paste(sub("\\,.*", "", date), year, sep = ", "),
         date = as.Date(date, format = "%B %e, %Y")) %>%
  filter(text!="Not available.") %>%
  filter(!is.na(year))


# ---- 2.2.2 create groups of duplicates, join to full text data for visual comparison ----

duplicates_wsj <-
  checkduplicates_wsj %>% .[order(.$docid),] %>% filter(!is.na(duplicate1)) %>%
  mutate(min_docid = ifelse(docid < duplicate1, docid, duplicate1),
         min_docid = ifelse(is.na(duplicate2), 
                            min_docid,
                            ifelse(min_docid < duplicate2, 
                                   min_docid, 
                                   duplicate2)),
         med_docid = ifelse(docid > duplicate1, docid, duplicate1),
         med_docid = ifelse(is.na(duplicate2), 
                            med_docid, 
                            ifelse((duplicate2!=min_docid) & (med_docid < duplicate2), 
                                   med_docid, 
                                   ifelse(duplicate2!=min_docid & med_docid > duplicate2,
                                          duplicate2,
                                          med_docid))),
         max_docid = ifelse(is.na(duplicate2), 
                            duplicate2,
                            ifelse(duplicate2 > med_docid,
                                   duplicate2,
                                   ifelse(docid > med_docid, 
                                          docid, 
                                          duplicate1))),
         doc_list = paste(min_docid, med_docid, max_docid, sep = ",")) %>%
  group_by(doc_list) %>%
  summarise(docid = docid[1],
            duplicate1 = duplicate1[1],
            duplicate2 = duplicate2[1]) %>%
  select(-doc_list) %>%
  mutate(groupid = row_number()) %>%
  pivot_longer(cols = c("docid", "duplicate1", "duplicate2"), values_to = "docid") %>% 
  na.omit() %>%
  select(groupid, docid) %>%
  left_join(docs_m_wsj %>% select(date, author, publication, docid, text), by = "docid")


# ---- 2.2.3 group duplicates by groupid and identify authorship and date range ----

duplicates_wsj_bygroup <-
  duplicates_wsj %>%
  group_by(groupid) %>%
  summarise(n = length(docid),
            docid = list(docid),
            date_range = case_when(n==2 ~ date[1] - date[2], 
                                   n==3 ~ date[1] - date[3], 
                                   n==4 ~ date[1] - date[4]),
            same_auth = case_when(n==2 & author[1]==author[2] ~ 1, 
                                  n==3 & author[1]==author[2] & author[1]==author[3] ~ 1,
                                  n==4 & author[1]==author[2] & author[1]==author[3] & author[1]==author[4] ~ 1)) %>% 
  left_join(duplicates_wsj, by = "groupid")


# ---- 2.2.4 look at duplicate with day range greater than 1 for closer visual inspection ----

duplicates_wsj_bygroup_grt1day <-
  duplicates_wsj_bygroup %>%
  filter(date_range!=0 & date_range!=1 & date_range!=-1)



# ---- 2.3 Decision rules for duplicates ----

# decision rule # 1: if same author, published within one month of each other, and otherwise >90% same text, consider a duplicate
#                    keep the more recent of the duplicates
# decision rule # 2: if a duplicate, but published more than one month apart from one another, consider it a re-run and keep both 




# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: REMOVE DUPLICATES, OUTPUT FINAL MEDIA DATA FRAME FOR ANALYSIS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Remove NYT duplicates ----

# data frame to identify duplicates to remove
for_removal_nyt <- 
  duplicates_nyt_bygroup %>%
  rename("docid" = "docid.y") %>%
  group_by(groupid) %>%
  mutate(to_keep = case_when(date_range==0 & docid==min(docid) ~ 1, # if published on same day, choose the doc that appeared first in the data frame
                             date_range!=0 & date_range<31 & date_range>-31 & date==max(date) ~ 1, # if published within a month of each other, choose the most recent
                             date_range>31 ~ 1, # if published more than a month of each other, keep both
                             date_range<(-31) ~ 1, # if published more than a month of each other, keep both
                             TRUE ~ 0)) %>%
  filter(to_keep==0)

# remove
docs_m_nyt <- 
  docs_m_nyt %>% 
  filter(!docid %in% for_removal_nyt$docid)


# ---- 3.2 Remove WSJ duplicates ----

# data frame to identify duplicates to remove
for_removal_wsj <- 
  duplicates_wsj_bygroup %>%
  rename("docid" = "docid.y") %>%
  group_by(groupid) %>%
  mutate(to_keep = case_when(date_range==0 & docid==min(docid) ~ 1, # if published on same day, choose the doc that appeared first in the data frame
                             date_range!=0 & date_range<31 & date_range>-31 & date==max(date) ~ 1, # if published within a month of each other, choose the most recent
                             date_range>31 ~ 1, # if published more than a month of each other, keep both
                             date_range<(-31) ~ 1, # if published more than a month of each other, keep both
                             TRUE ~ 0)) %>%
  filter(to_keep==0)

# remove
docs_m_wsj <- 
  docs_m_wsj %>% 
  filter(!docid %in% for_removal_wsj$docid)


# ---- 3.3 Join NYT & WSJ together, export ----

docs_m_forexport <- 
  rbind.data.frame(docs_m_nyt, docs_m_wsj)

write.csv(docs_m_forexport, "data/corpora/preprocessed/media/docs_m_filtered.csv", fileEncoding = "UTF-8")
