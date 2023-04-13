# 
# code: Import and pre-process corpora
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: June 2022
# 

# NOTE: Naming conventions for different corpora
#       - academic: (ends in) _a
#       - NGO: _n
#       - policy: _p
#       - media: _m


# ---- IMPORT LIBRARIES ----

pacman::p_load(rio, tidyverse)



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: ACADEMIC CORPUS DOCS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Import academic corpus files ----

# Identify list of folders
files_folders_a <- paste("data/corpora/preprocessed/academic", 
                                list.files("data/corpora/preprocessed/academic"), sep = "/")

# Identify common column names across .csv files
cols_a <- colnames(import(paste(files_folders_a[1], 
                                        list.files(files_folders_a[1])[1], 
                                        sep = "/")))

# Create empty data frame with appropriate column names
import_a <- data.frame(matrix(nrow = 0, ncol = length(cols_a)))
colnames(import_a) <- cols_a

# Import each file and append to data frame
for(i in files_folders_a) {
  
  files <- list.files(i)
  
  for(j in files) {
    
    dat <- import(paste(i, j, sep = "/"))
    
    import_a <- rbind.data.frame(import_a, dat)
    
  }
}


# ---- 1.2 Filter and pre-process ----

docs_a <- import_a %>%
  rename("title" = "Title",
         "year" = "Year",
         "volume" = "Volume",
         "issue" = "Issue",
         "journal" = "Source title",
         "citations" = "Cited by",
         "text" = "Abstract",
         "keywords" = "Index Keywords",
         "type" = "Document Type",
         "stage" = "Publication Stage") %>%
  select(title, year, journal, volume, issue, text, keywords, type, stage, citations) %>%
  filter(text!="[No abstract available]",
         type!="Erratum", 
         type!="Retracted") %>%
  # NOTE: may need to filter more article types
  mutate(journal = ifelse(grepl("Conservation Biology", journal, ignore.case = T)==T, "Conservation Biology", 
                          ifelse(grepl("Global Change Biology", journal, ignore.case = T)==T, "Global Change Biology", journal)),
         text = stringr::str_replace_all(text, ' Â©.*', ''), # remove back matter from abstracts
         text = stringr::str_replace_all(text, '©.*', ''), # remove back matter from abstracts
         text = stringr::str_replace_all(text, ' Conservation Biology published.*', ''), # remove back matter from abstracts
         text = stringr::str_replace_all(text, ' Copyright.*', ''), # remove back matter from abstracts
         text = stringr::str_replace_all(text, ' Global Change Biology published.*', ''), # remove back matter from abstracts
         text = stringr::str_replace_all(text, '\\(C\\) 2000 Elsevier.*', ''), # remove back matter from Elsevier abstracts from year 2000
         text = stringr::str_replace_all(text, 'Published by Elsevier.*', ''), # remove back matter from Elsevier abstracts
         text = stringr::str_replace_all(text, ' This article is categorized.*', ''), # remove back matter from Wiley Climate Change abstracts
         text = stringr::str_replace_all(text, '2019 John Wiley & Sons Ltd', ''), # specific back matter cleaning
         text = stringr::str_replace_all(text, '2016 Wiley Periodicals, Inc.', ''), # specific back matter cleaning
         text = stringr::str_replace_all(text, '2002 Elsevier Science.*', ''), # specific back matter cleaning
         text = stringr::str_replace_all(text, '2001 Elsevier Science.*', ''), # specific back matter cleaning
         text = stringr::str_replace_all(text, '� 2016 Elsevier Ltd', ''), # specific back matter cleaning
         text = stringr::str_replace_all(text, '\"\"', ''), # general cleaning
         text = stringr::str_replace_all(text, 'Â', ''), # general cleaning
         text = stringr::str_replace_all(text, "'", ''), # general cleaning
         text = stringr::str_replace_all(text, 'â', ' '), # general cleaning -- add a space so that it splits special character from other words
         text = stringr::str_replace_all(text, 'î', ' '), # general cleaning -- add a space so that it splits special character from other words
         text = stringr::str_replace_all(text, 'e\\.g\\.', ''), # general cleaning
         text = stringr::str_replace_all(text, 'i\\.e\\.', ''), # general cleaning
         text = stringr::str_replace_all(text, 'ã', ' '), # general cleaning -- add a space so that it splits special character from other words
         text = stringr::str_replace_all(text, 'ˆ', ' '), # general cleaning -- add a space so that it splits special character from other words
         text = stringr::str_replace_all(text, 'iii', ''), # general cleaning
         text = stringr::str_replace_all(text, 'ii', ''), # general cleaning
         text = stringr::str_replace_all(text, 'î', ' ')) # general cleaning


# Check number of article types & whether abstracts of certain types should be removed from corpus
docs_a %>%
  group_by(type) %>%
  summarise(count = n())

docs_a %>%
  group_by(journal) %>%
  summarise(count = n())

check_journaltype_a <- 
  docs_a %>%
  group_by(journal, type) %>%
  summarise(count = n())

checktypes <- docs_a %>% filter(type=="Abstract Report") %>% select(text)
checktypes[1,]

checkjournals <- docs_a %>% filter(journal=="Wiley Interdisciplinary Reviews: Climate Change") %>%
  select(text)
checkjournals[11,]


# Filtering decisions:
# --- Remove "Abstract not available"
# --- Remove Erratum (these are errors that are discussed)



# ---- 1.3 Summary stats ----

# Group by journal:
# Num journals, num articles, article types, num article types, num "no abstract available", year range

# Group by year:
# Num articles, journals included per year



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: NGO CORPUS DOCS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 Import NGO corpus files ----

# Identify list of folders
files_folders_n <- paste("data/corpora/preprocessed/ngo", 
                                list.files("data/corpora/preprocessed/ngo"), sep = "/")

# Identify files within each folder, import the data 

metadata_n <- data.frame(org_id = numeric(0),
                         org = character(),
                         year = numeric(0),
                         filename = character())

import_n <- data.frame(text = character(),
                       year = numeric(0),
                       org_id = numeric(0))


for(i in 1:length(files_folders_n)) {
  
  files <- list.files(files_folders_n[i])
  
  files_recent <- files[as.numeric(substr(files, 1, 4))>2016]
  
  for(j in files_recent) {
    
    dat <- readLines(paste(files_folders_n[i], j, sep = "/")) %>%
      str_replace_all(fixed("\n"), "") %>%
      str_replace_all(fixed("\r"), "") %>%
      str_replace_all(fixed("\t"), "") %>%
      str_replace_all(fixed("\""), "") %>%
      paste(sep = " ", collapse = " ") %>%
      str_squish() %>%
      as.data.frame() %>%
      rename("text" = ".") %>%
      separate_rows(text, sep = "——————————") %>%
      filter(text!="")
    
    metadata <- data.frame(org_id = as.numeric(substr(list.files("data/corpora/preprocessed/ngo")[i], 1, 3)),
                           org = NA,
                           year = as.numeric(substr(j, 1, 4)),
                           filename = j,
                           n_docs = length(dat$text))
    
    dat <- dat %>% cbind.data.frame(metadata %>% dplyr::select(year, org_id))
    
    # append the new metadata and text data to the master data frames
    metadata_n <- rbind.data.frame(metadata_n, metadata)
    import_n <- rbind.data.frame(import_n, dat)
    
  }
}


write.csv(import_n, "data/corpora/processed/docs_n.csv", row.names = F)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: POLICY CORPUS DOCS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: MEDIA CORPUS DOCS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 4.1 Import media corpus files ----

    # ---- 4.1.1 OPTION A: import from individual files (TIME CONSUMING, ONLY NEED TO RUN ONCE) ----

    # # Identify list of folders
    # files_folders_m <- paste("data/corpora/preprocessed/media",
    #                          list.files("data/corpora/preprocessed/media"), sep = "/")
    # 
    # # Create empty data frame with appropriate column names
    # import_m <- data.frame(matrix(nrow = 0, ncol = 1))
    # colnames(import_m) <- "raw"
    # 
    # 
    # # PRE-PROCESSING NOTE: The final four lines of each text file downloaded from ProQuest were manually deleted
    # #                      to remove "Contact us..." and to leave a blank line, for readLines function to use.
    # # Text was: ____________________________________________________________
    # #           Contact us at: http://about.proquest.com/go/pqissupportcontact
    # #           Database copyright © 2022 ProQuest LLC. All rights reserved.
    # #           Terms and Conditions: https://www.proquest.com/info/termsAndConditions
    # 
    # 
    # # Import each file downloaded from ProQuest (i.e., NYT and WSJ articles) and append to data frame
    # # -- These files are structured the same way, so can be imported and manipulated in one data frame.
    # # -- Anything downloaded from NexisUni or elsewhere will have a different format and need separate processing.
    # for(i in files_folders_m[1:2]) {
    # 
    #   files <- list.files(i)
    # 
    #   for(j in files) {
    # 
    #     dat <- readLines(paste(i, j, sep = "/")) %>%
    #             str_replace_all(fixed("\n"), "") %>%
    #             str_replace_all(fixed("\r"), "") %>%
    #             str_replace_all(fixed("\t"), "") %>%
    #             str_replace_all(fixed("\""), "") %>%
    #             paste(sep = " ", collapse = " ") %>%
    #             str_squish() %>%
    #             as.data.frame() %>%
    #             rename("raw" = ".") %>%
    #             separate_rows(raw, sep = "____________________________________________________________") %>%
    #             .[-1,]
    # 
    #     import_m <- rbind.data.frame(import_m, dat)
    # 
    #   }
    # }
    # 
    # # Import any additional news articles and append to data frame (i.e., BBC or AP)
    # 
    # # Export to .csv for easier access later
    # 
    # write.csv(import_m, 'data/corpora/preprocessed/media/media_docs_singlefile.csv', fileEncoding = "UTF-8")
    

    # ---- 4.1.2 OPTION B: import a single .csv of the texts (ONCE OPTION A HAS BEEN RUN AT LEAST ONCE) ----

    import_m <- read.csv('data/corpora/preprocessed/media/media_docs_singlefile.csv', fileEncoding = "UTF-8")
 
 
# ---- 4.2 Filter and pre-process (ONLY NEED TO RUN ONCE - FOR ANALYSIS, GO STRAIGHT TO SECTION 4.4) ----
# 
# docs_m <- import_m %>%
#   transmute(title = str_extract(raw, "(?<=Title:\\s).*.(?=\\sPublication title:)"), 
#             publication = str_extract(raw, "(?<=Publication title:\\s).*.(?=\\sPages:)"),
#             publication = ifelse(is.na(publication), 
#                                  str_extract(raw, "(?<=Publication title:\\s).*.(?=\\sPublication year:)"), 
#                                  publication),
#             publication = ifelse(is.na(publication), 
#                                  str_extract(raw, "(?<=Publication title:\\s).*.(?=\\sPublicationyear:)"), 
#                                  publication),
#             publication = ifelse(is.na(publication), 
#                                  str_extract(raw, "(?<=Publicationtitle:\\s).*.(?=\\sPublication year:)"), 
#                                  publication),
#             publication = ifelse(is.na(publication), 
#                                  str_extract(raw, "(?<=Publication title:\\s).*.(?=\\sPublication date:)"), 
#                                  publication),
#             year = str_extract(raw, "(?<=Publication year:\\s).*.(?=\\sPublication date:)"),
#             year = ifelse(is.na(year), 
#                           str_extract(raw, "(?<=Publicationyear:\\s).*.(?=\\sPublication date:)"), 
#                           year),
#             date = str_extract(raw, "(?<=Publication date:\\s)(.{12})"),
#             subject = str_extract(raw, "(?<=Subject:\\s).*.(?=\\sTitle:)") %>%
#               str_replace(., "Location:.*", ""),
#             author = str_extract(raw, "(?<=Author:\\s).*.(?=\\sPublication info:)"),
#             author = ifelse(is.na(author), 
#                             str_extract(raw, "(?<=Credit:\\s).*.(?=\\sSubject:)"),
#                             author),
#             section = str_extract(raw, "(?<=Section:\\s).*.(?=\\sPublisher:)"),
#             text = str_extract(raw, "(?<=Full text:\\s).*.(?=\\sSubject:)"),
#             text = ifelse(is.na(text),
#                           str_extract(raw, "(?<=Full text:\\s).*.(?=\\Location:)"),
#                           text),
#             text = text %>%
#               str_replace(., "\\(.*.New York Times\\)", "") %>%
#               str_replace(., "\\(.*.Associated Press\\)", "") %>%
#               str_replace(., "\\(Associated Press\\)", "") %>%
#               str_replace(., "\\(.*.Wildlife Conservation Society\\)", "") %>%
#               str_replace(., "\\(pg.*.\\)", "") %>%
#               str_replace(., "\\(See related.*.\\)", "") %>%
#               str_replace(., "Credit:.*", "") %>%
#               str_replace(., "â", "") %>%
#               str_replace(., "Enlarge this image. ", "") %>%
#               str_replace(., "PHOTOGRAPH BY.*.", "") %>%
#               str_replace(., "PHOTOGRAPHS BY.*.", "") %>%
#               str_replace(., "Photograph .*.", ""))
# 
# docs_m <- docs_m %>%
#   mutate(docid = row_number(),
#          publication = str_replace_all(publication, " ", "")) %>%
#   filter(!is.na(text)) # roughly 35 articles were unable to be located using the above wrangling procedure, and are unrecoverable with the given info (only know a single author's name for some)
# 
# write.csv(docs_m, "data/corpora/preprocessed/media/docs_m.csv", fileEncoding = "UTF-8")



# ---- 4.3 Output files to find duplicates using Python function in code/processing/findDuplicates.py (ONLY NEED TO RUN ONCE) ----

# docs_nyt_findDuplicates <-
#   docs_m %>%
#   filter(grepl("NewYorkTimes", publication)) %>%
#   select(docid, text)
# 
# docs_wsj_findDuplicates <-
#   docs_m %>%
#   filter(grepl("WallStreetJournal", publication)) %>%
#   select(docid, text)
# 
# 
# NOTE: write to the same directory that the python scripts will be run, for ease
# 
# write.csv(docs_nyt_findDuplicates, "code/processing/docs_nyt_findDuplicates.csv", row.names = F, fileEncoding = "UTF-8")
# write.csv(docs_wsj_findDuplicates, "code/processing/docs_wsj_findDuplicates.csv", row.names = F, fileEncoding = "UTF-8")



# ---- 4.X WORKFLOW NOTE ----

# After following sections 4.1 - 4.3, you will need to run the following two scripts in code/processing:
# --- findDuplicates.py
# --- removeDuplicates.R

# These scripts will identify duplicated articles and further process docs_m data frame to remove duplicates from analysis.
# --- removeDuplicates.R will output a flat file of docs_m, filtered to remove duplicates. This is what is sourced in section 4.4 below.


# ---- 4.4 Import docs_m_filtered for analysis ----

docs_m_filtered <- read.csv("data/corpora/preprocessed/media/docs_m_filtered.csv", fileEncoding = "UTF-8")


