# 
# code: Source and process stopword and thesaurus files
# 


pacman::p_load(lexicon, stopwords)


# ---- Lemma data ----


# Build upon hash_lemmas table from lexicon package

add_to_lemma <- bind_rows(# British English spellings
                          data.frame(token = "analyse", lemma = "analyze"),
                          data.frame(token = "behaviour", lemma = "behavior"),
                          data.frame(token = "programme", lemma = "program"),
                          data.frame(token = "recognise", lemma = "recognize"),
                          data.frame(token = "emphasise", lemma = "emphasize"),
                          data.frame(token = "prioritise", lemma = "prioritize"),
                          data.frame(token = "harbour", lemma = "harbor"),
                          data.frame(token = "colonisation", lemma = "colonization"),
                          data.frame(token = "prioritisation", lemma = "prioritization"),
                          data.frame(token = "generalise", lemma = "generalize"),
                          data.frame(token = "maximise", lemma = "maximize"),
                          data.frame(token = "organisation", lemma = "organization"),
                          data.frame(token = "mobilise", lemma = "mobilize"),
                          data.frame(token = "urbanisation", lemma = "urbanization"),
                          data.frame(token = "favourable", lemma = "favorable"),
                          data.frame(token = "marginalise", lemma = "marginalize"),
                          data.frame(token = "standardise", lemma = "standardize"),
  
                          # Hyphenated or abbreviated words
                          data.frame(token = "greenspace", lemma = "green_space"),
                          data.frame(token = "sdgs", lemma = "sdg"),
                          data.frame(token = "socioeconomic", lemma = "socio-economic"),
                          data.frame(token = "usa", lemma = "unite state"),
                          data.frame(token = "decision-making", lemma = "decision_making"),
                          data.frame(token = "manage-ment", lemma = "management"),
                          data.frame(token = "govern-ment", lemma = "government"),
                          data.frame(token = "partner-ship", lemma = "partnership"),
                          data.frame(token = "solu-tions", lemma = "solution"),
                          data.frame(token = "nbs", lemma = "nature-based_solution"),
                          
                          # Words with multiple nouns associated with same concept
                          data.frame(token = "equitability", lemma = "equitable"),
                          data.frame(token = "vulnerability", lemma = "vulnerable"),
                          data.frame(token = "vulnerabilities", lemma = "vulnerable"),
                          data.frame(token = "sustainability", lemma = "sustainable"),
                          data.frame(token = "accessibility", lemma = "accessible"),
                          data.frame(token = "resilient", lemma = "resilience"),
                          data.frame(token = "inclusion", lemma = "inclusive"),
                          data.frame(token = "collaboration", lemma = "collaborate"))

thesaurus <- rbind(hash_lemmas, add_to_lemma) %>%
  mutate(lemma = case_when(lemma=="analyse" ~ "analyze", 
                           lemma=="behaviour" ~ "behavior", 
                           lemma=="programme" ~ "program",
                           lemma=="recognise" ~ "recognize",
                           lemma=="emphasise" ~ "emphasize",
                           lemma=="prioritise" ~ "prioritize",
                           lemma=="harbour" ~ "harbor",
                           lemma=="colonisation" ~ "colonization",
                           lemma=="prioritisation" ~ "prioritization",
                           lemma=="generalise" ~ "generalize",
                           lemma=="maximise" ~ "maximize",
                           lemma=="organisation" ~ "organization",
                           lemma=="mobilise" ~ "mobilize",
                           lemma=="standardise" ~ "standardize",
                           TRUE ~ lemma)
  ) %>%
  filter(token!="number")


# ---- Stopwords ----

add_to_stopwords <- c(# highly frequent proper nouns
                      "tnc", "iucn", "wwf", "dswf", "ecohealth", "iied", "ci",
                      
                      
                      # abbreviations & numbers (not removed through tokenization process)
                      "μmol", "a2", "ca", "cm", "co2", "c4", "e.g", "es", "ess", "gi", "ha",  
                      "î", "kg", "km", "km2", "lt", "n2", "o2", "pa", "ph", "r", 
                      "-1", "-2", "-based", "0-3", "1-2", "10", "10-13", "10-20", "10-30", 
                      "10-year", "100-year", "100-150", "12th", "13c", "15n", "1800s", "1830s", "1880s", "19", "19th",
                      "1900s", "1920s", "1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990s", 
                      "1980-2000", "1988-1991", "1994-1995", "1995-1998", "1995-2000", "1996-1998", 
                      "1996-1999", "1997-1998", "1997-1999", "1998-1999", "1998-2000", "1998-2001", "2000-2001",
                      "2017-2018", "2018-", "2020-2021", "2020-2030", "2o", "2", "2-3", "2-4", "2-5", "2-9", 
                      "20", "20-30", "20-25", "20-40", "200b", "2-year", "20th", "21st",
                      "3", "3-4", "3-5", "3-7", "3d", "30-40", "30-50", "30-70", "30-year", 
                      "4", "4-10", "4-year", "40-50", "5", "5-8", "5-10", "5-11", "5-year", "6", 
                      "6-10", "6years", "6-year", "70th", "8-month", "9-year", "mr", "ms", "mrs", "dr"
                      ) 


# Import parts-of-speech tagged stopwords list (already includes standard stopwords list), 
# add additional words from above (manually identified)
stopwords_extended <- c(scan("code/source/stopwords_extended_pos.txt", character()), add_to_stopwords)


# collocates stopword list
collocates_stopwords <- c("birdlife_international", "birdlife_partner", "birdlife", "conservation_international",
                          "nature_conservancy", "david_shepherd_wildlife_foundation", "david_shepherd", 
                          "world_wildlife_fund", "wetlands_international")