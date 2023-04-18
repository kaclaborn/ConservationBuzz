# 
# code: Source and process stopword and thesaurus files
# 

# ---- Lemma data ----

# Build upon hash_lemmas table from lexicon package

add_to_lemma <- rbind(data.frame(token = "analyse", lemma = "analyze"),
                      data.frame(token = "behaviour", lemma = "behavior"),
                      data.frame(token = "greenspace", lemma = "green space"),
                      data.frame(token = "programme", lemma = "program"),
                      data.frame(token = "sdgs", lemma = "sdg"),
                      data.frame(token = "socioeconomic", lemma = "socio-economic"),
                      data.frame(token = "usa", lemma = "unite state"),
                      data.frame(token = "decision-making", lemma = "decision making"))


thesaurus <- rbind(hash_lemmas, add_to_lemma) %>%
  mutate(lemma = ifelse(lemma=="analyse", 
                        "analyze", 
                        ifelse(lemma=="behaviour", 
                               "behavior", 
                               lemma))
  )


# ---- Stopwords ----

add_to_stopwords <- c(# "-ly" adverbs
                      "accurately", "additionally", "adequately", "alternatively", "approximately", 
                      "broadly", "commonly", "consistent", "consistently", "considerably", "dangerously", "directly",
                      "effectively", "efficiently", "explicitly", "finally", "frequently", "generally",  
                      "globally", "greatly", "heavily", "highly", "increasingly",  
                      "largely", "locally", "positively", "potentially", 
                      "predominantly", "previously", "primarily", "rapidly", "rarely", "recently",
                      "significantly", "simultaneously", "slightly", "spatially", "specifically", 
                      "strongly", "substantially", "typically", "widely",
                      
                      # abbreviations & numbers (not removed through tokenization process)
                      "ca", "cm", "co2", "es", "ess", "gi", "ha", "î", "kg", "km", "km2", "lt", "pa", "ph", "r", 
                      "-1", "-2", "-based", "0-3", "1-2", "10-13", "10-20", "10-30", 
                      "10-year", "100-year", "100-150", "13c", "15n", "1800s", "1830s", "1880s", "19", "19th",
                      "1900s", "1920s", "1930s", "1950s", "1960s", "1970s", "1980s", "1990s", 
                      "1980-2000", "1988-1991", "1994-1995", "1995-1998", "1995-2000", "1996-1998", 
                      "1996-1999", "1997-1998", "1997-1999", "1998-1999", "1998-2000", "1998-2001", "2000-2001",
                      "2", "2-3", "2-4", "2-5", "2-9", "20", "20-30", "20-25", "20-40", "2-year", "20th", "21st",
                      "3", "3-4", "3-5", "3-7", "3d", "30-40", "30-50", "30-70", "30-year", 
                      "4", "4-10", "4-year", "40-50", "5", "5-8", "5-10", "5-11", "5-year", 
                      "6-10", "6years", "6-year", "8-month", "9-year", "mr", "ms", "mrs", "dr", 
                      ## Should I leave in references to decades and centuries and multi-year events? (e.g., "1990s", "21st century", "6-year")
                      
                      # highly frequent, non-discriminating words
                      "increase", "decrease", "reduce", "multiple", "vary", "similar", "lack", "datum", "low", "high", "remain", 
                      "decline", "due", "include", "exclude", "potential", "apply", "paper", "study", "add", "addition", "additional", "address",
                      "aim", "alter", "article", "aspect", "bring", "choice", "choose", "considerable",
                      
                      # highly frequent descriptors of findings or methods in generic terms
                      "result", "suggest", "show", "find", "significant", "examine", "investigate", "demonstrate", "finding",
                      "highlight", "consider", "explore", "conduct", "reveal", "report", "explain", "effect", "affect", "identify",
                      "provide", "assess", "determine", "compare", "observe", "analyze", "analysis", "argue", "associate", "assume", 
                      "assumption", "attempt", "avoid", "derive", "describe", "detail", "differ", "discuss", "emphasize", "estimate",
                      "expect") 


stopwords_extended <- c(stopwords(source = "smart"), add_to_stopwords)

# Import parts-of-speech tagged stopwords list
stopwords_extended_pos <- scan("code/source/stopwords_extended_pos.txt", character())

