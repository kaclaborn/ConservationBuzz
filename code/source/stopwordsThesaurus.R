# 
# code: Source and process stopword and thesaurus files
# 

# ---- Lemma data ----

# Build upon hash_lemmas table from lexicon package

add_to_lemma <- rbind(data.frame(token = "analyse", lemma = "analyze"),
                      data.frame(token = "analyses", lemma = "analyze"),
                      data.frame(token = "behaviour", lemma = "behavior"),
                      data.frame(token = "greenspace", lemma = "green space"),
                      data.frame(token = "programme", lemma = "program"),
                      data.frame(token = "sdgs", lemma = "sdg"),
                      data.frame(token = "socioeconomic", lemma = "socio-economic"),
                      data.frame(token = "usa", lemma = "unite state"))


thesaurus <- rbind(hash_lemmas, add_to_lemma)


# ---- Stopwords ----

add_to_stopwords <- c(# "-ly" adverbs
                      "accurately", "additionally", "adequately", "alternatively", "approximately", 
                      "broadly", "commonly", "consistent", "consistently", "directly",
                      "effectively", "explicitly", "finally", "frequently", "generally",  
                      "globally", "greatly", "heavily", "highly", "increasingly",  
                      "largely", "locally", "positively", "potentially", 
                      "predominantly", "previously", "primarily", "rapidly", "rarely", "recently",
                      "significantly", "simultaneously", "slightly", "spatially", "specifically", 
                      "strongly", "substantially", "typically", "widely",
                      
                      # abbreviations
                      "ca", "cm", "co2", "es", "ess", "gi", "ha", "î", "kg", "km", "km2", "lt", "pa", "ph", "r", 
                      
                      # highly frequent, non-discriminating words
                      "increase", "multiple", "vary", "similar", "lack", "datum",
                      
                      # highly frequent descriptors of findings or methods in generic terms
                      "result", "suggest", "show", "find", "significant", "examine", "investigate", "demonstrate", "finding",
                      "highlight", "consider", "explore", "conduct", "reveal", "report", "explain", "effect", "affect") 


stopwords_extended <- c(stopwords(source = "smart"), add_to_stopwords)
