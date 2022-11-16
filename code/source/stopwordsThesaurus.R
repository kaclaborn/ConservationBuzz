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

add_to_stopwords <- c("accurately", "additionally", "adequately", "alternatively", "approximately", 
                      "broadly", "ca", "cm", "commonly", "consistent", "consistently", "directly",
                      "effectively", "es", "ess", "explicitly", "finally", "frequently", "generally", "gi", 
                      "globally", "greatly", "ha", "heavily", "highly", "increasingly", "î", 
                      "kg", "km", "km2", "largely", "locally", "lt", "ph", "positively", "potentially", 
                      "predominantly", "previously", "primarily", "r", "rapidly", "rarely", "recently",
                      "significantly", "simultaneously", "slightly", "spatially", "specifically", "strongly", 
                      "substantially", "typically", "widely") 
# should I remove all "-ly" adverbs? Or, could lemmatize them? Or, leave them alone

stopwords_extended <- c(stopwords(source = "smart"), add_to_stopwords)
