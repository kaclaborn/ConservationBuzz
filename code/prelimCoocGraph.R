# 
# code: Preliminary word count, n-gram, and co-occurrence analysis
# 


# ---- Source thesaurus and stopword list ----

source('code/source/stopwordsThesaurus.R')


# ---- Define functions to run analyses for different year periods, with different parameters ----

source('code/source/calculateCoocStatistics.R')

# -- Subsetting function
subsetDTM <- function(dat = NULL, years = NULL) {
  
  # filter data & turn into corpus
  dat <- dat %>% filter(year%in%years)
  corp <- corpus(dat)
  sentences <- corpus_reshape(corp, to = "sentences")
  
  # tokenize
  sentence_tokens <- sentences %>% 
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
    tokens_tolower() %>% 
    tokens_remove(pattern = stopwords_extended, padding = T) %>%
    tokens_replace(thesaurus$token, thesaurus$lemma, valuetype = "fixed")
  
  # identify candidate collocations
  collocations <- textstat_collocations(sentence_tokens, min_count = 5)
  collocations <- collocations[1:500, ]
  
  sentence_tokens <- tokens_compound(sentence_tokens, collocations)
  
  
  # create DTM, prune vocabulary and set binary values for presence/absence of types
  binDTM <- sentence_tokens %>% 
    tokens_remove("") %>%
    dfm() %>% 
    dfm_trim(min_docfreq = 0.002, max_docfreq = 1, docfreq_type = "prop") %>%  # only include tokens that exist across at least 1% of documents
    dfm_weight("boolean")
  
  return(binDTM)
  
}



# -- Function to create a co-occurrence network, ready to be input to ORA
coocGraph <- function(dat = NULL, numCoocs = 200, coocTerm = NULL) {
  
  # Define parameters for the central co-occurrence term of interest & number of co-occurrences to include in analysis
  coocTerm <- coocTerm
  numberOfCoocs <- numCoocs
  counts <- t(dat) %*% dat
  
  # Calculate statistics for coocTerm 
  coocs <- calculateCoocStatistics(coocTerm, dat, measure = "LOGLIK")
  
  # Display the main terms (n = numberOfCoocs)
  print(coocs[1:numberOfCoocs])
  
  
  # Create dummy data frame for results 
  resultGraph <- data.frame(from = character(), 
                            to = character(), 
                            sig = numeric(0))
  
  
  # Structure of the temporary graph object is equal to that of the resultGraph
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  # Fill the data.frame to produce the correct number of lines
  tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
  # Entry of the search word into the first column in all lines
  tmpGraph[, 1] <- coocTerm
  # Entry of the co-occurrences into the second column of the respective line
  tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
  # Set the significances
  tmpGraph[, 3] <- coocs[1:numberOfCoocs]
  
  # attach the triples to resultGraph
  resultGraph <- rbind(resultGraph, tmpGraph)
  
  
  # iterate over the most significant numberOfCoocs co-occurrences search term
  for (i in 1:numberOfCoocs){
    
    # calling up the co-occurrence calculation for term i from the search words co-occurrences
    newCoocTerm <- names(coocs)[i]
    coocs2 <- calculateCoocStatistics(newCoocTerm, dat, measure = "LOGLIK")
    
    # print the co-occurrences
    coocs2[1:10]
    
    # structure of the temporary graph object
    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
    tmpGraph[, 1] <- newCoocTerm
    tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
    tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
    
    # append the result to the result graph
    resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
  }

  coocFreq <- numeric(0)
  
  for(i in 1:nrow(resultGraph)) {
    coocFreq[i] <- counts[resultGraph$from[i], resultGraph$to[i]]
  }
  
  resultGraph <- cbind(resultGraph, coocFreq)
  
  # post-process output resultGraph data frame, readying for visualization
  resultGraph <- resultGraph %>%
    mutate(from = stringr::str_replace_all(from, "_", " "),
           to = stringr::str_replace_all(to, "_", " "))
  
  return(resultGraph)
  
}

counts <- t(DTM_a_2020) %*% DTM_a_2020
coocs <- data.frame(sig = calculateCoocStatistics("conservation", DTM_a_2020, measure = "LOGLIK")) %>%
  mutate(word = rownames(.)) %>%
  filter(sig>=3.8)

coocFreq <- data.frame(from = character(),
                       to = character(),
                       coocFreq = numeric(0))

for(i in 1:nrow(coocs)) {
  coocFreq[i, "from"] <- "conservation"
  coocFreq[i, "to"] <- coocs$word[i]
  coocFreq[i, "coocFreq"] <- counts["conservation", coocs$word[i]]
}


coocGraph3Tier <- function(dat = NULL, coocTerm = NULL, sigval = 3.8) {
  
  # Define parameters for the central co-occurrence term of interest & number of co-occurrences to include in analysis
  coocTerm <- coocTerm
  counts <- t(dat) %*% dat
  
  # Calculate statistics for coocTerm, keeping all terms that are significant at a level of 0.05
  coocs <- data.frame(sig = calculateCoocStatistics(coocTerm, dat, measure = "DICE")) %>%
    mutate(word = rownames(.)) %>%
    filter(sig>=sigval)
  
  
  # Create dummy data frame for results 
  resultGraph <- data.frame(from = character(), 
                            to = character(), 
                            sig = numeric(0))
  
  
  # Structure of the temporary graph object is equal to that of the resultGraph
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  numSigCoocs <- length(coocs$word)
  
  # Fill the data.frame to produce the correct number of lines
  tmpGraph[1:numSigCoocs, 3] <- coocs[1:numSigCoocs, "sig"]
  # Entry of the search word into the first column in all lines
  tmpGraph[, 1] <- coocTerm
  # Entry of the co-occurrences into the second column of the respective line
  tmpGraph[, 2] <- coocs$word[1:numSigCoocs]
  # Set the significances
  tmpGraph[, 3] <- coocs$sig[1:numSigCoocs]
  
  # attach the triples to resultGraph
  resultGraph <- rbind(resultGraph, tmpGraph)
  
  
  # iterate over the most significant numberOfCoocs co-occurrences search term
  for (i in 1:numSigCoocs){
    
    # calling up the co-occurrence calculation for term i from the search words co-occurrences
    newCoocTerm <- coocs$word[i]
    coocs2 <- data.frame(sig = calculateCoocStatistics(newCoocTerm, dat, measure = "DICE")) %>%
      mutate(word = rownames(.)) %>%
      filter(sig>=sigval)
    
    # print the co-occurrences
    print(toupper(newCoocTerm))
    
    numSigCoocs2 <- length(coocs2$word)
    
    # structure of the temporary graph object
    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numSigCoocs2, 3] <- coocs2[1:numSigCoocs2, "sig"]
    tmpGraph[, 1] <- newCoocTerm
    tmpGraph[, 2] <- coocs2$word[1:numSigCoocs2]
    tmpGraph[, 3] <- coocs2$sig[1:numSigCoocs2]
    
    # append the result to the result graph
    resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
    
    for (j in 1:numSigCoocs2) {
      
      # calling up the co-occurrence calculation for term j from the search words co-occurrences
      newCoocTerm2 <- coocs2$word[j]
      
      coocs3 <- data.frame(sig = calculateCoocStatistics(newCoocTerm2, dat, measure = "DICE")) %>%
        mutate(word = rownames(.)) %>%
        filter(sig>=sigval)
    
      numSigCoocs3 <- length(coocs3$word)
      
      # print the co-occurrences
      print(paste("Tier 2: ", coocs$word[i], " (", i, " of ", numSigCoocs, ")", "; ", 
                  coocs2$word[j], " (", j, " of ", numSigCoocs2, ")", "; ", numSigCoocs3, " co-occurrences",
                  sep = ""))

      
      # structure of the temporary graph object
      tmpGraph2 <- data.frame(from = character(), to = character(), sig = numeric(0))
      tmpGraph2[1:numSigCoocs3, 3] <- coocs2[1:numSigCoocs3, "sig"]
      tmpGraph2[, 1] <- newCoocTerm2
      tmpGraph2[, 2] <- coocs3$word[1:numSigCoocs3]
      tmpGraph2[, 3] <- coocs3$sig[1:numSigCoocs3]
      
      # append the result to the result graph
      resultGraph <- rbind(resultGraph, tmpGraph2[2:length(tmpGraph2[, 1]), ])
    }
  }

  # remove an NA row from output network
  resultGraph <- resultGraph %>% filter(!is.na(from) & !is.na(to))
  
  
  # identify number of co-occurrences across the corpus per observation in output network
  coocFreq <- numeric(0)

  for(i in 1:nrow(resultGraph)) {
    coocFreq[i] <- counts[resultGraph$from[i], resultGraph$to[i]]
  }

  resultGraph <- cbind(resultGraph, coocFreq)

  # post-process output resultGraph data frame, readying for visualization
  resultGraph <- resultGraph %>%
    mutate(from = stringr::str_replace_all(from, "_", " "),
           to = stringr::str_replace_all(to, "_", " "))
  
  return(resultGraph)
  
}


# ---- ACADEMIA ----
  
# -- Full corpus (all years)

wordcount_a <-
  corpus(docs_a) %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords_extended, padding = T) %>%
  tokens_replace(thesaurus$token, thesaurus$lemma, valuetype = "fixed") %>%
  dfm(., remove_padding = TRUE) %>%
  textstat_frequency(., n = 4000)

DTM_a <- subsetDTM(dat = docs_a, years = 2000:2021)


coocGraph_a <- 
  DTM_a %>%
  coocGraph(dat = ., numCoocs = 100, coocTerm = as.character(wordcount_a[1,1]))


export(coocGraph_a, 'data/outputs/coocGraph_a_100cooc.csv')


# -- By year

wordcount_a_2020 <- 
  corpus(docs_a %>% filter(year==2020)) %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords_extended, padding = T) %>%
  tokens_replace(thesaurus$token, thesaurus$lemma, valuetype = "fixed") %>%
  dfm(., remove_padding = TRUE) %>%
  textstat_frequency(., n = 4000)
  

DTM_a_2020 <- subsetDTM(dat = docs_a, years = 2020)

coocGraph_a_2020 <- 
  DTM_a_2020 %>%
  coocGraph3Tier(dat = ., coocTerm = "conservation", sigval = 0.03) %>%
  distinct()


conductivity_a_2020 <- data.frame(conductivity = estimate_betweenness(graph.data.frame(coocGraph_a_2020, directed = T), cutoff = 2),
                                  degree = degree(graph.data.frame(coocGraph_a_2020, directed = T), mode = "total")) %>%
  mutate(node = rownames(.))


nodeFreq_a_2020 <- data.frame(node = colnames(DTM_a_2020),
                              freq = diag(t(DTM_a_2020) %*% DTM_a_2020)) %>%
  mutate(node = stringr::str_replace_all(node, "_", " "))


node_attributes_a_2020 <- 
  coocGraph_a_2020 %>%
  mutate(link1 = paste(from, to, sep = " -"),
         link2 = paste(to, from, sep = " -")) %>%
  pivot_longer(cols = c(link1, link2), values_to = "link") %>%
  select(link, sig, coocFreq) %>%
  distinct() %>%
  mutate(node = stringr::str_extract(link, ".*.(?=\\s-)")) %>%
  left_join(nodeFreq_a_2020, by = "node") %>%
  mutate(consensus = ifelse(coocFreq/freq>=0.1, 1, 0)) %>% # if co-occurrence exists at least 10% of the time the focal node is used, counts as "consensus"
  group_by(node) %>%
  summarise(consensus = sum(consensus) / length(node),
            density = length(node)) %>%
  left_join(nodeFreq_a_2020, by = "node") %>%
  left_join(conductivity_a_2020, by = "node")

coocGraph_a_2020_filtered <- 
  coocGraph_a_2020 %>% 
  left_join(node_attributes_a_2020, by = c("from" = "node"))


export(testCoocGraph3Tier, 'data/outputs/coocGraph_a_2020_numCooc_bysig.csv')
export(node_attributes_a_2020, 'data/outputs/node_attributes_a_2020_numCooc_bysig.csv')
# export(nodeFreq_a_2020, 'data/outputs/nodeFreq_a_2020.csv')


quantiles_a_2020 <- as.data.frame(cbind(quantile = paste0(seq(0, 100, by = 5), "%", sep = ""),
                                        consensus = as.numeric(quantile(node_attributes_a_2020$consensus, 
                                                                        seq(0, 1, by = 0.05))),
                                        degree = as.numeric(quantile(node_attributes_a_2020$degree, 
                                                                     seq(0, 1, by = 0.05))),
                                        conductivity = as.numeric(quantile(node_attributes_a_2020$conductivity, 
                                                                           seq(0, 1, by = 0.05))),
                                        freq = as.numeric(quantile(node_attributes_a_2020$freq,
                                                                   seq(0, 1, by = 0.05)))))

placeholders_a_2020 <- 
  node_attributes_a_2020 %>%
  filter(consensus<=as.numeric(quantiles_a_2020$consensus[quantiles_a_2020$quantile=="20%"]) & 
           degree>=as.numeric(quantiles_a_2020$degree[quantiles_a_2020$quantile=="80%"]) & 
           conductivity>=as.numeric(quantiles_a_2020$conductivity[quantiles_a_2020$quantile=="80%"]))

buzzwords_a_2020 <- 
  node_attributes_a_2020 %>%
  filter(consensus<=as.numeric(quantiles_a_2020$consensus[quantiles_a_2020$quantile=="20%"]) & 
           degree<=as.numeric(quantiles_a_2020$degree[quantiles_a_2020$quantile=="80%"]) & 
           conductivity>=as.numeric(quantiles_a_2020$conductivity[quantiles_a_2020$quantile=="80%"]))


# Compare 3 tier coocGraphs using different starting focal terms

coocGraph_compare <- data.frame(focal_node = character(), 
                                n_cooc = numeric(0),
                                n_nodes = numeric(0),
                                n_placeholders = numeric(0),
                                n_buzzwords = numeric(0),
                                avg_conductivity = numeric(0),
                                avg_degree = numeric(0),
                                clustering_coeff = numeric(0),
                                placeholders_1_10 = character(),
                                buzzwords_1_10 = character())


for(i in wordcount_a_2020$feature[1:5]){
  
  year <- 2020
  cooc <- i
  nCooc <- 20
  
  
  DTM_a <- subsetDTM(dat = docs_a, years = year)
  
  resultGraph3Tier_a <- 
    DTM_a %>%
    coocGraph3Tier(dat = ., numCoocs = nCooc, coocTerm = cooc) %>%
    distinct()
  
  conductivity_a <- data.frame(conductivity = betweenness(graph.data.frame(resultGraph3Tier_a, directed = T)),
                                    degree = degree(graph.data.frame(resultGraph3Tier_a, directed = T))) %>%
    mutate(node = rownames(.))
  
  
  nodeFreq_a <- data.frame(node = colnames(DTM_a),
                                freq = diag(t(DTM_a) %*% DTM_a)) %>%
    mutate(node = stringr::str_replace_all(node, "_", " "))
  
  
  node_attributes_a <- 
    resultGraph3Tier_a %>%
    mutate(link1 = paste(from, to, sep = " -"),
           link2 = paste(to, from, sep = " -")) %>%
    pivot_longer(cols = c(link1, link2), values_to = "link") %>%
    select(link, sig, coocFreq) %>%
    distinct() %>%
    mutate(node = stringr::str_extract(link, ".*.(?=\\s-)")) %>%
    left_join(nodeFreq_a_2020, by = "node") %>%
    mutate(consensus = ifelse(coocFreq/freq>=0.1, 1, 0)) %>% # if co-occurrence exists at least 10% of the time the focal node is used, counts as "consensus"
    group_by(node) %>%
    summarise(consensus = sum(consensus) / length(node),
              density = length(node)) %>%
    left_join(nodeFreq_a_2020, by = "node") %>%
    left_join(conductivity_a_2020, by = "node") %>%
    filter(conductivity!=0) # remove all nodes that only exist on the edge of the semantic network of interest
  

  quantiles_a <- as.data.frame(cbind(quantile = paste0(seq(0, 100, by = 5), "%", sep = ""),
                                          consensus = as.numeric(quantile(node_attributes_a$consensus, 
                                                                          seq(0, 1, by = 0.05))),
                                          degree = as.numeric(quantile(node_attributes_a$degree, 
                                                                       seq(0, 1, by = 0.05))),
                                          conductivity = as.numeric(quantile(node_attributes_a$conductivity, 
                                                                             seq(0, 1, by = 0.05)))))
  
  placeholders_a <- 
    node_attributes_a %>%
    filter(consensus<=quantiles_a$consensus[quantiles_a$quantile=="20%"] & 
             degree>=quantiles_a$degree[quantiles_a$quantile=="80%"] & 
             conductivity>=quantiles_a$conductivity[quantiles_a$quantile=="80%"])
  
  buzzwords_a <- 
    node_attributes_a %>%
    filter(consensus<=quantiles_a$consensus[quantiles_a$quantile=="20%"] & 
             degree<=quantiles_a$degree[quantiles_a$quantile=="80%"] & 
             conductivity>=quantiles_a$conductivity[quantiles_a$quantile=="80%"])
  
  output_tocompare <- 
    data.frame(focal_node = cooc, 
               n_cooc = nCooc,
               n_nodes = length(node_attributes_a$node),
               n_placeholders = length(placeholders_a$node),
               n_buzzwords = length(buzzwords_a$node),
               avg_conductivity = mean(node_attributes_a$conductivity),
               avg_degree = mean(node_attributes_a$degree),
               clustering_coeff = transitivity(graph.data.frame(resultGraph3Tier_a, directed = T)),
               placeholders_1_10 = paste(placeholders_a$node[1], placeholders_a$node[2],
                                          placeholders_a$node[3], placeholders_a$node[4],
                                          placeholders_a$node[5], placeholders_a$node[6],
                                          placeholders_a$node[7], placeholders_a$node[8],
                                          placeholders_a$node[9], placeholders_a$node[10],
                                          sep = ", "),
               buzzwords_1_10 = paste(buzzwords_a$node[1], buzzwords_a$node[2],
                                      buzzwords_a$node[3], buzzwords_a$node[4],
                                      buzzwords_a$node[5], buzzwords_a$node[6],
                                      buzzwords_a$node[7], buzzwords_a$node[8],
                                      buzzwords_a$node[9], buzzwords_a$node[10],
                                         sep = ", "))
  
  coocGraph_compare <- rbind(coocGraph_compare, output_tocompare)
  
}
  

# ---- Full corpus ----

wordcount_a <- 
  corpus(docs_a) %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords_extended, padding = T) %>%
  tokens_replace(thesaurus$token, thesaurus$lemma, valuetype = "fixed") %>%
  dfm(., remove_padding = TRUE) %>%
  textstat_frequency(., n = 4000)

DTM_a <- subsetDTM(dat = docs_a, years = 2000:2021)

coocGraph_a <- 
  DTM_a %>%
  coocGraph(dat = ., numCoocs = 200, coocTerm = "conservation")


conductivity_a <- data.frame(conductivity = betweenness(graph.data.frame(coocGraph_a, directed = T)),
                                  degree = degree(graph.data.frame(coocGraph_a, directed = T))) %>%
  mutate(node = rownames(.))


nodeFreq_a <- data.frame(node = colnames(DTM_a),
                              freq = diag(t(DTM_a) %*% DTM_a)) %>%
  mutate(node = stringr::str_replace_all(node, "_", " "))


node_attributes_a <- 
  coocGraph_a %>%
  mutate(link1 = paste(from, to, sep = " -"),
         link2 = paste(to, from, sep = " -")) %>%
  pivot_longer(cols = c(link1, link2), values_to = "link") %>%
  select(link, sig, coocFreq) %>%
  distinct() %>%
  mutate(node = stringr::str_extract(link, ".*.(?=\\s-)")) %>%
  left_join(nodeFreq_a, by = "node") %>%
  mutate(consensus = ifelse(coocFreq/freq>=0.1, 1, 0)) %>% # if co-occurrence exists at least 10% of the time the focal node is used, counts as "consensus"
  group_by(node) %>%
  summarise(consensus = sum(consensus) / length(node),
            density = length(node)) %>%
  left_join(nodeFreq_a, by = "node") %>%
  left_join(conductivity_a, by = "node") %>%
  filter(conductivity!=0) # remove all nodes that only exist on the edge of the semantic network of interest

coocGraph_a_filtered <- 
  coocGraph_a %>% 
  left_join(node_attributes_a, by = c("from" = "node"))


export(coocGraph_a, 'data/outputs/coocGraph_a.csv')
export(node_attributes_a, 'data/outputs/node_attributes_a.csv')
# export(nodeFreq_a_2010, 'data/outputs/nodeFreq_a_2010.csv')


quantiles_a <- as.data.frame(cbind(quantile = c("10%", "20%", "30%", "35%", "40%", "50%", "60%", "65%", "70%", "80%", "90%"),
                                        consensus = quantile(node_attributes_a$consensus, c(0.1, 0.2, 0.3, 0.35, 0.4, 0.5, 0.6, 0.65, 0.7, 0.8, 0.9)),
                                        degree = quantile(node_attributes_a$degree, c(0.1, 0.2, 0.3, 0.35, 0.4, 0.5, 0.6, 0.65, 0.7, 0.8, 0.9)),
                                        conductivity = quantile(node_attributes_a$conductivity, c(0.1, 0.2, 0.3, 0.35, 0.4, 0.5, 0.6, 0.65, 0.7, 0.8, 0.9))))

placeholders_a <- 
  node_attributes_a %>%
  filter(consensus<=quantiles_a$consensus[quantiles_a$quantile=="30%"] & 
           degree>=quantiles_a$degree[quantiles_a$quantile=="70%"] & 
           conductivity>=quantiles_a$conductivity[quantiles_a$quantile=="70%"])

buzzwords_a <- 
  node_attributes_a %>%
  filter(consensus<=quantiles_a$consensus[quantiles_a$quantile=="30%"] & 
           degree<=quantiles_a$degree[quantiles_a$quantile=="70%"] & 
           conductivity>=quantiles_a$conductivity[quantiles_a$quantile=="70%"])

# -- GRAB BAG



# ---- 1.2 Break apart academic corpora by year, run co-occurrence analysis  ----

for(i in 2000:2021) {
  corp <- corpus(docs_a %>% filter(year==i))
  
  corp_sent <- corpus_reshape(corp, to = "sentences") %>%
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
    tokens_tolower() %>% 
    tokens_remove(pattern = stopwords_extended, padding = T) %>%
    tokens_replace(thesaurus$token, thesaurus$lemma, valuetype = "fixed")


  
  # Calculate multi-word unit candidates to add to co-occurrence analysis
  
  corp_coll <- textstat_collocations(corp_sent, min_count = 25)
  corp_coll <- corp_coll[1:250, ]
  
  # Add collocations back into tokenized sentence corpus
  corp_sent <- tokens_compound(corp_sent, corp_coll)
  
  # Create DTM, prune vocabulary and set binary values for presence/absence of types
  binDTM <- corp_sent %>% 
    tokens_remove("") %>%
    dfm() %>% 
    dfm_trim(min_docfreq = 10, max_docfreq = 100000) %>% 
    dfm_weight("boolean")
  
  assign(paste("corpus_a_", i, sep = ""), dat)
}

# ---- MEDIA ----

corpus_m_tokens <- corpus_m %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords_extended, padding = TRUE) %>%
  tokens_replace(thesaurus$token, thesaurus$lemma, valuetype = "fixed")


corpus_m_dfm <- dfm(corpus_m_tokens, remove_padding = TRUE)

corpus_m_wordcount <- textstat_frequency(corpus_m_dfm, n = 4000)

corpus_m_bigrams <- textstat_collocations(corpus_m_tokens, size = 2, min_count = 25) %>%
  arrange(desc(count))

corpus_m_wordcount[corpus_m_wordcount$feature=="sustainability",]




hash_lemmas[hash_lemmas$token=="sustainability",]
