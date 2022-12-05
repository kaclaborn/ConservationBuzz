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
  # sentences <- corpus_reshape(corp, to = "sentences")
  
  # tokenize
 tokens <- corp %>%
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
    tokens_tolower() %>% 
    tokens_replace(thesaurus$token, thesaurus$lemma, valuetype = "fixed") %>%
    tokens_remove(pattern = stopwords_extended, padding = T)

  
  # identify candidate collocations
  collocations <- textstat_collocations(tokens, min_count = 5)
  collocations <- collocations[1:500, ]
  
  tokens <- tokens_compound(tokens, collocations)
  
  
  # create DTM, prune vocabulary and set binary values for presence/absence of types
  binDTM <- tokens %>% 
    tokens_remove("") %>%
    dfm() %>% 
    dfm_trim(min_docfreq = 0.002, max_docfreq = 1, docfreq_type = "prop") %>%  # only include tokens that exist across at least 1% of documents
    dfm_weight("boolean")
  
  return(binDTM)
  
}


DTM_a <- subsetDTM(dat = docs_a, years = 2000:2021)

# identify summary stats for co-occurrence significance tests for "conservation" (to determine threshold for significance)
conservation_coocs_a <- data.frame(sig = calculateCoocStatistics("conservation", DTM_a, measure = "DICE")) %>%
  mutate(word = rownames(.))


summary(conservation_coocs_a)
mean(conservation_coocs_a$sig) + 2*sd(conservation_coocs_a$sig)
ggplot(conservation_coocs_a) + geom_histogram(aes(sig))


# -- Function to create a co-occurrence network, using a three-tiered approach to co-occurrence identification
coocGraph3Tier <- function(dat = NULL, coocTerm = NULL, sd_multiplier = 2) {
  
  # Define parameters for the central co-occurrence term of interest & number of co-occurrences to include in analysis
  coocTerm <- coocTerm
  counts <- t(dat) %*% dat
  
  # Calculate statistics for coocTerm, keeping all terms that are significant at a level of 0.05
  coocs <- data.frame(sig = calculateCoocStatistics(coocTerm, dat, measure = "DICE")) %>%
    mutate(word = rownames(.)) 
  
  sigval <- mean(conservation_coocs_a$sig) + sd_multiplier*sd(conservation_coocs_a$sig)
  
  coocs <- coocs %>%
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
  tokens_replace(thesaurus$token, thesaurus$lemma, valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T) %>%
  dfm(., remove_padding = TRUE) %>%
  textstat_frequency(., n = 4000)

DTM_a <- subsetDTM(dat = docs_a, years = 2000:2021)

# identify summary stats for co-occurrence significance tests for "conservation"
conservation_coocs_a <- data.frame(sig = calculateCoocStatistics("conservation", DTM_a, measure = "DICE")) %>%
  mutate(word = rownames(.))
other_coocs_a <- data.frame(sig = calculateCoocStatistics("stakeholder", DTM_a, measure = "DICE")) %>%
  mutate(word = rownames(.))

summary(conservation_coocs_a)
mean(other_coocs_a$sig) + 2*sd(other_coocs_a$sig)
ggplot(conservation_coocs_a) + geom_histogram(aes(sig))

coocGraph_a <- 
  DTM_a %>%
  coocGraph(dat = ., numCoocs = 100, coocTerm = as.character(wordcount_a[1,1]))


export(coocGraph_a, 'data/outputs/coocGraph_a_100cooc.csv')


# -- By year

wordcount_a_2020 <- 
  corpus(docs_a %>% filter(year==2020)) %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(thesaurus$token, thesaurus$lemma, valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T) %>%
  dfm(., remove_padding = TRUE) %>%
  textstat_frequency(., n = 4000)
  

DTM_a_2020 <- subsetDTM(dat = docs_a, years = 2020)
# counts <- t(DTM_a_2020) %*% DTM_a_2020
coocGraph_a_2020 <- 
  DTM_a_2020 %>%
  coocGraph3Tier(dat = ., coocTerm = "conservation", sd_multiplier = 2) %>%
  distinct()


conductivity_a_2020 <- data.frame(conductivity = estimate_betweenness(graph.data.frame(coocGraph_a_2020, directed = F), cutoff = 2),
                                  degree = degree(graph.data.frame(coocGraph_a_2020, directed = F), mode = "total")) %>%
  mutate(node = rownames(.))


nodeFreq_a_2020 <- data.frame(node = colnames(DTM_a_2020),
                              freq = diag(t(DTM_a_2020) %*% DTM_a_2020)) %>%
  mutate(node = stringr::str_replace_all(node, "_", " "))


node_attributes_a_2020 <- 
  coocGraph_a_2020 %>%
  # mutate(link1 = paste(from, to, sep = " -"),
  #        link2 = paste(to, from, sep = " -")) %>%
  # pivot_longer(cols = c(link1, link2), values_to = "link") %>%
  # select(link, sig, coocFreq) %>%
  distinct() %>%
  # mutate(node = stringr::str_extract(link, ".*.(?=\\s-)")) %>%
  left_join(nodeFreq_a_2020, by = c("from" = "node")) %>%
  left_join(nodeFreq_a_2020, by = c("to" = "node")) %>%
  mutate(lower.freq = ifelse(freq.x<freq.y, freq.x, freq.y)) %>%
  mutate(consensus = ifelse(coocFreq/lower.freq>=0.33, 1, 0)) %>% # if co-occurrence exists at least 10% of the time the focal node is used, counts as "consensus"
  rename("node" = "from") %>%
  group_by(node) %>%
  summarise(consensus = sum(consensus) / length(node),
            density = length(node)) %>%
  left_join(nodeFreq_a_2020, by = "node") %>%
  left_join(conductivity_a_2020, by = "node") %>%
  filter(conductivity!=0)

ggplot(node_attributes_a_2020) + geom_histogram(aes(consensus))

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
  filter(consensus<=as.numeric(quantiles_a_2020$consensus[quantiles_a_2020$quantile=="30%"]) & 
           degree>=as.numeric(quantiles_a_2020$degree[quantiles_a_2020$quantile=="70%"]) & 
           conductivity>=as.numeric(quantiles_a_2020$conductivity[quantiles_a_2020$quantile=="70%"]))

# placeholders_a_2020 <-
#   node_attributes_a_2020 %>%
#   filter(consensus<=mean(node_attributes_a_2020$consensus)-0.5*sd(node_attributes_a_2020$consensus) &
#            degree>=mean(node_attributes_a_2020$degree)+0.5*sd(node_attributes_a_2020$degree) &
#            conductivity>=mean(node_attributes_a_2020$conductivity)+0.5*sd(node_attributes_a_2020$conductivity))

placeholders_a_2020 <-
  node_attributes_a_2020 %>%
  filter(consensus<median(node_attributes_a_2020$consensus)-0.25*sd(node_attributes_a_2020$consensus) &
           degree>median(node_attributes_a_2020$degree)+0.25*sd(node_attributes_a_2020$degree) &
           conductivity>median(node_attributes_a_2020$conductivity)+0.25*sd(node_attributes_a_2020$conductivity))

buzzwords_a_2020 <-
  node_attributes_a_2020 %>%
  filter(consensus<median(node_attributes_a_2020$consensus) &
           degree<median(node_attributes_a_2020$degree) &
           conductivity>median(node_attributes_a_2020$conductivity))

ordinary_a_2020 <- 
  node_attributes_a_2020 %>%
  filter(consensus<median(node_attributes_a_2020$consensus) &
           degree<median(node_attributes_a_2020$degree) &
           conductivity<median(node_attributes_a_2020$conductivity))

standard_a_2020 <- 
  node_attributes_a_2020 %>%
  filter(consensus>median(node_attributes_a_2020$consensus) &
           degree>median(node_attributes_a_2020$degree) &
           conductivity>median(node_attributes_a_2020$conductivity))

buzzwords_a_2020 <- 
  node_attributes_a_2020 %>%
  filter(consensus<=as.numeric(quantiles_a_2020$consensus[quantiles_a_2020$quantile=="30%"]) & 
           degree<=as.numeric(quantiles_a_2020$degree[quantiles_a_2020$quantile=="30%"]) & 
           conductivity>=as.numeric(quantiles_a_2020$conductivity[quantiles_a_2020$quantile=="70%"]))

standard_a_2020 <- 
  node_attributes_a_2020 %>%
  filter(consensus>=as.numeric(quantiles_a_2020$consensus[quantiles_a_2020$quantile=="80%"]) & 
           degree>=as.numeric(quantiles_a_2020$degree[quantiles_a_2020$quantile=="80%"]) & 
           conductivity>=as.numeric(quantiles_a_2020$conductivity[quantiles_a_2020$quantile=="80%"]))


# ---- Parallel computing for creating co-occurrence graphs per year ----

library(parallel)
detectCores()

# Identify years to run
years <- 2000:2021

# Create DTM and 
DTM_byyear <- mclapply(years, function(i) {
    
    subsetDTM(dat = docs_a, years = i)
  
    }, 
  mc.cores = 22)


for(i in 1:length(DTM_byyear)) {
  assign(paste("DTM_a_", years[i], sep = ""), 
         DTM_byyear[[i]])
}


coocGraph_byyear <- mclapply(years, function(i) {
  
  coocGraph3Tier(dat = get(paste("DTM_a_", i, sep = "")), 
                        coocTerm = "conservation", 
                        sd_multiplier = 2)
  },
  mc.cores = 22)


for(i in 1:length(coocGraph_byyear)) {
  assign(paste("coocGraph_a_", years[i], sep = ""), 
         coocGraph_byyear[[i]])
}

# years_output <- c(2000, 2001, 2003:2014, 2016:2021)
# 
# for(i in 1:length(years_output)) {
#   export(get(paste("coocGraph_a_", years_output[i], sep = "")),
#          paste("data/outputs/coocGraphs/coocGraph_a_", years_output[i], ".csv", sep = ""))
# }


# ---- Create outputs of node attributes per year ----

conductivity_a_2020 <- data.frame(conductivity = estimate_betweenness(graph.data.frame(coocGraph_a_2020, directed = F), cutoff = 2),
                                  degree = degree(graph.data.frame(coocGraph_a_2020, directed = F), mode = "total")) %>%
  mutate(node = rownames(.))


nodeFreq_a_2020 <- data.frame(node = colnames(DTM_a_2020),
                              freq = diag(t(DTM_a_2020) %*% DTM_a_2020)) %>%
  mutate(node = stringr::str_replace_all(node, "_", " "))


node_attributes_a_2020 <- 
  coocGraph_a_2020 %>%
  # mutate(link1 = paste(from, to, sep = " -"),
  #        link2 = paste(to, from, sep = " -")) %>%
  # pivot_longer(cols = c(link1, link2), values_to = "link") %>%
  # select(link, sig, coocFreq) %>%
  distinct() %>%
  # mutate(node = stringr::str_extract(link, ".*.(?=\\s-)")) %>%
  left_join(nodeFreq_a_2020, by = c("from" = "node")) %>%
  left_join(nodeFreq_a_2020, by = c("to" = "node")) %>%
  mutate(lower.freq = ifelse(freq.x<freq.y, freq.x, freq.y)) %>%
  mutate(consensus = ifelse(coocFreq/lower.freq>=0.33, 1, 0)) %>% # if co-occurrence exists at least 10% of the time the focal node is used, counts as "consensus"
  rename("node" = "from") %>%
  group_by(node) %>%
  summarise(consensus = sum(consensus) / length(node),
            density = length(node)) %>%
  left_join(nodeFreq_a_2020, by = "node") %>%
  left_join(conductivity_a_2020, by = "node")
  # filter(conductivity!=0)

placeholders_a_2020 <-
  node_attributes_a_2020 %>%
  filter(consensus<median(node_attributes_a_2020$consensus)-0.25*sd(node_attributes_a_2020$consensus) &
           degree>median(node_attributes_a_2020$degree)+0.25*sd(node_attributes_a_2020$degree) &
           conductivity>median(node_attributes_a_2020$conductivity)+0.25*sd(node_attributes_a_2020$conductivity))

buzzwords_a_2020 <-
  node_attributes_a_2020 %>%
  filter(consensus<median(node_attributes_a_2020$consensus) &
           degree<median(node_attributes_a_2020$degree) &
           conductivity>median(node_attributes_a_2020$conductivity))

sort(rownames(t(DTM_a_2004) %*% DTM_a_2004))

# ---- Compare coocGraphs across years ----

coocGraph_compare <- data.frame(focal_node = character(), 
                                n_nodes = numeric(0),
                                n_placeholders = numeric(0),
                                n_buzzwords = numeric(0),
                                avg_conductivity = numeric(0),
                                avg_degree = numeric(0),
                                clustering_coeff = numeric(0),
                                placeholders_1_10 = character(),
                                buzzwords_1_10 = character())


for(i in years_output){
  
  
  DTM <- get(paste("DTM_a_", i, sep = ""))
  coocGraph <- get(paste("coocGraph_a_", i, sep = ""))
  
  conductivity <- data.frame(conductivity = betweenness(graph.data.frame(coocGraph, directed = F)),
                                    degree = degree(graph.data.frame(coocGraph, directed = F))) %>%
    mutate(node = rownames(.))
  
  
  nodeFreq <- data.frame(node = colnames(DTM),
                                freq = diag(t(DTM) %*% DTM)) %>%
    mutate(node = stringr::str_replace_all(node, "_", " "))
  
  
  node_attributes <- 
    coocGraph %>%
    distinct() %>%
    # mutate(node = stringr::str_extract(link, ".*.(?=\\s-)")) %>%
    left_join(nodeFreq, by = c("from" = "node")) %>%
    left_join(nodeFreq, by = c("to" = "node")) %>%
    mutate(lower.freq = ifelse(freq.x<freq.y, freq.x, freq.y)) %>%
    mutate(consensus = ifelse(coocFreq/lower.freq>=0.33, 1, 0)) %>% # if co-occurrence exists at least 10% of the time the focal node is used, counts as "consensus"
    rename("node" = "from") %>%
    group_by(node) %>%
    summarise(consensus = sum(consensus) / length(node),
              density = length(node)) %>%
    left_join(nodeFreq, by = "node") %>%
    left_join(conductivity, by = "node")
  

  quantiles <- as.data.frame(cbind(quantile = paste0(seq(0, 100, by = 5), "%", sep = ""),
                                          consensus = as.numeric(quantile(node_attributes$consensus, 
                                                                          seq(0, 1, by = 0.05))),
                                          degree = as.numeric(quantile(node_attributes$degree, 
                                                                       seq(0, 1, by = 0.05))),
                                          conductivity = as.numeric(quantile(node_attributes$conductivity, 
                                                                             seq(0, 1, by = 0.05)))))
  
  placeholders <- 
    node_attributes %>%
    filter(consensus<=as.numeric(quantiles$consensus[quantiles$quantile=="30%"]) & 
             degree>=as.numeric(quantiles$degree[quantiles$quantile=="70%"]) & 
             conductivity>=as.numeric(quantiles$conductivity[quantiles$quantile=="70%"]))
  
  buzzwords <- 
    node_attributes %>%
    filter(consensus<=as.numeric(quantiles$consensus[quantiles$quantile=="30%"]) & 
             degree<=as.numeric(quantiles$degree[quantiles$quantile=="30%"]) & 
             conductivity>=as.numeric(quantiles$conductivity[quantiles$quantile=="70%"]))
  
  assign(paste("placeholders_a_", i, sep = ""), placeholders)
  assign(paste("buzzwords_a_", i, sep = ""), buzzwords)
  
  output_tocompare <- 
    data.frame(focal_node = "conservation", 
               n_nodes = length(node_attributes$node),
               n_placeholders = length(placeholders$node),
               n_buzzwords = length(buzzwords$node),
               avg_conductivity = mean(node_attributes$conductivity),
               avg_degree = mean(node_attributes$degree),
               clustering_coeff = transitivity(graph.data.frame(coocGraph, directed = F)),
               placeholders_1_10 = paste(placeholders$node[1], placeholders$node[2],
                                          placeholders$node[3], placeholders$node[4],
                                          placeholders$node[5], placeholders$node[6],
                                          placeholders$node[7], placeholders$node[8],
                                          placeholders$node[9], placeholders$node[10],
                                          sep = ", "),
               buzzwords_1_10 = paste(buzzwords$node[1], buzzwords$node[2],
                                      buzzwords$node[3], buzzwords$node[4],
                                      buzzwords$node[5], buzzwords$node[6],
                                      buzzwords$node[7], buzzwords$node[8],
                                      buzzwords$node[9], buzzwords$node[10],
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
