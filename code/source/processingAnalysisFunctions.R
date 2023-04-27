#
# code: Define processing and analysis functions
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: November 2022
# 
# 

# ---- Subsetting / tokenizing / document feature matrix function ----

subsetDTM <- function(dat, years, stopword_list) {
  
  # filter data & turn into corpus
  dat <- dat %>% filter(year%in%years)
  corp <- corpus(dat)
  
  # tokenize into "bag of words" per document
  tokens <- corp %>%
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
    tokens_tolower() %>% 
    tokens_remove(pattern = stopword_list, padding = T) %>%
    tokens_replace(thesaurus$token, thesaurus$lemma, valuetype = "fixed")
    
  
  # identify candidate collocations
  collocations <- textstat_collocations(tokens, min_count = 5)
  
  if(length(collocations)>500) {
  collocations <- collocations[1:500, ]
  }
  
  tokens <- tokens_compound(tokens, collocations)
  
  
  # create DTM, prune vocabulary and set binary values for presence/absence of types
  binDTM <- tokens %>% 
    tokens_remove("") %>%
    dfm() %>% 
    dfm_trim(min_docfreq = 0.002, max_docfreq = 1, docfreq_type = "prop") %>%  # only include tokens that exist across at least 0.2% of documents
    dfm_weight("boolean")
  
  return(binDTM)
  
}


# ---- Co-occurrence graph function ----

# "Three-tier" network development procedure, finding significant co-occurrences using DICE coefficient threshold 
# based on the average DICE coefficient for all "conservation" co-occurrences for a given corpus.

coocGraph3Tier <- function(dat, coocTerm, sd_multiplier, input_suffix) {
  
  # Define parameters for the central co-occurrence term of interest & number of co-occurrences to include in analysis
  coocTerm <- coocTerm
  counts <- t(dat) %*% dat
  
  # Identify the significance value a number of std deviations above the average DICE coefficient 
  # for all co-occurrences associated with the central coocTerm ("conservation") across the full corpus
  get_sigval <- get(paste("conservation_coocs_", input_suffix, sep = ""))
  sigval <- mean(get_sigval$sig) + sd_multiplier*sd(get_sigval$sig)
  
  
  # Calculate statistics for coocTerm, keeping all terms that are significant at a pre-specified significance value
  coocs <- data.frame(sig = calculateCoocStatistics(coocTerm, dat, measure = "DICE")) %>%
    mutate(word = rownames(.)) 
  
  
  
  coocs <- coocs %>%
    filter(sig>=sigval)
  
  
  # Create dummy data frame for results 
  resultGraph <- data.frame(from = character(), 
                            to = character(), 
                            sig = numeric(0),
                            cooc = character())
  
  # Create running list of already-calculated search terms (to avoid re-running them in third tier)
  alreadyCalculated <- c(coocTerm)
  
  # Structure of the temporary graph object is equal to that of the resultGraph
  tmpGraph <- data.frame(from = coocTerm, 
                         to = coocs$word, 
                         sig = coocs$sig) %>%
    mutate(cooc1 = paste(from, to, sep = "-"), 
           cooc2 = paste(to, from, sep = "-")) %>%
    pivot_longer(cols = c(cooc1, cooc2), values_to = "cooc") %>%
    group_by(from, to, sig) %>%
    summarise(cooc = sort(cooc)[1]) %>% 
    ungroup()
  
  
  # attach the triples to resultGraph
  resultGraph <- rbind.data.frame(resultGraph, tmpGraph)
  
  
  # iterate over the most significant co-occurrences of the search term
  for (i in 1:length(coocs$word)){
    
    
    # calling up the co-occurrence calculation for term i from the search words co-occurrences
    newCoocTerm <- coocs$word[i]
    
    # append new term to running list of already-calculated search terms
    alreadyCalculated <- append(newCoocTerm, alreadyCalculated)
    
    coocs2 <- data.frame(sig = calculateCoocStatistics(newCoocTerm, dat, measure = "DICE")) %>%
      mutate(word = rownames(.)) %>%
      filter(sig>=sigval)
    
    # print the co-occurrences
    print(toupper(newCoocTerm))
    
    
    # structure of the temporary graph object
    tmpGraph <- data.frame(from = newCoocTerm, 
                           to = coocs2$word, 
                           sig = coocs2$sig) %>%
      mutate(cooc1 = paste(from, to, sep = "-"), 
             cooc2 = paste(to, from, sep = "-")) %>%
      pivot_longer(cols = c(cooc1, cooc2), values_to = "cooc") %>%
      group_by(from, to, sig) %>%
      summarise(cooc = sort(cooc)[1]) %>%
      ungroup()
    
    
    # append the result to the result graph
    resultGraph <- rbind.data.frame(resultGraph, tmpGraph)
    
    for (j in 1:length(coocs2$word)) {
      
      # calling up the co-occurrence calculation for term j from the search words co-occurrences
      newCoocTerm2 <- coocs2$word[j]
      
      if(!newCoocTerm2%in%alreadyCalculated){
        
        coocs3 <- data.frame(sig = calculateCoocStatistics(newCoocTerm2, dat, measure = "DICE")) %>%
          mutate(word = rownames(.)) %>%
          filter(sig>=sigval)
        
        # append new term to running list of already-calculated search terms
        alreadyCalculated <- append(newCoocTerm2, alreadyCalculated)
        
        # print the co-occurrences
        print(paste("Tier 2: ", coocs$word[i], " (", i, " of ", length(coocs$word), ")", "; ", 
                    coocs2$word[j], " (", j, " of ", length(coocs2$word), ")", "; ", length(coocs3$word), " co-occurrences",
                    sep = ""))
        
        
        # structure of the temporary graph object
        tmpGraph2 <- data.frame(from = newCoocTerm2, 
                                to = coocs3$word, 
                                sig = coocs3$sig) %>%
          mutate(cooc1 = paste(from, to, sep = "-"), 
                 cooc2 = paste(to, from, sep = "-")) %>%
          pivot_longer(cols = c(cooc1, cooc2), values_to = "cooc") %>%
          group_by(from, to, sig) %>%
          summarise(cooc = sort(cooc)[1]) %>%
          ungroup()
        
        # append the result to the result graph
        resultGraph <- rbind.data.frame(resultGraph, tmpGraph2)
      }
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
           to = stringr::str_replace_all(to, "_", " "),
           sd_multiplier = sd_multiplier)
  
  return(resultGraph)
  
}


# ---- Function to run subsetting and co-occurrence graph functions ----

# Uses parallel processing to produce co-occurrence graphs per year, per corpus.
# Exports to .csv so that this function only needs to be run once per corpus.

coocGraphsPerYear <- function(input_data, input_suffix, sd_multiplier, years, coocTerm, stopword_list, n_cores = 1) {
  
  assign(paste("DTM_", input_suffix, sep = ""),
         subsetDTM(dat = input_data, years = years, stopword_list = stopword_list))
  
  # define the co-occurrences with "conservation", to be used to calculate significance value threshold across the corpus
  assign(paste("conservation_coocs_", input_suffix, sep = ""),
         data.frame(sig = calculateCoocStatistics("conservation", 
                                                  get(paste("DTM_", input_suffix, sep = "")), 
                                                  measure = "DICE")) %>%
           mutate(word = rownames(.)),
         envir = .GlobalEnv)
  
  # subset DTMs by year
  DTM_byyear <- mclapply(years, function(i) {
    
    subsetDTM(dat = input_data, years = i, stopword_list = stopword_list)
    
  },  
  
  mc.cores = length(years))

  
  for(i in 1:length(DTM_byyear)) {
    assign(paste("DTM", input_suffix, years[i], sep = "_"), 
           DTM_byyear[[i]])
  }
  
  # Export document term matrices, to source in next time
  
  # Create output directories as needed (and for today's date)
  dir.create("data/outputs/DTMs/")
  dir.create(paste("data/outputs/DTMs/", format(Sys.Date(),"%Y%m%d"), sep = ""))
  output_dir <- paste("data/outputs/DTMs/", format(Sys.Date(),"%Y%m%d"), sep = "")
  
  for(i in 1:length(years)) {
    saveRDS(get(paste("DTM", input_suffix, years[i], sep = "_")),
            paste(output_dir, "/DTM_", input_suffix, "_", years[i], ".rds", sep = ""))
  }
  
  # Create co-occurrence graph (COMPUTATIONALLY INTENSIVE)
  coocGraph_byyear <- mclapply(years, function(i) {
    
    coocGraph3Tier(dat = get(paste("DTM", input_suffix, i, sep = "_")), 
                   coocTerm = coocTerm, 
                   sd_multiplier = sd_multiplier,
                   input_suffix = input_suffix)
  },
  mc.cores = n_cores)
  
  
  for(i in 1:length(coocGraph_byyear)) {
    assign(paste("coocGraph", input_suffix, years[i], sep = "_"), 
           coocGraph_byyear[[i]])
  }
  
  # Export co-occurrence graphs, to source in next time
  
  # Create output directories as needed (and for today's date)
  dir.create("data/outputs/coocGraphs/")
  dir.create(paste("data/outputs/coocGraphs/", format(Sys.Date(),"%Y%m%d"), sep = ""))
  output_dir <- paste("data/outputs/coocGraphs/", format(Sys.Date(),"%Y%m%d"), sep = "")
  
  for(i in 1:length(years)) {
    write.csv(get(paste("coocGraph", input_suffix, years[i], sep = "_")),
              paste(output_dir, "/coocGraph_", input_suffix, "_", years[i], ".csv", sep = ""),
              row.names = F)
  }
  
  
}


# ---- Function to source in pre-exported DTMs and coocGraphs and calculate basic network measures ----

findNodeAttributes <- function(input_suffix, years, consensus_thresholds, percentile_thresholds, coocTerm) {
  
  # find most recent folders to import data from
  most_recent_DTM_folder <- last(list.files("data/outputs/DTMs"))
  most_recent_coocGraph_folder <- last(list.files("data/outputs/coocGraphs"))
  
  # import DTM and coocGraph data
  for(i in 1:length(years)) {
    assign(paste("DTM", input_suffix, years[i], sep = "_"),
           readRDS(paste("data/outputs/DTMs/", most_recent_DTM_folder, "/DTM_", input_suffix, "_", years[i], ".rds", sep = "")))
    
    assign(paste("coocGraph", input_suffix, years[i], sep = "_"),
           read_csv(paste("data/outputs/coocGraphs/", most_recent_coocGraph_folder, "/coocGraph_", input_suffix, "_", years[i], ".csv", sep = ""),
                    locale = readr::locale(encoding = "UTF-8"))
    )
  }
  
  
  # create empty object to hold all quantiles from network measures across years/thresholds
  quantiles_allthresholds <- NULL
  
  
  # --- Compare network metrics across years
  
  for(i in years){
    
    DTM <- get(paste("DTM", "_", input_suffix, "_", i, sep = ""))
    coocGraph <- get(paste("coocGraph", "_", input_suffix, "_", i, sep = ""))
    docs <- get(paste("docs_", input_suffix, sep = ""))
    
    corpus_name <- ifelse(input_suffix=="a", "academic", 
                          ifelse(input_suffix=="n", "ngo", 
                                 ifelse(input_suffix=="m", "media", 
                                        ifelse(input_suffix=="p", "policy", NA))))
  
    # get nodes and links per year i
    graph_attr <- coocGraph %>%
      summarise(links = length(from)) %>%
      cbind.data.frame(nodes = length(unique(coocGraph$from)))
    
    # create random network to use as threshold basis for degree measure
    # NOTE: size the random network based on the number of nodes and links of year i's coocGraph
    assign("rand_graph", 
           sample_gnm(graph_attr$nodes, 
                      graph_attr$links), envir = .GlobalEnv)
    
    rand_graph_betweenness <- rand_graph %>% betweenness(.)
    rand_graph_degree <- rand_graph %>% degree(.)
    
  
    
    conductivity <- data.frame(conductivity = betweenness(graph.data.frame(coocGraph, directed = F)),
                               degree = degree(graph.data.frame(coocGraph, directed = F))) %>%
      mutate(node = rownames(.))
    
    
    nodeFreq <- data.frame(node = colnames(DTM),
                           freq = diag(t(DTM) %*% DTM)) %>%
      mutate(node = stringr::str_replace_all(node, "_", " "),
             year = i)
    
    # define empty object for all consensus & percentile thresholds to bind to for each year i
    node_attributes_allthresholds <- NULL
    
    
    # --- Define node attributes for each consensus threshold j, for year i
    
    for(j in consensus_thresholds){
      
      node_attributes <- 
        coocGraph %>%
        distinct() %>%
        left_join(nodeFreq, by = c("from" = "node")) %>%
        left_join(nodeFreq, by = c("to" = "node")) %>%
        mutate(lower.freq = ifelse(freq.x<freq.y, freq.x, freq.y)) %>%
        mutate(consensus = ifelse(coocFreq/lower.freq>=j, 1, 0)) %>% # if co-occurrence exists at least XX% of the time the less frequent of the two nodes is used, counts as "consensus"
        rename("node" = "from") %>%
        group_by(node, sd_multiplier) %>%
        summarise(consensus = sum(consensus) / length(node),
                  num_links = length(node),
                  consensus_threshold = j) %>%
        left_join(nodeFreq, by = "node") %>%
        left_join(conductivity, by = "node")
      
      
      quantiles <- as.data.frame(cbind(quantile = seq(0, 1, by = 0.05),
                                       consensus = as.numeric(quantile(node_attributes$consensus, 
                                                                       seq(0, 1, by = 0.05))),
                                       degree = mean(rand_graph_degree),
                                       conductivity = as.numeric(quantile(node_attributes$conductivity, 
                                                                          seq(0, 1, by = 0.05))),
                                       year = i,
                                       consensus_threshold = j))
      
      
      # --- Define symbol type for each percentile threshold k (for low or high network measures)

      for(k in percentile_thresholds) {
        
        low_consensus <- as.numeric(quantiles %>% filter(quantile==as.character(k)) %>% select(consensus))
        high_consensus <- as.numeric(quantiles %>% filter(quantile==as.character(1-k)) %>% select(consensus))
        
        mean_rand_degree <- mean(rand_graph_degree)

        low_conductivity <- as.numeric(quantiles %>% filter(quantile==as.character(k)) %>% select(conductivity))
        high_conductivity <- as.numeric(quantiles %>% filter(quantile==as.character(1-k)) %>% select(conductivity))

        node_attributes_percentile <-
          node_attributes %>%
          mutate(percentile_threshold = k,
                 consensus_score = ifelse(consensus<=low_consensus, "low", ifelse(consensus>high_consensus, "high", "neither")),
                 degree_score = ifelse(degree<mean_rand_degree, "low", "high"),
                 conductivity_score = ifelse(conductivity<low_conductivity, "low", ifelse(conductivity>high_conductivity, "high", "neither")),
                 symbol_type = case_when(consensus_score=="low" & degree_score=="low" & conductivity_score=="low" ~ "ordinary",
                                         consensus_score=="high" & degree_score=="low" & conductivity_score=="low" ~ "factoid",
                                         consensus_score=="low" & degree_score=="high" & conductivity_score=="low" ~ "allusion",
                                         consensus_score=="low" & degree_score=="low" & conductivity_score=="high" ~ "buzzword",
                                         consensus_score=="high" & degree_score=="high" & conductivity_score=="low" ~ "stereotype",
                                         consensus_score=="high" & degree_score=="low" & conductivity_score=="high" ~ "emblem",
                                         consensus_score=="low" & degree_score=="high" & conductivity_score=="high" ~ "placeholder",
                                         consensus_score=="high" & degree_score=="high" & conductivity_score=="high" ~ "standard"))

        node_attributes_allthresholds <- rbind(node_attributes_percentile, node_attributes_allthresholds)
        quantiles_allthresholds <- rbind(quantiles, quantiles_allthresholds)

      }
    }

    assign(paste0("node_attributes_", input_suffix, "_", i, sep = ""), node_attributes_allthresholds, envir = .GlobalEnv)
    assign(paste("node_freq_", input_suffix, "_", i, sep = ""), nodeFreq, envir = .GlobalEnv)
      
    # get graph attributes per year for summary stats
    assign(paste("graph_attr_", input_suffix, "_", i, sep = ""),
           coocGraph %>%
             summarise(links = length(from)) %>%
             cbind.data.frame(nodes = length(unique(coocGraph$from)),
                              ndoc = length(docs$text[docs$year==i]),
                              nwords_avg = round(mean(ntoken(DTM))), #NOTE: these are number of words after stopwords removed
                              nwords_sd = sd(ntoken(DTM)),
                              nwords_min = min(ntoken(DTM)),
                              nwords_max = max(ntoken(DTM)),
                              year = i,
                              corpus = corpus_name),
           envir = .GlobalEnv)

  }

  # Put all node attributes & frequencies across years into single data frames
  assign(paste("node_attributes_", input_suffix, sep = ""),
         do.call(rbind, lapply(paste0("node_attributes_", input_suffix, "_", years, sep = ""), get) ),
         envir = .GlobalEnv)
  
  assign(paste("node_freq_", input_suffix, sep = ""),
         do.call(rbind, lapply(paste0("node_freq_", input_suffix, "_", years, sep = ""), get) ),
         envir = .GlobalEnv)

  # Put all graph attributes across years into a single data frame
  assign(paste("graph_attr_", input_suffix, sep = ""),
         do.call(rbind, lapply(paste0("graph_attr_", input_suffix, "_", years, sep = ""), get) ),
         envir = .GlobalEnv)

  # Assign quantiles to an object in global environment
  assign(paste("quantiles_", input_suffix, "_allthresholds", sep = ""),
         get("quantiles_allthresholds", inherits = T),
         envir = .GlobalEnv)

  # Export to easily pull in for next time?
  write.csv(get(paste("node_attributes_", input_suffix, sep = "")),
            paste("data/outputs/node_attributes_", input_suffix, ".csv", sep = ""),
            row.names = F)
  
  write.csv(get(paste("node_freq_", input_suffix, sep = "")),
            paste("data/outputs/node_freq_", input_suffix, ".csv", sep = ""),
            row.names = F)
  
  write.csv(get(paste("graph_attr_", input_suffix, sep = "")),
            paste("data/outputs/graph_attr_", input_suffix, ".csv", sep = ""),
            row.names = F)

  # Define clustering coefficients for each year's co-occurrence network
  clustering_coeffs <-
    data.frame(coeff = mapply(i = paste0("coocGraph", "_", input_suffix, "_", years),
                              function(i) {transitivity(graph.data.frame(get(i), directed = F))})) %>%
    mutate(year = as.numeric(substr(rownames(.), 13, 16)))

  # Quick comparison across years
  assign(paste("years_compare_", input_suffix, sep = ""),
         get(paste("node_attributes_", input_suffix, sep = "")) %>%
           group_by(year, consensus_threshold, percentile_threshold) %>%
           summarise(focal_node = coocTerm,
                     n_nodes = length(node),
                     avg_conductivity = mean(conductivity),
                     avg_degree = mean(degree),
                     avg_consensus = mean(consensus, na.rm = T),
                     sd_consensus = sd(consensus, na.rm = T),
                     n_consensus_na = length(consensus[is.na(consensus)]),
                     n_placeholders = length(node[symbol_type=="placeholder" & !is.na(symbol_type)]),
                     n_buzzwords = length(node[symbol_type=="buzzword"  & !is.na(symbol_type)]),
                     n_standard = length(node[symbol_type=="standard"  & !is.na(symbol_type)]),
                     n_ordinary = length(node[symbol_type=="ordinary"  & !is.na(symbol_type)]),
                     n_emblem = length(node[symbol_type=="emblem"  & !is.na(symbol_type)]),
                     n_allusion = length(node[symbol_type=="allusion"  & !is.na(symbol_type)]),
                     n_factoid = length(node[symbol_type=="factoid"  & !is.na(symbol_type)]),
                     n_stereotype = length(node[symbol_type=="stereotype"  & !is.na(symbol_type)]),
                     n_nosymbol = length(node[is.na(symbol_type)]),
                     placeholders_1_10 = list(node[symbol_type=="placeholder"  & !is.na(symbol_type)][1:10]),
                     buzzwords_1_10 = list(node[symbol_type=="buzzword"  & !is.na(symbol_type)][1:10]),
                     standard_1_10 = list(node[symbol_type=="standard"  & !is.na(symbol_type)][1:10])) %>%
           left_join(clustering_coeffs, by = "year"),
         envir = .GlobalEnv)

    }

