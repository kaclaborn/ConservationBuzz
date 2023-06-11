#
# code: Focal words analysis
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: March 2023
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE LIBRARIES, FUNCTIONS, DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 1.1 Source libraries, functions ----

pacman::p_load(spam, spam64, ggplot2, grid, gridExtra)

options(spam.force64 = TRUE)


source("code/source/plotThemes.R")


# ---- 1.2 Import node attribute data ----

node_attributes_a <- read_csv("data/outputs/node_attributes_a.csv")
node_attributes_n <- read_csv("data/outputs/node_attributes_n.csv")
node_attributes_m_nyt_filt <- read_csv("data/outputs/node_attributes_m_nyt_filt.csv")
node_attributes_p <- read_csv("data/outputs/node_attributes_p.csv")

node_freq_a <- read_csv("data/outputs/node_freq_a.csv")
node_freq_n <- read_csv("data/outputs/node_freq_n.csv")
node_freq_m_nyt_filt <- read_csv("data/outputs/node_freq_m_nyt_filt.csv")
node_freq_p <- read_csv("data/outputs/node_freq_p.csv")

graph_attr_a <- read_csv("data/outputs/graph_attr_a.csv")
graph_attr_n <- read_csv("data/outputs/graph_attr_n.csv")
graph_attr_m_nyt_filt <- read_csv("data/outputs/graph_attr_m_nyt_filt.csv")
graph_attr_p <- read_csv("data/outputs/graph_attr_p.csv")


# ---- Identify focal words to explore ----

# focal_words <- c("biodiversity", "sustainable", "partnership", 
#                  "action", "landscape", "ecosystem service", 
#                  "climate change", "solution", "livelihood", 
#                  "restoration", "future", "global", "local",
#                  "wildlife", "sustainability", "nature-based solution",
#                  "contribution", "nature contribution", "governance",
#                  "local community")

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: WRANGLE FOCAL WORDS DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 Join word count and node attribute data for each corpus ----

words_a <- 
  node_freq_a %>% 
  left_join(graph_attr_a, by = "year") %>%
  left_join(node_attributes_a %>% select(-freq), by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs)

words_n <- 
  node_freq_n %>% 
  left_join(graph_attr_n, by = "year") %>%
  left_join(node_attributes_n %>% select(-freq), by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs)

words_m <- 
  node_freq_m_nyt_filt %>% 
  left_join(graph_attr_m_nyt_filt, by = "year") %>%
  left_join(node_attributes_m_nyt_filt %>% select(-freq), by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs)

words_p <- 
  node_freq_p %>% 
  left_join(graph_attr_p, by = "year") %>%
  left_join(node_attributes_p %>% select(-freq), by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs)


# ---- 2.2 Define focal word attributes table ----

focalword_attributes <- 
  words_a %>% filter(consensus_threshold==0.25 | is.na(consensus_threshold)) %>%
  bind_rows(words_n %>% filter(consensus_threshold==0.5 | is.na(consensus_threshold))) %>%
  bind_rows(words_m %>% filter(consensus_threshold==0.75 | is.na(consensus_threshold)) %>% mutate(corpus = "media")) %>%
  bind_rows(words_p %>% filter(consensus_threshold==0.75 | is.na(consensus_threshold))) %>%
  filter(percentile_threshold==0.5 | is.na(percentile_threshold)) %>% 
  select(node, year, corpus, freq, rel_freq, total_docs, consensus, conductivity, degree, symbol_type, 
         sd_multiplier, percentile_threshold, consensus_threshold) %>%
  mutate(shapetype = case_when(symbol_type%in%c("buzzword", "placeholder") ~ "buzzword/placeholder", 
                             symbol_type=="standard" ~ "standard",
                             symbol_type=="ordinary" ~ "ordinary",
                             symbol_type%in%c("emblem", "stereotype") ~ "emblem/stereotype",
                             symbol_type%in%c("allusion", "factoid") ~ "allusion/factoid",
                             TRUE ~ "not classified"),
         shapetype = factor(shapetype, levels = c("buzzword/placeholder", "standard", 
                                                  "emblem/stereotype", "allusion/factoid",
                                                  "ordinary", "not classified"),
                            ordered = T),
         shapesize = case_when(shapetype=="buzzword/placeholder" ~ "big",
                               TRUE ~ "small"),
         corpus = factor(corpus, levels = unique(corpus)))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: DEFINE PLOTTING FUNCTIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Plot word trajectories over time, by corpus ----

PlotTrajectories_compareCorpora <- function(focal_word, data, upper_limit = 0.5) {
  
  title <- paste('"', str_to_title(focal_word), '"', sep = "")
  break_points <- ifelse(upper_limit > 0.5, 0.1, 0.05)
  
  data <- data %>% filter(year%in%c(2017:2022))
  
  plot <- 
    ggplot() +
    geom_line(data = data %>% filter(node==focal_word & corpus!="policy"),
              aes(x = year, y = rel_freq, color = corpus)) +
    geom_point(data = data %>% filter(node==focal_word),
               aes(x = year, y = rel_freq, color = corpus, shape = shapetype, size = shapesize)) +
    geom_text(data = data %>% 
                filter(node==focal_word & corpus=="policy") %>% 
                mutate(label = ifelse(year==2019, "IPBES", "UNCBD")),
              aes(x = year, y = rel_freq + 0.03, label = label),
              size = 2.5) +
    scale_shape_manual(name = "",
                       values = c("buzzword/placeholder" = 16, "standard" = 0, "emblem/stereotype" = 2,
                                  "allusion/factoid" = 6, "ordinary" = 5, "not classified" = 4),
                       drop = F) +
    scale_size_manual(values = c("big" = 3, "small" = 2),
                      guide = "none") +
    scale_color_manual(name = "",
                       values = fillcols.4categories,
                       drop = F) + 
    scale_x_continuous(expand = c(0, 0),
                       limits = c(2016.5, 2022.5),
                       breaks = seq(2017, 2022, by = 1)) +
    scale_y_continuous(expand = c(0, 0),
                       labels = scales::percent_format(accuracy = 1),
                       limits = c(0, upper_limit), 
                       breaks = seq(0, upper_limit, by = break_points)) +
    labs(x = "", y = "Relative document frequency\n",
         title = title,
         subtitle = "Trajectory of use across institutions") +
    time.plot.theme
  
  return(plot)
}

PlotTrajectories_compareWords <- function(focal_corpus, words, data, years = c(2017:2021),
                                          upper_limit = 0.5, points_on = T) {
  
  title <- paste(str_to_title(focal_corpus), "Corpus", sep = " ")
  break_points <- ifelse(upper_limit > 0.5, 0.1, 0.05)
  break_points_x <- ifelse(length(years)>14, 3, 1)
  
  data <- data %>% filter(corpus==focal_corpus & node%in%words & year%in%years)
  
  plot <- 
    ggplot(data = data) +
    geom_line(aes(x = year, y = rel_freq, color = node)) +
    scale_color_manual(name = "",
                       values = fillcols.6categories,
                       drop = F) + 
    scale_x_continuous(expand = c(0, 0),
                       limits = c(min(years)-0.5, max(years)+0.5),
                       breaks = seq(min(years), 
                                    max(years), 
                                    by = break_points_x)) +
    scale_y_continuous(expand = c(0, 0),
                       labels = scales::percent_format(accuracy = 1),
                       limits = c(0, upper_limit), 
                       breaks = seq(0, upper_limit, by = break_points)) +
    labs(x = "", y = "Relative document frequency\n",
         title = title,
         subtitle = "Trajectories of use") +
    time.plot.theme
  
  if(points_on==T){
    plot <- 
      plot + 
      geom_point(aes(x = year, y = rel_freq, color = node, shape = shapetype, size = shapesize)) +
      scale_shape_manual(name = "",
                         values = c("buzzword/placeholder" = 16, "standard" = 0, "emblem/stereotype" = 2,
                                    "allusion/factoid" = 6, "ordinary" = 5, "not classified" = 4),
                         drop = F) +
      scale_size_manual(values = c("big" = 3, "small" = 2),
                        guide = "none")
  } 
  if(points_on==F) {
    plot <- 
      plot + 
      geom_line(aes(x = year, y = rel_freq, color = node),
                size = 1)
  }
  if(length(years)>10) {
    plot <-
      plot +
      theme(axis.text.x = element_text(angle = 320,
                                       hjust = 0))
  }
  return(plot)
}


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: WORD TRAJECTORY PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Frequent buzzwords (in at least one corpus), comparing across corpora ----

timeplot_biodiversity <- PlotTrajectories_compareCorpora("biodiversity", focalword_attributes, upper_limit = 0.6)
timeplot_landscape <- PlotTrajectories_compareCorpora("landscape", focalword_attributes, upper_limit = 0.6)
timeplot_ecosystem_service <- PlotTrajectories_compareCorpora("ecosystem service", focalword_attributes, upper_limit = 0.6)
timeplot_climate_change <- PlotTrajectories_compareCorpora("climate change", focalword_attributes, upper_limit = 0.85)
timeplot_restoration <- PlotTrajectories_compareCorpora("restoration", focalword_attributes, upper_limit = 0.6)
timeplot_local <- PlotTrajectories_compareCorpora("local", focalword_attributes, upper_limit = 0.6)
timeplot_global <- PlotTrajectories_compareCorpora("global", focalword_attributes, upper_limit = 0.6)
timeplot_community <- PlotTrajectories_compareCorpora("community", focalword_attributes, upper_limit = 0.8)
timeplot_development <- PlotTrajectories_compareCorpora("development", focalword_attributes, upper_limit = 0.8)
timeplot_partnership <- PlotTrajectories_compareCorpora("partnership", focalword_attributes, upper_limit = 0.6)
timeplot_livelihood <- PlotTrajectories_compareCorpora("livelihood", focalword_attributes, upper_limit = 0.6)
timeplot_wildlife <- PlotTrajectories_compareCorpora("wildlife", focalword_attributes, upper_limit = 0.6)
timeplot_state <- PlotTrajectories_compareCorpora("state", focalword_attributes, upper_limit = 0.6)
timeplot_challenge <- PlotTrajectories_compareCorpora("challenge", focalword_attributes, upper_limit = 0.8)
timeplot_solution <- PlotTrajectories_compareCorpora("solution", focalword_attributes, upper_limit = 0.8)
timeplot_approach <- PlotTrajectories_compareCorpora("approach", focalword_attributes, upper_limit = 0.8)
timeplot_system <- PlotTrajectories_compareCorpora("system", focalword_attributes, upper_limit = 0.8)


# ---- 4.2 Words from interviews, comparing across corpora ----

timeplot_sustainability <- PlotTrajectories_compareCorpora("sustainability", focalword_attributes, upper_limit = 0.6)
timeplot_sustainable <- PlotTrajectories_compareCorpora("sustainable", focalword_attributes, upper_limit = 0.6)
timeplot_climate_change <- PlotTrajectories_compareCorpora("climate change", focalword_attributes, upper_limit = 0.85)
timeplot_biodiversity <- PlotTrajectories_compareCorpora("biodiversity", focalword_attributes, upper_limit = 0.6)

timeplot_naturebased_solution <- PlotTrajectories_compareCorpora("nature-based solution", focalword_attributes, upper_limit = 0.6)
timeplot_transformative <- PlotTrajectories_compareCorpora("transformative", focalword_attributes, upper_limit = 0.6)
timeplot_transformative_change <- PlotTrajectories_compareCorpora("transformative change", focalword_attributes, upper_limit = 0.6)
timeplot_transformation <- PlotTrajectories_compareCorpora("transformation", focalword_attributes, upper_limit = 0.6)

timeplot_diversity <- PlotTrajectories_compareCorpora("diversity", focalword_attributes, upper_limit = 0.8)
timeplot_equitable <- PlotTrajectories_compareCorpora("equitable", focalword_attributes, upper_limit = 0.8)
timeplot_equity <- PlotTrajectories_compareCorpora("equity", focalword_attributes, upper_limit = 0.8)
timeplot_inclusive <- PlotTrajectories_compareCorpora("inclusive", focalword_attributes, upper_limit = 0.8)


# ---- 4.3 Exploring other words, across corpora ----

timeplot_governance <- PlotTrajectories_compareCorpora("governance", focalword_attributes, upper_limit = 0.6)
timeplot_nature_contribution <- PlotTrajectories_compareCorpora("nature contribution", focalword_attributes, upper_limit = 0.6)
timeplot_local_community <- PlotTrajectories_compareCorpora("local community", focalword_attributes, upper_limit = 0.6)
timeplot_nature <- PlotTrajectories_compareCorpora("nature", focalword_attributes, upper_limit = 0.8)
timeplot_crisis <- PlotTrajectories_compareCorpora("crisis", focalword_attributes, upper_limit = 0.6)
timeplot_extinction <- PlotTrajectories_compareCorpora("extinction", focalword_attributes, upper_limit = 0.6)
timeplot_illegal <- PlotTrajectories_compareCorpora("illegal", focalword_attributes, upper_limit = 0.6)
timeplot_deforestation <- PlotTrajectories_compareCorpora("deforestation", focalword_attributes, upper_limit = 0.6)
timeplot_framework <- PlotTrajectories_compareCorpora("framework", focalword_attributes, upper_limit = 0.8)
timeplot_conservation <- PlotTrajectories_compareCorpora("conservation", focalword_attributes, upper_limit = 0.8)
timeplot_resilience <- PlotTrajectories_compareCorpora("resilience", focalword_attributes, upper_limit = 0.6)
timeplot_resilient <- PlotTrajectories_compareCorpora("resilient", focalword_attributes, upper_limit = 0.6)
timeplot_safeguard <- PlotTrajectories_compareCorpora("safeguard", focalword_attributes, upper_limit = 0.6)
timeplot_risk <- PlotTrajectories_compareCorpora("risk", focalword_attributes, upper_limit = 0.6)
timeplot_adaptation <- PlotTrajectories_compareCorpora("adaptation", focalword_attributes, upper_limit = 0.6)
timeplot_vulnerable <- PlotTrajectories_compareCorpora("vulnerable", focalword_attributes, upper_limit = 0.6)


# ---- 4.4 Compare words across academic corpus (2000-2021) ----

compareplot_a1 <- PlotTrajectories_compareWords(focal_corpus = "academic", 
                                             data = focalword_attributes,
                                             words = c("biodiversity", "ecosystem service", 
                                                       "climate change"),
                                             years = c(2000:2021),
                                             upper_limit = 0.3,
                                             points_on = F)

compareplot_a2 <- PlotTrajectories_compareWords(focal_corpus = "academic", 
                                                data = focalword_attributes,
                                                words = c("global", "local", "region", "landscape"),
                                                years = c(2000:2021),
                                                upper_limit = 0.3,
                                                points_on = F)


# ---- 4.4 Compare words across NGO corpus (2017-2021) ----

# working with others, dei, etc.
compareplot_n1 <- PlotTrajectories_compareWords(focal_corpus = "ngo", 
                                                data = focalword_attributes,
                                                words = c("partnership", "community", 
                                                          "inclusive", "equity"),
                                                upper_limit = 0.6,
                                                points_on = T)
# geographic, temporal scale
compareplot_n2 <- PlotTrajectories_compareWords(focal_corpus = "ngo", 
                                                data = focalword_attributes,
                                                words = c("global", "local", "future", 
                                                          "decade", "time"),
                                                upper_limit = 0.6,
                                                points_on = T)
# ways of working, action-oriented
compareplot_n3 <- PlotTrajectories_compareWords(focal_corpus = "ngo", 
                                                data = focalword_attributes,
                                                words = c("landscape", "nature-based solution",
                                                          "action", "management", "safeguard"),
                                                upper_limit = 0.6,
                                                points_on = T)
# characterizing the problem
compareplot_n4 <- PlotTrajectories_compareWords(focal_corpus = "ngo", 
                                                data = focalword_attributes,
                                                words = c("biodiversity", "nature", 
                                                          "system", "climate change"),
                                                upper_limit = 0.6,
                                                points_on = T)
# desired future
compareplot_n5 <- PlotTrajectories_compareWords(focal_corpus = "ngo", 
                                                data = focalword_attributes,
                                                words = c("transformation", "livelihood", "sustainable"),
                                                upper_limit = 0.6,
                                                points_on = T)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: WORD LINKAGES THROUGH TIME ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 5.1 Wrangle semantic maps for looking at links ----

focalword_coocGraph <- function(focal_word, years = 2017:2021, 
                                input_corpora = c("a", "n", "m_nyt_filt"),
                                input_file = NULL) {
  
  if(is.null(input_file)){
    # find most recent folders to import data from
    most_recent_coocGraph_folder <- last(list.files("data/outputs/coocGraphs"))
  } else {
    most_recent_coocGraph_folder <- paste("data/outputs/coocGraphs/", input_file, sep = "")
  }
  
  coocGraph <- NULL
  
  # import coocGraph data, mutate and join with other corpora/years
  for(i in years) {
    
    for(j in input_corpora) {
      
      coocGraph <- 
        bind_rows(coocGraph,
                  read_csv(paste("data/outputs/coocGraphs/", most_recent_coocGraph_folder, "/coocGraph_", j, "_", i, ".csv", sep = ""),
                           locale = readr::locale(encoding = "UTF-8")) %>%
                    filter(from==focal_word) %>%
                    mutate(corpus = case_when(j=="a" ~ "academic",
                                              j=="n" ~ "ngo",
                                              j=="m_nyt_filt" ~ "media",
                                              j=="m" ~ "media",
                                              j=="p" ~ "policy"),
                           year = i) %>%
                    left_join(get(paste("node_freq_", j, sep = "")), 
                              by = c("to" = "node", "year")) %>%
                    rename("node_freq" = "freq") %>%
                    left_join(get(paste("node_freq_", j, sep = "")), 
                              by = c("from" = "node", "year")) %>%
                    rename("focal_freq" = "freq") %>%
                    mutate(prop_node_freq = coocFreq/node_freq,
                           prop_focal_freq = coocFreq/focal_freq)
      )
    }
  }
  
  coocGraph <- 
    coocGraph %>%
    rename("focal_word" = "from",
           "node" = "to")
  
  return(coocGraph)
}

# ---- biodiversity ----

coocGraph_biodiversity <- 
  focalword_coocGraph(focal_word = "biodiversity") 

linkages_biodiversity <-
  coocGraph_biodiversity %>%
  distinct() %>%
  group_by(focal_word, corpus, year) %>%
  arrange(desc(prop_focal_freq), .by_group = T) %>%
  summarise(n_cooc = length(node),
            coocs = list(node))

# ---- nature ----

coocGraph_nature <- 
  focalword_coocGraph(focal_word = "nature") 

linkages_nature <-
  coocGraph_nature %>%
  distinct() %>%
  group_by(focal_word, corpus, year) %>%
  arrange(desc(prop_focal_freq), .by_group = T) %>%
  summarise(n_cooc = length(node),
            coocs = list(node))

# ---- climate change ----

coocGraph_climate_change <- 
  focalword_coocGraph(focal_word = "climate change") 

linkages_climate_change <-
  coocGraph_climate_change %>%
  distinct() %>%
  group_by(focal_word, corpus, year) %>%
  arrange(desc(prop_focal_freq), .by_group = T) %>%
  summarise(n_cooc = length(node),
            coocs = list(node))
