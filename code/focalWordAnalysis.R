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

pacman::p_load(spam, spam64, tidyverse, ggplot2, grid, gridExtra)

options(spam.force64 = TRUE)


source("code/source/plotThemes.R")


# ---- 1.2 Import node attribute data ----

node_attributes_a <- read_csv("data/outputs/node_attributes_a.csv")
node_attributes_n <- read_csv("data/outputs/node_attributes_n.csv")
node_attributes_m <- read_csv("data/outputs/node_attributes_m.csv")
node_attributes_p <- read_csv("data/outputs/node_attributes_p.csv")

node_freq_a <- read_csv("data/outputs/node_freq_a.csv")
node_freq_n <- read_csv("data/outputs/node_freq_n.csv")
node_freq_m <- read_csv("data/outputs/node_freq_m.csv")
node_freq_p <- read_csv("data/outputs/node_freq_p.csv")

graph_attr_a <- read_csv("data/outputs/graph_attr_a.csv")
graph_attr_n <- read_csv("data/outputs/graph_attr_n.csv")
graph_attr_m <- read_csv("data/outputs/graph_attr_m.csv")
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
  mutate(rel_freq = freq / total_docs) %>%
  filter((consensus_threshold==0.25 | is.na(consensus_threshold)) &  # filter consensus threshold to 0.25 for academic
           (percentile_threshold==0.5 | is.na(percentile_threshold)))

words_n <- 
  node_freq_n %>% 
  left_join(graph_attr_n, by = "year") %>%
  left_join(node_attributes_n %>% select(-freq), by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs) %>%
  filter((consensus_threshold==0.75 | is.na(consensus_threshold)) & # filter consensus threshold to 0.75 for ngo
           (percentile_threshold==0.5 | is.na(percentile_threshold)))


words_m <- 
  node_freq_m %>% 
  left_join(graph_attr_m, by = "year") %>%
  left_join(node_attributes_m %>% select(-freq), by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs,
         corpus = "media") %>%
  filter((consensus_threshold==0.5 | is.na(consensus_threshold)) & # filter consensus threshold to 0.5 for media
           (percentile_threshold==0.5 | is.na(percentile_threshold)))


words_p <- 
  node_freq_p %>% 
  left_join(graph_attr_p, by = "year") %>%
  left_join(node_attributes_p %>% select(-freq), by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs,
         corpus = case_when(year==2019 ~ "IPBES",
                            year==2022 ~ "UNCBD")) %>%
  filter((consensus_threshold==0.75 | is.na(consensus_threshold)) & # filter consensus threshold to 0.75 for policy
           (percentile_threshold==0.5 | is.na(percentile_threshold)))


# ---- 2.2 Define focal word attributes table ----

focalword_attributes <- 
  words_a %>% filter(year%in%2017:2021) %>%
  bind_rows(words_n) %>%
  bind_rows(words_m) %>%
  bind_rows(words_p %>% mutate(corpus = "policy")) %>%
  select(node, year, corpus, freq, rel_freq, total_docs, consensus, conductivity, degree, symbol_type, 
         sd_multiplier, percentile_threshold, consensus_threshold) %>%
  mutate(shapetype = case_when(symbol_type=="placeholder" ~ "buzzword", 
                               symbol_type%in%c("standard", "ordinary", "emblem", 
                                                "stereotype", "allusion", "factoid", "buzzword") ~ "not buzzword",
                               TRUE ~ "not classified"),
         shapetype = factor(shapetype, levels = c("buzzword", "not buzzword", "not classified"),
                            ordered = T),
         # shapetype = case_when(symbol_type%in%c("buzzword", "placeholder") ~ "buzzword/placeholder", 
         #                     symbol_type=="standard" ~ "standard",
         #                     symbol_type=="ordinary" ~ "ordinary",
         #                     symbol_type%in%c("emblem", "stereotype") ~ "emblem/stereotype",
         #                     symbol_type%in%c("allusion", "factoid") ~ "allusion/factoid",
         #                     TRUE ~ "not classified"),
         # shapetype = factor(shapetype, levels = c("buzzword/placeholder", "standard",
         #                                          "emblem/stereotype", "allusion/factoid",
         #                                          "ordinary", "not classified"),
         #                    ordered = T),
         # shapesize = case_when(shapetype=="buzzword/placeholder" ~ "big",
         #                       TRUE ~ "small"),
         corpus = factor(corpus, levels = unique(corpus)))


focalwords_acrosscorpora <-
  words_a %>%
  bind_rows(words_n) %>%
  bind_rows(words_m) %>%
  bind_rows(words_p %>% mutate(corpus=="policy")) %>%
  filter(symbol_type=="placeholder") %>%
  group_by(node) %>%
  summarise(n_corpora_buzzplace = length(unique(corpus)),
            most_recent_buzzplace = max(year),
            max_rel_freq = max(rel_freq))


focalwords_biggestchange <-
  words_a %>%
  bind_rows(words_n) %>%
  bind_rows(words_m) %>%
  group_by(node, corpus) %>%
  arrange(year) %>%
  mutate(delta_freq = rel_freq - lag(rel_freq),
         year_before = lag(year)) %>%
  summarise(max_delta = max(delta_freq, na.rm = T),
            max_delta_year_lower = year_before[delta_freq==max_delta & !is.na(delta_freq)],
            max_delta_year_upper = year[delta_freq==max_delta & !is.na(delta_freq)],
            perc_change_max_delta = (rel_freq[year==max_delta_year_upper]-rel_freq[year==max_delta_year_lower])/rel_freq[year==max_delta_year_lower],
            min_year_2017 = min(year[year>=2017]),
            max_year_2017 = max(year[year>=2017]),
            highest_freq_2017 = max(rel_freq[year>=2017]),
            highest_freq_year = max(year[rel_freq==highest_freq_2017]),
            lowest_freq_2017 = min(rel_freq[year>=2017]),
            lowest_freq_year = min(year[rel_freq==lowest_freq_2017]),
            perc_change_lowest_highest = case_when(highest_freq_year>lowest_freq_year & !is.na(lowest_freq_year) ~ 
                                                     (highest_freq_2017-lowest_freq_2017)/lowest_freq_2017,
                                                   highest_freq_year<lowest_freq_year & !is.na(lowest_freq_year) ~
                                                     (lowest_freq_2017-highest_freq_2017)/highest_freq_2017),
            perc_change_2017_2021 = ifelse(!is.na(min_year_2017),
                                           (rel_freq[year==max_year_2017]-rel_freq[year==min_year_2017])/rel_freq[year==min_year_2017],
                                           NA),
            n_placeholder_2017_2021 = length(node[symbol_type=="placeholder" & year%in%c(2017:2021) & !is.na(symbol_type)]),
            years_placeholder = list(year[symbol_type=="placeholder" & year%in%c(2017:2021) & !is.na(symbol_type)]), 
            min_year = min(year),
            max_year = max(year),
            absolute_change_min_max_year = (rel_freq[year==max_year]-rel_freq[year==min_year]),
            perc_change_min_max_year = (rel_freq[year==max_year]-rel_freq[year==min_year])/rel_freq[year==min_year],
            n_placeholder_total = length(node[symbol_type=="placeholder" & !is.na(symbol_type)]))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: DEFINE PLOTTING FUNCTIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Plot word trajectories over time, by corpus ----

PlotTrajectories_compareCorpora <- function(focal_word, data, upper_limit = 0.5, legend = T) {
  
  title <- str_to_title(focal_word)
  break_points <- ifelse(upper_limit > 0.5, 0.1, 0.05)
  
  data <- data %>% filter(year%in%c(2017:2022))
  
  plot <- 
    ggplot() +
    geom_line(data = data %>% filter(node==focal_word & corpus!="policy"),
              aes(x = year, y = rel_freq, color = corpus)) +
    geom_point(data = data %>% filter(node==focal_word),
               aes(x = year, y = rel_freq, color = corpus, shape = shapetype),
               size = 2.5) +
    geom_text(data = data %>% 
                 filter(node==focal_word & corpus=="policy") %>% 
                 mutate(label = ifelse(year==2019, "IPBES", "UNCBD")),
               aes(x = year, y = rel_freq + 0.03, label = label),
              size = 2.5) +
    scale_shape_manual(name = "",
                       values = c("buzzword" = 16, "not buzzword" = 1, "not classified" = 4),
                       drop = F) +
    # scale_shape_manual(name = "",
    #                    values = c("buzzword/placeholder" = 16, "standard" = 0, "emblem/stereotype" = 2,
    #                               "allusion/factoid" = 6, "ordinary" = 5, "not classified" = 4),
    #                    drop = F) +
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
    labs(x = "", y = "",
         title = title,
         subtitle = "Document frequency (%) across institutions") +
    time.plot.theme
  
  if(legend==F) {
    plot <-
      ggplot() +
      geom_line(data = data %>% filter(node==focal_word & corpus!="policy"),
                aes(x = year, y = rel_freq, color = corpus),
                show.legend = F) +
      geom_point(data = data %>% filter(node==focal_word),
                 aes(x = year, y = rel_freq, color = corpus, shape = shapetype),
                 size = 2.5, show.legend = F) +
      geom_text(data = data %>%
                  filter(node==focal_word & corpus=="policy") %>%
                  mutate(label = ifelse(year==2019, "IPBES", "UNCBD")),
                aes(x = year, y = rel_freq + 0.03, label = label),
                size = 2.5) +
      scale_shape_manual(values = c("buzzword" = 16, "not buzzword" = 1, "not classified" = 4),
                         drop = F) +
      scale_color_manual(values = fillcols.4categories,
                         drop = F) +
      scale_x_continuous(expand = c(0, 0),
                         limits = c(2016.5, 2022.5),
                         breaks = seq(2017, 2022, by = 1)) +
      scale_y_continuous(expand = c(0, 0),
                         labels = scales::percent_format(accuracy = 1),
                         limits = c(0, upper_limit),
                         breaks = seq(0, upper_limit, by = break_points)) +
      labs(x = "", y = "",
           title = title,
           subtitle = "") +
      time.plot.theme
  }
  
  return(plot)
}

PlotTrajectories_compareWords <- function(focal_corpus, words, data, upper_limit = 0.5) {
  
  title <- paste(str_to_title(focal_corpus), "Corpus", sep = " ")
  break_points <- ifelse(upper_limit > 0.5, 0.1, 0.05)
  
  data <- data %>% filter(corpus==focal_corpus & node%in%words)
  
  plot <- 
    ggplot(data = data) +
    geom_line(aes(x = year, y = rel_freq, color = node)) +
    geom_point(aes(x = year, y = rel_freq, color = node, shape = shapetype, size = shapesize)) +
    scale_shape_manual(name = "",
                       values = c("buzzword/placeholder" = 16, "standard" = 0, "emblem/stereotype" = 2,
                                  "allusion/factoid" = 6, "ordinary" = 5, "not classified" = 4),
                       drop = F) +
    scale_size_manual(values = c("big" = 3, "small" = 2),
                      guide = "none") +
    scale_color_manual(name = "",
                       labels = str_to_title(words),
                       values = fillcols.4categories,
                       drop = F) + 
    scale_x_continuous(expand = c(0, 0),
                       limits = c(min(unique(data$year))-0.5, max(unique(data$year)+0.5)),
                       breaks = seq(min(unique(data$year)), max(unique(data$year)), by = 1)) +
    scale_y_continuous(expand = c(0, 0),
                       labels = scales::percent_format(accuracy = 1),
                       limits = c(0, upper_limit), 
                       breaks = seq(0, upper_limit, by = break_points)) +
    labs(x = "", y = "Relative document frequency\n",
         title = title,
         subtitle = "Trajectories of use") +
    time.plot.theme
  
  return(plot)
}


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: WORD TRAJECTORY PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

timeplot_example <- PlotTrajectories_compareCorpora("context", focalword_attributes, upper_limit = 0.6)

# ---- 4.1 Frequent buzzwords (in at least one corpus), comparing across corpora ----

timeplot_biodiversity <- PlotTrajectories_compareCorpora("biodiversity", focalword_attributes, upper_limit = 0.6)
timeplot_landscape <- PlotTrajectories_compareCorpora("landscape", focalword_attributes, upper_limit = 0.6)
timeplot_local <- PlotTrajectories_compareCorpora("local", focalword_attributes, upper_limit = 0.6)
timeplot_community <- PlotTrajectories_compareCorpora("community", focalword_attributes, upper_limit = 0.8)
timeplot_challenge <- PlotTrajectories_compareCorpora("challenge", focalword_attributes, upper_limit = 0.8)
timeplot_system <- PlotTrajectories_compareCorpora("system", focalword_attributes, upper_limit = 0.8)
timeplot_strategy <- PlotTrajectories_compareCorpora("strategy", focalword_attributes, upper_limit = 0.6)
timeplot_language <- PlotTrajectories_compareCorpora("language", focalword_attributes, upper_limit = 0.6)
timeplot_value <- PlotTrajectories_compareCorpora("value", focalword_attributes, upper_limit = 0.6)
timeplot_contribution <- PlotTrajectories_compareCorpora("contribution", focalword_attributes, upper_limit = 0.6)
timeplot_hope <- PlotTrajectories_compareCorpora("hope", focalword_attributes, upper_limit = 0.6)
timeplot_woman <- PlotTrajectories_compareCorpora("woman", focalword_attributes, upper_limit = 0.6)
timeplot_indigenous <- PlotTrajectories_compareCorpora("indigenous", focalword_attributes, upper_limit = 0.6)
timeplot_stakeholder <- PlotTrajectories_compareCorpora("stakeholder", focalword_attributes, upper_limit = 0.6)


# trending buzzwords
timeplot_movement <- PlotTrajectories_compareCorpora("movement", focalword_attributes, upper_limit = 0.6)
timeplot_nature_based <- PlotTrajectories_compareCorpora("nature-based solution", focalword_attributes, upper_limit = 0.6)
timeplot_climate_crisis <- PlotTrajectories_compareCorpora("climate crisis", focalword_attributes, upper_limit = 0.6)
timeplot_adaptation <- PlotTrajectories_compareCorpora("adaptation", focalword_attributes, upper_limit = 0.6)
timeplot_indigenous_people <- PlotTrajectories_compareCorpora("indigenous people", focalword_attributes, upper_limit = 0.6)
timeplot_investment <- PlotTrajectories_compareCorpora("investment", focalword_attributes, upper_limit = 0.6)


# More standard words, compare across corpora
timeplot_species <- PlotTrajectories_compareCorpora("species", focalword_attributes, upper_limit = 0.6)
timeplot_habitat <- PlotTrajectories_compareCorpora("habitat", focalword_attributes, upper_limit = 0.6)
timeplot_conservation <- PlotTrajectories_compareCorpora("conservation", focalword_attributes, upper_limit = 0.8)
timeplot_nature <- PlotTrajectories_compareCorpora("nature", focalword_attributes, upper_limit = 0.8)

# Examples from interviews
timeplot_sustainable <- PlotTrajectories_compareCorpora("sustainable", focalword_attributes, upper_limit = 0.6)
timeplot_climate_change <- PlotTrajectories_compareCorpora("climate change", focalword_attributes, upper_limit = 0.85)
timeplot_biodiversity <- PlotTrajectories_compareCorpora("biodiversity", focalword_attributes, upper_limit = 0.6)
timeplot_naturebased_solution <- PlotTrajectories_compareCorpora("nature-based solution", focalword_attributes, upper_limit = 0.6)
timeplot_natural_climate_solution <- PlotTrajectories_compareCorpora("natural climate solution", focalword_attributes, upper_limit = 0.6)

timeplot_transformative <- PlotTrajectories_compareCorpora("transformative", focalword_attributes, upper_limit = 0.3)
timeplot_transformative_change <- PlotTrajectories_compareCorpora("transformative change", focalword_attributes, upper_limit = 0.3)
timeplot_transformation <- PlotTrajectories_compareCorpora("transformation", focalword_attributes, upper_limit = 0.3)

timeplot_diversity <- PlotTrajectories_compareCorpora("diversity", focalword_attributes, legend = F, upper_limit = 0.3)
# timeplot_equitable <- PlotTrajectories_compareCorpora("equitable", focalword_attributes, upper_limit = 0.3)
timeplot_equity <- PlotTrajectories_compareCorpora("equity", focalword_attributes, legend = F, upper_limit = 0.3)
timeplot_inclusive <- PlotTrajectories_compareCorpora("inclusive", focalword_attributes, legend = T, upper_limit = 0.3)

timeplot_restoration <- PlotTrajectories_compareCorpora("restoration", focalword_attributes, upper_limit = 0.6)
timeplot_innovation <- PlotTrajectories_compareCorpora("innovation", focalword_attributes, upper_limit = 0.3)
timeplot_adaptation <- PlotTrajectories_compareCorpora("adaptation", focalword_attributes, upper_limit = 0.6)
timeplot_climate_smart <- PlotTrajectories_compareCorpora("climate-smart", focalword_attributes, upper_limit = 0.6)
timeplot_vulnerable <- PlotTrajectories_compareCorpora("vulnerable", focalword_attributes, upper_limit = 0.6)


# doesn't show up at all 
timeplot_nature_positive <- PlotTrajectories_compareCorpora("nature positive", focalword_attributes, upper_limit = 0.6)
timeplot_net_zero <- PlotTrajectories_compareCorpora("net zero", focalword_attributes, upper_limit = 0.6)
timeplot_extinction <- PlotTrajectories_compareCorpora("extinction", focalword_attributes, upper_limit = 0.6)
timeplot_illegal <- PlotTrajectories_compareCorpora("illegal", focalword_attributes, upper_limit = 0.6)
timeplot_deforestation <- PlotTrajectories_compareCorpora("deforestation", focalword_attributes, upper_limit = 0.6)
timeplot_woke <- PlotTrajectories_compareCorpora("woke", focalword_attributes, upper_limit = 0.6)
timeplot_anthropocene <- PlotTrajectories_compareCorpora("anthropocene", focalword_attributes, upper_limit = 0.6)


# Academic speak
timeplot_benefit <- PlotTrajectories_compareCorpora("benefit", focalword_attributes, upper_limit = 0.8)
timeplot_framework <- PlotTrajectories_compareCorpora("framework", focalword_attributes, upper_limit = 0.8)
timeplot_ecosystem_service <- PlotTrajectories_compareCorpora("ecosystem service", focalword_attributes, upper_limit = 0.6)
timeplot_ecosystem <- PlotTrajectories_compareCorpora("ecosystem", focalword_attributes, upper_limit = 0.6)
timeplot_anthropogenic <- PlotTrajectories_compareCorpora("anthropogenic", focalword_attributes, upper_limit = 0.6)


# NGO speak
timeplot_hope <- PlotTrajectories_compareCorpora("hope", focalword_attributes, upper_limit = 0.6)
timeplot_safeguard <- PlotTrajectories_compareCorpora("safeguard", focalword_attributes, upper_limit = 0.6)
timeplot_naturebased_solution <- PlotTrajectories_compareCorpora("nature-based solution", focalword_attributes, upper_limit = 0.3)
timeplot_partnership <- PlotTrajectories_compareCorpora("partnership", focalword_attributes, upper_limit = 0.6)
timeplot_local_community <- PlotTrajectories_compareCorpora("local community", focalword_attributes, upper_limit = 0.6)
timeplot_livelihood <- PlotTrajectories_compareCorpora("livelihood", focalword_attributes, upper_limit = 0.6)
timeplot_indigenous_people <- PlotTrajectories_compareCorpora("indigenous people", focalword_attributes, upper_limit = 0.8)
timeplot_engagement <- PlotTrajectories_compareCorpora("engagement", focalword_attributes, upper_limit = 0.6)
timeplot_collaborate <- PlotTrajectories_compareCorpora("collaborate", focalword_attributes, upper_limit = 0.6)
timeplot_resilience <- PlotTrajectories_compareCorpora("resilience", focalword_attributes, upper_limit = 0.6)
timeplot_stronghold <- PlotTrajectories_compareCorpora("stronghold", focalword_attributes, upper_limit = 0.6)


timeplot_net <- PlotTrajectories_compareCorpora("net", focalword_attributes, upper_limit = 0.6)



# UN CBD and IPBES speak
timeplot_context <- PlotTrajectories_compareCorpora("context", focalword_attributes, upper_limit = 0.6)
timeplot_knowledge <- PlotTrajectories_compareCorpora("knowledge", focalword_attributes, upper_limit = 0.6)
timeplot_role <- PlotTrajectories_compareCorpora("role", focalword_attributes, upper_limit = 0.6)
timeplot_nature_contribution <- PlotTrajectories_compareCorpora("nature contribution", focalword_attributes, upper_limit = 0.6)
timeplot_contribution <- PlotTrajectories_compareCorpora("contribution", focalword_attributes, upper_limit = 0.6)
timeplot_scenario <- PlotTrajectories_compareCorpora("scenario", focalword_attributes, upper_limit = 0.6)
timeplot_governance <- PlotTrajectories_compareCorpora("governance", focalword_attributes, upper_limit = 0.6)

# Media speak
timeplot_future <- PlotTrajectories_compareCorpora("future", focalword_attributes, upper_limit = 0.6)
timeplot_scientist <- PlotTrajectories_compareCorpora("scientist", focalword_attributes, upper_limit = 0.6)
timeplot_decade <- PlotTrajectories_compareCorpora("decade", focalword_attributes, upper_limit = 0.6)
timeplot_local <- PlotTrajectories_compareCorpora("local", focalword_attributes, upper_limit = 0.6)
timeplot_research <- PlotTrajectories_compareCorpora("research", focalword_attributes, upper_limit = 0.6)
timeplot_government <- PlotTrajectories_compareCorpora("government", focalword_attributes, upper_limit = 0.6)
timeplot_risk <- PlotTrajectories_compareCorpora("risk", focalword_attributes, upper_limit = 0.6)

timeplot_dei_arranged <-
grid.arrange(timeplot_diversity, timeplot_equity, timeplot_inclusive, get_legend(timeplot_biodiversity),
             ncol = 4, widths = c(1, 1, 1, 0.3),
             left = textGrob(label = "Document frequency (%) across institutions",
                             rot = 90, y = unit(45, "pt"),
                             just = "left",
                             gp = gpar(fontsize = 10, fontface = "bold")))


# EXPORT
output_folder <- "data/outputs/figures/time_plots/"


png(paste(output_folder, "dei.png", sep = ""),
    units = "in", height = 4, width = 12, res = 400)
grid.newpage()
grid.draw(timeplot_dei_arranged)
dev.off()

png(paste(output_folder, "stakeholder.png", sep = ""),
    units = "in", height = 4, width = 6, res = 400)
grid.newpage()
grid.draw(timeplot_stakeholder)
dev.off()


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


# ---- 4.5 Compare words across NGO corpus (2017-2021) ----

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

focalword_coocGraph <- function(focal_word, years = 2017:2022, 
                                input_corpora = c("a", "n", "m"),
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
      
      if(file.exists(paste("data/outputs/coocGraphs/", most_recent_coocGraph_folder, "/coocGraph_", j, "_", i, ".csv", sep = ""))) {
    
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
      } else { next }
    }
  }
  
  coocGraph <- 
    coocGraph %>%
    rename("focal_word" = "from",
           "node" = "to")
  
  return(coocGraph)
}

makeTrendsHeatmap <- function(data, focal_word, institution, n_slice = 6) {
  
  title <- str_to_title(focal_word)
  subtitle <- paste(ifelse(institution=="ngo", str_to_upper(institution), str_to_title(institution)), 
                    " trends in associated words",
                    sep = "")
    
  linkages_data <-
    data %>%
    distinct() %>%
    filter(corpus==institution) %>%
    group_by(year) %>%
    slice_max(prop_focal_freq, n = n_slice) %>%
    mutate(keep = 1) %>%
    ungroup() %>%
    select(node, keep) %>% 
    distinct() %>%
    left_join(data %>% filter(corpus==institution), ., by = "node") %>%
    filter(keep==1) %>%
    add_row(year = 2017, .after = 1) %>%
    add_row(year = 2018, .after = 1) %>%
    add_row(year = 2019, .after = 1) %>%
    add_row(year = 2020, .after = 1) %>%
    add_row(year = 2021, .after = 1) %>%
    arrange(year, prop_focal_freq) %>%
    mutate(node = factor(node, levels = unique(node), ordered = T),
           year = factor(year, levels = unique(year), ordered = T)) %>%
    drop_na()
  
  expand_x <- ifelse(institution%in%c("ngo", "media") & min(linkages_data$year)!="2017", 1, 0)
  
  heatmap <-
    ggplot(linkages_data, 
           aes(y = node, x = year, fill = prop_focal_freq)) +
    geom_tile(alpha = 0.8) +
    scale_fill_distiller(name = paste("Proportion\nword\nappears with\n'", focal_word, "'", sep = ""),
                         palette = "Blues",
                         direction = 1) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(title = title, subtitle = subtitle, y = "", x = "") +
    theme_minimal() +
    theme(axis.text = element_text(size = 11),
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12, face = "italic"))
  
  if(expand_x==1) {
    heatmap <- 
      heatmap +
      scale_x_discrete(expand = expansion(mult = c(0.12, 0)),
                       drop = F)
  } else { heatmap <- heatmap + scale_x_discrete(expand = c(0, 0), drop = F) }
  
  return(heatmap)
  
}


# ---- biodiversity ----

coocGraph_biodiversity <- 
  focalword_coocGraph(focal_word = "biodiversity", 
                      input_corpora = c("a", "n", "m_nyt_filt", "p"),
                      years = 2000:2022)

trends_heatmap_biodiversity_ngo <- makeTrendsHeatmap(data = coocGraph_biodiversity, 
                                                     focal_word = "biodiversity", 
                                                     institution = "ngo",
                                                     n_slice = 6)
trends_heatmap_biodiversity_academic <- makeTrendsHeatmap(data = coocGraph_biodiversity, 
                                                          focal_word = "biodiversity", 
                                                          institution = "academic",
                                                          n_slice = 8)
trends_heatmap_biodiversity_media <- makeTrendsHeatmap(data = coocGraph_biodiversity, 
                                                          focal_word = "biodiversity", 
                                                          institution = "media",
                                                          n_slice = 5)

linkages_biodiversity <-
  coocGraph_biodiversity %>%
  distinct() %>%
  # filter to most recent year for each corpus (since biodiversity is very prevalent in all corpora/years)
  filter((corpus=="ngo" & year==2021) |
           (corpus=="media" & year==2021) |
           (corpus=="policy" & year==2022) | 
           (corpus=="academic" & year==2021)) %>%
  group_by(focal_word, corpus) %>%
  slice_max(prop_focal_freq, n = 8) %>%
  mutate(keep = 1) %>%
  ungroup() %>%
  select(node, keep) %>% 
  distinct() %>%
  left_join(coocGraph_biodiversity %>% filter((corpus=="ngo" & year==2021) |
                                             (corpus=="media" & year==2021) |
                                             (corpus=="policy" & year==2022) | 
                                             (corpus=="academic" & year==2021)), ., by = "node") %>%
  filter(keep==1) %>%
  arrange(corpus, prop_focal_freq) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T))



biodiversity_allinstitutions_heatmap <-
  ggplot(linkages_biodiversity, 
         aes(y = node, x = corpus, fill = prop_focal_freq)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = "Proportion\nword\nappears with\n'biodiversity'",
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = c(0, 0),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", "Policy\n(UNCBD 2022)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Biodiversity ", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))



# ---- safeguard ----

coocGraph_safeguard <- 
  focalword_coocGraph(focal_word = "safeguard",
                      input_corpora = c("a", "n", "m_nyt_filt", "p")) 

trends_heatmap_safeguard_ngo <- makeTrendsHeatmap(data = coocGraph_safeguard, 
                                                     focal_word = "safeguard", 
                                                     institution = "ngo",
                                                     n_slice = 8)
linkages_safeguard <-
  coocGraph_safeguard %>%
  distinct() %>%
  # filter to years of data for each corpus that safeguard was most recently a buzzword
  filter((corpus=="academic" & year==2021) |
           (corpus=="ngo" & year==2021) |
           (corpus=="media" & year==2018) |
           (corpus=="policy" & year==2019)) %>%
  group_by(focal_word, corpus) %>%
  slice_max(prop_focal_freq, n = 6) %>%
  mutate(keep = 1) %>%
  ungroup() %>%
  select(node, keep) %>% 
  distinct() %>%
  left_join(coocGraph_safeguard %>% filter((corpus=="academic" & year==2021) |
                                             (corpus=="ngo" & year==2021) |
                                             (corpus=="media" & year==2018) |
                                             (corpus=="policy" & year==2019)), ., by = "node") %>%
  filter(keep==1) %>%
  add_row(corpus = "academic", .after = 1) %>%
  arrange(corpus, prop_focal_freq) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T),
         corpus = factor(corpus)) %>%
  drop_na()
  
  

safeguard_allinstitutions_heatmap <-
  ggplot(linkages_safeguard, 
         aes(y = node, x = corpus, fill = prop_focal_freq)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = "Proportion\nword\nappears with\n'safeguard'",
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0)),
                   labels = c("Academic\n(any year)", "Media\n(2018)", "NGO\n(2021)", "Policy\n(IPBES 2019)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Safeguard", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))


# ---- hope ----

coocGraph_hope <- 
  focalword_coocGraph(focal_word = "hope",
                      input_corpora = c("a", "n", "m_nyt_filt", "p")) 

trends_heatmap_hope_media <- makeTrendsHeatmap(data = coocGraph_hope, 
                                                  focal_word = "hope", 
                                                  institution = "media",
                                                  n_slice = 10)
trends_heatmap_hope_ngo <- makeTrendsHeatmap(data = coocGraph_hope, 
                                                  focal_word = "hope", 
                                                  institution = "ngo",
                                                  n_slice = 6)

linkages_hope <-
  coocGraph_hope %>%
  distinct() %>%
  filter((corpus=="ngo" & year==2021) |
           (corpus=="media" & year==2021)) %>%
  group_by(focal_word, corpus) %>%
  slice_max(prop_focal_freq, n = 6) %>%
  mutate(keep = 1) %>%
  ungroup() %>%
  select(node, keep) %>% 
  distinct() %>%
  left_join(coocGraph_hope %>% filter((corpus=="ngo" & year==2021) |
                                             (corpus=="media" & year==2021)), ., by = "node") %>%
  filter(keep==1) %>%
  add_row(corpus = "academic", .after = 1) %>%
  add_row(corpus = "policy", .after = 1) %>%
  arrange(corpus, prop_focal_freq) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T),
         corpus = factor(corpus)) %>%
  drop_na()


hope_allinstitutions_heatmap <-
  ggplot(linkages_hope, 
         aes(y = node, x = corpus, fill = prop_focal_freq)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = "Proportion\nword\nappears with\n'hope'",
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(any year)", "Media\n(2021)", "NGO\n(2021)", "Policy\n(any report)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Hope", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))



# ---- landscape ----

coocGraph_landscape <- 
  focalword_coocGraph(focal_word = "landscape",
                      input_corpora = c("a", "n", "m_nyt_filt", "p"),
                      years = 2000:2022) 

trends_heatmap_landscape_ngo <- makeTrendsHeatmap(data = coocGraph_landscape, 
                                                     focal_word = "landscape", 
                                                     institution = "ngo",
                                                     n_slice = 6)
trends_heatmap_landscape_academic <- makeTrendsHeatmap(data = coocGraph_landscape, 
                                                          focal_word = "landscape", 
                                                          institution = "academic",
                                                          n_slice = 6)
trends_heatmap_landscape_media <- makeTrendsHeatmap(data = coocGraph_landscape, 
                                                       focal_word = "landscape", 
                                                       institution = "media",
                                                       n_slice = 5)

linkages_landscape <-
  coocGraph_landscape %>%
  distinct() %>%
  filter((corpus=="ngo" & year==2021) |
           (corpus=="media" & year==2021) |
           (corpus=="academic" & year==2021) |
           (corpus=="policy" & year==2019)) %>%
  group_by(focal_word, corpus) %>%
  slice_max(prop_focal_freq, n = 6) %>%
  mutate(keep = 1) %>%
  ungroup() %>%
  select(node, keep) %>% 
  distinct() %>%
  left_join(coocGraph_landscape %>% filter((corpus=="ngo" & year==2021) |
                                        (corpus=="media" & year==2021) |
                                          (corpus=="academic" & year==2021) |
                                          (corpus=="policy" & year==2019)), ., by = "node") %>%
  filter(keep==1) %>%
  arrange(corpus, prop_focal_freq) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T))


landscape_allinstitutions_heatmap <-
  ggplot(linkages_landscape, 
         aes(y = node, x = corpus, fill = prop_focal_freq)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = "Proportion\nword\nappears with\n'landscape'",
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = c(0, 0),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", "Policy\n(IPBES 2019)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Landscape", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))

# ---- sustainability ----

coocGraph_sustainable <- 
  focalword_coocGraph(focal_word = "sustainable",
                      input_corpora = c("a", "n", "m_nyt_filt", "p"),
                      years = 2000:2022)

trends_heatmap_sustainable_ngo <- makeTrendsHeatmap(data = coocGraph_sustainable, 
                                                  focal_word = "sustainable", 
                                                  institution = "ngo",
                                                  n_slice = 6)
trends_heatmap_sustainable_academic <- makeTrendsHeatmap(data = coocGraph_sustainable, 
                                                       focal_word = "sustainable", 
                                                       institution = "academic",
                                                       n_slice = 4)
trends_heatmap_sustainable_media <- makeTrendsHeatmap(data = coocGraph_sustainable, 
                                                    focal_word = "sustainable", 
                                                    institution = "media",
                                                    n_slice = 5)

linkages_sustainable <-
  coocGraph_sustainable %>%
  distinct() %>%
  filter((corpus=="ngo" & year==2021) |
           (corpus=="media" & year==2021) |
           (corpus=="academic" & year==2021) |
           (corpus=="policy" & year==2019)) %>%
  group_by(focal_word, corpus) %>%
  slice_max(prop_focal_freq, n = 6) %>%
  mutate(keep = 1) %>%
  ungroup() %>%
  select(node, keep) %>% 
  distinct() %>%
  left_join(coocGraph_sustainable %>% filter((corpus=="ngo" & year==2021) |
                                             (corpus=="media" & year==2021) |
                                             (corpus=="academic" & year==2021) |
                                             (corpus=="policy" & year==2019)), ., by = "node") %>%
  filter(keep==1) %>%
  arrange(corpus, prop_focal_freq) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T))


sustainable_allinstitutions_heatmap <-
  ggplot(linkages_sustainable, 
         aes(y = node, x = corpus, fill = prop_focal_freq)) +
  geom_tile(alpha = 0.8) +
  scale_fill_distiller(name = "Proportion\nword\nappears with\n'sustainable'",
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = c(0, 0),
                   labels = c("Academic\n(2021)", "Media\n(2021)", "NGO\n(2021)", "Policy\n(IPBES 2019)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Sustainable", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))

# ---- nature-based solution ----

coocGraph_nbs <- 
  focalword_coocGraph(focal_word = "nature-based solution",
                      input_corpora = c("a", "n", "m_nyt_filt", "p")) 

trends_heatmap_nbs_ngo <- makeTrendsHeatmap(data = coocGraph_nbs, 
                                                    focal_word = "nature-based solution", 
                                                    institution = "ngo",
                                                    n_slice = 6)

# ---- ecosystem service ----

coocGraph_ecosystem_service <- 
  focalword_coocGraph(focal_word = "ecosystem service",
                      input_corpora = c("a", "n", "m_nyt_filt", "p"),
                      years = 2000:2022) 

trends_heatmap_ecosystem_service_academic <- makeTrendsHeatmap(data = coocGraph_ecosystem_service, 
                                                               focal_word = "ecosystem service", 
                                                               institution = "academic",
                                                               n_slice = 6)
# ---- local community ----

coocGraph_local_community <- 
  focalword_coocGraph(focal_word = "local community",
                      input_corpora = c("a", "n", "m", "p")) 


trends_heatmap_local_community_ngo <- makeTrendsHeatmap(data = coocGraph_local_community, 
                                             focal_word = "local community", 
                                             institution = "ngo",
                                             n_slice = 6)

linkages_local_community <-
  coocGraph_local_community %>%
  distinct() %>%
  filter((corpus=="ngo" & year==2021) |
           (corpus=="media" & year==2021) |
           corpus=="policy") %>%
  group_by(focal_word, corpus) %>%
  slice_max(prop_focal_freq, n = 10) %>%
  mutate(keep = 1) %>%
  ungroup() %>%
  select(node, keep) %>% 
  distinct() %>%
  left_join(coocGraph_local_community %>% filter((corpus=="ngo" & year==2021) |
                                                   (corpus=="media" & year==2021) |
                                                   corpus=="policy"), ., by = "node") %>%
  filter(keep==1) %>%
  add_row(corpus = "academic", .after = 1) %>%
  arrange(corpus, prop_focal_freq) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T),
         corpus = ifelse(corpus=="policy" & year==2019, "IPBES", corpus),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES", "policy"))) %>%
  drop_na()


local_community_allinstitutions_heatmap <-
  ggplot(linkages_local_community, 
         aes(y = node, x = corpus, fill = prop_focal_freq)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = "Proportion\nword\nappears with\n'local community'",
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(any year)", "Media\n(2021)", "NGO\n(2021)", "Policy\n(IPBES)", "Policy\n(UNCBD)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Local Community", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))

# ---- indigenous people ----

coocGraph_indigenous_people <- 
  focalword_coocGraph(focal_word = "indigenous people",
                      input_corpora = c("a", "n", "m", "p")) 


trends_heatmap_indigenous_people_ngo <- makeTrendsHeatmap(data = coocGraph_indigenous_people, 
                                                        focal_word = "indigenous people", 
                                                        institution = "ngo",
                                                        n_slice = 6)

linkages_indigenous_people <-
  coocGraph_indigenous_people %>%
  distinct() %>%
  filter((corpus=="ngo" & year==2021) |
           (corpus=="media" & year==2021) |
           corpus=="policy") %>%
  group_by(focal_word, corpus) %>%
  slice_max(prop_focal_freq, n = 10) %>%
  mutate(keep = 1) %>%
  ungroup() %>%
  select(node, keep) %>% 
  distinct() %>%
  left_join(coocGraph_indigenous_people %>% filter((corpus=="ngo" & year==2021) |
                                                     (corpus=="media" & year==2021) |
                                                   corpus=="policy"), ., by = "node") %>%
  filter(keep==1) %>%
  add_row(corpus = "academic", .after = 1) %>%
  arrange(corpus, prop_focal_freq) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T),
         corpus = ifelse(corpus=="policy" & year==2019, "IPBES", corpus),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES", "policy"))) %>%
  drop_na()


indigenous_people_allinstitutions_heatmap <-
  ggplot(linkages_indigenous_people, 
         aes(y = node, x = corpus, fill = prop_focal_freq)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = "Proportion\nword\nappears with\n'indigenous people'",
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(any year)", "Media\n(2021)", "NGO\n(2021)", "Policy\n(IPBES)", "Policy\n(UNCBD)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Indigenous People", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))

# ---- inclusive ----

coocGraph_inclusive <- 
  focalword_coocGraph(focal_word = "inclusive",
                      input_corpora = c("a", "n", "m", "p")) 


trends_heatmap_inclusive_ngo <- makeTrendsHeatmap(data = coocGraph_inclusive, 
                                                          focal_word = "inclusive", 
                                                          institution = "ngo",
                                                          n_slice = 6)

linkages_inclusive <-
  coocGraph_inclusive %>%
  distinct() %>%
  filter((corpus=="ngo" & year==2021) |
           corpus=="policy") %>%
  group_by(focal_word, corpus) %>%
  slice_max(prop_focal_freq, n = 10) %>%
  mutate(keep = 1) %>%
  ungroup() %>%
  select(node, keep) %>% 
  distinct() %>%
  left_join(coocGraph_inclusive %>% filter((corpus=="ngo" & year==2021) |
                                                     corpus=="policy"), ., by = "node") %>%
  filter(keep==1) %>%
  add_row(corpus = "academic", .after = 1) %>%
  add_row(corpus = "media", .after = 1) %>%
  arrange(corpus, prop_focal_freq) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T),
         corpus = ifelse(corpus=="policy" & year==2019, "IPBES", corpus),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES", "policy"))) %>%
  drop_na()


inclusive_allinstitutions_heatmap <-
  ggplot(linkages_inclusive, 
         aes(y = node, x = corpus, fill = prop_focal_freq)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = "Proportion\nword\nappears with\n'inclusive'",
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(any year)", "Media\n(any year)", "NGO\n(2021)", "Policy\n(IPBES)", "Policy\n(UNCBD)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Inclusive", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))

# ---- stakeholder ----

coocGraph_stakeholder <- 
  focalword_coocGraph(focal_word = "stakeholder",
                      input_corpora = c("a", "n", "m", "p")) 


trends_heatmap_stakeholder_ngo <- makeTrendsHeatmap(data = coocGraph_stakeholder, 
                                                  focal_word = "stakeholder", 
                                                  institution = "ngo",
                                                  n_slice = 6)

linkages_stakeholder <-
  coocGraph_stakeholder %>%
  distinct() %>%
  filter((corpus=="academic" & year==2021) |
           (corpus=="ngo" & year==2021) |
           corpus=="policy") %>%
  group_by(focal_word, corpus) %>%
  slice_max(prop_focal_freq, n = 10) %>%
  mutate(keep = 1) %>%
  ungroup() %>%
  select(node, keep) %>% 
  distinct() %>%
  left_join(coocGraph_stakeholder %>% filter((corpus=="academic" & year==2021) |
                                               (corpus=="ngo" & year==2021) |
                                             corpus=="policy"), ., by = "node") %>%
  filter(keep==1) %>%
  add_row(corpus = "policy", .after = 1) %>%
  add_row(corpus = "media", .after = 1) %>%
  arrange(corpus, prop_focal_freq) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T),
         corpus = ifelse(corpus=="policy" & year==2019, "IPBES", corpus),
         corpus = factor(corpus, levels = c("academic", "media", "ngo", "IPBES", "policy"))) %>%
  drop_na()


stakeholder_allinstitutions_heatmap <-
  ggplot(linkages_stakeholder, 
         aes(y = node, x = corpus, fill = prop_focal_freq)) +
  geom_tile(alpha = 0.7) +
  scale_fill_distiller(name = "Proportion\nword\nappears with\n'stakeholder'",
                       palette = "Blues",
                       direction = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.15)),
                   labels = c("Academic\n(any year)", "Media\n(any year)", "NGO\n(2021)", "Policy\n(IPBES)", "Policy\n(UNCBD)"),
                   drop = F) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Stakeholder", y = "", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"))

# ---- EXPORT ----

heatmap_folder <- "data/outputs/figures/heatmaps/"


png(paste(heatmap_folder, "biodiversity_allinstitutions.png", sep = ""),
    units = "in", height = 6, width = 10, res = 400)
grid.newpage()
grid.draw(biodiversity_allinstitutions_heatmap)
dev.off()

png(paste(heatmap_folder, "safeguard_allinstitutions.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(safeguard_allinstitutions_heatmap)
dev.off()

png(paste(heatmap_folder, "hope_allinstitutions.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(hope_allinstitutions_heatmap)
dev.off()

png(paste(heatmap_folder, "landscape_allinstitutions.png", sep = ""),
    units = "in", height = 6, width = 10, res = 400)
grid.newpage()
grid.draw(landscape_allinstitutions_heatmap)
dev.off()

png(paste(heatmap_folder, "biodiversity_academic.png", sep = ""),
    units = "in", height = 6, width = 18, res = 400)
grid.newpage()
grid.draw(trends_heatmap_biodiversity_academic)
dev.off()

png(paste(heatmap_folder, "sustainable_academic.png", sep = ""),
    units = "in", height = 6, width = 18, res = 400)
grid.newpage()
grid.draw(trends_heatmap_sustainable_academic)
dev.off()

png(paste(heatmap_folder, "ecosystem_service_academic.png", sep = ""),
    units = "in", height = 6, width = 12, res = 400)
grid.newpage()
grid.draw(trends_heatmap_ecosystem_service_academic)
dev.off()

png(paste(heatmap_folder, "nbs_ngo.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(trends_heatmap_nbs_ngo)
dev.off()

png(paste(heatmap_folder, "safeguard_ngo.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(trends_heatmap_safeguard_ngo)
dev.off()

png(paste(heatmap_folder, "hope_ngo.png", sep = ""),
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(trends_heatmap_hope_ngo)
dev.off()
