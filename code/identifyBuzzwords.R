# 
# code: Identify buzzwords by institution, year, and type (trending, ubiquitous, bridging)
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: September 2023
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE LIBRARIES, FUNCTIONS, DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 1.1 Import libraries, source scripts ----

pacman::p_load(spam, spam64, tidyverse, ggplot2, grid, gridExtra)

options(spam.force64 = TRUE)


source("code/source/plotThemes.R")


# ---- 1.2 Import processed corpus documents ----

docs_a <- read_csv("data/corpora/processed/docs_a.csv", locale = readr::locale(encoding = "UTF-8"))
docs_n <- read_csv("data/corpora/processed/docs_n.csv", locale = readr::locale(encoding = "UTF-8"))
docs_m <- read_csv("data/corpora/processed/docs_m.csv", locale = readr::locale(encoding = "UTF-8"))
docs_p <- read_csv("data/corpora/processed/docs_p.csv", locale = readr::locale(encoding = "UTF-8"))


# ---- 1.3 Import node attribute & co-occurrence graph data ----

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


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: WRANGLE DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 Join word count and node attribute data for each corpus ----
# -- filter by: consensus threshold (unique per corpus), percentile threshold (same across all corpora) -- while keeping all nodes that were not classified at all
words_a <- 
  node_freq_a %>% 
  left_join(graph_attr_a, by = "year") %>%
  left_join(node_attributes_a %>% select(-freq), by = c("node", "year")) %>%
  rename("total_docs" = "ndoc") %>%
  mutate(rel_freq = freq / total_docs) %>%
  filter((consensus_threshold==0.3 | is.na(consensus_threshold)) &  # filter consensus threshold to 0.3 for academic
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

words_all <- 
  bind_rows(words_a, words_n, words_m, words_p)

words_centralnodes <-
  words_all %>%
  filter(central_node==1)


# ---- 2.2 Identify word attributes across all words (for trend analyses) ----
# -- regardless of whether the word was a central node, non-central node, or not even included in the semantic network

word_attributes <- 
  bind_rows(words_a %>% filter(year%in%2017:2021), 
            words_n, 
            words_m, 
            words_p %>% mutate(corpus = "policy")) %>%
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


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: CALCULATE MEASURES FOR BUZZWORD TYPE ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
         
# ---- 3.1 FOCAL WORD MEASURE 1: Highest frequency ----

criteria_percentile <- 0.95


buzzwords_highestfreq<-
  words_centralnodes %>% filter(year%in%2017:2022 & !is.na(consensus_threshold)) %>%
  group_by(corpus, consensus_threshold, percentile_threshold, year) %>%
  mutate(placeholder = case_when(symbol_type=="placeholder" ~ 1, 
                                 TRUE ~ 0)) %>%
  select(node, corpus, consensus_threshold, percentile_threshold, year, placeholder, rel_freq) %>%
  filter(placeholder==1) %>%
  mutate(freq_percentile = percent_rank(rel_freq)) %>%
  ungroup() %>%
  filter(freq_percentile>=criteria_percentile) %>%
  arrange(corpus, desc(year), desc(freq_percentile))


# ---- 3.2 FOCAL WORD MEASURE 2: Biggest percent increase in frequency ----

buzzwords_biggestchange <-
  words_all %>% filter(year%in%2017:2022) %>% # want to keep all words for time lag analysis, then filter to only placeholders that were central nodes
  group_by(node, corpus) %>%
  arrange(year) %>%
  mutate(delta_freq = rel_freq - lag(rel_freq),
         year_before = lag(year)) %>%
  summarise(perc_change_delta_2021 = delta_freq[year==2021]/rel_freq[year==2020],
            perc_change_delta_2020 = delta_freq[year==2020]/rel_freq[year==2019],
            max_delta = max(delta_freq, na.rm = T),
            max_delta_year_lower = year_before[delta_freq==max_delta & !is.na(delta_freq)],
            max_delta_year_upper = year[delta_freq==max_delta & !is.na(delta_freq)],
            perc_change_max_delta = (rel_freq[year==max_delta_year_upper]-rel_freq[year==max_delta_year_lower])/rel_freq[year==max_delta_year_lower],
            min_year = min(year),
            max_year = max(year),
            highest_freq = max(rel_freq),
            highest_freq_year = max(year[rel_freq==highest_freq]),
            lowest_freq = min(rel_freq),
            lowest_freq_year = min(year[rel_freq==lowest_freq]),
            perc_change_lowest_highest = case_when(highest_freq_year>lowest_freq_year & !is.na(lowest_freq_year) ~ 
                                                     (highest_freq-lowest_freq)/lowest_freq,
                                                   highest_freq_year<lowest_freq_year & !is.na(lowest_freq_year) ~
                                                     (lowest_freq-highest_freq)/highest_freq),
            perc_change_total = ifelse(!is.na(min_year),
                                       (rel_freq[year==max_year]-rel_freq[year==min_year])/rel_freq[year==min_year],
                                       NA),
            absolute_change_total = (rel_freq[year==max_year]-rel_freq[year==min_year]),
            n_placeholder_years = length(node[central_node==1 & symbol_type=="placeholder" & !is.na(symbol_type)]),
            years_placeholder = paste0(unique(year[central_node==1 & symbol_type=="placeholder" & !is.na(symbol_type)]), collapse = ', ')) %>%
  filter(n_placeholder_years>0) %>%
  ungroup() %>%
  group_by(corpus) %>%
  mutate(perc_change_percentile = percent_rank(perc_change_total)) %>%
  filter(perc_change_delta_2021>0.5)



# ---- 3.3 FOCAL WORD MEASURE 3: Most conductive ----

buzzwords_mostconductive <-
  words_centralnodes %>% filter(year%in%2017:2022 & !is.na(consensus_threshold)) %>%
  group_by(corpus, consensus_threshold, percentile_threshold, year) %>%
  mutate(placeholder = case_when(symbol_type=="placeholder" & !is.na(symbol_type) ~ 1, 
                                 TRUE ~ 0)) %>%
  select(node, corpus, consensus_threshold, percentile_threshold, year, placeholder, conductivity) %>%
  filter(placeholder==1) %>%
  mutate(conductivity_percentile = percent_rank(conductivity)) %>%
  ungroup() %>%
  filter(conductivity_percentile>=criteria_percentile) %>%
  arrange(corpus, desc(year), desc(conductivity_percentile))

buzzwords_mostconductive %>% filter(corpus=="media") %>% ggplot() + geom_histogram(aes(x = conductivity))


# ---- 3.4 FOCAL WORD MEASURE 4: Longest lived ----

buzzwords_longestlived <-
  words_centralnodes %>% filter(year%in%2017:2022 & !is.na(consensus_threshold)) %>%
  group_by(node, corpus, consensus_threshold, percentile_threshold) %>%
  arrange(year) %>%
  mutate(placeholder = case_when(symbol_type=="placeholder" & !is.na(symbol_type) ~ 1,
                                 TRUE ~ 0),
         two_year_placeholder = case_when(placeholder==1 ~ lag(placeholder),
                                          TRUE ~ 0),
         three_year_placeholder = case_when(placeholder==1 & two_year_placeholder==1 ~ lag(two_year_placeholder),
                                            TRUE ~ 0 ),
         four_year_placeholder = case_when(placeholder==1 & two_year_placeholder==1 & three_year_placeholder==1 ~ lag(three_year_placeholder),
                                           TRUE ~ 0 ),
         five_year_placeholder = case_when(placeholder==1 & two_year_placeholder==1 & three_year_placeholder==1 & four_year_placeholder==1 ~ lag(four_year_placeholder),
                                           TRUE ~ 0 ),
         span = case_when(five_year_placeholder==1 ~ 5,
                          four_year_placeholder==1 & !is.na(four_year_placeholder) ~ 4, 
                          three_year_placeholder==1 & !is.na(three_year_placeholder) ~ 3,
                          two_year_placeholder==1 & !is.na(two_year_placeholder) ~ 2,
                          placeholder==1 & !is.na(placeholder) ~ 1,
                          TRUE ~ 0)) %>%
  summarise(n_placeholder_years = length(year[placeholder==1]),
            longest_span = max(span),
            min_placeholder_year = min(year[placeholder==1]),
            max_placeholder_year = max(year[placeholder==1])) %>%
  filter(n_placeholder_years>0)


buzzwords_freq_change_cond_longest <-
  buzzwords_highestfreq %>%
  full_join(buzzwords_mostconductive) %>%
  group_by(node, corpus, conductivity) %>%
  summarise(most_recent_conductivity_percentile = conductivity_percentile[year==max(year)],
            highest_conductivity_percentile = max(conductivity_percentile, na.rm = T),
            most_recent_freq_percentile = freq_percentile[year==max(year)],
            highest_freq_percentile = max(freq_percentile, na.rm = T),
            years_highfreq = length(year[!is.na(freq_percentile)]),
            years_highcond = length(year[!is.na(conductivity_percentile)]),
            years_highfreq_highcond = length(year[!is.na(freq_percentile) & !is.na(conductivity_percentile)])) %>%
  full_join(buzzwords_longestlived) %>%
  full_join(buzzwords_biggestchange) %>%
  ungroup()
  

# ---- 3.5 FOCAL WORD MEASURE 5: Institution-spanning ----

buzzwords_acrossinstitutions <-
  buzzwords_freq_change_cond_longest %>%
  group_by(node) %>%
  summarise(n_corpora = length(corpus),
            corpus_1 = corpus[1],
            corpus_2 = ifelse(n_corpora>1, corpus[2], NA),
            corpus_3 = ifelse(n_corpora>2, corpus[3], NA),
            corpus_4 = ifelse(n_corpora>3, corpus[4], NA),
            corpus_5 = ifelse(n_corpora>4, corpus[5], NA))


# ---- TEST regression model looking for correlation between corpus/year and 
model_data <-
  words_centralnodes %>%
  mutate(placeholder = case_when(symbol_type=="placeholder" & !is.na(symbol_type) ~ 1, 
                                               TRUE ~ 0)) %>%
  group_by(node) %>%
  arrange(year) %>%
  mutate(lastyear = case_when(lag(placeholder)==1 & !is.na(lag(placeholder)) ~ "Y", 
                              TRUE ~ "N")) %>%
  filter(year%in%2017:2021)


model <- glm(placeholder ~ corpus + lastyear + corpus:lastyear, data = model_data %>% filter(corpus%in%c("academic","media", "ngo")), family = "binomial")
summary(model)

model_probs <- exp(model$coefficients)/(1 + exp(model$coefficients))


# ---- Identifying final buzzword types (trending, ubiquitous, bridging) -- meeting at least 2 of the 6 criteria ----

buzzword_bytypecriteria <-
  buzzwords_freq_change_cond_longest %>%
  mutate(corpus_policy_consolidate = ifelse(corpus%in%c("IPBES", "UNCBD"), "policy", corpus),
         criteria_freq = case_when(!is.na(most_recent_freq_percentile) ~ 1, 
                                   TRUE ~ 0),
         criteria_cond = case_when(!is.na(most_recent_conductivity_percentile) ~ 1, 
                                   TRUE ~ 0),
         criteria_percchange = case_when(!is.na(perc_change_percentile) ~ 1, 
                                         TRUE ~ 0)) %>%
  group_by(node) %>%
  summarise(n_corpora = length(unique(corpus_policy_consolidate)),
            most_recent_year = max(max_placeholder_year)[1],
            corpora_included = paste0(unique(sort(corpus_policy_consolidate)), collapse = ', '),
            most_placeholder_years = max(n_placeholder_years)[1],
            corpus_most_placeholder_years = paste0(unique(corpus_policy_consolidate[n_placeholder_years==most_placeholder_years]), collapse = ', '),
            conductivity = max(conductivity, na.rm = T),
            criteria_recent = ifelse(most_recent_year>=2021, 1, 0),
            criteria_longspan = ifelse(max(longest_span)>2, 1, 0),
            criteria_span2institutions = ifelse(n_corpora>1, 1, 0),
            criteria_span3institutions = ifelse(n_corpora>2, 1, 0),
            criteria_freq = ifelse(max(criteria_freq)==1, 1, 0),
            criteria_cond = ifelse(max(criteria_cond)==1, 1, 0),
            criteria_percchange = ifelse(max(criteria_percchange)==1, 1, 0)) %>%
  mutate(bridge = rowSums(select(., criteria_span3institutions, criteria_cond)),
         ubiquitous = rowSums(select(., criteria_longspan, criteria_freq, criteria_span2institutions)),
         trending = rowSums(select(., criteria_recent, criteria_percchange))) %>%
  filter(bridge==2 | ubiquitous==3 | trending==2)

bridges <- buzzword_bytypecriteria %>% filter(bridge==2) %>% 
  mutate(n_corpora = factor(n_corpora, levels = c("1", "2", "3", "4")))

ubiquitous <- buzzword_bytypecriteria %>% filter(ubiquitous==3) %>%
  left_join(buzzwords_highestfreq %>% group_by(node) %>% summarise(max_rel_freq = max(rel_freq))) %>%
  arrange(n_corpora, corpora_included) %>%
  mutate(corpora_included = factor(corpora_included, unique(corpora_included), ordered = T),
         n_corpora = factor(n_corpora, levels = c("1", "2", "3", "4")))

trending <- buzzword_bytypecriteria %>% filter(trending==2) %>% 
  left_join(buzzwords_biggestchange %>% group_by(node) %>% summarise(max_perc_change = max(perc_change_delta_2021))) %>%
  arrange(n_corpora, corpora_included) %>%
  mutate(corpora_included = factor(corpora_included, unique(corpora_included), ordered = T),
         n_corpora = factor(n_corpora, levels = c("1", "2", "3", "4")))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: PLOTS AND TABLES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- Top 30 buzzwords by institution, most recent year ----

academic_top30_mostrecent_plot <-
  words_centralnodes %>%
  filter(year==2021 &
           corpus=="academic" &
           symbol_type=="placeholder") %>%
  arrange(desc(rel_freq)) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  slice_head(n = 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 0.65),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "Academic, 2021") +
  lollipop.plot.theme + lollipop.legend.guide

ngo_top30_mostrecent_plot <-
  words_centralnodes %>%
  filter(year==2021 &
           corpus=="ngo" &
           symbol_type=="placeholder") %>%
  arrange(desc(rel_freq)) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  slice_head(n = 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 0.65),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "NGO, 2021") +
  lollipop.plot.theme + lollipop.legend.guide

media_top30_mostrecent_plot <-
  words_centralnodes %>%
  filter(year==2021 &
           corpus=="media" &
           symbol_type=="placeholder") %>%
  arrange(desc(rel_freq)) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  slice_head(n = 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 0.65),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "Media, 2021") +
  lollipop.plot.theme + lollipop.legend.guide


ipbes_top30_mostrecent_plot <-
  words_centralnodes %>%
  filter(year==2019 &
           corpus=="IPBES" &
           symbol_type=="placeholder") %>%
  arrange(desc(rel_freq)) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  slice_head(n = 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 0.65),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "IPBES, 2019") +
  lollipop.plot.theme + lollipop.legend.guide


uncbd_top30_mostrecent_plot <-
  words_centralnodes %>%
  filter(year==2022 &
           corpus=="UNCBD" &
           symbol_type=="placeholder") %>%
  arrange(desc(rel_freq)) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  slice_head(n = 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 0.65),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "UNCBD, 2021") +
  lollipop.plot.theme + lollipop.legend.guide

top30_byinstitution_mostrecent_arranged <-
  plot_list(richtext_grob(text = "**Top 30 Buzzwords**",
                          x = unit(33, "pt"), gp = gpar(fontsize = 20), hjust = 0),
            richtext_grob(text = "_Most recent & frequent per institution_", 
                          x = unit(33, "pt"), gp = gpar(fontsize = 14), hjust = 0),
            grid.arrange(academic_top30_mostrecent_plot, ngo_top30_mostrecent_plot, media_top30_mostrecent_plot,
                         ipbes_top30_mostrecent_plot, uncbd_top30_mostrecent_plot,
                         ncol = 3, layout_matrix = rbind(c(1, 1, 2, 2, 3, 3), c(NA, 4, 4, 5, 5, NA))),
            ncol = 1, heights = c(0.03, 0.06, 1))

# -- export 

png("data/outputs/figures/top30_buzzwords_byinstitution.png",
    units = "in", height = 11, width = 12, res = 400)
grid.newpage()
grid.draw(top30_byinstitution_mostrecent_arranged)
dev.off()



# ---- plot buzzword types ----

bridges_plot <- 
  bridges %>% arrange(conductivity) %>% mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  ggplot() +
  geom_bar(aes(x = conductivity, y = node, fill = n_corpora), stat = "identity") +
  scale_fill_manual(name = "Number institution(s)\nidentified as a buzzword",
                    values = c("#6699CC", "#332288", "#44AA99", "#CC6677"),
                    drop = FALSE) +
  scale_x_continuous(expand = expansion(c(0, 0.05))) +
  labs(x = "Maximum conductivity\n(across all institutions and years)", y = "", title = "Bridging Buzzwords", 
       subtitle = "Criteria: conductivity in 95th percentile") +
  focalword.plot.theme + theme(plot.margin = unit(c(1,0.1,0.1,0.1), "cm"),
                               legend.position = "bottom") + 
  guides(fill = guide_legend(ncol = 4, title.position = "left"))

ubiquitous_plot <- 
  ubiquitous %>% arrange(max_rel_freq, desc(node)) %>% mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  ggplot() +
  geom_bar(aes(x = max_rel_freq, y = node, fill = n_corpora), stat = "identity", show.legend = FALSE) +
  scale_fill_manual(name = "Number institution(s)\nidentified as a buzzword",
                    values = c("#6699CC", "#332288", "#44AA99", "#CC6677"), 
                    drop = FALSE) +
  scale_x_continuous(expand = expansion(c(0, 0.05)),
                     breaks = seq(0, 0.5, by = 0.1)) +
  labs(x = "Maximum relative document frequency\n(across all institutions and years)", y = "", title = "Ubiquitous Buzzwords", 
       subtitle = "Criteria: frequency in 95th percentile") +
  focalword.plot.theme + 
  theme(plot.margin = unit(c(1,0.1,0.1,0.1), "cm"),
        legend.position = "bottom") + 
  guides(fill = guide_legend(ncol = 4, title.position = "left"))

trending_plot <- 
  trending %>% arrange(max_perc_change, desc(node)) %>% mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  ggplot() +
  geom_bar(aes(x = max_perc_change, y = node, fill = n_corpora), stat = "identity", show.legend = FALSE) +
  scale_fill_manual(name = "Number institution(s)\nidentified as a buzzword",
                    values = c("#6699CC", "#332288", "#44AA99", "#CC6677"), 
                    drop = FALSE) +
  scale_x_continuous(expand = expansion(c(0, 0.05)),
                     labels = scales::percent_format(),
                     breaks = seq(0, 6, by = 1)) +
  labs(x = "Maximum percent change between 2020 and 2021\n(across all institutions)", y = "", title = "Trending Buzzwords", 
       subtitle = "Criteria: 50%+ increase between 2020-2021") +
  focalword.plot.theme + 
  theme(plot.margin = unit(c(1,0.1,0.1,0.1), "cm"),
        legend.position = "bottom") + 
  guides(fill = guide_legend(ncol = 4, title.position = "left"))

# arrange all three into one plot

sample_legend_plot <-
  trending %>% 
  ggplot() +
  geom_bar(aes(x = max_perc_change, y = node, fill = n_corpora), stat = "identity") +
  scale_fill_manual(name = "Number institution(s)\nidentified as a buzzword",
                    values = c("#6699CC", "#332288", "#44AA99", "#CC6677"),
                    drop = FALSE) +
  focalword.plot.theme + theme(legend.position = "bottom") + 
  guides(fill = guide_legend(ncol = 4, title.position = "left"))

sample_legend <- get_legend(sample_legend_plot)

three_types_arranged <-
  grid.arrange(ubiquitous_plot, trending_plot, bridges_plot,
               layout_matrix = matrix(c(1, 1, 2, 2, 4, 3, 3, 4), nrow = 2, byrow = TRUE),
               heights = c(1, 1))


# -- export

dir.create("data/outputs/figures/focal_words")
output_dir <- "data/outputs/figures/focal_words/"

png(paste(output_dir, "bridging_95perc_0.5t.png", sep = ""),
    units = "in", height = 9.5, width = 6, res = 400)
grid.newpage()
grid.draw(bridges_plot)
dev.off()

png(paste(output_dir, "ubiquitous_95perc_0.5t.png", sep = ""),
    units = "in", height = 9, width = 7, res = 400)
grid.newpage()
grid.draw(ubiquitous_plot)
dev.off()

png(paste(output_dir, "trending_0.5delt_0.5t.png", sep = ""),
    units = "in", height = 9, width = 7, res = 400)
grid.newpage()
grid.draw(trending_plot)
dev.off()

png("data/outputs/figures/buzzwords_threetype_95perc.png",
    units = "in", height = 16, width = 12, res = 400)
grid.newpage()
grid.draw(three_types_arranged)
dev.off()



# ---- Top 30 standard symbols by institution, most recent year ----

academic_top30_mostrecent_standard_plot <-
  words_centralnodes %>%
  filter(year==2021 &
           corpus=="academic" &
           symbol_type=="standard") %>%
  arrange(desc(rel_freq)) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  slice_head(n = 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 0.9),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "Academic, 2021") +
  lollipop.plot.theme + lollipop.legend.guide

ngo_top30_mostrecent_standard_plot <-
  words_centralnodes %>%
  filter(year==2021 &
           corpus=="ngo" &
           symbol_type=="standard") %>%
  arrange(desc(rel_freq)) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  slice_head(n = 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 0.9),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "NGO, 2021") +
  lollipop.plot.theme + lollipop.legend.guide

media_top30_mostrecent_standard_plot <-
  words_centralnodes %>%
  filter(year==2021 &
           corpus=="media" &
           symbol_type=="standard") %>%
  arrange(desc(rel_freq)) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  slice_head(n = 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 0.9),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "Media, 2021") +
  lollipop.plot.theme + lollipop.legend.guide


ipbes_top30_mostrecent_standard_plot <-
  words_centralnodes %>%
  filter(year==2019 &
           corpus=="IPBES" &
           symbol_type=="standard") %>%
  arrange(desc(rel_freq)) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  slice_head(n = 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 0.9),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "IPBES, 2019") +
  lollipop.plot.theme + lollipop.legend.guide


uncbd_top30_mostrecent_standard_plot <-
  words_centralnodes %>%
  filter(year==2022 &
           corpus=="UNCBD" &
           symbol_type=="standard") %>%
  arrange(desc(rel_freq)) %>%
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  slice_head(n = 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 0.9),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "UNCBD, 2021") +
  lollipop.plot.theme + lollipop.legend.guide

top30_byinstitution_mostrecent_standard_arranged <-
  plot_list(richtext_grob(text = "**Top 30 Standard Symbols**",
                          x = unit(33, "pt"), gp = gpar(fontsize = 20), hjust = 0),
            richtext_grob(text = "_Most recent & frequent per institution_", 
                          x = unit(33, "pt"), gp = gpar(fontsize = 14), hjust = 0),
            grid.arrange(academic_top30_mostrecent_standard_plot, ngo_top30_mostrecent_standard_plot, media_top30_mostrecent_standard_plot,
                         ipbes_top30_mostrecent_standard_plot, uncbd_top30_mostrecent_standard_plot,
                         ncol = 3, layout_matrix = rbind(c(1, 1, 2, 2, 3, 3), c(NA, 4, 4, 5, 5, NA))),
            ncol = 1, heights = c(0.03, 0.06, 1))

# -- export 

png("data/outputs/figures/top30_standard_byinstitution.png",
    units = "in", height = 11, width = 14, res = 400)
grid.newpage()
grid.draw(top30_byinstitution_mostrecent_standard_arranged)
dev.off()

