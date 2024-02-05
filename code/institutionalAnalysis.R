
# code: Institutional level comparisons and analyses (network differences, buzzwords)

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE LIBRARIES, FUNCTIONS, DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- source ----

# SOURCE dependencies: identifyBuzzwords.R script (will be renamed from identifyFocalWords.R)


# ---- import additional libraries ----

pacman::p_load(ggVennDiagram, venneuler, aplot, gridtext, grid, gridExtra)



# ---- function ----

yearsCompare <- function(input_suffix){
  assign(paste("years_compare_", input_suffix, sep = ""),
         get(paste("node_attributes_", input_suffix, sep = "")) %>%
           group_by(year, consensus_threshold, percentile_threshold) %>%
           summarise(n_nodes = length(node),
                     n_centralnodes = length(node[central_node==1]),
                     avg_conductivity = mean(conductivity[central_node==1]),
                     avg_degree = mean(degree[central_node==1]),
                     avg_consensus = mean(consensus[central_node==1], na.rm = T),
                     sd_consensus = sd(consensus[central_node==1], na.rm = T),
                     n_consensus_na = length(consensus[central_node==1 & is.na(consensus)]),
                     n_placeholders = length(node[central_node==1 & symbol_type=="placeholder" & !is.na(symbol_type)]),
                     n_buzzwords = length(node[central_node==1 & symbol_type=="buzzword"  & !is.na(symbol_type)]),
                     n_standard = length(node[central_node==1 & symbol_type=="standard"  & !is.na(symbol_type)]),
                     n_ordinary = length(node[central_node==1 & symbol_type=="ordinary"  & !is.na(symbol_type)]),
                     n_emblem = length(node[central_node==1 & symbol_type=="emblem"  & !is.na(symbol_type)]),
                     n_allusion = length(node[central_node==1 & symbol_type=="allusion"  & !is.na(symbol_type)]),
                     n_factoid = length(node[central_node==1 & symbol_type=="factoid"  & !is.na(symbol_type)]),
                     n_stereotype = length(node[central_node==1 & symbol_type=="stereotype"  & !is.na(symbol_type)]),
                     n_nosymbol = length(node[central_node==1 & is.na(symbol_type)]),
                     placeholders_1_10 = list(node[central_node==1 & symbol_type=="placeholder"  & !is.na(symbol_type)][1:10]),
                     buzzwords_1_10 = list(node[central_node==1 & symbol_type=="buzzword"  & !is.na(symbol_type)][1:10]),
                     standard_1_10 = list(node[central_node==1 & symbol_type=="standard"  & !is.na(symbol_type)][1:10])),
         envir = .GlobalEnv)
}


# ---- semantic networks, per institution per year ----

graph_attr <- 
  graph_attr_a %>%
  bind_rows(graph_attr_n) %>%
  bind_rows(graph_attr_m %>% mutate(corpus="media")) %>%
  bind_rows(graph_attr_p)



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: INSTITUTIONAL NETWORK COMPARISONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- average network measures, per institution (across all years) ----

yearsCompare(input_suffix = "a")
yearsCompare(input_suffix = "n")
yearsCompare(input_suffix = "m") 
yearsCompare(input_suffix = "p")

years_compare <-
  bind_rows(years_compare_a %>% 
              filter(consensus_threshold==0.3 & percentile_threshold==0.5 & year%in%c(2017:2021)) %>%
              mutate(corpus="academic"),
            years_compare_n %>%
              filter(consensus_threshold==0.75 & percentile_threshold==0.5) %>%
              mutate(corpus="ngo"),
            years_compare_m %>%
              filter(consensus_threshold==0.5 & percentile_threshold==0.5 & !is.na(year)) %>%
              mutate(corpus="media"),
            years_compare_p %>%
              filter(consensus_threshold==0.75 & percentile_threshold==0.5 & year==2019) %>%
              mutate(corpus="IPBES"),
            years_compare_p %>%
              filter(consensus_threshold==0.75 & percentile_threshold==0.5 & year==2022) %>%
              mutate(corpus="UNCBD")) %>% 
  ungroup()

network_avgs <-
  years_compare %>%
  group_by(corpus, consensus_threshold) %>%
  summarise(avg_degree = mean(avg_degree),
            avg_conductivity = mean(avg_conductivity),
            avg_consensus = mean(avg_consensus),
            avg_nodes = mean(n_nodes),
            avg_centralnodes = mean(n_centralnodes),
            avg_prop_placeholders = mean(n_placeholders) / avg_centralnodes)


# --- average word classification breakdown, per institution (across all years) ----

avg_placeholder_length_a <-
  node_attributes_a %>%
  filter(consensus_threshold==0.3 & percentile_threshold==0.5 & 
           symbol_type=="placeholder" & year%in%c(2017:2021) &
           central_node==1) %>%
  group_by(node) %>%
  summarise(n_years_placeholder = length(node)) %>%
  ungroup() %>%
  summarise(corpus = "academic",
            avg_years_placeholder = mean(n_years_placeholder),
            n_unique_placeholders = length(node))

avg_placeholder_length_n <-
  node_attributes_n %>%
  filter(consensus_threshold==0.75 & percentile_threshold==0.5 & 
           symbol_type=="placeholder" & central_node==1) %>%
  group_by(node) %>%
  summarise(n_years_placeholder = length(node)) %>%
  ungroup() %>%
  summarise(corpus = "ngo",
            avg_years_placeholder = mean(n_years_placeholder),
            n_unique_placeholders = length(node))

avg_placeholder_length_m <-
  node_attributes_m %>%
  filter(consensus_threshold==0.5 & percentile_threshold==0.5 & 
           symbol_type=="placeholder" & central_node==1) %>%
  group_by(node) %>%
  summarise(n_years_placeholder = length(node)) %>%
  ungroup() %>%
  summarise(corpus = "media",
            avg_years_placeholder = mean(n_years_placeholder),
            n_unique_placeholders = length(node))

avg_placeholder_length_p_ipbes <-
  node_attributes_p %>%
  filter(consensus_threshold==0.75 & percentile_threshold==0.5 & 
           symbol_type=="placeholder" & year==2019 & 
           central_node==1) %>%
  summarise(corpus = "ipbes",
            n_unique_placeholders = length(node))

avg_placeholder_length_p_uncbd <-
  node_attributes_p %>%
  filter(consensus_threshold==0.75 & percentile_threshold==0.5 & 
           symbol_type=="placeholder" & year==2022 &
           central_node==1) %>%
  summarise(corpus = "uncbd",
            n_unique_placeholders = length(node))


avg_placeholder_length_bycorpus <-
  bind_rows(avg_placeholder_length_a, avg_placeholder_length_n, avg_placeholder_length_m, 
            avg_placeholder_length_p_ipbes, avg_placeholder_length_p_uncbd)


# ---- placeholders by year, and across institutions ----
# -- number per year that are unique to the institution, span multiple institutions, etc.

placeholder_byyear <-
  words_centralnodes %>% filter(symbol_type=="placeholder" & year%in%c(2017:2022)) %>%
  mutate(corpus = case_when(corpus=="IPBES" | corpus=="UNCBD" ~ "policy",
                            TRUE ~ corpus)) %>%
  group_by(year, node) %>%
  summarise(n_corpus = length(node),
            academic = length(corpus[corpus=="academic"]),
            ngo = length(corpus[corpus=="ngo"]),
            media = length(corpus[corpus=="media"]),
            policy = length(corpus[corpus=="policy"]))
  

# colnames_to_remove <- c("ngo_1_1_0_0", "ngo_1_1_0_1", "ngo_1_1_1_0", "media_1_0_1_0", "media_1_1_1_0", "media_0_1_1_0", "media_0_1_1_1", 
#                          "policy_1_0_0_1", "policy_1_1_0_1", "policy_0_1_0_1", "policy_0_0_1_1", "policy_0_1_1_1")
# 
# full_colnames_df <- data.frame(matrix(nrow = 0, ncol = 15)) 
# colnames(full_colnames_df) <- 
#   c("year", "academic_1_0_0_0" ,"academic_1_1_0_0", "academic_1_0_1_0", "academic_1_0_0_1", "academic_1_1_1_0", 
#     "academic_1_1_0_1", "academic_1_0_1_1", "ngo_0_1_0_0", "ngo_0_1_1_0", "ngo_0_1_0_1", "ngo_0_1_1_1", 
#     "media_0_0_1_0", "media_0_0_1_1", "policy_0_0_0_1")
# 
# placeholder_allcategories <-
#   placeholder_byyear %>%
#   pivot_wider(id_cols = "year", values_from = c("academic", "ngo", "media", "policy"), values_fn = sum,
#               names_from = c("academic", "ngo", "media", "policy")) %>%
#   select(-where(~max(.x, na.rm = T)==0)) %>%
#   select(!contains(colnames_to_remove)) %>%
#   bind_rows(full_colnames_df) %>%
#   rename("academic_x_x_x" = "academic_1_0_0_0",
#          "academic_ngo_x_x" = "academic_1_1_0_0",
#          "academic_x_media_x" = "academic_1_0_1_0",
#          "academic_x_x_policy" = "academic_1_0_0_1",
#          "academic_ngo_media_x" = "academic_1_1_1_0",
#          "academic_ngo_x_policy" = "academic_1_1_0_1",
#          "academic_x_media_policy" = "academic_1_0_1_1",
#          "x_ngo_x_x" = "ngo_0_1_0_0",
#          "x_ngo_media_x" = "ngo_0_1_1_0",
#          "x_ngo_x_policy" = "ngo_0_1_0_1",
#          "x_ngo_media_policy" = "ngo_0_1_1_1",
#          "x_x_media_x" = "media_0_0_1_0",
#          "x_x_media_policy" = "media_0_0_1_1",
#          "x_x_x_policy" = "policy_0_0_0_1") %>%
#   ungroup() %>%
#   mutate(academic_total = rowSums(.[grep("academic", names(.))], na.rm = T),
#          ngo_total = rowSums(.[grep("ngo", names(.))], na.rm = T),
#          media_total = rowSums(.[grep("media", names(.))], na.rm = T),
#          policy_total = rowSums(.[grep("policy", names(.))], na.rm = T),
#          prop_academic_only = academic_x_x_x / academic_total,
#          prop_ngo_only = x_ngo_x_x / ngo_total,
#          prop_media_only = x_x_media_x / media_total,
#          prop_policy_only = x_x_x_policy / policy_total)
# 
# avg_placeholder_overlap <-
#   placeholder_allcategories %>% filter(year<2022) %>%
#   summarise(prop_academic_only = mean(prop_academic_only, na.rm = T),
#             prop_ngo_only = mean(prop_ngo_only, na.rm = T),
#             prop_media_only = mean(prop_media_only, na.rm = T),
#             prop_policy_only = mean(prop_policy_only, na.rm = T))
# 
# 
# placeholder_allcategories <-
#   placeholder_byyear %>%
#   group_by(year) %>%
#   summarise(academic = length(node[n_corpus==1 & corpus_1=="academic"]),
#             ngo = length(node[n_corpus==1 & corpus_1=="ngo"]),
#             media = length(node[n_corpus==1 & corpus_1=="media"]),
#             policy = length(node[n_corpus==1 & corpus_1=="policy"]),
#             academic_ngo = length(node[n_corpus==2 & corpus_1=="academic" & corpus_2=="ngo"]),
#             academic_media = length(node[n_corpus==2 & corpus_1=="academic" & corpus_2=="media"]),
#             academic_policy = length(node[n_corpus==2 & corpus_1=="academic" & corpus_2=="policy"]),
#             ngo_media = length(node[n_corpus==2 & corpus_1=="ngo" & corpus_2=="media"]),
#             ngo_policy = length(node[n_corpus==2 & corpus_1=="ngo" & corpus_2=="policy"]),
#             media_policy = length(node[n_corpus==2 & corpus_1=="media" & corpus_2=="policy"]),
#             academic_ngo_media = length(node[n_corpus==3 & corpus_3=="media"]),
#             academic_ngo_policy = length(node[n_corpus==3 & corpus_2=="ngo" & corpus_3=="policy"]),
#             academic_media_policy = length(node[n_corpus==3 & corpus_2=="media"]),
#             ngo_media_policy = length(node[n_corpus==3 & corpus_3=="media"]),
#             total_academic = academic + academic_ngo + academic_media + academic_ngo_media,
#             total_ngo = ngo + academic_ngo + ngo_media + academic_ngo_media,
#             total_media = media + academic_media + ngo_media + academic_ngo_media) %>%
#   mutate(prop_academic_only = academic / total_academic,
#          prop_academic_ngo = academic_ngo / total_academic,
#          prop_academic_media = academic_media / total_academic,
#          prop_ngo_only = ngo / total_ngo,
#          prop_ngo_academic = academic_ngo / total_ngo,
#          prop_ngo_media = ngo_media / total_ngo,
#          prop_media_only = media / total_media,
#          prop_media_academic = academic_media / total_media,
#          prop_media_ngo = ngo_media / total_media)
# 
# 
# placeholder_2021 <-
#   focalword_attributes %>% filter(year==2021 & symbol_type=="placeholder") %>%
#   select(node, corpus)
# 
# placeholder_2020 <-
#   focalword_attributes %>% filter(year==2020 & symbol_type=="placeholder") %>%
#   select(node, corpus)
# 
# placeholder_2019 <-
#   focalword_attributes %>% filter(year==2019 & symbol_type=="placeholder") %>%
#   select(node, corpus)
# 
# placeholder_2019_overlap <- 
#   placeholder_byyear %>%
#   filter(year==2019)
# 
# 
# placeholder_2021_list <- 
#   list(aca = placeholder_2021$node[placeholder_2021$corpus=="academic"],
#        ngo = placeholder_2021$node[placeholder_2021$corpus=="ngo"],
#        media = placeholder_2021$node[placeholder_2021$corpus=="media"])
# 
# 
# venn_placeholder_2021 <-
#   ggVennDiagram(x = placeholder_2021_list,
#                 set_color = "#303030", edge_size = 0) + 
#   scale_fill_gradient(low = "#F4FAFE", high = "#4981BF",
#                       guide = "none") +
#   scale_color_manual(values = c("#303030", "#303030", "#303030"))
# 
# venn_placeholder_2021 <-
#   plot(venneuler(placeholder_2021))
# 
# venn_placeholder_2020 <-
#   plot(venneuler(placeholder_2020))
# 
# venn_placeholder_2019 <-
#   plot(venneuler(placeholder_2019))
#                                   

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: INSTITUTIONAL BUZZWORD COMPARISONS (AND OVERLAP) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- pairwise analyses and institution-specific buzzwords ----

top_buzzwords_bycorpus_byfreq <- 
  words_centralnodes %>% 
  filter(percentile_threshold==0.5 &
           symbol_type=="placeholder" & 
           ((corpus%in%c("academic", "ngo", "media") & year==2021) |
              (corpus=="IPBES" & year==2019) |
              (corpus=="UNCBD" & year==2022))) %>%
  group_by(node) %>%
  summarise(num_corpora = length(unique(corpus)),
            which_corpora = paste0(list(unique(corpus))),
            max_rel_freq = max(rel_freq),
            which_corpus_freq = corpus[rel_freq==max_rel_freq],
            max_conductivity = max(conductivity),
            which_corpus_cond = corpus[conductivity==max_conductivity],
            academic = case_when(which_corpora=="academic" ~ 1, 
                                 TRUE ~ 0),
            ngo = case_when(which_corpora=="ngo" ~ 1, 
                            TRUE ~ 0),
            media = case_when(which_corpora=="media" ~ 1, 
                              TRUE ~ 0),
            ipbes = case_when(which_corpora=="IPBES" ~ 1, 
                              TRUE ~ 0),
            uncbd = case_when(which_corpora=="UNCBD" ~ 1, 
                              TRUE ~ 0),
            academic_ngo = case_when(grepl("academic", which_corpora) & grepl("ngo", which_corpora) ~ 1, 
                                     TRUE ~ 0),
            academic_media = case_when(grepl("academic", which_corpora) & grepl("media", which_corpora) ~ 1, 
                                       TRUE ~ 0),
            academic_ipbes = case_when(grepl("academic", which_corpora) & grepl("IPBES", which_corpora) ~ 1, 
                                       TRUE ~ 0),
            academic_uncbd = case_when(grepl("academic", which_corpora) & grepl("UNCBD", which_corpora) ~ 1, 
                                       TRUE ~ 0),
            ngo_media = case_when(grepl("ngo", which_corpora) & grepl("media", which_corpora) ~ 1, 
                                  TRUE ~ 0),
            ngo_ipbes = case_when(grepl("ngo", which_corpora) & grepl("IPBES", which_corpora) ~ 1, 
                                  TRUE ~ 0),
            ngo_uncbd = case_when(grepl("ngo", which_corpora) & grepl("UNCBD", which_corpora) ~ 1, 
                                  TRUE ~ 0),
            media_ipbes = case_when(grepl("media", which_corpora) & grepl("IPBES", which_corpora) ~ 1, 
                                    TRUE ~ 0),
            media_uncbd = case_when(grepl("media", which_corpora) & grepl("UNCBD", which_corpora) ~ 1, 
                                    TRUE ~ 0),
            ipbes_uncbd = case_when(grepl("IPBES", which_corpora) & grepl("UNCBD", which_corpora) ~ 1, 
                                    TRUE ~ 0))

top_buzzwords_overlap_bycorpus <-
  top_buzzwords_bycorpus_byfreq %>%
  group_by(which_corpora) %>%
  summarise(n_unique_buzzwords = length(node),
            perc_all_unique_buzzwords = n_unique_buzzwords / length(top_buzzwords_bycorpus_byfreq$node)) %>%
  left_join(years_compare %>% filter(year==2021 | (corpus%in%c("IPBES", "UNCBD"))) %>% select(corpus, n_placeholders, n_centralnodes), 
            by = c("which_corpora" = "corpus")) %>%
  mutate(perc_buzzwords_institution_specific = n_unique_buzzwords/n_placeholders)


top_buzzwords_pairwise_overlap <-
  top_buzzwords_bycorpus_byfreq %>%
  summarise(across(c("academic_ngo", "academic_media", "academic_ipbes", "academic_uncbd",
                     "ngo_media", "ngo_ipbes", "ngo_uncbd", "media_ipbes", "media_uncbd", "ipbes_uncbd"), 
                   ~ sum(.x))) %>%
  pivot_longer(everything(), names_to = "pair", values_to = "n_overlap") %>%
  mutate(perc_all_unique_buzzwords = n_overlap / length(top_buzzwords_bycorpus_byfreq$node))


# ---- plot buzzwords present in at least three institutions ----

institution_spanning_buzzwords_plot <-
  top_buzzwords_bycorpus_byfreq %>% filter(num_corpora>=3) %>%
  arrange(desc(max_rel_freq)) %>% 
  mutate(node = factor(node, levels = unique(node), ordered = T),
         which_corpora = stringr::str_remove(which_corpora, 'c\\('),
         which_corpora = stringr::str_remove_all(which_corpora, '\\"'),
         which_corpora = stringr::str_remove(which_corpora, '\\)'),
         which_corpora = stringr::str_replace(which_corpora, "academic", "Academic"),
         which_corpora = stringr::str_replace(which_corpora, "ngo", "NGO"),
         which_corpora = stringr::str_replace(which_corpora, "media", "Media"),
         which_corpora = stringr::str_replace(which_corpora, "ipbes", "IPBES"),
         which_corpora = stringr::str_replace(which_corpora, "uncbd", "UNCBD")) %>%
  head(., 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = max_rel_freq, y = node, yend = node, color = which_corpora),
               stat = "identity") +
  geom_point(aes(x = max_rel_freq, y = node, color = which_corpora), 
             size = 4,
             fill = NA) +
  scale_color_manual(name = "Institutions",
                     values = c("#332288", "#DDCC77", "#44AA99", "#117733", "#CC6677", "#6699CC", "#661100")) +
  scale_x_continuous(expand = expansion(c(0, 0.05)),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents\n(from institution with highest frequency)", y = "",
       title = "Institution-Spanning Buzzwords",
       subtitle = "Across at least three institutions") +
  lollipop.plot.theme + lollipop.legend.guide + 
  guides(color = guide_legend(title.theme = element_text(face= "bold")))

# -- export

png("data/outputs/figures/institution_spanning_buzzwords.png",
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(institution_spanning_buzzwords_plot)
dev.off()


# ---- plot institution-specific buzzwords ----

academic_specific_buzzwords_plot <-
  top_buzzwords_bycorpus_byfreq %>% filter(which_corpora=="academic") %>%
  arrange(desc(max_rel_freq)) %>% 
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  head(., 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = max_rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = max_rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = expansion(c(0, 0)),
                     limits = c(0, 0.4),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "Academic",
       subtitle = "19 total") +
  lollipop.plot.theme + lollipop.legend.guide

ngo_specific_buzzwords_plot <-
  top_buzzwords_bycorpus_byfreq %>% filter(which_corpora=="ngo") %>%
  arrange(desc(max_rel_freq)) %>% 
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  head(., 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = max_rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = max_rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = expansion(c(0, 0)),
                     limits = c(0, 0.4),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "NGO",
       subtitle = "61 total") +
  lollipop.plot.theme + lollipop.legend.guide

media_specific_buzzwords_plot <-
  top_buzzwords_bycorpus_byfreq %>% filter(which_corpora=="media") %>%
  arrange(desc(max_rel_freq)) %>% 
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  head(., 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = max_rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = max_rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = expansion(c(0, 0)),
                     limits = c(0, 0.4),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "Media",
       subtitle = "110 total") +
  lollipop.plot.theme + lollipop.legend.guide

ipbes_specific_buzzwords_plot <-
  top_buzzwords_bycorpus_byfreq %>% filter(which_corpora=="IPBES") %>%
  arrange(desc(max_rel_freq)) %>% 
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  head(., 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = max_rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = max_rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = expansion(c(0, 0)),
                     limits = c(0, 0.4),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "IPBES",
       subtitle = "104 total") +
  lollipop.plot.theme + lollipop.legend.guide

uncbd_specific_buzzwords_plot <-
  top_buzzwords_bycorpus_byfreq %>% filter(which_corpora=="UNCBD") %>%
  arrange(desc(max_rel_freq)) %>% 
  mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
  head(., 30) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = max_rel_freq, y = node, yend = node),
               color = "#332288",
               stat = "identity") +
  geom_point(aes(x = max_rel_freq, y = node),
             color = "#332288", size = 4,
             fill = NA) +
  scale_x_continuous(expand = expansion(c(0, 0)),
                     limits = c(0, 0.4),
                     labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  labs(x = "% of documents", y = "",
       title = "UNCBD",
       subtitle = "17 total") +
  lollipop.plot.theme + lollipop.legend.guide

institution_specific_buzzwords_arranged <- 
  plot_list(richtext_grob(text = "**Institution-Specific Buzzwords**",
                          x = unit(33, "pt"), gp = gpar(fontsize = 20), hjust = 0),
            richtext_grob(text = "_Top 30 most frequent_", 
                          x = unit(33, "pt"), gp = gpar(fontsize = 14), hjust = 0),
            grid.arrange(academic_specific_buzzwords_plot, ngo_specific_buzzwords_plot, media_specific_buzzwords_plot,
                         ipbes_specific_buzzwords_plot, uncbd_specific_buzzwords_plot,
                         ncol = 3, layout_matrix = rbind(c(1, 1, 2, 2, 3, 3), c(NA, 4, 4, 5, 5, NA))),
            richtext_grob(text = "__Note:__ All comparisons are made using the following years of data: academic = 2021; NGO = 2021; media = 2021; IPBES = 2019; UNCBD = 2022", 
                          x = unit(33, "pt"),
                          gp = gpar(fontsize = 12), hjust = 0),
            ncol = 1, heights = c(0.03, 0.06, 1, 0.08))


# -- export

png("data/outputs/figures/institution_specific_buzzwords.png",
    units = "in", height = 11, width = 12, res = 400)
grid.newpage()
grid.draw(institution_specific_buzzwords_arranged)
dev.off()



# ---- plot percent pairwise overlap ----

academic_pairwise_overlap_plot <-
  top_buzzwords_pairwise_overlap %>%
  filter(grepl("academic", pair)) %>%
  mutate(pair = stringr::str_remove(pair, "academic"),
         pair = stringr::str_remove(pair, "_"),
         pair = case_when(pair=="media" ~ "Media", 
                          TRUE ~ toupper(pair)),
         pair = factor(pair, levels = unique(pair))) %>%
  ggplot() +
  geom_bar(aes(x = pair, y = perc_all_unique_buzzwords),
           stat = "identity", fill = "#332288") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0),
                     limits = c(0, 0.18)) +
  labs(title = "Academic Pairs", x = "", y = "% of all most recent buzzwords") +
  time.plot.theme

ngo_pairwise_overlap_plot <-
  top_buzzwords_pairwise_overlap %>%
  filter(grepl("ngo", pair)) %>%
  mutate(pair = stringr::str_remove(pair, "ngo"),
         pair = stringr::str_remove(pair, "_"),
         pair = case_when(pair=="media" ~ "Media", 
                          pair=="academic" ~ "Academic",
                          TRUE ~ toupper(pair)),
         pair = factor(pair, levels = unique(pair))) %>%
  ggplot() +
  geom_bar(aes(x = pair, y = perc_all_unique_buzzwords),
           stat = "identity", fill = "#44AA99") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0),
                     limits = c(0, 0.18)) +
  labs(title = "NGO Pairs", x = "", y = "% of all most recent buzzwords") +
  time.plot.theme

media_pairwise_overlap_plot <-
  top_buzzwords_pairwise_overlap %>%
  filter(grepl("media", pair)) %>%
  mutate(pair = stringr::str_remove(pair, "media"),
         pair = stringr::str_remove(pair, "_"),
         pair = case_when(pair=="academic" ~ "Academic",
                          TRUE ~ toupper(pair)),
         pair = factor(pair, levels = unique(pair))) %>%
  ggplot() +
  geom_bar(aes(x = pair, y = perc_all_unique_buzzwords),
           stat = "identity", fill = "#6699CC") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0),
                     limits = c(0, 0.18)) +
  labs(title = "Media Pairs", x = "", y = "% of all most recent buzzwords") +
  time.plot.theme

ipbes_pairwise_overlap_plot <-
  top_buzzwords_pairwise_overlap %>%
  filter(grepl("ipbes", pair)) %>%
  mutate(pair = stringr::str_remove(pair, "ipbes"),
         pair = stringr::str_remove(pair, "_"),
         pair = case_when(pair=="academic" ~ "Academic",
                          pair=="media" ~ "Media",
                          TRUE ~ toupper(pair)),
         pair = factor(pair, levels = unique(pair))) %>%
  ggplot() +
  geom_bar(aes(x = pair, y = perc_all_unique_buzzwords),
           stat = "identity", fill = "#CC6677") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0),
                     limits = c(0, 0.18)) +
  labs(title = "IPBES Pairs", x = "", y = "% of all most recent buzzwords") +
  time.plot.theme

uncbd_pairwise_overlap_plot <-
  top_buzzwords_pairwise_overlap %>%
  filter(grepl("uncbd", pair)) %>%
  mutate(pair = stringr::str_remove(pair, "uncbd"),
         pair = stringr::str_remove(pair, "_"),
         pair = case_when(pair=="academic" ~ "Academic",
                          pair=="media" ~ "Media",
                          TRUE ~ toupper(pair)),
         pair = factor(pair, levels = unique(pair))) %>%
  ggplot() +
  geom_bar(aes(x = pair, y = perc_all_unique_buzzwords),
           stat = "identity", fill = "#117733") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0),
                     limits = c(0, 0.18)) +
  labs(title = "UNCBD Pairs", x = "", y = "% of all most recent buzzwords") +
  time.plot.theme


pairwise_buzzwords_overlap_arranged <- 
  plot_list(richtext_grob(text = "**Pairwise Overlap of Buzzwords**",
                          x = unit(48, "pt"), gp = gpar(fontsize = 20), hjust = 0),
            richtext_grob(text = "_Pairings do not exclude other institutions from also using the buzzword_", 
                          x = unit(48, "pt"), gp = gpar(fontsize = 16), hjust = 0),
            grid.arrange(academic_pairwise_overlap_plot, ngo_pairwise_overlap_plot, media_pairwise_overlap_plot, 
                         ipbes_pairwise_overlap_plot, uncbd_pairwise_overlap_plot,
                         ncol = 3, layout_matrix = rbind(c(1, 1, 2, 2, 3, 3), c(NA, 4, 4, 5, 5, NA))),
            textGrob(label = "All comparisons are made using the following years of data: academic = 2021; NGO = 2021; media = 2021; IPBES = 2019; UNCBD = 2022", 
                          x = unit(48, "pt"),
                          gp = gpar(fontsize = 15), hjust = 0),
            ncol = 1, heights = c(0.04, 0.08, 1, 0.08))

# -- export

png("data/outputs/figures/pairwise_buzzwords_overlap.png",
    units = "in", height = 9, width = 14, res = 400)
grid.newpage()
grid.draw(pairwise_buzzwords_overlap_arranged)
dev.off()


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: TABLES FOR EXPORT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

institutional_buzzwords_table <-
  top_buzzwords_overlap_bycorpus %>% 
  filter(which_corpora%in%c("academic", "ngo", "media", "IPBES", "UNCBD")) %>% 
  mutate(perc_buzzwords_to_total = n_placeholders/n_centralnodes) %>%
  left_join(top_buzzwords_bycorpus_byfreq %>% 
              filter(which_corpora%in%c("academic", "ngo", "media", "IPBES", "UNCBD")) %>%
              group_by(which_corpora) %>%
              summarise(node_max_rel_freq = node[max_rel_freq==max(max_rel_freq)],
                        max_rel_freq = max(max_rel_freq)))
  
  
  