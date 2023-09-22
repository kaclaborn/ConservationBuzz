
pacman::p_load(ggVennDiagram, venneuler)

graph_attr <- 
  graph_attr_a %>%
  bind_rows(graph_attr_n) %>%
  bind_rows(graph_attr_m_nyt_filt %>% mutate(corpus="media")) %>%
  bind_rows(graph_attr_p)

# graph_attr_corpus_avgs <-
#   graph_attr %>%
#   group_by(corpus) %>%
#   summarise(avg_ndocs = ,
#             avg_nwords = ,
#             avg_)


placeholder_byyear <-
  focalword_attributes %>% filter(symbol_type=="placeholder" & year%in%c(2017:2022)) %>%
  group_by(year, node) %>%
  summarise(n_corpus = length(node),
            academic = length(corpus[corpus=="academic"]),
            ngo = length(corpus[corpus=="ngo"]),
            media = length(corpus[corpus=="media"]),
            policy = length(corpus[corpus=="policy"]))
  

colnames_to_remove <- c("ngo_1_1_0_0", "ngo_1_1_0_1", "ngo_1_1_1_0", "media_1_0_1_0", "media_1_1_1_0", "media_0_1_1_0", "media_0_1_1_1", 
                         "policy_1_0_0_1", "policy_1_1_0_1", "policy_0_1_0_1", "policy_0_0_1_1", "policy_0_1_1_1")

full_colnames_df <- data.frame(matrix(nrow = 0, ncol = 15)) 
colnames(full_colnames_df) <- 
  c("year", "academic_1_0_0_0" ,"academic_1_1_0_0", "academic_1_0_1_0", "academic_1_0_0_1", "academic_1_1_1_0", 
    "academic_1_1_0_1", "academic_1_0_1_1", "ngo_0_1_0_0", "ngo_0_1_1_0", "ngo_0_1_0_1", "ngo_0_1_1_1", 
    "media_0_0_1_0", "media_0_0_1_1", "policy_0_0_0_1")

placeholder_allcategories <-
  placeholder_byyear %>%
  pivot_wider(id_cols = "year", values_from = c("academic", "ngo", "media", "policy"), values_fn = sum,
              names_from = c("academic", "ngo", "media", "policy")) %>%
  select(-where(~max(.x, na.rm = T)==0)) %>%
  select(!contains(colnames_to_remove)) %>%
  bind_rows(full_colnames_df) %>%
  rename("academic_x_x_x" = "academic_1_0_0_0",
         "academic_ngo_x_x" = "academic_1_1_0_0",
         "academic_x_media_x" = "academic_1_0_1_0",
         "academic_x_x_policy" = "academic_1_0_0_1",
         "academic_ngo_media_x" = "academic_1_1_1_0",
         "academic_ngo_x_policy" = "academic_1_1_0_1",
         "academic_x_media_policy" = "academic_1_0_1_1",
         "x_ngo_x_x" = "ngo_0_1_0_0",
         "x_ngo_media_x" = "ngo_0_1_1_0",
         "x_ngo_x_policy" = "ngo_0_1_0_1",
         "x_ngo_media_policy" = "ngo_0_1_1_1",
         "x_x_media_x" = "media_0_0_1_0",
         "x_x_media_policy" = "media_0_0_1_1",
         "x_x_x_policy" = "policy_0_0_0_1") %>%
  ungroup() %>%
  mutate(academic_total = rowSums(.[grep("academic", names(.))], na.rm = T),
         ngo_total = rowSums(.[grep("ngo", names(.))], na.rm = T),
         media_total = rowSums(.[grep("media", names(.))], na.rm = T),
         policy_total = rowSums(.[grep("policy", names(.))], na.rm = T),
         prop_academic_only = academic_x_x_x / academic_total,
         prop_ngo_only = x_ngo_x_x / ngo_total,
         prop_media_only = x_x_media_x / media_total,
         prop_policy_only = x_x_x_policy / policy_total)

avg_placeholder_overlap <-
  placeholder_allcategories %>% filter(year<2022) %>%
  summarise(prop_academic_only = mean(prop_academic_only, na.rm = T),
            prop_ngo_only = mean(prop_ngo_only, na.rm = T),
            prop_media_only = mean(prop_media_only, na.rm = T),
            prop_policy_only = mean(prop_policy_only, na.rm = T))


placeholder_allcategories <-
  placeholder_byyear %>%
  group_by(year) %>%
  summarise(academic = length(node[n_corpus==1 & corpus_1=="academic"]),
            ngo = length(node[n_corpus==1 & corpus_1=="ngo"]),
            media = length(node[n_corpus==1 & corpus_1=="media"]),
            policy = length(node[n_corpus==1 & corpus_1=="policy"]),
            academic_ngo = length(node[n_corpus==2 & corpus_1=="academic" & corpus_2=="ngo"]),
            academic_media = length(node[n_corpus==2 & corpus_1=="academic" & corpus_2=="media"]),
            academic_policy = length(node[n_corpus==2 & corpus_1=="academic" & corpus_2=="policy"]),
            ngo_media = length(node[n_corpus==2 & corpus_1=="ngo" & corpus_2=="media"]),
            ngo_policy = length(node[n_corpus==2 & corpus_1=="ngo" & corpus_2=="policy"]),
            media_policy = length(node[n_corpus==2 & corpus_1=="media" & corpus_2=="policy"]),
            academic_ngo_media = length(node[n_corpus==3 & corpus_3=="media"]),
            academic_ngo_policy = length(node[n_corpus==3 & corpus_2=="ngo" & corpus_3=="policy"]),
            academic_media_policy = length(node[n_corpus==3 & corpus_2=="media"]),
            ngo_media_policy = length(node[n_corpus==3 & corpus_3=="media"]),
            total_academic = academic + academic_ngo + academic_media + academic_ngo_media,
            total_ngo = ngo + academic_ngo + ngo_media + academic_ngo_media,
            total_media = media + academic_media + ngo_media + academic_ngo_media) %>%
  mutate(prop_academic_only = academic / total_academic,
         prop_academic_ngo = academic_ngo / total_academic,
         prop_academic_media = academic_media / total_academic,
         prop_ngo_only = ngo / total_ngo,
         prop_ngo_academic = academic_ngo / total_ngo,
         prop_ngo_media = ngo_media / total_ngo,
         prop_media_only = media / total_media,
         prop_media_academic = academic_media / total_media,
         prop_media_ngo = ngo_media / total_media)


placeholder_2021 <-
  focalword_attributes %>% filter(year==2021 & symbol_type=="placeholder") %>%
  select(node, corpus)

placeholder_2020 <-
  focalword_attributes %>% filter(year==2020 & symbol_type=="placeholder") %>%
  select(node, corpus)

placeholder_2019 <-
  focalword_attributes %>% filter(year==2019 & symbol_type=="placeholder") %>%
  select(node, corpus)

placeholder_2019_overlap <- 
  placeholder_byyear %>%
  filter(year==2019)


placeholder_2021_list <- 
  list(aca = placeholder_2021$node[placeholder_2021$corpus=="academic"],
       ngo = placeholder_2021$node[placeholder_2021$corpus=="ngo"],
       media = placeholder_2021$node[placeholder_2021$corpus=="media"])


venn_placeholder_2021 <-
  ggVennDiagram(x = placeholder_2021_list,
                set_color = "#303030", edge_size = 0) + 
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF",
                      guide = "none") +
  scale_color_manual(values = c("#303030", "#303030", "#303030"))

venn_placeholder_2021 <-
  plot(venneuler(placeholder_2021))

venn_placeholder_2020 <-
  plot(venneuler(placeholder_2020))

venn_placeholder_2019 <-
  plot(venneuler(placeholder_2019))
                                  