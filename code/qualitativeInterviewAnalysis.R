
# code: Interview analysis (qualitative coding)

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT LIBRARIES, DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

pacman::p_load(tidyverse, grid, gridExtra)

source("code/source/plotThemes.R")

# ---- Import ----

open_ended_codes <- read_csv("data/corpora/transcripts/open_ended_questions/coded_segments_open_ended.csv", 
                             locale = readr::locale(encoding = "UTF-8")) %>%
  rename("doc_group" = "Document group",
         "doc" = "Document name",
         "code_long" = "Code",
         "segment" = "Segment",
         "created" = "Created") %>%
  mutate(parent_code = sub("\\\\.*", "", code_long),
         child_code = sub(".*\\\\", "", code_long),
         child_subcode = tolower(Comment),
         child_subcode = case_when(child_code=="purposely mislead" ~ "purposely mislead",
                                   TRUE ~ child_subcode),
         child_code = case_when(child_code=="purposely mislead" ~ "co-opt meaning, weaponize",
                                TRUE ~ child_code)) %>%
  select(-c(Comment, Color, `Modified by`, `Created by`, 
            `Modified`, Beginning, End, `Weight score`,
            Area, `Coverage %`))

validate_model_codes <- read_csv("data/corpora/transcripts/open_ended_questions/coded_segments_validating_model.csv", 
                             locale = readr::locale(encoding = "UTF-8")) %>%
  rename("doc_group" = "Document group",
         "doc" = "Document name",
         "code_long" = "Code",
         "segment" = "Segment",
         "created" = "Created") %>%
  mutate(parent_code = sub("\\\\.*", "", code_long),
         child_code = sub(".*\\\\", "", code_long),
         # define child subcode and context
         child_subcode = tolower(Comment) %>% 
           stringr::str_replace_all("suggestion -", "suggestion,") %>% 
           stringr::str_replace_all("agree -", "agree,"),
         subcode_context = stringr::str_split(child_subcode, ', ', simplify = TRUE)[,2],
         # clean child subcode and context
         child_subcode = stringr::str_split(child_subcode, ', ', simplify = TRUE)[,1],
         subcode_context = ifelse(subcode_context=="", NA, subcode_context),
         subcode_context = case_when(child_code=="purposely mislead" ~ "purposely mislead",
                                   TRUE ~ subcode_context),
         child_code = case_when(child_code=="purposely mislead" ~ "co-opt meaning, weaponize",
                                TRUE ~ child_code)) %>%
  select(-c(Comment, Color, `Modified by`, `Created by`, 
            `Modified`, Beginning, End, `Weight score`,
            Area, `Coverage %`))


interviewee_info <-
  data.frame(id = c("01", "02", "03", "04", "05", "06",
                    "07", "08", "09", "10", "11", "12",
                    "13", "14", "15", "16", "17"),
             org_type = c("policy", "academia", "media/comms", "policy", "media/comms", "ngo",
                          "academia", "ngo", "ngo", "ngo", "academia", "ngo",
                          "academia", "policy", "academia", "media/comms", "ngo")) %>%
  mutate(doc = paste0("transcript_", id, sep = ""))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: OPEN ENDED QUESTIONS ANALYSIS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

prop_open_ended_codes_byinterviewee <-
  open_ended_codes %>%
  select(doc, parent_code, child_code, child_subcode) %>%
  distinct() %>%
  filter(parent_code!="Examples") %>% 
  group_by(parent_code, child_code) %>%
  summarize(n = length(doc),
            prop = n / 17,
            subcode1 = unique(child_subcode[!is.na(child_subcode)])[1],
            n_subcode1 = length(doc[child_subcode==subcode1 & !is.na(child_subcode)]),
            prop_subcode1 = n_subcode1/17,
            subcode2 = unique(child_subcode[!is.na(child_subcode)])[2],
            n_subcode2 = length(doc[child_subcode==subcode2 & !is.na(child_subcode) & !is.na(subcode2)]),
            prop_subcode2 = n_subcode2/17)
  
prop_validate_model_byinterviewee <-
  validate_model_codes %>%
  filter(parent_code!="Examples" & 
           (grepl("agree", child_subcode, ignore.case = T) | grepl("neither", child_subcode, ignore.case = T)) & 
           !grepl("agreement", child_subcode)) %>% 
  mutate(agreement = case_when(grepl("disagree", child_subcode, ignore.case = T) ~ "disagree",
                               grepl("neither", child_subcode, ignore.case = T) ~ "didn't respond",
                               grepl("(sort of)", child_subcode, ignore.case = T) |
                                 grepl("(kind of)", child_subcode, ignore.case = T) ~ "agree (sort of)",
                               grepl("hesitation", subcode_context, ignore.case = T) ~ "agree (some hesitation)",
                               TRUE ~ "agree"),
         direction = case_when(grepl("both", subcode_context, ignore.case = T) | 
                                 grepl("increase or decrease", subcode_context, ignore.case = T) | 
                                 grepl("also could decrease", subcode_context, ignore.case = T) ~ "both",
                               grepl("context dependent", subcode_context, ignore.case = T) ~ "context dependent",
                               TRUE ~ "not specificed or agreed with suggestion")) %>%
  select(doc, parent_code, child_code, agreement, direction) %>%
  distinct() %>%
  group_by(parent_code, child_code, agreement) %>%
  summarize(n = length(unique(doc)),
            prop = n / 17,
            n_bidirectional = length(unique(doc[direction=="both"])),
            prop_bidirectional = n_bidirectional / 17,
            n_context = length(unique(doc[direction=="context dependent"])),
            prop_context = n_context / 17)

prop_suggested_codes_byinterviewee <-
  validate_model_codes %>%
  filter(parent_code!="Examples" & grepl("suggestion", child_subcode, ignore.case = T)) %>% 
  select(doc, parent_code, child_code, subcode_context) %>%
  distinct() %>%
  group_by(parent_code, child_code) %>%
  summarize(n = length(unique(doc)),
            prop = n / 17,
            context1 = unique(subcode_context[!is.na(subcode_context)])[1],
            n_context1 = length(unique(doc[subcode_context==context1 & !is.na(subcode_context)])),
            prop_context1 = n_context1/17,
            context2 = unique(subcode_context[!is.na(subcode_context)])[2],
            n_context2 = length(unique(doc[subcode_context==context2 & !is.na(subcode_context)])),
            prop_context2 = n_context2/17,
            context3 = unique(subcode_context[!is.na(subcode_context)])[3],
            n_context3 = length(unique(doc[subcode_context==context3 & !is.na(subcode_context)])),
            prop_context3 = n_context3/17,
            context4 = unique(subcode_context[!is.na(subcode_context)])[4],
            n_context4 = length(unique(doc[subcode_context==context4 & !is.na(subcode_context)])),
            prop_context4 = n_context4/17)

prop_unprompted_across_full_interview <-
  validate_model_codes %>%
  bind_rows(open_ended_codes) %>%
  filter(parent_code!="Examples" & parent_code!="Lens" &
           (grepl("suggestion", child_subcode, ignore.case = T) | 
              doc_group=="Open-ended")) %>% 
  mutate(subcode_context = ifelse(doc_group=="Open-ended", child_subcode, subcode_context)) %>%
  select(doc, parent_code, child_code, subcode_context) %>%
  distinct() %>%
  group_by(parent_code, child_code) %>%
  summarize(n = length(unique(doc)),
            prop = n / 17,
            context1 = unique(subcode_context[!is.na(subcode_context)])[1],
            n_context1 = length(unique(doc[subcode_context==context1 & !is.na(subcode_context)])),
            prop_context1 = n_context1/17,
            context2 = unique(subcode_context[!is.na(subcode_context)])[2],
            n_context2 = length(unique(doc[subcode_context==context2 & !is.na(subcode_context)])),
            prop_context2 = n_context2/17,
            context3 = unique(subcode_context[!is.na(subcode_context)])[3],
            n_context3 = length(unique(doc[subcode_context==context3 & !is.na(subcode_context)])),
            prop_context3 = n_context3/17,
            context4 = unique(subcode_context[!is.na(subcode_context)])[4],
            n_context4 = length(unique(doc[subcode_context==context4 & !is.na(subcode_context)])),
            prop_context4 = n_context4/17)

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: EXAMPLE BUZZWORDS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

example_buzzwords_list <-
  bind_rows(open_ended_codes %>% select(doc_group, doc, code_long, segment, parent_code, child_code), 
                   validate_model_codes %>% select(doc_group, doc, code_long, segment, parent_code, child_code)) %>%
  filter(parent_code=="Examples" & child_code!="example context") %>%
  mutate(segment = tolower(segment),
         segment = case_when(segment=="deij" | segment=="diversity, equity and inclusion" | segment=="dei" ~ "dei*",
                             segment=="sustainability" | segment=="sustainable" | 
                               segment=="sustainable development" | segment=="sustainable materials"~ "sustainability*",
                             segment=="anti-racism" | segment=="antiracist" ~ "anti-racism*", 
                             TRUE ~ segment),
         segment = stringr::str_replace(segment, "solutions", "solution"),
         segment = stringr::str_replace(segment, "transformational", "transformative")) %>%
  left_join(interviewee_info, by = "doc") %>%
  group_by(doc, org_type) %>%
  summarize(segment = unique(segment)) %>%
  group_by(segment) %>%
  summarize(n_total = length(doc),
            n_academia = length(doc[org_type=="academia"]),
            n_ngo = length(doc[org_type=="ngo"]),
            n_policy = length(doc[org_type=="policy"]),
            n_media = length(doc[org_type=="media/comms"]),
            total_academia = length(interviewee_info$id[interviewee_info$org_type=="academia"]),
            total_ngo = length(interviewee_info$id[interviewee_info$org_type=="ngo"]),
            total_policy = length(interviewee_info$id[interviewee_info$org_type=="policy"]),
            total_media = length(interviewee_info$id[interviewee_info$org_type=="media/comms"]),
            prop_academia = n_academia/total_academia,
            prop_ngo = n_ngo/total_ngo,
            prop_policy = n_policy/total_policy,
            prop_media = n_media/total_media)

plot_example_buzzwords <-
  example_buzzwords_list %>% 
  filter(n_total>1) %>%
  arrange(n_total, desc(segment)) %>%
  mutate(segment = factor(segment, levels = unique(segment), ordered = T)) %>%
  ggplot() +
  geom_bar(aes(y = segment, x = n_total),
           stat = "identity", fill = "#332288", width = 0.75) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(1, 15, by = 2),
                     limits = c(0, 17),
                     sec.axis = dup_axis(trans = ~., 
                                         name = "",
                                         labels = rep("", 8))) +
  example.buzzword.plot.theme +
  labs(y = "", x = "Number of respondents (total = 17) who identified word as an example buzzword")

plot_example_buzzwords_arranged <-
  grid.arrange(plot_example_buzzwords,
               bottom = textGrob("*Includes other versions or conjugations of the word (e.g., 'sustainable', 'diversity, equity, and inclusion', 'anti-racist')",
                                 hjust = 0.5,
                                 gp = gpar(fontsize = 11)))

png("data/outputs/figures/example_buzzwords_frominterviews.png",
    units = "in", height = 7, width = 10, res = 400)
grid.newpage()
grid.draw(plot_example_buzzwords_arranged)
dev.off()

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: LENSES FOR INTERPRETING BUZZWORDS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

lenses <- 
  validate_model_codes %>%
  bind_rows(open_ended_codes) %>%
  filter(parent_code=="Lens") %>%
  mutate(parent_subcode = case_when(child_code%in%c("context dependent",
                                                    "unintentional, careless",
                                                    "intentional") ~ "intentionality",
                                    child_code%in%c("critical, skeptical",
                                                    "idealistic, positive",
                                                    "cynical, negative",
                                                    "sentiment") ~ "sentiment",
                                    child_code=="feedbacks, dynamics" ~ "dynamics",
                                    child_code=="context dependence" ~ "context dependence"))

lenses_parentsubcode <-
  lenses %>%
  group_by(parent_subcode, child_code) %>%
  summarise(n = length(unique(doc)),
            prop = n / 17)

lenses_institutiontype <-
  lenses %>%
  left_join(interviewee_info, by = "doc") %>%
  group_by(org_type) %>%
  mutate(n_org_type = length(unique(doc))) %>%
  ungroup() %>%
  group_by(parent_subcode, child_code, org_type, n_org_type) %>%
  summarise(n = length(unique(doc)),
            prop = n / unique(n_org_type))

lenses_byinterview <-
  lenses %>%
  group_by(doc) %>%
  summarise(n_sentiment = length(segment[parent_subcode=="sentiment"]),
            n_dynamics = length(segment[parent_subcode=="dynamics"]),
            n_context = length(segment[parent_subcode=="context dependence"]),
            n_intentionality = length(segment[parent_subcode=="intentionality"]),
            n_pos_sentiment = length(segment[child_code=="idealistic, positive"]),
            n_neg_sentiment = length(segment[child_code=="cynical, negative"]),
            n_context_sentiment = length(segment[child_code=="critical, skeptical"])) %>%
  left_join(interviewee_info, by = "doc")
