
# code: Interview analysis (qualitative coding)

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT LIBRARIES, DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

pacman::p_load(tidyverse)

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
         child_subcode = tolower(Comment)) %>%
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
         child_subcode = tolower(Comment) %>% stringr::str_replace_all("suggestion -", "suggestion,"),
         subcode_context = stringr::str_split(child_subcode, ', ', simplify = TRUE)[,2],
         # clean child subcode and context
         child_subcode = stringr::str_split(child_subcode, ', ', simplify = TRUE)[,1],
         subcode_context = ifelse(subcode_context=="", NA, subcode_context)) %>%
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
  


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: EXAMPLE BUZZWORDS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


example_buzzwords_list <-
  rbind.data.frame(open_ended_codes %>% select(doc_group, doc, code_long, segment, parent_code, child_code), 
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
            n_media = length(doc[org_type=="media/comms"]))
