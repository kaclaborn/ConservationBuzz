
# code: survey processing & analysis

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES, DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- load libraries ----

pacman::p_load(tidyverse, Hmisc, corrplot, ltm, grid, gridExtra)
source("code/source/process/process.R")
source("code/source/plotThemes.R")


# ---- load pretest data ----

# pretest
pretest_raw <- read_csv("data/surveys/pretest_raw_20231017.csv")

pretest_cols <- c("response_id", "status", "ip", "timestamp", "duplicate", "completion_time", "prolific_id", "country", "region",
                  "consent", "aa", "ab", "ba", "bb", "trust_org", "trust_project", "cred_honest", "cred_rep", "cred_expert",
                  "cred_record", "cred_needs", "cred_relationships", "cred_relevant", "cred_novel", "cred_genuine", "cred_values",
                  "comp_clear", "comp_easy", "buzzword", "buzzword_why", "dem_age", "dem_gender", "dem_gender_comment", "dem_education",
                  "dem_socialmedia", "final_feedback")

colnames(pretest_raw) <- pretest_cols

# pretest 2 (just testing for buzzword effect)
pretest2_raw <- read_csv("data/surveys/pretest2_raw_20231027.csv")

pretest2_cols <- c("response_id", "status", "ip", "timestamp", "duplicate", "completion_time", "prolific_id", "country", "region",
                  "consent", "a", "b", "comp_clear", "comp_easy", "buzzword", "buzzword_why", "dem_age", "dem_gender", "dem_gender_comment", "dem_education")

colnames(pretest2_raw) <- pretest2_cols


# ---- load experiment data ----

experiment_raw <- read_csv("data/surveys/experiment_raw_20231115.csv", 
                           locale = locale(encoding="latin1"))

experiment_cols <- c("response_id", "status", "timestamp", "duplicate", "completion_time", "prolific_id", "country", "region",
                     "consent", "aa", "ba", "ab", "bb", "donate_personal", "donate_public", "donate_community", "fund_recommend", "fund_worthy", "fund_priority",
                     "feedback_decision", "sentiment_project", "sentiment_org", "trust_org", "trust_project", "cred_honest", "cred_rep", "cred_expert",
                     "cred_record", "cred_needs", "cred_relationships", "cred_relevant", "cred_novel", "cred_genuine", "cred_values",
                     "values_identify", "values_align", "values_community", "values_common", "skeptical_project", "skeptical_intentions", "skeptical_capabilities",
                     "comp_clear", "comp_easy", "comp_buzzwordy", "buzzword_why", "feedback_final", "dem_age", "dem_gender", "dem_gender_comment", "dem_education",
                     "dem_income", "dem_householdsize", "dem_socialmedia")

colnames(experiment_raw) <- experiment_cols


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: PRE-TEST ANALYSES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- pre-test filtering and analysis ----

pretest_dat <- 
  pretest_raw %>%
  filter(status=="Completed") %>%
  mutate(no_prof_pitch = case_when(is.na(aa) & is.na(ab) & is.na(ba) & is.na(bb) ~ TRUE,
                                   TRUE ~ FALSE),
         profile = case_when(no_prof_pitch==FALSE & (aa==1 | ab==1) ~ "high trust",
                           no_prof_pitch==FALSE & (ba==1 | bb==1) ~ "low trust",
                           TRUE ~ "no record"),
         pitch = case_when(no_prof_pitch==FALSE & (aa==1 | ba==1) ~ "buzzword",
                           no_prof_pitch==FALSE & (ab==1 | bb==1) ~ "non-buzzword",
                           TRUE ~ "no record"),
         version = case_when(no_prof_pitch==FALSE & aa==1 ~ "buzz-high",
                             no_prof_pitch==FALSE & ba==1 ~ "buzz-low",
                             no_prof_pitch==FALSE & ab==1 ~ "nobuzz-high",
                             no_prof_pitch==FALSE & bb==1 ~ "nobuzz-low",
                             TRUE ~ "no record")) %>%
  filter(no_prof_pitch==FALSE) %>%
  mutate(version = factor(version, levels = unique(version)))


pretest_dat_short <- 
  pretest_dat %>%
  dplyr::select(profile, pitch, buzzword, buzzword_why, trust_org, trust_project, final_feedback)


pretest_bypitch <- 
  pretest_dat %>%
  group_by(pitch) %>%
  summarise(buzzword_mean = mean(buzzword, na.rm = T),
            buzzword_sd = sd(buzzword, na.rm = T),
            buzzword_na = length(response_id[is.na(buzzword)]),
            trust_mean = mean(trust_org, na.rm = T),
            trust_sd = sd(trust_org, na.rm = T),
            trust_na = length(response_id[is.na(trust_org)]))

pretest_byprofile <- 
  pretest_dat %>%
  group_by(profile) %>%
  summarise(buzzword_mean = mean(buzzword, na.rm = T),
            buzzword_sd = sd(buzzword, na.rm = T),
            buzzword_na = length(response_id[is.na(buzzword)]),
            trust_mean = mean(trust_org, na.rm = T),
            trust_sd = sd(trust_org, na.rm = T),
            trust_na = length(response_id[is.na(trust_org)]),
            clarity_mean = mean(comp_clear, na.rm = T),
            understand_mean = mean(comp_easy, na.rm = T))


buzzwordy_test_bypitch <-
  t.test(pretest_dat$buzzword[pretest_dat$pitch=="buzzword"], 
         pretest_dat$buzzword[pretest_dat$pitch=="non-buzzword"])
trust_test_bypitch <-
  t.test(pretest_dat$trust_org[pretest_dat$pitch=="buzzword"], 
         pretest_dat$trust_org[pretest_dat$pitch=="non-buzzword"])
clarity_test_bypitch <-
  t.test(pretest_dat$comp_clear[pretest_dat$pitch=="buzzword"], 
         pretest_dat$comp_clear[pretest_dat$pitch=="non-buzzword"])
easyunderstand_test_bypitch <-
  t.test(pretest_dat$comp_easy[pretest_dat$pitch=="buzzword"], 
         pretest_dat$comp_easy[pretest_dat$pitch=="non-buzzword"])

trust_test_byprofile <-
  t.test(pretest_dat$trust_org[pretest_dat$profile=="high trust"], 
         pretest_dat$trust_org[pretest_dat$profile=="low trust"])
buzzwordy_test_byprofile <-
  t.test(pretest_dat$buzzword[pretest_dat$profile=="high trust"], 
         pretest_dat$buzzword[pretest_dat$profile=="low trust"])
clarity_test_byprofile <-
  t.test(pretest_dat$comp_clear[pretest_dat$profile=="high trust"], 
         pretest_dat$comp_clear[pretest_dat$profile=="low trust"])
easyunderstand_test_byprofile <-
  t.test(pretest_dat$comp_easy[pretest_dat$profile=="high trust"], 
         pretest_dat$comp_easy[pretest_dat$profile=="low trust"])



# ---- pre-test 2 (testing buzzword manipulation) filtering and analysis ----

pretest2_dat <- 
  pretest2_raw %>%
  filter(status=="Completed") %>%
  mutate(report = case_when(b==1 ~ "buzzword",
                           a==1 ~ "non-buzzword",
                           TRUE ~ "no record"))

pretest2_byreport <- 
  pretest2_dat %>%
  group_by(report) %>%
  summarise(buzzword_mean = mean(buzzword, na.rm = T),
            buzzword_sd = sd(buzzword, na.rm = T),
            buzzword_na = length(response_id[is.na(buzzword)]),
            clarity_mean = mean(comp_clear, na.rm = T),
            easyunderstand_mean = mean(comp_easy, na.rm = T))


buzzwordy_test2_byreport <-
  t.test(pretest2_dat$buzzword[pretest2_dat$report=="buzzword"], 
         pretest2_dat$buzzword[pretest2_dat$report=="non-buzzword"])
clarity_test2_byreport <-
  t.test(pretest2_dat$comp_clear[pretest2_dat$report=="buzzword"], 
         pretest2_dat$comp_clear[pretest2_dat$report=="non-buzzword"])
easyunderstand_test2_byreport <-
  t.test(pretest2_dat$comp_easy[pretest2_dat$report=="buzzword"], 
         pretest2_dat$comp_easy[pretest2_dat$report=="non-buzzword"])


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: EXPERIMENT ANALYSES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

medium_purchasing_power <- 69021 / 2.6 # median US household income betwee 2017-2021 divided by median houosehold size

experiment_dat <- 
  experiment_raw %>%
  filter(status=="Completed") %>%
  mutate(profile = case_when(aa==1 | ab==1 ~ "high trust",
                             ba==1 | bb==1 ~ "low trust",
                             TRUE ~ "no record"),
         profile_factor = factor(profile, levels = c("high trust", "low trust")),
         report = case_when(aa==1 | ba==1 ~ "non-buzzword",
                            ab==1 | bb==1 ~ "buzzword",
                            TRUE ~ "no record"),
         report_factor = factor(report, levels = c("non-buzzword", "buzzword")),
         version = case_when(aa==1 ~ "nobuzz-high",
                             ba==1 ~ "nobuzz-low",
                             ab==1 ~ "buzz-high",
                             bb==1 ~ "buzz-low",
                             TRUE ~ "no record"),
         profile_binary = ifelse(profile=="high trust", 1, ifelse(profile=="low trust", 0, NA)),
         dem_householdsize = ifelse(dem_householdsize<1, NA, dem_householdsize),
         education = case_when(dem_education=="Less than high school diploma" |
                                 dem_education=="High school" ~ "High school or less",
                               dem_education=="Some college" | 
                                 dem_education=="Trade/technical/vocational" |
                                 dem_education=="Associate's degree" ~ "Some college, technical training, or associate's",
                               dem_education=="Bachelor's degree" ~ "Bachelor's degree",
                               dem_education=="Master's, professional, or doctoral degree" ~ "Advanced degree",
                               TRUE ~ NA),
         income_numeric = case_when(dem_income=="$0 - $24,999" ~ 12500,
                                    dem_income=="$25,000 - $49,999" ~ 37500,
                                    dem_income=="$50,000 - $74,999" ~ 62500,
                                    dem_income=="$75,000 - $99,999" ~ 87500,
                                    dem_income=="$100,000 - $149,999" ~ 125000,
                                    dem_income=="$150,000 - $199,999" ~ 175000,
                                    dem_income=="$200,000 or greater" ~ 200000,
                                    TRUE ~ NA),
         purchasing_power = income_numeric/dem_householdsize,
         purchasing_power_category = case_when(purchasing_power<medium_purchasing_power-10000 ~ "low",
                                               purchasing_power>=medium_purchasing_power-10000 & 
                                                 purchasing_power<medium_purchasing_power+10000 ~ "medium",
                                               purchasing_power>=medium_purchasing_power+10000 ~ "high"),
         donate = rowMeans(dplyr::select(., c(donate_personal, donate_public, donate_community)), na.rm = T),
         fund = rowMeans(dplyr::select(., c(fund_recommend, fund_worthy, fund_priority)), na.rm = T),
         comprehension = rowMeans(dplyr::select(., c(comp_clear, comp_easy)), na.rm = T),
         skepticism = rowMeans(dplyr::select(., c(skeptical_project, skeptical_intentions, skeptical_capabilities)), na.rm = T),
         values = rowMeans(dplyr::select(., c(values_align, values_identify, values_community, values_common)), na.rm = T),
         credibility = rowMeans(dplyr::select(., c(cred_honest, cred_rep, cred_expert, cred_record, cred_needs, 
                                               cred_relationships, cred_relevant, cred_novel, cred_genuine, cred_values)), na.rm = T)) %>%
  mutate(version_factor = factor(version, levels = c("nobuzz-high", "nobuzz-low", "buzz-high", "buzz-low")),
         version_num = case_when(version=="nobuzz-high" ~ 1,
                                 version=="nobuzz-low" ~ 2, 
                                 version=="buzz-high" ~ 3,
                                 version=="buzz-low" ~ 4))

experiment_byversion <- 
  experiment_dat %>%
  group_by(version_factor) %>%
  summarise(report_factor = unique(report_factor),
            profile_factor = unique(profile_factor),
            n = length(response_id), 
            fund_mean = mean(fund),
            fund_sd = sd(fund),
            donate_mean = mean(donate),
            donate_sd = sd(donate))

purchasing_power_hist <- 
  ggplot(experiment_dat) +
  geom_histogram(aes(x = purchasing_power))

purchasing_power_cat_hist <- 
  ggplot(experiment_dat) +
  geom_bar(aes(x = purchasing_power_category),
                 stat = "count")


buzzwordy_experiment_byreport <-
  t.test(experiment_dat$comp_buzzwordy[experiment_dat$report=="buzzword"], 
         experiment_dat$comp_buzzwordy[experiment_dat$report=="non-buzzword"])
trust_experiment_byprofile <-
  t.test(experiment_dat$trust_org[experiment_dat$profile=="high trust"], 
         experiment_dat$trust_org[experiment_dat$profile=="low trust"])


donate_corrs <- cor(experiment_dat %>% dplyr::select(donate_personal, donate_public, donate_community), method = "pearson", use = "complete.obs")
donate_corrplot <- corrplot(donate_corrs, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

fund_corrs <- cor(experiment_dat %>% dplyr::select(fund_recommend, fund_worthy, fund_priority), method = "pearson", use = "complete.obs")
fund_corrplot <- corrplot(fund_corrs, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

skepticism_corrs <- cor(experiment_dat %>% dplyr::select(skeptical_project, skeptical_intentions, skeptical_capabilities), method = "pearson", use = "complete.obs")
skepticism_corrplot <- corrplot(skepticism_corrs, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

values_corrs <- cor(experiment_dat %>% dplyr::select(values_align, values_identify, values_community, values_common), method = "pearson", use = "complete.obs")
values_corrplot <- corrplot(values_corrs, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

cred_corrs <- cor(experiment_dat %>% dplyr::select(cred_honest, cred_rep, cred_expert, cred_record, cred_needs, 
                                            cred_relationships, cred_relevant, cred_novel, cred_genuine, cred_values),
                  method = "pearson", use = "complete.obs")
cred_corrplot <- corrplot(cred_corrs, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

comp_corrs <- cor(experiment_dat %>% dplyr::select(comp_clear, comp_easy), method = "pearson", use = "complete.obs")
comp_corrplot <- corrplot(comp_corrs, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


donate_alpha <- cronbach.alpha(experiment_dat %>% dplyr::select(donate_personal, donate_public, donate_community))
fund_alpha <- cronbach.alpha(experiment_dat %>% dplyr::select(fund_recommend, fund_worthy, fund_priority))
skepticism_alpha <- cronbach.alpha(experiment_dat %>% dplyr::select(skeptical_project, skeptical_intentions, skeptical_capabilities))
values_alpha <- cronbach.alpha(experiment_dat %>% dplyr::select(values_align, values_identify, values_community, values_common))
cred_alpha <- cronbach.alpha(experiment_dat %>% dplyr::select(cred_honest, cred_rep, cred_expert, cred_record, cred_needs, 
                                                              cred_relationships, cred_relevant, cred_novel, cred_genuine, cred_values))
comp_alpha <- cronbach.alpha(experiment_dat %>% dplyr::select(comp_clear, comp_easy))


direct_effects_willingness <-
  lm(donate ~ report_factor + profile_factor + report_factor:profile_factor + dem_age + education + purchasing_power_category, data = experiment_dat)
direct_effects_willingness <-
  lm(donate ~ version_factor + dem_age + education + purchasing_power_category, data = experiment_dat)

direct_effects_worthiness <-
  lm(fund ~ report_factor + profile_factor + report_factor:profile_factor + dem_age + education + purchasing_power_category, data = experiment_dat)
direct_effects_worthiness <-
  lm(fund ~ version_factor + dem_age + education + purchasing_power_category, data = experiment_dat)


direct_effects_skepticism <- 
  lm(skepticism ~ report_factor + profile_factor + report_factor:profile_factor + dem_age + education + purchasing_power_category, data = experiment_dat)

direct_effects_values <-
  lm(values ~ report_factor + profile_factor + report_factor:profile_factor + dem_age + education + purchasing_power_category, data = experiment_dat)

direct_effects_credibility <-
  lm(credibility ~ report_factor + profile_factor + report_factor:profile_factor + dem_age + education + purchasing_power_category, data = experiment_dat)


indirect_effects_worthiness_skepticism <-
  lm(fund ~ report_factor + profile_factor + report_factor:profile_factor + skepticism + dem_age + education + purchasing_power_category, data = experiment_dat)

indirect_effects_worthiness_values <-
  lm(fund ~ report_factor + profile_factor + report_factor:profile_factor + values + dem_age + education + purchasing_power_category, data = experiment_dat)

indirect_effects_worthiness_credibility <-
  lm(fund ~ report_factor + profile_factor + report_factor:profile_factor + credibility + dem_age + education + purchasing_power_category, data = experiment_dat)


indirect_effects_worthiness_all <-
  lm(fund ~ report_factor + profile_factor + report_factor:profile_factor + skepticism + values + credibility + dem_age + education + purchasing_power_category, data = experiment_dat)


summary(direct_effects_willingness)
summary(direct_effects_worthiness)
summary(direct_effects_skepticism)
summary(direct_effects_values)
summary(direct_effects_credibility)
summary(indirect_effects_worthiness_skepticism)
summary(indirect_effects_worthiness_values)
summary(indirect_effects_worthiness_credibility)
summary(indirect_effects_worthiness_all)

# include bar charts of means/sd of dvs, two-way anova of conditions (bar charts -- dv on y axis, color = trust, grouped by = buzz) -- 
# table of means/sds across four conditions for dvs
# test of experimental manipulations "how trustworthy, how buzzwordy"? (supplemental -- manipulation check)

process(data = experiment_dat, y = "fund", x = "version_num", m = c("skepticism", "credibility", "values"), mcx = 1, model = 4, total = 1)

skept_indirect_lowtrust_nobuzz_pval <- pnorm(0.1354/0.8428)

cred_indirect_lowtrust_nobuzz_pval <- pnorm(-9.2295/1.8899)
cred_indirect_lowtrust_buzz_pval <- pnorm(-6.3861/1.5234)

val_indirect_lowtrust_nobuzz_pval <- pnorm(-3.9447/1.3013)
val_indirect_lowtrust_buzz_pval <- pnorm(-1.2675/1.0365)


# ---- plot findings ----

donate_means_plot <- 
  ggplot(experiment_byversion) +
  geom_bar(aes(x = profile_factor, y = donate_mean, group = report_factor, fill = report_factor),
           stat = "identity", position = "dodge", show.legend = F) +
  geom_errorbar(aes(x = profile_factor, ymin = donate_mean - 0.5*donate_sd, ymax = donate_mean + 0.5*donate_sd, group = report_factor),
                stat = "identity", position = position_dodge(width = 0.9), width = 0.4) +
  scale_y_continuous(name = "Mean",
                     expand = expansion(0, c(0, 5)),
                     breaks = seq(0, 70, by = 10)) +
  scale_x_discrete(name = "") +
  scale_fill_manual(name = "",
                    values = c("#44AA99", "#332288")) +
  labs(title = "Willingness to Donate",
       subtitle = "composite index ranging from 0-100") +
  time.plot.theme

fund_means_plot <- 
  ggplot(experiment_byversion) +
  geom_bar(aes(x = profile_factor, y = fund_mean, group = report_factor, fill = report_factor),
           stat = "identity", position = "dodge", show.legend = F) +
  geom_errorbar(aes(x = profile_factor, ymin = fund_mean - 0.5*fund_sd, ymax = fund_mean + 0.5*fund_sd, group = report_factor),
                stat = "identity", position = position_dodge(width = 0.9), width = 0.4) +
  scale_y_continuous(name = "Mean",
                     expand = expansion(0, c(0, 5)),
                     breaks = seq(0, 80, by = 10)) +
  scale_x_discrete(name = "") +
  scale_fill_manual(name = "",
                    values = c("#44AA99", "#332288")) +
  labs(title = "Worthiness of Funding",
       subtitle = "composite index ranging from 0-100") +
  time.plot.theme

dv_means_legend_plot <- 
  ggplot(experiment_byversion) +
  geom_bar(aes(x = profile_factor, y = fund_mean, fill = report_factor), stat = "identity") +
  scale_fill_manual(name = "",
                    values = c("#44AA99", "#332288")) +
  time.plot.theme

dv_means_legend <- get_legend(dv_means_legend_plot)

dv_means_plots_arranged <- 
  grid.arrange(donate_means_plot, fund_means_plot, dv_means_legend,
               ncol = 3, widths = c(1, 1, 0.3))

# ---- export findings and plots ----

dir.create("data/outputs/experiment")
experiment_output_dir <- "data/outputs/experiment/"

png(paste(experiment_output_dir, "dv_means_plot.png", sep = ""),
    units = "in", height = 4, width = 10, res = 400)
grid.newpage()
grid.draw(dv_means_plots_arranged)
dev.off()
