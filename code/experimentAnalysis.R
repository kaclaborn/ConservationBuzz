
# code: survey processing & analysis

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES, DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- load libraries ----

pacman::p_load(tidyverse, Hmisc, corrplot, ltm, grid, gridExtra, ggpubr, ggtext)
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


# ---- calculate constants ----

medium_purchasing_power <- 69021 / 2.6 # median US household income betwee 2017-2021 divided by median houosehold size


# ---- data wrangling for models and plots ----

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
         education_factor = factor(education, levels = c("High school or less", "Some college, technical training, or associate's",
                                                         "Bachelor's degree", "Advanced degree")),
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
         purchasing_power_factor = factor(purchasing_power_category, levels = c("low", "medium", "high")),
         social_media_category = case_when(dem_socialmedia=="Nearly all the time" | dem_socialmedia=="Most of the time" ~ "Nearly all or most of the time",
                                           dem_socialmedia=="About half the time" ~ "About half the time",
                                           dem_socialmedia=="Once in a while" | dem_socialmedia=="Never" ~ "Once in a while or never"),
         social_media_factor = factor(social_media_category, levels = c("Once in a while or never", "About half the time", "Nearly all or most of the time")),
         donate = rowMeans(dplyr::select(., c(donate_personal, donate_public, donate_community)), na.rm = T),
         fund = rowMeans(dplyr::select(., c(fund_recommend, fund_worthy, fund_priority)), na.rm = T),
         comprehension = rowMeans(dplyr::select(., c(comp_clear, comp_easy)), na.rm = T),
         skepticism = rowMeans(dplyr::select(., c(skeptical_project, skeptical_intentions, skeptical_capabilities)), na.rm = T),
         skepticism = skepticism * 10, # this is the only set of mediator questions scaled from 0 - 10 (steps of 1) instead of 0 - 100 (steps of 10)
         values = rowMeans(dplyr::select(., c(values_align, values_identify, values_community, values_common)), na.rm = T),
         credibility = rowMeans(dplyr::select(., c(cred_honest, cred_rep, cred_expert, cred_record, cred_needs, 
                                               cred_relationships, cred_relevant, cred_novel, cred_genuine, cred_values)), na.rm = T)) %>%
  mutate(version_factor = factor(version, levels = c("nobuzz-high", "nobuzz-low", "buzz-high", "buzz-low")),
         version_num = case_when(version=="nobuzz-high" ~ 1,
                                 version=="nobuzz-low" ~ 2, 
                                 version=="buzz-high" ~ 3,
                                 version=="buzz-low" ~ 4),
         education_highschool = case_when(education=="High school or less" ~ 1,
                                          !is.na(education) ~ 0),
         education_somecoll = case_when(education=="Some college, technical training, or associate's" ~ 1,
                                        !is.na(education) ~ 0),
         education_bach = case_when(education=="Bachelor's degree" ~ 1,
                                        !is.na(education) ~ 0),
         education_adv = case_when(education=="Advanced degree" ~ 1,
                                        !is.na(education) ~ 0),
         purchasing_power_low = case_when(purchasing_power_category=="low" ~ 1, 
                                          !is.na(purchasing_power_category) ~ 0),
         purchasing_power_med = case_when(purchasing_power_category=="medium" ~ 1, 
                                          !is.na(purchasing_power_category) ~ 0),
         purchasing_power_high = case_when(purchasing_power_category=="high" ~ 1, 
                                          !is.na(purchasing_power_category) ~ 0),
         social_media_never = case_when(social_media_category=="Once in a while or never" ~ 1,
                                        !is.na(social_media_category) ~ 0),
         social_media_half = case_when(social_media_category=="About half the time" ~ 1,
                                        !is.na(social_media_category) ~ 0),
         social_media_all = case_when(social_media_category=="Nearly all or most of the time" ~ 1,
                                        !is.na(social_media_category) ~ 0))

experiment_byversion <- 
  experiment_dat %>%
  group_by(version_factor) %>%
  summarise(report_factor = unique(report_factor),
            profile_factor = unique(profile_factor),
            n = length(response_id), 
            fund_mean = mean(fund),
            fund_se = sd(fund)/sqrt(length(fund)),
            donate_mean = mean(donate),
            donate_se = sd(donate)/sqrt(length(donate)),
            comprehension_mean = mean(comprehension),
            comprehension_se = sd(comprehension)/sqrt(length(comprehension)),
            skepticism_mean = mean(skepticism),
            skepticism_se = sd(skepticism)/sqrt(length(skepticism)),
            values_mean = mean(values),
            values_se = sd(values)/sqrt(length(values)),
            credibility_mean = mean(credibility),
            credibility_se = sd(credibility)/sqrt(length(credibility)))

purchasing_power_hist <- 
  ggplot(experiment_dat) +
  geom_histogram(aes(x = purchasing_power))

purchasing_power_cat_hist <- 
  ggplot(experiment_dat) +
  geom_bar(aes(x = purchasing_power_category),
                 stat = "count")


# ---- participant demographics ----

experiment_participants <-
  experiment_dat %>%
  drop_na(., c(fund, donate, dem_age, education, purchasing_power, social_media_category)) %>% # only include participants with complete responses for regressions
  summarise(n_man = length(response_id[dem_gender=="Man"]),
            n_woman = length(response_id[dem_gender=="Woman"]),
            n_nonbinary = length(response_id[dem_gender=="Non-binary"]),
            perc_man = n_man/length(response_id),
            perc_woman = n_woman/length(response_id),
            perc_nonbinary = n_nonbinary/length(response_id),
            avg_age = mean(dem_age),
            min_age = min(dem_age),
            max_age = max(dem_age),
            sd_age = sd(dem_age),
            n_education_highschool = length(response_id[education_highschool==1]),
            n_education_somecoll = length(response_id[education_somecoll==1]),
            n_education_bach = length(response_id[education_bach==1]),
            n_education_adv = length(response_id[education_adv==1]),
            n_purchasing_power_low = length(response_id[purchasing_power_low==1]),
            n_purchasing_power_med = length(response_id[purchasing_power_med==1]),
            n_purchasing_power_high = length(response_id[purchasing_power_high==1]),
            n_social_media_never = length(response_id[social_media_never==1]),
            n_social_media_half = length(response_id[social_media_half==1]),
            n_social_media_all = length(response_id[social_media_all==1]))



# ---- experimental manipulation check ----

buzzwordy_experiment_byreport <-
  t.test(experiment_dat$comp_buzzwordy[experiment_dat$report=="buzzword"], 
         experiment_dat$comp_buzzwordy[experiment_dat$report=="non-buzzword"])
trust_experiment_byprofile <-
  t.test(experiment_dat$trust_org[experiment_dat$profile=="high trust"], 
         experiment_dat$trust_org[experiment_dat$profile=="low trust"])


# ---- correlation and cronbach's alpha for composite indices ----

donate_corrs <- cor(experiment_dat %>% dplyr::select(donate_personal, donate_public, donate_community), method = "pearson", use = "complete.obs")
donate_corrplot <- corrplot(donate_corrs, method = "square", type = "lower", order = "hclust", addCoef.col = "black", tl.col = "black", tl.srt = 45, diag = FALSE)

fund_corrs <- cor(experiment_dat %>% dplyr::select(fund_recommend, fund_worthy, fund_priority), method = "pearson", use = "complete.obs")
fund_corrplot <- corrplot(fund_corrs, method = "square", type = "lower", order = "hclust", addCoef.col = "black", tl.col = "black", tl.srt = 45, diag = FALSE)

skepticism_corrs <- cor(experiment_dat %>% dplyr::select(skeptical_project, skeptical_intentions, skeptical_capabilities), method = "pearson", use = "complete.obs")
skepticism_corrplot <- corrplot(skepticism_corrs, method = "square", type = "lower", order = "hclust", addCoef.col = "black", tl.col = "black", tl.srt = 45, diag = FALSE)

values_corrs <- cor(experiment_dat %>% dplyr::select(values_align, values_identify, values_community, values_common), method = "pearson", use = "complete.obs")
values_corrplot <- corrplot(values_corrs, method = "square", type = "lower", order = "hclust", addCoef.col = "black", tl.col = "black", tl.srt = 45, diag = FALSE)

cred_corrs <- cor(experiment_dat %>% dplyr::select(cred_honest, cred_rep, cred_expert, cred_record, cred_needs, 
                                            cred_relationships, cred_relevant, cred_novel, cred_genuine, cred_values),
                  method = "pearson", use = "complete.obs")
cred_corrplot <- corrplot(cred_corrs, method = "square", type = "lower", order = "hclust", addCoef.col = "black", tl.col = "black", tl.srt = 45, diag = FALSE)


comp_corrs <- cor(experiment_dat %>% dplyr::select(comp_clear, comp_easy), method = "pearson", use = "complete.obs")
comp_corrplot <- corrplot(comp_corrs, method = "square", type = "lower", order = "hclust", addCoef.col = "black", tl.col = "black", tl.srt = 45)


mediator_corrs <- cor(experiment_dat %>% dplyr::select(comprehension, credibility, values, skepticism), method = "pearson", use = "complete.obs")
mediator_corrs_sig <- cor.mtest(experiment_dat %>% dplyr::select(comprehension, credibility, values, skepticism), conf.level = 0.95)
mediator_corrplot <- { 
  corrplot(mediator_corrs, method = "square", type = "lower", order = "hclust", addCoef.col = "black", tl.col = "black", tl.srt = 45);
  recordPlot()
} 


# export corrplots to png
png("data/outputs/experiment/corrplots/mediator_corrplot.png",
    units = "in", height = 3, width = 3, res = 400)
grid.newpage()
grid.draw(replayPlot(mediator_corrplot))
dev.off()

# Cronbach's alpha
donate_alpha <- cronbach.alpha(experiment_dat %>% dplyr::select(donate_personal, donate_public, donate_community))
fund_alpha <- cronbach.alpha(experiment_dat %>% dplyr::select(fund_recommend, fund_worthy, fund_priority))
skepticism_alpha <- cronbach.alpha(experiment_dat %>% dplyr::select(skeptical_project, skeptical_intentions, skeptical_capabilities))
values_alpha <- cronbach.alpha(experiment_dat %>% dplyr::select(values_align, values_identify, values_community, values_common))
cred_alpha <- cronbach.alpha(experiment_dat %>% dplyr::select(cred_honest, cred_rep, cred_expert, cred_record, cred_needs, 
                                                              cred_relationships, cred_relevant, cred_novel, cred_genuine, cred_values))
comp_alpha <- cronbach.alpha(experiment_dat %>% dplyr::select(comp_clear, comp_easy))


# ---- Models 1-2 - total effects models, no mediators ----

# Willingness to donate
basic_model_willingness <-
  lm(donate ~ version_factor + dem_age + education_factor + purchasing_power_factor + social_media_factor, data = experiment_dat)

summary(basic_model_willingness)
coeffs_basic_model_willingness <- as.data.frame(summary(basic_model_willingness)$coefficients) %>% mutate(variable = row.names(.))
# write_csv(coeffs_basic_model_willingness, "data/outputs/experiment/regressions/basic_model_willingness.csv")

basic_model_willingness_aov <- aov(basic_model_willingness)
summary(basic_model_willingness_aov)

basic_model_willingness_tukey <- TukeyHSD(basic_model_willingness_aov, which = "version_factor")
# write.csv(basic_model_willingness_tukey$version_factor, "data/outputs/experiment/tukey_willingness.csv")

# Worthiness of funding
basic_model_worthiness <-
  lm(fund ~ version_factor + dem_age + education_factor + purchasing_power_factor + social_media_factor, data = experiment_dat)

summary(basic_model_worthiness)
coeffs_basic_model_worthiness <- as.data.frame(summary(basic_model_worthiness)$coefficients) %>% mutate(variable = row.names(.))
# write.csv(coeffs_basic_model_worthiness, "data/outputs/experiment/regressions/basic_model_worthiness.csv")

basic_model_worthiness_aov <- aov(basic_model_worthiness)
summary(basic_model_worthiness_aov)

basic_model_worthiness_tukey <- TukeyHSD(basic_model_worthiness_aov, which = "version_factor")
# write_csv(basic_model_worthiness_tukey$version_factor, "data/outputs/experiment/tukey_worthiness.csv")

# ---- Models M1 - M4 - mediator models ----

# Skepticism
M4_skepticism <- 
  lm(skepticism ~ version_factor + dem_age + education_factor + purchasing_power_factor + social_media_factor, data = experiment_dat)

M4_skepticism_aov <- aov(M4_skepticism)
summary(M4_skepticism_aov)

M4_skepticism_tukey <- TukeyHSD(M4_skepticism_aov, which = "version_factor")
# write.csv(M4_skepticism_tukey$version_factor, "data/outputs/experiment/tukey_skepticism.csv")

# Group identity / shared values
M3_values <-
  lm(values ~ version_factor + dem_age + education_factor + purchasing_power_factor + social_media_factor, data = experiment_dat)

M3_values_aov <- aov(M3_values)
summary(M3_values_aov)

M3_values_tukey <- TukeyHSD(M3_values_aov, which = "version_factor")
# write.csv(M3_values_tukey$version_factor, "data/outputs/experiment/tukey_values.csv")

# Credibility
M2_credibility <-
  lm(credibility ~ version_factor + dem_age + education_factor + purchasing_power_factor + social_media_factor, data = experiment_dat)

M2_credibility_aov <- aov(M2_credibility)
summary(M2_credibility_aov)

M2_credibility_tukey <- TukeyHSD(M2_credibility_aov, which = "version_factor")
# write.csv(M2_credibility_tukey$version_factor, "data/outputs/experiment/tukey_credibility.csv")

# Comprehension
M1_comprehension <-
  lm(comprehension ~ version_factor + dem_age + education_factor + purchasing_power_factor + social_media_factor, data = experiment_dat)

M1_comprehension_aov <- aov(M1_comprehension)
summary(M1_comprehension_aov)

M1_comprehension_tukey <- TukeyHSD(M1_comprehension_aov, which = "version_factor")
# write.csv(M1_comprehension_tukey$version_factor, "data/outputs/experiment/tukey_comprehension.csv")


# -- export results
summary(M1_comprehension)
coeffs_M1_comprehension <- as.data.frame(summary(M1_comprehension)$coefficients) %>% mutate(variable = row.names(.))
# write_csv(coeffs_M1_comprehension, "data/outputs/experiment/regressions/M1_comprehension.csv")

summary(M2_credibility)
coeffs_M2_credibility <- as.data.frame(summary(M2_credibility)$coefficients) %>% mutate(variable = row.names(.))
# write_csv(coeffs_M2_credibility, "data/outputs/experiment/regressions/M2_credibility.csv")

summary(M3_values)
coeffs_M3_values <- as.data.frame(summary(M3_values)$coefficients) %>% mutate(variable = row.names(.))
# write_csv(coeffs_M3_values, "data/outputs/experiment/regressions/M3_values.csv")

summary(M4_skepticism)
coeffs_M4_skepticism <- as.data.frame(summary(M4_skepticism)$coefficients) %>% mutate(variable = row.names(.))
# write_csv(coeffs_M4_skepticism, "data/outputs/experiment/regressions/M4_skepticism.csv")


# ---- PROCESS mediation models ----

# Full PROCESS model for willingness to donate
process(data = experiment_dat, y = "donate", x = "version_num", 
        m = c("comprehension", "credibility", "values", "skepticism"), 
        cov = c("dem_age", "education_somecoll", "education_bach", "education_adv",
                "purchasing_power_med", "purchasing_power_high",
                "social_media_half", "social_media_all"),
        mcx = 1, model = 4, total = 1)

# Full PROCESS model for worthiness of funding
process(data = experiment_dat, y = "fund", x = "version_num", 
        m = c("comprehension", "credibility", "values", "skepticism"), 
        cov = c("dem_age", "education_somecoll", "education_bach", "education_adv",
                "purchasing_power_med", "purchasing_power_high",
                "social_media_half", "social_media_all"),
        mcx = 1, model = 4, total = 1)

# pvals for bootstrapped std err for worthiness of funding PROCESS model
comp_indirect_lowtrust_buzz_pval <- pnorm(0.8466/0.7887)

cred_indirect_lowtrust_nobuzz_pval <- pnorm(-10.3432/2.0494)
cred_indirect_hightrust_buzz_pval <- pnorm(-0.7764/1.1004)
cred_indirect_lowtrust_buzz_pval <- pnorm(-7.4412/1.6541)

val_indirect_lowtrust_nobuzz_pval <- pnorm(-4.7365/1.4334)
val_indirect_hightrust_buzz_pval <- pnorm(-0.0740/0.9914)
val_indirect_lowtrust_buzz_pval <- pnorm(-1.8352/1.0996)

skept_indirect_lowtrust_buzz_pval <- pnorm(0.1354/0.4753)



# ---- Explore education interaction & buzzword-only effects (e.g., both high and low trust combined) ----
# Worthiness of funding, buzzword condition vs. advanced degree or not
worthiness_buzzeducation_interaction <-
  lm(fund ~ report_factor + education_adv + report_factor:education_adv + 
       credibility + comprehension + values + skepticism +
       dem_age + purchasing_power_factor + social_media_factor, data = experiment_dat)

summary(worthiness_buzzeducation_interaction)


# Credibility, buzzword condition vs. advanced degree or not
credibility_buzzeducation_interaction <-
  lm(credibility ~ report_factor + education_adv + report_factor:education_adv + 
       comprehension + values + skepticism + # NOTE: including the other mediators explains most of the variation 
       dem_age + purchasing_power_factor + social_media_factor, data = experiment_dat)

summary(credibility_buzzeducation_interaction)


# Worthiness of funding, buzzword condition (both low and high trust combined)
worthiness_buzz <-
  lm(fund ~ report_factor + education_adv + dem_age + purchasing_power_factor + social_media_factor, data = experiment_dat)

summary(worthiness_buzz)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: OUTPUT PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# dependent variables mean/se
donate_means_plot <- 
  ggplot(experiment_byversion) +
  geom_bar(aes(x = profile_factor, y = donate_mean, group = report_factor, fill = report_factor),
           stat = "identity", position = "dodge", show.legend = T) +
  geom_errorbar(aes(x = profile_factor, ymin = donate_mean - donate_se, ymax = donate_mean + donate_se, group = report_factor),
                stat = "identity", position = position_dodge(width = 0.9), width = 0.15, linewidth = 0.5) +
  geom_bracket(xmin = 1.6, xmax = 2.4, y.position = 65, label = "\U0394 = 5.42, p = 0.2113", 
               size = 0.75, tip.length = 0.1, vjust = -0.2, label.size = 3) +
  scale_y_continuous(name = "Mean",
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 10),
                     limits = c(0, 100)) +
  scale_x_discrete(name = "") +
  scale_fill_manual(name = "",
                    values = c("#44AA99", "#332288")) +
  labs(title = "Willingness to Donate",
       subtitle = "composite index ranging from 0-100") +
  time.plot.theme

fund_means_plot <- 
  ggplot(experiment_byversion) +
  geom_bar(aes(x = profile_factor, y = fund_mean, group = report_factor, fill = report_factor),
           stat = "identity", position = "dodge", show.legend = T) +
  geom_errorbar(aes(x = profile_factor, ymin = fund_mean - fund_se, ymax = fund_mean + fund_se, group = report_factor),
                stat = "identity", position = position_dodge(width = 0.9), width = 0.15, linewidth = 0.5) +
  geom_bracket(xmin = 1.6, xmax = 2.4, y.position = 80, label = "\U0394 = 6.75, p = 0.0647", 
               size = 0.75, tip.length = 0.1, vjust = -0.2, label.size = 3) +
  scale_y_continuous(name = "Mean",
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 10),
                     limits = c(0, 100)) +
  scale_x_discrete(name = "") +
  scale_fill_manual(name = "",
                    values = c("#44AA99", "#332288")) +
  labs(title = "Worthiness of Funding",
       subtitle = "composite index ranging from 0-100") +
  time.plot.theme



# mediators mean/se
comprehension_means_plot <- 
  ggplot(experiment_byversion) +
  geom_bar(aes(x = profile_factor, y = comprehension_mean, group = report_factor, fill = report_factor),
           stat = "identity", position = "dodge", show.legend = F) +
  geom_errorbar(aes(x = profile_factor, ymin = comprehension_mean - comprehension_se, ymax = comprehension_mean + comprehension_se, group = report_factor),
                stat = "identity", position = position_dodge(width = 0.9), width = 0.15, linewidth = 0.5) +
  geom_bracket(xmin = 1.6, xmax = 2.4, y.position = 80, label = "\U0394 = 2.64, p = 0.7888", 
               size = 0.75, tip.length = 0.1, vjust = -0.2, label.size = 3) +
  scale_y_continuous(name = "Mean",
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 10),
                     limits = c(0, 100)) +
  scale_x_discrete(name = "") +
  scale_fill_manual(name = "",
                    values = c("#44AA99", "#332288")) +
  labs(title = "Comprehension",
       subtitle = "composite index ranging from 0-100") +
  time.plot.theme

skepticism_means_plot <- 
  ggplot(experiment_byversion) +
  geom_bar(aes(x = profile_factor, y = skepticism_mean, group = report_factor, fill = report_factor),
           stat = "identity", position = "dodge", show.legend = F) +
  geom_errorbar(aes(x = profile_factor, ymin = skepticism_mean - skepticism_se, ymax = skepticism_mean + skepticism_se, group = report_factor),
                stat = "identity", position = position_dodge(width = 0.9), width = 0.15, linewidth = 0.5) +
  geom_bracket(xmin = 1.6, xmax = 2.4, y.position = 64, label = "\U0394 = -10.36, p = 0.0183", 
               size = 0.75, tip.length = 0.1, vjust = -0.2, label.size = 3) +
  scale_y_continuous(name = "Mean",
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 10),
                     limits = c(0, 100)) +
  scale_x_discrete(name = "") +
  scale_fill_manual(name = "",
                    values = c("#44AA99", "#332288")) +
  labs(title = "Skepticism",
       subtitle = "composite index ranging from 0-100") +
  time.plot.theme

values_means_plot <- 
  ggplot(experiment_byversion) +
  geom_bar(aes(x = profile_factor, y = values_mean, group = report_factor, fill = report_factor),
           stat = "identity", position = "dodge", show.legend = F) +
  geom_errorbar(aes(x = profile_factor, ymin = values_mean - values_se, ymax = values_mean + values_se, group = report_factor),
                stat = "identity", position = position_dodge(width = 0.9), width = 0.15, linewidth = 0.5) +
  geom_bracket(xmin = 1.6, xmax = 2.4, y.position = 75, label = "\U0394 = 7.91, p = 0.0542", 
               size = 0.75, tip.length = 0.1, vjust = -0.2, label.size = 3) +
  scale_y_continuous(name = "Mean",
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 10),
                     limits = c(0, 100)) +
  scale_x_discrete(name = "") +
  scale_fill_manual(name = "",
                    values = c("#44AA99", "#332288")) +
  labs(title = "Group Identity",
       subtitle = "composite index ranging from 0-100") +
  time.plot.theme

credibility_means_plot <- 
  ggplot(experiment_byversion) +
  geom_bar(aes(x = profile_factor, y = credibility_mean, group = report_factor, fill = report_factor),
           stat = "identity", position = "dodge", show.legend = F) +
  geom_errorbar(aes(x = profile_factor, ymin = credibility_mean - credibility_se, ymax = credibility_mean + credibility_se, group = report_factor),
                stat = "identity", position = position_dodge(width = 0.9), width = 0.15, linewidth = 0.5) +
  geom_bracket(xmin = 1.6, xmax = 2.4, y.position = 74, label = "\U0394 = 5.72, p = 0.0999", 
               size = 0.75, tip.length = 0.1, vjust = -0.2, label.size = 3) +
  scale_y_continuous(name = "Mean",
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 10),
                     limits = c(0, 100)) +
  scale_x_discrete(name = "") +
  scale_fill_manual(name = "",
                    values = c("#44AA99", "#332288")) +
  labs(title = "Credibility",
       subtitle = "composite index ranging from 0-100") +
  time.plot.theme


means_legend <-
  get_legend(ggplot(experiment_byversion) + 
               geom_bar(aes(x = profile_factor, y = credibility_mean, fill = report_factor), 
                        stat = "identity") + 
               scale_fill_manual(name = "", values = c("#44AA99", "#332288")) +
               guides(fill = guide_legend(ncol = 2))
  )
  
mediator_means_plots_arranged <-
  grid.arrange(comprehension_means_plot, credibility_means_plot,
               values_means_plot, skepticism_means_plot,
               means_legend,
               heights = c(1, 1, 0.1),
               layout_matrix = rbind(c(1 , 1, 2, 2), c(3, 3, 4, 4), c(NA, 5, 5, NA)))


# ---- export findings and plots ----

dir.create("data/outputs/experiment")
experiment_output_dir <- "data/outputs/experiment/"


png(paste(experiment_output_dir, "fund_means_plot.png", sep = ""),
    units = "in", height = 3.5, width = 5, res = 400)
grid.newpage()
grid.draw(fund_means_plot)
dev.off()

png(paste(experiment_output_dir, "donate_means_plot.png", sep = ""),
    units = "in", height = 3.5, width = 5, res = 400)
grid.newpage()
grid.draw(donate_means_plot)
dev.off()

# 
# png(paste(experiment_output_dir, "dv_means_plot.png", sep = ""),
#     units = "in", height = 4, width = 8, res = 400)
# grid.newpage()
# grid.draw(dv_means_plots_arranged)
# dev.off()

png(paste(experiment_output_dir, "mediator_means_plot.png", sep = ""),
    units = "in", height = 7, width = 7, res = 400)
grid.newpage()
grid.draw(mediator_means_plots_arranged)
dev.off()
