
pacman::p_load(spam, spam64, ggplot2, grid, gridExtra)

options(spam.force64 = TRUE)


source("code/source/plotThemes.R")


# Visualize the top terms per symbol type

makeTopSymbolPlots <- function(sort_by = "yearfreq", input_data, input_suffix, input_year,
                               corpus, symbol, consensus, percentile, num_years_cutoff = 3, 
                               title_on = TRUE) {
  
  
  dir.create("data/outputs/figures/")
  dir.create(paste("data/outputs/figures/", format(Sys.Date(),"%Y%m%d"), sep = ""))
  output_dir <- paste("data/outputs/figures/", format(Sys.Date(),"%Y%m%d"), sep = "")
  
  symbol_label <- ifelse(symbol=="buzzplace", "buzzword", symbol)
  percentile_label <- paste(percentile*100, "th", sep = "")
  
  sym_type1 <- ifelse(symbol=="buzzplace", "buzzword", symbol)
  sym_type2 <- ifelse(symbol=="buzzplace", "placeholder", symbol)
  
    dat <- 
      input_data %>% 
      filter(get(paste(symbol, "_years_2017_2021", sep = ""))>0 &
               consensus_threshold==consensus &
               percentile_threshold==percentile) %>%
      mutate(most_recent_year = ifelse(symbol_type_2021%in%c(sym_type1, sym_type2) & !is.na(symbol_type_2021), 2021, 
                                       ifelse(symbol_type_2020%in%c(sym_type1, sym_type2) & !is.na(symbol_type_2020), 2020, 
                                              ifelse(symbol_type_2019%in%c(sym_type1, sym_type2) & !is.na(symbol_type_2019), 2019, 
                                                     ifelse(symbol_type_2018%in%c(sym_type1, sym_type2) & !is.na(symbol_type_2018), 2018, 2017)))),
             rel_freq = case_when(most_recent_year==2021 ~ rel_freq_2021,
                                  most_recent_year==2020 ~ rel_freq_2020,
                                  most_recent_year==2019 ~ rel_freq_2019, 
                                  most_recent_year==2018 ~ rel_freq_2018,
                                  most_recent_year==2017 ~ rel_freq_2017),
             conductivity = case_when(most_recent_year==2021 ~ conductivity_2021,
                                      most_recent_year==2020 ~ conductivity_2020,
                                      most_recent_year==2019 ~ conductivity_2019, 
                                      most_recent_year==2018 ~ conductivity_2018,
                                      most_recent_year==2017 ~ conductivity_2017))
    
    if(title_on){
    plot.title <- paste(corpus, " ", 
                        symbol_label, "s, ",
                        "2017-2021", sep = "")
    } else {plot.title <- NULL}
    
    if(sort_by=="yearfreq") {
      dat <- dat %>%
      arrange(desc(get(paste(symbol, "_years_2017_2021", sep = ""))), desc(rel_freq)) %>%
      mutate(node = factor(node, levels = unique(node), ordered = T),
             size = as.character(get(paste(symbol, "_years_2017_2021", sep = ""))),
             size = factor(size,
                           levels = c("1", "2", "3", "4", "5"),
                           ordered = T))
    
      dat <- dat %>% filter(get(paste(symbol, "_years_2017_2021", sep = ""))>=num_years_cutoff)
      
      if(title_on){
      plot.subtitle <- str_wrap(paste("By number of years & relative document frequency, ",
                                      consensus, " consensus threshold, ",
                                      percentile_label, " percentile threshold",
                                      sep = ""), width = 51)
      } else {plot.subtitle <- NULL}
    }
    
    if(sort_by=="freq") {
      dat <- dat %>%
        arrange(desc(rel_freq)) %>%
        mutate(node = factor(node, levels = unique(node), ordered = T),
               size = as.character(get(paste(symbol, "_years_2017_2021", sep = "")))) %>%
        slice_head(n = 30)
      
      if(title_on){
      plot.subtitle <- str_wrap(paste("Top 30 by relative document frequency,     ",
                                      consensus, " consensus threshold, ",
                                      percentile_label, " percentile threshold",
                                      sep = ""), width = 51)
      } else {plot.subtitle <- NULL}
    }
    
    if(sort_by=="freq_byyear") {
      dat <- dat %>%
        filter(get(paste("symbol_type_", input_year, sep = ""))%in%c(sym_type1, sym_type2) & 
                     !is.na(get(paste("symbol_type_", input_year, sep = "")))) %>%
        arrange(desc(rel_freq)) %>%
        mutate(node = factor(node, levels = unique(node), ordered = T),
               size = as.character(get(paste(symbol, "_years_2017_2021", sep = "")))) %>%
        slice_head(n = 30)
      
      if(title_on){
      plot.title <- paste(corpus, " ", 
                          symbol_label, "s, ", 
                          input_year, sep = "")
      plot.subtitle <- str_wrap(paste("Top 30 by relative document frequency,     ",
                                      consensus, " consensus threshold, ",
                                      percentile_label, " percentile threshold",
                                      sep = ""), width = 51)
      } else {plot.title <- NULL
              plot.subtitle <- NULL}
    }
    
    if(sort_by=="conductivity") {
      dat <- dat %>%
        arrange(desc(conductivity)) %>%
        mutate(node = factor(node, levels = unique(node), ordered = T),
               size = conductivity) %>%
        slice_head(n = 30)
      
      if(title_on){
      plot.subtitle <- str_wrap(paste("Top 30 by network conductivity,     ",
                                      consensus, " consensus threshold, ",
                                      percentile_label, " percentile threshold",
                                      sep = ""), width = 51)
      } else {plot.subtitle <- NULL}
      
      size.legend <- "network connectivity"
      size.values <- NULL
    }
    
    # make preliminary plot
    plot <- 
      ggplot(dat) +
      geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node, color = as.character(most_recent_year)),
                   stat = "identity") +
      geom_point(aes(x = rel_freq, y = node, size = size, color = as.character(most_recent_year)),
                 fill = NA) +
      scale_color_manual(name = str_wrap(paste("most recently a ", symbol_label, sep = ""),
                                         width = 11),
                         values = c("2017" = "#661100",
                                    "2018" = "#CC6677", 
                                    "2019" = "#DDCC77", 
                                    "2020" = "#44AA99", 
                                    "2021" = "#332288"),
                         drop = F) +
      scale_x_continuous(expand = c(0,0),
                         limits = c(0, max(dat$rel_freq)+0.05)) +
      scale_y_discrete(limits = rev) +
      labs(x = "% of documents", y = "", 
           title = plot.title,
           subtitle = plot.subtitle) +
      lollipop.plot.theme + lollipop.legend.guide
    
    # adjust scaling based on type of plot
    if(sort_by=="freq" | sort_by=="yearfreq" | sort_by=="freq_byyear") {
      plot <- plot + 
        scale_size_manual(name = str_wrap(paste("number years as ", symbol_label, sep = ""),
                                          width = 12),
                          values = c("1" = 1.5,
                                     "2" = 2.75,
                                     "3" = 4,
                                     "4" = 5.25,
                                     "5" = 6.5),
                          drop = F)
    }
    
    if(sort_by=="conductivity") {
      plot <- plot + 
        scale_size_binned(name = "network conductivity", 
                          n.breaks = 6)
    }
  
    
    # export plot
    output_filename <- ifelse(sort_by=="freq_byyear", 
                              paste(output_dir, "/", symbol, "_", 
                                    input_suffix, "_", sort_by, "_", input_year, "_c", consensus, "_p", percentile, ".png", sep = ""),
                              paste(output_dir, "/", symbol, "_", 
                                    input_suffix, "_", sort_by, "_c", consensus, "_p", percentile, ".png", sep = ""))
    
    png(output_filename,
        units = "in", height = 10, width = 6, res = 400)
    grid.newpage()
    grid.draw(plot)
    dev.off()
}

# ---- make top symbol plots sorting by number of years and freq ----

# -- academic
makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = compare_symbol_types_a, 
                   input_suffix = "a", 
                   corpus = "Academic", 
                   symbol = "buzzplace",
                   consensus = 0.25,
                   percentile = 0.45, 
                   num_years_cutoff = 3,
                   title_on = FALSE)

# -- ngo
makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = compare_symbol_types_n, 
                   input_suffix = "n", 
                   corpus = "NGO", 
                   symbol = "buzzplace",
                   consensus = 0.5,
                   percentile = 0.45,
                   num_years_cutoff = 2,
                   title_on = FALSE)


# -- media -- NYT
makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = compare_symbol_types_m_nyt, 
                   input_suffix = "m_nyt_filt", 
                   corpus = "Media - NYT", 
                   symbol = "buzzplace",
                   consensus = 0.5,
                   percentile = 0.45,
                   num_years_cutoff = 3,
                   title_on = FALSE)


# ---- make top symbol plots just sorting by frequency only ----

# -- academic
makeTopSymbolPlots(sort_by = "freq",
                   input_data = compare_symbol_types_a, 
                   input_suffix = "a", 
                   corpus = "Academic", 
                   symbol = "buzzplace",
                   consensus = 0.25,
                   percentile = 0.5)

# -- ngo
makeTopSymbolPlots(sort_by = "freq",
                   input_data = compare_symbol_types_n, 
                   input_suffix = "n", 
                   corpus = "NGO", 
                   symbol = "buzzplace",
                   consensus = 0.5,
                   percentile = 0.5)

# -- media
makeTopSymbolPlots(sort_by = "freq",
                   input_data = compare_symbol_types_m_nyt, 
                   input_suffix = "m_nyt_filt", 
                   corpus = "Media - NYT", 
                   symbol = "buzzplace",
                   consensus = 0.5,
                   percentile = 0.5)


# ---- make top symbols plots by conductivity ----

# -- academic 
makeTopSymbolPlots(sort_by = "conductivity",
                   input_data = compare_symbol_types_a, 
                   input_suffix = "a", 
                   corpus = "Academic", 
                   symbol = "buzzplace",
                   consensus = 0.25,
                   percentile = 0.5)

# -- ngo 
makeTopSymbolPlots(sort_by = "conductivity",
                   input_data = compare_symbol_types_n, 
                   input_suffix = "n", 
                   corpus = "NGO", 
                   symbol = "buzzplace",
                   consensus = 0.5,
                   percentile = 0.5)

# -- media 
makeTopSymbolPlots(sort_by = "conductivity",
                   input_data = compare_symbol_types_m_nyt, 
                   input_suffix = "m_nyt_filt", 
                   corpus = "Media - NYT", 
                   symbol = "buzzplace",
                   consensus = 0.75,
                   percentile = 0.5)


# ---- make top symbol plots for top symbols per year, sorting by frequency ----

# -- academic

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_a, 
                   input_suffix = "a", 
                   input_year = 2021,
                   corpus = "Academic", 
                   symbol = "buzzplace",
                   consensus = 0.25,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_a, 
                   input_suffix = "a", 
                   input_year = 2020,
                   corpus = "Academic", 
                   symbol = "buzzplace",
                   consensus = 0.25,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_a, 
                   input_suffix = "a", 
                   input_year = 2019,
                   corpus = "Academic", 
                   symbol = "buzzplace",
                   consensus = 0.25,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_a, 
                   input_suffix = "a", 
                   input_year = 2018,
                   corpus = "Academic", 
                   symbol = "buzzplace",
                   consensus = 0.25,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_a, 
                   input_suffix = "a", 
                   input_year = 2017,
                   corpus = "Academic", 
                   symbol = "buzzplace",
                   consensus = 0.25,
                   percentile = 0.5)

# -- ngo

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_n, 
                   input_suffix = "n", 
                   input_year = 2021,
                   corpus = "NGO", 
                   symbol = "buzzplace",
                   consensus = 0.5,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_n, 
                   input_suffix = "n", 
                   input_year = 2020,
                   corpus = "NGO", 
                   symbol = "buzzplace",
                   consensus = 0.5,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_n, 
                   input_suffix = "n", 
                   input_year = 2019,
                   corpus = "NGO", 
                   symbol = "buzzplace",
                   consensus = 0.5,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_n, 
                   input_suffix = "n", 
                   input_year = 2018,
                   corpus = "NGO", 
                   symbol = "buzzplace",
                   consensus = 0.5,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_n, 
                   input_suffix = "n", 
                   input_year = 2017,
                   corpus = "NGO", 
                   symbol = "buzzplace",
                   consensus = 0.5,
                   percentile = 0.5)

# -- media -- NYT

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_m_nyt, 
                   input_suffix = "m_nyt_filt", 
                   input_year = 2021,
                   corpus = "Media - NYT", 
                   symbol = "buzzplace",
                   consensus = 0.75,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_m_nyt, 
                   input_suffix = "m_nyt_filt", 
                   input_year = 2020,
                   corpus = "Media - NYT", 
                   symbol = "buzzplace",
                   consensus = 0.75,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_m_nyt, 
                   input_suffix = "m_nyt_filt", 
                   input_year = 2019,
                   corpus = "Media - NYT", 
                   symbol = "buzzplace",
                   consensus = 0.75,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_m_nyt, 
                   input_suffix = "m_nyt_filt", 
                   input_year = 2018,
                   corpus = "Media - NYT", 
                   symbol = "buzzplace",
                   consensus = 0.75,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = compare_symbol_types_m_nyt, 
                   input_suffix = "m_nyt_filt", 
                   input_year = 2017,
                   corpus = "Media - NYT", 
                   symbol = "buzzplace",
                   consensus = 0.75,
                   percentile = 0.5)

# ---- make top symbol plots for academic subgroups, by yearfreq ----

makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = compare_symbol_types_a_consbio, 
                   input_suffix = "a_consbio", 
                   corpus = "Conservation Biology", 
                   symbol = "buzzplace",
                   consensus = 0.5,
                   percentile = 0.45)

makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = compare_symbol_types_a_gec, 
                   input_suffix = "a_gec", 
                   corpus = "Global Env Change", 
                   symbol = "buzzplace",
                   consensus = 0.75,
                   percentile = 0.3)

makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = compare_symbol_types_a_sust, 
                   input_suffix = "a_sust", 
                   corpus = "Sustainability Science", 
                   symbol = "buzzplace",
                   consensus = 0.75,
                   percentile = 0.3)

