
# code: visualize lists of symbol types (e.g., buzzwords) per institution


# ---- import libraries, source plot themes ----

pacman::p_load(spam, spam64, ggplot2, grid, gridExtra)

options(spam.force64 = TRUE)


source("code/source/plotThemes.R")


# ---- Function to visualize the top terms per symbol type ----

makeTopSymbolPlots <- function(sort_by = "yearfreq", input_data, input_suffix, input_year,
                               input_corpus, input_symbol, input_consensus, input_percentile, num_years_cutoff = 3, 
                               title_on = TRUE) {
  
  
  dir.create("data/outputs/figures/")
  dir.create(paste("data/outputs/figures/", format(Sys.Date(),"%Y%m%d"), sep = ""))
  output_dir <- paste("data/outputs/figures/", format(Sys.Date(),"%Y%m%d"), sep = "")
  
  percentile_label <- paste(input_percentile*100, "th", sep = "") # define label for percentile
  
  if(length(input_year)>1) {
    year_label <- paste(as.character(input_year[1]), "-", as.character(input_year[length(input_year)]), sep = "")[1] # define label for years included in plot
  } else { year_label <- as.character(input_year) }

  symbol_label <- case_when(input_symbol=="placeholder" ~ "buzzword",
                            input_symbol=="buzzword" ~ "buzzword (semantic def)",
                            TRUE ~ input_symbol)

  corpus_filter <- case_when(input_corpus%in%c("Academic", "NGO", "Media") ~ tolower(input_corpus),
                             TRUE ~ input_corpus)

    dat <-
      input_data %>%
      filter(year%in%input_year &
               corpus==corpus_filter &
               consensus_threshold==input_consensus &
               percentile_threshold==input_percentile &
               symbol_type==input_symbol) %>%
      group_by(node) %>%
      summarise(most_recent_year = max(year),
                num_years = length(node),
                rel_freq = rel_freq[year==most_recent_year],
                conductivity = conductivity[year==most_recent_year]) %>%
      mutate(most_recent_year = factor(as.character(most_recent_year), 
                                       levels = c("2017", "2018", "2019", "2020", "2021", "2022"), 
                                       ordered = T))

    if(title_on){
    plot.title <- paste(input_corpus, " ",
                        symbol_label, "s, ",
                        year_label, sep = "")
    } else {plot.title <- NULL}

    if(sort_by=="yearfreq") {
      dat <- dat %>%
      arrange(desc(num_years), desc(rel_freq)) %>%
      mutate(node = factor(node, levels = unique(node), ordered = T),
             size = as.character(num_years),
             size = factor(size,
                           levels = c("1", "2", "3", "4", "5"),
                           ordered = T))

      dat <- dat %>% filter(num_years>=num_years_cutoff)

      if(title_on){
      plot.subtitle <- str_wrap(paste("By number of years & relative document frequency, ",
                                      input_consensus, " consensus threshold, ",
                                      percentile_label, " percentile threshold",
                                      sep = ""), width = 51)
      } else {plot.subtitle <- NULL}
    }

    if(sort_by=="freq") {
      dat <- dat %>%
        arrange(desc(rel_freq)) %>%
        mutate(node = factor(node, levels = unique(node), ordered = T),
               size = as.character(num_years)) %>%
        slice_head(n = 30)

      if(title_on){
      plot.subtitle <- str_wrap(paste("Top 30 by relative document frequency,     ",
                                      input_consensus, " consensus threshold, ",
                                      percentile_label, " percentile threshold",
                                      sep = ""), width = 51)
      } else {plot.subtitle <- NULL}
    }

    if(sort_by=="freq_byyear") {
      dat <- input_data %>%
        filter(year==input_year &
                 corpus==corpus_filter &
                 consensus_threshold==input_consensus &
                 percentile_threshold==input_percentile &
                 symbol_type==input_symbol) %>%
        arrange(desc(rel_freq)) %>%
        mutate(node = factor(node, levels = unique(node), ordered = T)) %>%
        slice_head(n = 30)

      if(title_on){
      plot.title <- paste(input_corpus, " ",
                          symbol_label, "s, ",
                          input_year, sep = "")
      plot.subtitle <- str_wrap("Top 30 by relative document frequency", width = 51)
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
                                      input_consensus, " consensus threshold, ",
                                      percentile_label, " percentile threshold",
                                      sep = ""), width = 51)
      } else {plot.subtitle <- NULL}

      size.legend <- "network connectivity"
      size.values <- NULL
    }

    # make preliminary plot
    if(sort_by=="freq" | sort_by=="yearfreq" | sort_by=="conductivity") {
      
      plot <-
        ggplot(dat) +
        geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node, color = most_recent_year),
                     stat = "identity") +
        geom_point(aes(x = rel_freq, y = node, size = size, color = most_recent_year),
                   fill = NA) +
        scale_color_manual(name = str_wrap(paste("most recently a ", symbol_label, sep = ""),
                                           width = 11),
                           values = c("2017" = "#661100",
                                      "2018" = "#CC6677",
                                      "2019" = "#DDCC77",
                                      "2020" = "#44AA99",
                                      "2021" = "#332288",
                                      "2022" = "#117733"),
                           drop = F) +
        scale_x_continuous(expand = expansion(c(0, 0.05)),
                           labels = scales::percent_format()) +
        scale_y_discrete(limits = rev) +
        labs(x = "% of documents", y = "",
             title = plot.title,
             subtitle = plot.subtitle) +
        lollipop.plot.theme + lollipop.legend.guide
    }
    
    # adjust scaling based on type of plot
    if(sort_by=="freq" | sort_by=="yearfreq") {
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

    # create separate plot if only doing a single year at a time (sort_by==freq_byyear)
    if(sort_by=="freq_byyear") {
      plot <-
        ggplot(dat) +
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
             title = plot.title,
             subtitle = plot.subtitle) +
        lollipop.plot.theme + lollipop.legend.guide
    }
    
    # export plot
    output_filename <- ifelse(sort_by=="freq_byyear",
                              paste(output_dir, "/", input_symbol, "_",
                                    input_suffix, "_", sort_by, "_", input_year, "_c", input_consensus, "_p", input_percentile, ".png", sep = ""),
                              paste(output_dir, "/", input_symbol, "_",
                                    input_suffix, "_", sort_by, "_c", input_consensus, "_p", input_percentile, ".png", sep = ""))

    png(output_filename,
        units = "in", height = 7, width = 5, res = 400)
    grid.newpage()
    grid.draw(plot)
    dev.off()

}

# ---- make top symbol plots sorting by number of years and freq ----

# -- academic
makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = words_centralnodes, 
                   input_suffix = "a", 
                   input_year = 2017:2021,
                   input_corpus = "Academic", 
                   input_symbol = "placeholder",
                   input_consensus = 0.3,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)


# -- ngo
makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = words_centralnodes, 
                   input_suffix = "n", 
                   input_year = 2017:2021,
                   input_corpus = "NGO", 
                   input_symbol = "placeholder",
                   input_consensus = 0.75,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)


# -- media -- NYT
makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = words_centralnodes, 
                   input_suffix = "m", 
                   input_year = 2017:2021,
                   input_corpus = "Media", 
                   input_symbol = "placeholder",
                   input_consensus = 0.5,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)


# ---- make top symbol plots just sorting by frequency only ----

# -- academic
makeTopSymbolPlots(sort_by = "freq",
                   input_data = words_centralnodes, 
                   input_suffix = "a", 
                   input_year = 2017:2021,
                   input_corpus = "Academic", 
                   input_symbol = "placeholder",
                   input_consensus = 0.3,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)


# -- ngo
makeTopSymbolPlots(sort_by = "freq",
                   input_data = words_centralnodes, 
                   input_suffix = "n", 
                   input_year = 2017:2021,
                   input_corpus = "NGO", 
                   input_symbol = "placeholder",
                   input_consensus = 0.75,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)


# -- media
makeTopSymbolPlots(sort_by = "freq",
                   input_data = words_centralnodes, 
                   input_suffix = "m", 
                   input_year = 2017:2021,
                   input_corpus = "Media", 
                   input_symbol = "placeholder",
                   input_consensus = 0.5,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)



# ---- make top symbols plots by conductivity ----

# -- academic 
makeTopSymbolPlots(sort_by = "conductivity",
                   input_data = words_centralnodes, 
                   input_suffix = "a", 
                   input_year = 2017:2021,
                   input_corpus = "Academic", 
                   input_symbol = "placeholder",
                   input_consensus = 0.3,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)


# -- ngo 
makeTopSymbolPlots(sort_by = "conductivity",
                   input_data = words_centralnodes, 
                   input_suffix = "n", 
                   input_year = 2017:2021,
                   input_corpus = "NGO", 
                   input_symbol = "placeholder",
                   input_consensus = 0.75,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)


# -- media 
makeTopSymbolPlots(sort_by = "conductivity",
                   input_data = words_centralnodes, 
                   input_suffix = "m", 
                   input_year = 2017:2021,
                   input_corpus = "Media", 
                   input_symbol = "placeholder",
                   input_consensus = 0.5,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)


# ---- make top symbol plots for top symbols per year, sorting by frequency ----

# -- academic

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = words_centralnodes, 
                   input_suffix = "a", 
                   input_year = 2021,
                   input_corpus = "Academic", 
                   input_symbol = "placeholder",
                   input_consensus = 0.3,
                   input_percentile = 0.5, 
                   title_on = TRUE)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = words_centralnodes, 
                   input_suffix = "n", 
                   input_year = 2021,
                   input_corpus = "NGO", 
                   input_symbol = "placeholder",
                   input_consensus = 0.75,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = words_centralnodes, 
                   input_suffix = "m", 
                   input_year = 2021,
                   input_corpus = "Media", 
                   input_symbol = "placeholder",
                   input_consensus = 0.5,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = words_centralnodes, 
                   input_suffix = "ipbes", 
                   input_year = 2019,
                   input_corpus = "IPBES", 
                   input_symbol = "placeholder",
                   input_consensus = 0.75,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)

makeTopSymbolPlots(sort_by = "freq_byyear",
                   input_data = words_centralnodes, 
                   input_suffix = "uncbd", 
                   input_year = 2022,
                   input_corpus = "UNCBD", 
                   input_symbol = "placeholder",
                   input_consensus = 0.75,
                   input_percentile = 0.5, 
                   num_years_cutoff = 3,
                   title_on = TRUE)

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

