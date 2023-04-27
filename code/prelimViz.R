
pacman::p_load(spam, spam64, ggplot2, grid, gridExtra)

options(spam.force64 = TRUE)
colSums(DTM_example)

# Visualize the top terms per symbol type

makeTopSymbolPlots <- function(sort_by = "yearfreq", input_data, input_suffix, corpus, symbol, consensus, percentile) {
  
  
  dir.create("data/outputs/figures/")
  dir.create(paste("data/outputs/figures/", format(Sys.Date(),"%Y%m%d"), sep = ""))
  output_dir <- paste("data/outputs/figures/", format(Sys.Date(),"%Y%m%d"), sep = "")
    
  
    dat <- 
      input_data %>% 
      filter(get(paste(symbol, "_years_2017_2021", sep = ""))>0 &
               consensus_threshold==consensus &
               percentile_threshold==percentile) %>%
      mutate(most_recent_year = ifelse(!is.na(rel_freq_2021), 2021, 
                                       ifelse(!is.na(rel_freq_2020), 2020, 
                                              ifelse(!is.na(rel_freq_2019), 2019, 
                                                     ifelse(!is.na(rel_freq_2018), 2018, 2017)))),
             rel_freq = case_when(most_recent_year==2021 ~ rel_freq_2021,
                                  most_recent_year==2020 ~ rel_freq_2020,
                                  most_recent_year==2019 ~ rel_freq_2019, 
                                  most_recent_year==2018 ~ rel_freq_2018,
                                  most_recent_year==2017 ~ rel_freq_2017))
    
    if(sort_by=="yearfreq") {
      dat <- dat %>%
      arrange(desc(get(paste(symbol, "_years_2017_2021", sep = ""))), desc(rel_freq)) %>%
      mutate(node = factor(node, levels = unique(node), ordered = T),
             size = as.character(get(paste(symbol, "_years_2017_2021", sep = ""))),
             size = factor(size,
                           levels = c("1", "2", "3", "4", "5"),
                           ordered = T))
    
      if(symbol=="placeholder") {
        if(input_suffix=="a") { dat <- dat %>% filter(get(paste(symbol, "_years_2017_2021", sep = ""))>2) }
        if(input_suffix=="n") { dat <- dat %>% filter(get(paste(symbol, "_years_2017_2021", sep = ""))>1) }
      }
      
      if(symbol=="buzzword"){
        dat <- dat %>% filter(get(paste(symbol, "_years_2017_2021", sep = ""))>1)
      }
      
      plot.subtitle <- str_wrap(paste("By number of years & relative document frequency, ",
                                      consensus, " consensus threshold, ",
                                      percentile, " percentile threshold",
                                      sep = ""), width = 51)
    }
    
    if(sort_by=="freq") {
      dat <- dat %>%
        arrange(desc(rel_freq)) %>%
        mutate(node = factor(node, levels = unique(node), ordered = T),
               size = as.character(get(paste(symbol, "_years_2017_2021", sep = "")))) %>%
        slice_head(n = 30)
      
      plot.subtitle <- str_wrap(paste("Top 30 by relative document frequency,     ",
                                      consensus, " consensus threshold, ",
                                      percentile, " percentile threshold",
                                      sep = ""), width = 51)
    }
    

    
    plot <- 
      ggplot(dat) +
      geom_segment(aes(x = 0, xend = rel_freq, y = node, yend = node, color = as.character(most_recent_year)),
               stat = "identity") +
      geom_point(aes(x = rel_freq, y = node, size = size, color = as.character(most_recent_year)),
                 fill = NA) +
      scale_size_manual(name = paste("years as ", symbol, sep = ""),
                        values = c("1" = 1.5,
                                   "2" = 2.75,
                                   "3" = 4,
                                   "4" = 5.25,
                                   "5" = 6.5),
                        drop = F) +
      scale_color_manual(name = "most recent",
                           values = c("2017" = "#661100",
                                      "2018" = "#CC6677", 
                                      "2019" = "#DDCC77", 
                                      "2020" = "#44AA99", 
                                      "2021" = "#332288"),
                         drop = F) +
      scale_x_continuous(expand = c(0,0),
                         limits = c(0, 0.27)) +
      scale_y_discrete(limits = rev) +
      labs(x = "% of documents", y = "", 
           title = paste(corpus, " ", 
                         symbol, "s, ",
                         "2017-2021", sep = ""),
           subtitle = plot.subtitle) +
      lollipop.plot.theme + lollipop.legend.guide
    
    # export plot
    png(paste(output_dir, "/", symbol, "_", 
              input_suffix, "_", sort_by, "_c", consensus, "_p", percentile, ".png", sep = ""),
        units = "in", height = 10, width = 6, res = 400)
    grid.newpage()
    grid.draw(plot)
    dev.off()

}

# make top symbol plots sorting by number of years and freq
makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = compare_symbol_types_a, 
                   input_suffix = "a", 
                   corpus = "Academic", 
                   symbol = "placeholder",
                   consensus = 0.25,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = compare_symbol_types_a, 
                   input_suffix = "a", 
                   corpus = "Academic", 
                   symbol = "buzzword",
                   consensus = 0.25,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = compare_symbol_types_n, 
                   input_suffix = "n", 
                   corpus = "NGO", 
                   symbol = "placeholder",
                   consensus = 0.5,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "yearfreq",
                   input_data = compare_symbol_types_n, 
                   input_suffix = "n", 
                   corpus = "NGO", 
                   symbol = "buzzword",
                   consensus = 0.5,
                   percentile = 0.5)


# make top symbol plots just sorting by frequency only
makeTopSymbolPlots(sort_by = "freq",
                   input_data = compare_symbol_types_a, 
                   input_suffix = "a", 
                   corpus = "Academic", 
                   symbol = "placeholder",
                   consensus = 0.25,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq",
                   input_data = compare_symbol_types_a, 
                   input_suffix = "a", 
                   corpus = "Academic", 
                   symbol = "buzzword",
                   consensus = 0.25,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq",
                   input_data = compare_symbol_types_n, 
                   input_suffix = "n", 
                   corpus = "NGO", 
                   symbol = "placeholder",
                   consensus = 0.5,
                   percentile = 0.5)

makeTopSymbolPlots(sort_by = "freq",
                   input_data = compare_symbol_types_n, 
                   input_suffix = "n", 
                   corpus = "NGO", 
                   symbol = "buzzword",
                   consensus = 0.5,
                   percentile = 0.5)

