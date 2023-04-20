
pacman::p_load(spam, spam64, ggplot2, grid, gridExtra)

options(spam.force64 = TRUE)

findNodeAttributes(input_suffix = "n", 
                   years = 2017:2021, 
                   consensus_thresholds = 0.5, 
                   percentile_thresholds = 0.5,
                   coocTerm = "conservation")



# Pull out words and frequencies (to size the words)


place_a_2021_consensus33 <- 
  node_attributes_a_2021 %>% 
  filter(symbol_type=="placeholder" & consensus_threshold==0.33) %>%
  left_join(ndocs, by = "year") %>%
  arrange(desc(freq)) %>%
  mutate(freq_prop = freq/ndocs,
         node = factor(node, levels = unique(node), ordered = T))


buzz_n_2021_consensus5 <-
  node_attributes_n_2021 %>% 
  filter(symbol_type=="buzzword" & consensus_threshold==0.5) %>%
  left_join(ndocs, by = "year") %>%
  arrange(desc(freq)) %>%
  mutate(freq_prop = freq/ndocs,
         node = factor(node, levels = unique(node), ordered = T))

# Visualize the terms 

makeFreqPlots <- function(input_data, input_suffix, corpus, years, symbol, consensus_thresholds) {
  
  
  dir.create("data/outputs/figures/")
  dir.create(paste("data/outputs/figures/", format(Sys.Date(),"%Y%m%d"), sep = ""))
  output_dir <- paste("data/outputs/figures/", format(Sys.Date(),"%Y%m%d"), sep = "")
  
  ndocs <- 
    get(paste("docs_", input_suffix, sep = "")) %>% 
    group_by(year) %>% summarise(ndocs = length(text))
  
  for(i in years) {
    for(j in consensus_thresholds){
      dat <- 
        get(paste("node_attributes_", input_suffix, "_", i, sep = "")) %>%
        dplyr::filter(symbol_type==symbol & consensus_threshold==j) %>%
        left_join(ndocs, by = "year") %>%
        arrange(desc(freq)) %>%
        mutate(freq_prop = freq/ndocs,
               node = factor(node, levels = unique(node), ordered = T))
      
    assign(paste(symbol, "_", input_suffix, "_", i, "_consensus", j, sep = ""),
           dat,
           envir = .GlobalEnv)
    
    # if(length(dat$node)>30) { dat <- dat[1:30,] }
    
    plot <- 
      ggplot(dat) +
      geom_bar(aes(x = freq_prop, y = node),
               stat = "identity",
               fill = "#332288") +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_discrete(limits = rev) +
      labs(x = "% of documents", y = "", 
           subtitle = paste(corpus, ", ", 
                            symbol, ", ",
                            i, ", ",
                            j, " consensus threshold",
                            sep = "")) +
      lollipop.plot.theme
    
    # export plot
    png(paste(output_dir, "/", symbol,"_", 
              input_suffix, "_", i, "_consensus", j, ".png", sep = ""),
        units = "in", height = 10, width = 6, res = 400)
    grid.newpage()
    grid.draw(plot)
    dev.off()
    
    } 
  }

}

makeFreqPlots(input_suffix = "n", corpus = "NGO", 
              symbol = "buzzword", years = 2017:2021,
              consensus_thresholds = 0.5)
