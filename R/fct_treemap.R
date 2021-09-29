treemap <- function(df, taxo_level) {
  
  taxo_level_1 = taxo_levels[which(taxo_levels == taxo_level) + 1] 
  taxo_level_2 = taxo_levels[which(taxo_levels == taxo_level) + 2]   
  # Group
  df <- df %>%
    count(!!as.symbol(taxo_level_1), !!as.symbol(taxo_level_2), wt=n_reads) %>% 
    ungroup()
  
  
  # Do a treemap
  
  # ggplot(df, aes(area = n, fill = {{level2}}, subgroup = {{level1}}, label = {{level2}})) +
  #   treemapify::geom_treemap()
  
  ggplot(df, aes(area = n, 
                 fill= !!as.symbol(taxo_level_1), 
                 subgroup = !!as.symbol(taxo_level_1), 
                 label = !!as.symbol(taxo_level_2))) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
    treemapify::geom_treemap_subgroup_border() +
    treemapify::geom_treemap_subgroup_text(place = "topleft", grow = F, 
                                           alpha = 0.5, colour = "black", 
                                           min.size = 0) +
    theme_bw() +
    scale_color_brewer() +
    guides(fill = FALSE)
  
}