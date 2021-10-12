
# Fn to draw treemaps -----------------------------------------------------


treemap <- function(df, taxo_level) {
  
  taxo_level_number = which(global$taxo_levels == taxo_level)
  
  # Do not go beyond ASV level (taxo_level_number = 9)
  if(taxo_level_number < 8 ){
    taxo_level_1 = global$taxo_levels[taxo_level_number + 1] 
    taxo_level_2 = global$taxo_levels[taxo_level_number + 2] 
  } else {
    taxo_level_1 = global$taxo_levels[taxo_level_number] 
    taxo_level_2 = global$taxo_levels[taxo_level_number + 1]   
  }
  
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
    treemapify::geom_treemap_text(colour = "white", place = "centre", grow = FALSE) +
    treemapify::geom_treemap_subgroup_border() +
    treemapify::geom_treemap_subgroup_text(place = "topleft", grow = F, 
                                           alpha = 0.5, colour = "black", 
                                           min.size = 0) +
    theme_bw() +
    scale_color_brewer() +
    guides(fill = FALSE)
  
}
# UI ----------------------------------------------------------------------

treemapUI <- function(id) {
  ns <- NS(id)
  tagList(
    p("Number of reads have been normalized (not rarefield) to 100 with 3 decimals."), 
    shinycssloaders::withSpinner(uiOutput(ns('treemap')))
  )
}



# Server ------------------------------------------------------------------


treemapServer <- function(id, df_selected, taxo) {
  # stopifnot(is.reactive(taxo))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)

    output$treemap <- renderUI({
      req(df_selected(), taxo())
      tagList(
        p(""),
        renderPlot({
          treemap(df_selected(), taxo_level = taxo()$level)
        },  height = 800, width = 800, res = 96)
      )  
    })  

  })
  
} 