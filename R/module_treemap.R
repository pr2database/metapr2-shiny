
# Fn to draw treemaps -----------------------------------------------------


treemap <- function(df, taxo_level) {
  
    taxo_level_number = which(global$taxo_levels == taxo_level)
  
  # Do not go beyond ASV level (taxo_level_number = 9)
  if(taxo_level_number >= 8 ) taxo_level_number = 7
    
    taxo_level_1 = global$taxo_levels[taxo_level_number + 1] 
    taxo_level_2 = global$taxo_levels[taxo_level_number + 2]
  
  # Group
  # df <- df %>%
  #   count(!!as.symbol(taxo_level_1), !!as.symbol(taxo_level_2), wt=n_reads) %>% 
  #   ungroup()
  # cat("Level: ",taxo_level ,"\n")  
  # cat("Level #: ",taxo_level_number ,"\n")  
  # cat("Level 1: ",taxo_level_1 ,"\n")
  # cat("Level 2: ",taxo_level_2 ,"\n")
  # print(df)
  
  df <- df %>%
    count(across(any_of(c(taxo_level_1, taxo_level_2))), wt=n_reads_pct) %>% 
    ungroup()
  
  
  # Do a treemap
  
  # ggplot(df, aes(area = n, fill = {{level2}}, subgroup = {{level1}}, label = {{level2}})) +
  #   treemapify::geom_treemap()
  
  g <- ggplot(df, aes(area = n, 
                 fill = .data[[taxo_level_1]],
                 subgroup = .data[[taxo_level_1]], 
                 label = .data[[taxo_level_2]])) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(colour = "white", place = "centre", grow = FALSE) +
    treemapify::geom_treemap_subgroup_border() +
    treemapify::geom_treemap_subgroup_text(place = "topleft", grow = F, 
                                           alpha = 0.5, colour = "black", 
                                           min.size = 0) +
    theme_bw() +
    scale_color_brewer() +
    guides(fill = FALSE)
  
  # cat("Treemap: ")
  # print(pryr::mem_used())
  
  return(g)
  
}
# UI ----------------------------------------------------------------------

treemapUI <- function(id) {
  ns <- NS(id)
  tagList(
    p("Number of reads have been normalized (not rarefield) to 100 with 3 decimals."), 
    # div(actionButton(ns("update_treemap"), "Update treemap", class = "btn-primary"), style="text-align: left;"),
    shinycssloaders::withSpinner(uiOutput(ns('treemap')))
  )
}



# Server ------------------------------------------------------------------


treemapServer <- function(id, df_selected, taxo, messages) {
  # stopifnot(is.reactive(taxo))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    update_plot <- reactiveVal(FALSE)
    
    observeEvent(input$update_treemap, {update_plot(TRUE)})

    # Trigger by button
        output$treemap <- renderUI({
        tagList(
          p(""),
          if(nrow(df_selected()) > 0) {
            renderPlot({
              treemap(df_selected(), taxo_level = taxo()$level)
            },  height = 800, width = 800, res = 96)}
          else {
            messages$no_data
          }
        )
      })
        # update_plot(FALSE)
        # print(update_plot())
        
    # })

  })
  
} 