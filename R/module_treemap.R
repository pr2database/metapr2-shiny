# UI ----------------------------------------------------------------------

treemapUI <- function(id) {
  ns <- NS(id)
  tagList(
    p("Number of reads have been normalized (not rarefield) to 100 with 3 decimals."), 
    shinycssloaders::withSpinner(uiOutput(ns('treemap')))
  )
}



# Server ------------------------------------------------------------------


treemapServer <- function(id, df_selected_taxa_one, taxo) {
  # stopifnot(is.reactive(taxo))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)

    output$treemap <- renderUI({
      req(df_selected_taxa_one(), taxo())
      tagList(
        p(""),
        renderPlot({
          treemap(df_selected_taxa_one(), taxo_level = taxo()$level)
        },  height = 800, width = 800, res = 96)
      )  
    })  

  })
  
} 