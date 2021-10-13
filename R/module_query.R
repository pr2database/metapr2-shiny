
# To do

# - Merge all ASVs that are similar and do map as % of eukaryotes or of other level...


# UI ----------------------------------------------------------------------

queryUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('ui_query')),
    shinycssloaders::withSpinner(uiOutput(ns('ui_query_results')))
  )
}



# Server ------------------------------------------------------------------


queryServer <- function(id, fasta_selected) {
  # stopifnot(is.reactive(taxo))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    


    # Compute similarity between query sequence and all ASVs
    asv_all <- eventReactive(input$button_match, {
      
      iv_query <- shinyvalidate::InputValidator$new()
      iv_query$add_rule("query", ~ if(!sequence_check(.)) " Please enter valid sequence at least 250 bp long.")
      
      iv_query$enable()
      
      req(iv_query$is_valid(), fasta_selected())
    
      # match_asv(top_n(asv_set$fasta, 2000), input$query) # For testing using only 2000 asv
      match_asv(fasta_selected(), input$query)
      })
    
    # Filter the ASV based on % ID
    asv_filtered <- eventReactive(
      {input$button_match
        input$pct_id_min}, 
      {
      req(asv_all())
      asv_all() %>%
        dplyr::filter(pid >= input$pct_id_min)
    })
    
    
    # output$query_valid <- renderText(asv_all())
    
    # UI for Query ------------------------------------------------------------
    
    output$ui_query <- renderUI({
      tagList(
    
        h4("BLAST-like search for  metabarcodes similar to query sequence."),
        p("Aligns query sequence to selected metabarcodes and return those above a fixed threshold"),
        p(),
        
        sliderInput(ns("pct_id_min"), label ="% identity min", min = 95.0, max = 100.0, 
                    step = 0.1, value = 100, width = "400px"),
        
        textAreaInput(ns("query"), label = "Query - at least 250 bp", value = "", 
                      width = "100%", height = "100px",
                      cols = NULL, rows = NULL, 
                      placeholder = "GTAGTTGGATTTCTGTTGAGGACGGC...", resize = NULL),
        
        actionButton(ns("button_match"), "Search"),
    )
    })
    
    
    
    # UI <- Make a table of the filtered ASVs ------------------------------
    
    output$ui_query_results <- renderUI({
      req(asv_filtered())
      tagList(
        p("Matching ASVs"),
        p(),
        DT::renderDT(asv_filtered() %>% 
                    mutate(sequence = gsub("(.{80})","\\1\n",sequence)),
                     rownames = FALSE,
                     options = list(pageLength = 20,
                                    autoWidth = TRUE,
                                    scrollX=TRUE,
                                    columnDefs = list(list(width = '10%', targets = c(1)),
                                                      list(width = '20%', targets = c(9:12)))
                                    )
                      )
    
      )
    })

  })
  
} 
