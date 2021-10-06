# To do

# - Merge all ASVs that are similar and do map as % of eukaryotes or of other level...





# Compute similarity between query sequence and all ASVs
asv_all <- eventReactive(input$button_match, {
  
  iv_query <- shinyvalidate::InputValidator$new()
  iv_query$add_rule("query", ~ if(!sequence_check(.)) " Please enter valid sequence at least 300 bp long.")
  
  iv_query$enable()
  
  req(iv_query$is_valid())

  match_asv(top_n(asv_set$fasta, 2000), input$query)
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

    h4("BLAST-like searchfor  metabarcode similar to existing sequence."),
    p("Aligns query sequence to all metabarcodes and return those above a fixed threshold"),
    p(),
    
    sliderInput("pct_id_min", label ="% identity min", min = 98.0, max = 100.0, 
                step = 0.1, value = 100, width = "300px"),
    
    textAreaInput("query", label = "Query - at least 300 bp", value = "", 
                  width = "100%", height = "100px",
                  cols = NULL, rows = NULL, 
                  placeholder = "GTAGTTGGATTTCTGTTGAGGACGGC...", resize = NULL),
    
    actionButton("button_match", "Search"),
)
})



# UI <- Make a table of the filtered ASVs ------------------------------

output$ui_query_results <- renderUI({
  req(asv_filtered())
  tagList(
    p("Matching ASVs"),
    p(),
    DT::renderDT(asv_filtered() %>% 
                   mutate(sequence = gsub("(.{80})","\\1\n",sequence),
                          asv_code = str_c(str_sub(asv_code, 1, 8),"...")),
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
