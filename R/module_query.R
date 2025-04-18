
# To do

# - Merge all ASVs that are similar and do map as % of eukaryotes or of other level...


# UI ----------------------------------------------------------------------

queryUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('ui_query')),
    uiOutput(ns('ui_query_results')),
    uiOutput(ns('ui_map_asv')),
    # shinycssloaders::withSpinner(DT::dataTableOutput(ns("asv_filtered")))
  )
}



# Server ------------------------------------------------------------------


queryServer <- function(id, samples_selected, df_all, fasta_all) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    


    # Compute similarity between query sequence and all ASVs
    asv_blast <- eventReactive(input$button_match, {
      
      iv_query <- shinyvalidate::InputValidator$new()
      iv_query$add_rule("query", ~ if(!sequence_check(.)) " Please enter valid sequence at least 130 bp long without extra characters (can be in FASTA format).")
      
      iv_query$enable()
      
      req(iv_query$is_valid())
    
      cat("N rows fasta: ", nrow(fasta_all()), "\n")
      
      blast <- blaster_asv(fasta_all(), input$query)
      if(!is.null(blast)){ 
        blast <- blast %>% 
          tibble::column_to_rownames(var = "asv_code") %>% 
          relocate(sum_reads_asv, .after = pid) %>% 
          arrange(desc(pid), desc(sum_reads_asv))
      }
      return(blast)
      })
    
    # Filter the ASV based on % ID
    asv_filtered <- eventReactive(
      {input$button_match
        input$pct_id_min}, 
      {
      if(!is.null(asv_blast())){
          asv_blast()  %>% 
            dplyr::filter(pid >= input$pct_id_min)
      } else {
          NULL
        }
    })
    
    
    # output$query_valid <- renderText(asv_blast())
    
    # UI for Query ------------------------------------------------------------
    
    output$ui_query <- renderUI({
      tagList(
    
        h4("BLAST search for  ASVs similar to query sequence."),
        p("All ASVs are searched but only those above % identity min are shown"),
        p("Only ASVs from the selected datasets/ samples are mapped"),
        p(),
        
        sliderInput(ns("pct_id_min"), label ="% identity min", min = 80.0, max = 100.0, 
                    step = 0.2, value = 95, width = "500px"),
        
        textAreaInput(ns("query"), label = "Query - at least 130 bp", value = "", 
                      width = "100%", height = "100px",
                      cols = NULL, rows = NULL, 
                      placeholder = "GTAGTTGGATTTCTGTTGAGGACGGC...", resize = NULL),
        
        actionButton(ns("button_match"), "Search"),
    )
    })
    
    
    
    # UI <- Make a table of the filtered ASVs ------------------------------
    
    
    output$asv_filtered <-   DT::renderDT(asv_filtered() %>% 
                     mutate(sequence = gsub("(.{80})","\\1\n",sequence)),
                   rownames = TRUE,
                   selection = 'single',
                   options = list(pageLength = 5,
                                  autoWidth = TRUE,
                                  scrollX=TRUE,
                                  columnDefs = list(list(width = '10%', targets = c(1)),
                                                    list(width = '20%', targets = c(9:11)))
                   )
      )
    
    output$ui_query_results <- renderUI({
      # req(asv_filtered())
      if(!is.null(asv_filtered())){
        tagList(
          p(),
          h4("Matching ASVs"),
          p(),
          shinycssloaders::withSpinner(DT::dataTableOutput(ns("asv_filtered"))),
          # renderPrint({
          #   cat("ASV selected: ",  asv_selected())
          # })
          
        )
      } else {
        tagList(
          h3("No Matching ASVs")
        )
      }
    })
    
    asv_selected <- eventReactive(
      input$asv_filtered_rows_selected,
      row.names(asv_filtered()[input$asv_filtered_rows_selected,])
      )
    
    # Reformat df for maps ----------------------------------------------------
    # Note: here we do not filter by taxonomy or number of total reads
    
    df_map <- reactive({
      req(samples_selected(), asv_selected() ) 
      df_all() %>%
        filter(file_code %in% samples_selected()$file_code) %>% 
        left_join(samples_selected()) %>% 
        left_join(select(fasta_all(), asv_code, domain:species, sum_reads_asv)) %>%
        filter(asv_code == asv_selected()) %>% 
        reformat_df_map(samples = samples_selected(), taxo_level = "asv_code")
    })
    
    # Computer number of samples with present and absent ----------------------
    
    
    n_samples_with_taxa <- reactive(n_distinct(df_map()$present$file_code))
    n_samples_without_taxa <- reactive(n_distinct(df_map()$absent$file_code))
    
    output$sample_number <- renderText({stringr::str_c("Number of samples with ASV: <b>",n_samples_with_taxa(),
                                                       "</b>, without taxon: <b>",n_samples_without_taxa(),"</b>",
                                                       sep=" ")})
    
    output$taxo_selected <- renderText({stringr::str_c("ASV code: <b>", asv_selected(),"</b>", sep=" ")})
    
    
    # Create the map ----------------------------------------------------------
    
    # Initialize
    output$map_1 <- renderLeaflet({map_leaflet_init()%>%
                    map_leaflet(df_map(),
                    pct_max = round(max(df_map()$present$pct),0),
                    map_type = "dominant")
      })
    

    # UI for map --------------------------------------------------------------
    
    
    output$ui_map_asv <- renderUI({
      req(asv_selected())
      tagList(
        p(),
        htmlOutput(ns("taxo_selected")),
        htmlOutput(ns("sample_number")),
        p(),
        p("Crosses indicate samples where ASV is missing"),
        shinycssloaders::withSpinner(leafletOutput(ns("map_1"), width ="auto", height = 800))
      )
    })
    
  })
  
} 
