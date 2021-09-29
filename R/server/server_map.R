
# legend_title <- reactive( if_else(TRUE, "% of euks", "% of supergroup"))

legend_title <- function(){"% of euks"} 

# output$test1 <- renderText(taxo()$level)
# output$test2 <- renderText(taxo()$name)

# Debug
# browser()

df_map <- reactive({
  df_selected_taxa_one() %>%
    reformat_df_map(samples = samples_selected(), taxo_level = taxo()$level, taxo_name = taxo()$name)
})

n_samples_with_taxa <- reactive(n_distinct(df_map()$present$file_code))
n_samples_without_taxa <- reactive(n_distinct(df_map()$absent$file_code))

output$sample_number <- renderText({stringr::str_c("Number of samples with taxon: <b>",n_samples_with_taxa(),
                                                   "</b>, without taxon: <b>",n_samples_without_taxa(),"</b>",
                                                   sep=" ")})

output$taxo_selected <- renderText({stringr::str_c("Taxo level: <b>", taxo()$level, 
                                                   "</b>- Taxon name: <b>", taxo()$name,"</b>", sep=" ")})

output$map_1 <- renderLeaflet({map_leaflet_init()})

observe({
  leafletProxy("map_1") %>%
    clearControls() %>%
    clearMarkers() %>%
    map_leaflet(df_map(), pct_max = input$pct_max, legend_title = legend_title())
})



# Debug
# browser()

# df_test <- reactive({
#   asv_set$df %>%
#     filter(file_code %in% samples()$file_code)
# })
# 
# df2 <- select(asv_set$fasta, any_of(taxo_levels))
#   
# 
# output$test3 <- renderDataTable(df_test()) 
# output$test4 <- renderDataTable(df2) 