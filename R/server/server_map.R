# Lend title --------------------------------------------------------------

# legend_title <- reactive( if_else(TRUE, "% of euks", "% of supergroup"))

legend_title <- function(){"% of euks"} 

# Reformat df for maps ----------------------------------------------------

df_map <- reactive({
  req(df_selected_taxa_one())
  df_selected_taxa_one() %>%
    reformat_df_map(samples = samples_selected(), taxo_level = taxo()$level, taxo_name = taxo()$name)
})

# Computer number of samples with present and absent ----------------------


n_samples_with_taxa <- reactive(n_distinct(df_map()$present$file_code))
n_samples_without_taxa <- reactive(n_distinct(df_map()$absent$file_code))

output$sample_number <- renderText({stringr::str_c("Number of samples with taxon: <b>",n_samples_with_taxa(),
                                                   "</b>, without taxon: <b>",n_samples_without_taxa(),"</b>",
                                                   sep=" ")})

output$taxo_selected <- renderText({stringr::str_c("Taxo level: <b>", taxo()$level, 
                                                   "</b>- Taxon name: <b>", taxo()$name,"</b>", sep=" ")})


# Create the map ----------------------------------------------------------

# Initialize
  output$map_1 <- renderLeaflet({map_leaflet_init()})

# Render the map
  observe({
    req(input$pct_max, input$map_type)
    leafletProxy("map_1") %>%
      clearControls() %>%
      clearMarkers() %>% 
      leaflet.minicharts::clearMinicharts() %>% 
      map_leaflet(df_map(), 
                  pct_max = input$pct_max, 
                  legend_title = legend_title(),
                  map_type = input$map_type)
})


# UI for map --------------------------------------------------------------


output$ui_map <- renderUI({
    tagList(
      includeMarkdown("readme/map.md"),
      p(),
      htmlOutput("sample_number"),
      htmlOutput("taxo_selected"),
      p(),
      fluidRow(
        column(2, 
            radioButtons(
            inputId = "map_type",
            label = "Map type",
            choices = c("pie chart" = "pie", "dominant taxon" = "dominant"),
            selected = "pie",
            inline = TRUE,
            width = NULL)
        ),
        column(3,
           shinyWidgets::sliderTextInput(inputId =  "pct_max", 
                                         label ="Change circle scale - % max",
                                         choices = c(seq(from = 100, to = 10, by = -10),
                                                     seq(from = 9, to = 0, by = -1))  ,
                                         selected = 100, grid = TRUE, post = " %",
                                         width = "200%"
           )
        )
      ),
      shinycssloaders::withSpinner(leafletOutput("map_1", width ="auto", height = 800))
    )
})
  
# Debug -------------------------------------------------------------------



# Debug
# browser()

# df_test <- reactive({
#   asv_set$df %>%
#     filter(file_code %in% samples()$file_code)
# })
# 
# df2 <- select(asv_set$fasta, any_of(global$taxo_levels))
#   
# 
# output$test3 <- renderDataTable(df_map()$present) 
# output$test4 <- renderDataTable(df2) 