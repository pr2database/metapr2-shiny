
# UI ----------------------------------------------------------------------

mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('ui_map'))
  )
}



# Server ------------------------------------------------------------------


mapServer <- function(id, df_selected_taxa_one, samples_selected, taxo) {
  # stopifnot(is.reactive(taxo))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    

    # Legend title --------------------------------------------------------------
    
    # legend_title <- reactive( if_else(TRUE, "% of euks", "% of supergroup"))
    
    legend_title <- function(){"% of euks"} 
    
    # Reformat df for maps ----------------------------------------------------
    
    df_map <- reactive({
      req(df_selected_taxa_one(), samples_selected(), taxo())
      df_selected_taxa_one() %>%
        reformat_df_map(samples = samples_selected(), taxo_level = taxo()$level, taxo_name = taxo()$name)
    })
    
    # Computer number of samples with present and absent ----------------------
    
    
    n_samples_with_taxa <- reactive(n_distinct(df_map()$present$file_code))
    n_samples_without_taxa <- reactive(n_distinct(df_map()$absent$file_code))

    output$sample_number <- renderText({stringr::str_c("Number of samples with taxon: <b>",n_samples_with_taxa(),
                                                       "</b>, without taxon: <b>",n_samples_without_taxa(),"</b>",
                                                       sep=" ")})
    
    output$taxo_selected <- renderText({stringr::str_c("Taxo level: <b>", taxo$level, 
                                                       "</b>- Taxon name: <b>", taxo$name,"</b>", sep=" ")})
    
    
    # Create the map ----------------------------------------------------------
    
    # Initialize
      output$map_1 <- renderLeaflet({map_leaflet_init()})
    
    # Render the map
      observe({
        req(df_map, input$pct_max, input$map_type)
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
          includeMarkdown(system.file("readme", 'map.md', package = "metapr2")),
          p(),
          htmlOutput(ns("taxo_selected")),
          htmlOutput(ns("sample_number")),
          p(),
          fluidRow(
            column(3, 
                radioButtons(
                inputId = ns("map_type"),
                label = "Map type",
                choices = c("pie chart" = "pie", "dominant taxon" = "dominant"),
                selected = "pie",
                inline = TRUE,
                width = NULL)
            ),
            column(3,
               shinyWidgets::sliderTextInput(inputId =  ns("pct_max"), 
                                             label ="Change circle scale - % max",
                                             choices = c(seq(from = 100, to = 10, by = -10),
                                                         seq(from = 9, to = 0, by = -1))  ,
                                             selected = 100, grid = TRUE, post = " %",
                                             width = "200%"
               )
            )
          ),
          shinycssloaders::withSpinner(leafletOutput(ns("map_1"), width ="auto", height = 800))
        )
    })
      
  })
  
}  
