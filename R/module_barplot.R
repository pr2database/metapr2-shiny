

# UI ----------------------------------------------------------------------


barplotUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('ui_barplot')),
    shinycssloaders::withSpinner(uiOutput(ns('graph_barplot')))
  )
}



# Server ------------------------------------------------------------------


barplotServer <- function(id, df, taxo, messages) {
  # stopifnot(is.reactive(df))
  
  ns <- NS(id)
  
  moduleServer(id, function(input, output, session) {
    

    output$ui_barplot <- renderUI({
      tagList(
        includeMarkdown(system.file("readme", 'barplot.md', package = "metapr2")),
        p(),
        radioButtons(ns("color_coding"), "Colors correspond to ", inline = TRUE,
                     choices = c("taxonomy", "ecological_function"),
                     selected = c("taxonomy")),
        p(),
        fluidRow(
          column(9, radioButtons(ns("barplot_variable"), "Variable to use for barplots:", inline = TRUE,
                                 choices = c("fraction_name", "ecosystem", "substrate", "depth_level", 
                                             "depth","DNA_RNA", "latitude", "temperature", "salinity", "year", "month", "day"),
                                 selected = c("latitude")))
        ),
      )
    })
    
    

    output$graph_barplot <- renderUI({
      req(df(), taxo(), input$barplot_variable, input$color_coding)
      

      # req(input$barplot_variable)
      tagList(
            if(nrow(df()) > 0) {
            plotly::renderPlotly({
              plotly::ggplotly(
                barplot(df(),
                       variable = input$barplot_variable,
                       color_coding = input$color_coding, 
                       taxo_level = ifelse(taxo()$level != "asv_code",
                                           global$taxo_levels[which(global$taxo_levels == taxo()$level) + 1],
                                           "asv_code")
                       ),
                height = 1000, width=1200
              )
            })}
            # },) }
            else {
              messages$no_data
            }
            )
    })
    })
  
}  



# TestApp <- function() {
#   ui <- fluidPage(
#     sidebarLayout(
#       sidebarPanel(
#       ),
#       mainPanel(
#         barplotUI("barplot") 
#       )
#     )
#   )
#   server <- function(input, output, session) {
#     barplotServer("barplot", df)
#   }
#   shinyApp(ui, server)
# }
# 
# TestApp()



