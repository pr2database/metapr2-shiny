
barplot <- function(df, variable, taxo_level) {
  
  # For depth only use the first 250 m
  
    if (variable == "depth") {
      df <- df %>% 
        filter(depth <=250)
    }  
  # Discretize the data
  
    df <- df %>%
      mutate(latitude =  cut_width(latitude, width=10, boundary=0),
             temperature =  cut_width(temperature, width=5, boundary=0),
             depth =  cut_width(depth, width=25, boundary=0)) 

  
  gg <- df %>% 
    select(any_of(c(taxo_level, variable)), n_reads) %>% 
    group_by(across(any_of(c(taxo_level, variable)))) %>%
    summarize(n_reads = sum(n_reads)) %>%
    group_by(across(any_of(variable))) %>% 
    mutate(n_reads = n_reads/sum(n_reads)*100) %>% 
    ggplot() +
    geom_col(aes(y=.data[[variable]], x=n_reads, fill=.data[[taxo_level]])) +
    scale_fill_viridis_d() +
    xlab("% of reads") + ylab("") +
    theme_bw()
  
  # cat("Barplot: ")
  # print(pryr::mem_used())
  
  return(gg)
  
}


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
  
  moduleServer(id, function(input, output, session) {
    

    output$ui_barplot <- renderUI({
      tagList(
        
        fluidRow(
          column(9, radioButtons(NS(id, "barplot_variable"), "Variable to use for barplots:", inline = TRUE,
                                 choices = c("fraction_name", "substrate", "depth_level", 
                                             "depth","DNA_RNA", "latitude", "temperature"),
                                 selected = c("depth_level")))
        ),
      )
    })
    
    

    output$graph_barplot <- renderUI({
      req(df(), taxo(), input$barplot_variable)
      

      # req(input$barplot_variable)
      tagList(
        if(nrow(df()) > 0) {
        renderPlot({
          barplot(df(),
                 variable = input$barplot_variable,
                 taxo_level = global$taxo_levels[which(global$taxo_levels == taxo()$level) + 1])
                 # taxo_level = "supergroup") # For testing
        }, height = 800, width=1000) }
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



