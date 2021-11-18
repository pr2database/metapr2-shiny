
barplot <- function(df, variable, taxo_level) {
  
  # For depth only use the first 250 m
  
    if (variable == "depth") {
      df <- df %>% 
        filter(depth <=250)
    }  
  # Discretize the data (must make sure that there is more than one value)
  

    if (length(unique(df$depth)) > 1) {
      df <- df %>%
      mutate(depth =  cut_width(depth, width=25, boundary=0))
    } else {
      df <- df %>%
      mutate(depth =  as.factor(depth))
    }
  
  if (length(unique(df$temperature)) > 1) {
    df <- df %>%
      mutate(temperature =  cut_width(temperature, width=25, boundary=0))
  } else {
    df <- df %>%
      mutate(temperature =  as.factor(temperature))
  }
  
  if (length(unique(df$latitude)) > 1) {
    df <- df %>%
      mutate(latitude =  cut_width(latitude, width=25, boundary=0))
  } else {
    df <- df %>%
      mutate(latitude =  as.factor(latitude))
  }
    
  gg <- df %>% 
    select(any_of(c(taxo_level, variable)), n_reads_pct) %>% 
    group_by(across(any_of(c(taxo_level, variable)))) %>%
    summarize(n_reads_pct = sum(n_reads_pct)) %>%
    group_by(across(any_of(variable))) %>% 
    mutate(n_reads_pct = n_reads_pct/sum(n_reads_pct)*100) %>% 
    ggplot() +
    geom_col(aes(y=.data[[variable]], x=n_reads_pct, fill=.data[[taxo_level]])) +
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
        includeMarkdown(system.file("readme", 'barplot.md', package = "metapr2")),
        p(),
        fluidRow(
          column(9, radioButtons(NS(id, "barplot_variable"), "Variable to use for barplots:", inline = TRUE,
                                 choices = c("fraction_name", "ecosystem", "substrate", "depth_level", 
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



