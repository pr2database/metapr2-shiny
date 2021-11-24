
barplot <- function(df, variable, color_coding, taxo_level) {
  
  variable_date <- c("year", "month", "day")
  
  if (variable == "year") {
    date_breaks = "1 year"
    date_labels = "%Y"
    date_angle = 0
  }
  if (variable %in%c( "month", "day")) { 
    date_breaks = "1 month"
    date_labels = "%Y-%m"
    date_angle = 45
    }
  
  
  
  if(color_coding == "taxonomy")  {
    color_col <- taxo_level }
  else {
    color_col <- color_coding
  }
  
  variable_to_use <- variable
  
  if(variable %in% variable_date) {
    variable_to_use <- "date"
  } 
  
  # https://statisticsglobe.com/aggregate-daily-data-to-month-year-intervals-in-r
  
  
  
  
  
  df <- df %>% 
    select(any_of(c(color_col, variable_to_use)), n_reads_pct)
  
  # For depth only use the first 250 m
  
    if (variable == "depth") {
      df <- df %>% 
        filter(depth <=250)
      if (length(unique(df$depth)) > 1) {
        df <- df %>%
        mutate(depth =  cut_width(depth, width=25, boundary=0))
      } else {
        df <- df %>%
        mutate(depth =  as.factor(depth))
      }
    }  

  # floor_date: Round date-times down.
  
  if(variable %in% variable_date) {
    df <- df %>%
      filter(!is.na(date)) %>% 
      mutate(date =  lubridate::floor_date(as.Date(date), variable))
  } 
  
  # Discretize the data (must make sure that there is more than one value)
  

  if (variable == "temperature") {
    if (length(unique(df$temperature)) > 1) {
      df <- df %>%
        mutate(temperature =  fct_rev(cut_width(temperature, width=5, boundary=0)))
    } else {
      df <- df %>%
        mutate(temperature =  as.factor(temperature))
    }
  }
  
  if (variable == "salinity") {
    if (length(unique(df$salinity)) > 1) {
      df <- df %>%
        mutate(salinity =  fct_rev(cut_width(salinity, width=5, boundary=0)))
    } else {
      df <- df %>%
        mutate(salinity =  as.factor(salinity))
    }
  }

  if (variable == "latitude") {  
    if (length(unique(df$latitude)) > 1) {
      df <- df %>%
        mutate(latitude =  fct_rev(cut_width(latitude, width=20, boundary=0)))
    } else {
      df <- df %>%
        mutate(latitude =  as.factor(latitude))
    }
  }
  
  df <- df %>% 
    group_by(across(any_of(c(color_col, variable_to_use)))) %>%
    summarize(n_reads_pct = sum(n_reads_pct)) %>%
    group_by(across(any_of(variable_to_use))) %>% 
    mutate(n_reads_pct = n_reads_pct/sum(n_reads_pct)*100)
  
  
  # cat(variable_to_use, "\n")
  # print(df)
  
  gg <- df  %>% 
    ggplot() +
    xlab("% of reads") + ylab("") +
    theme_bw()
    if(variable_to_use == "date") {
      gg <- gg +
        geom_col(aes (x= .data[[variable_to_use]],
                      y=n_reads_pct, 
                      fill=.data[[color_col]])) +
        scale_x_date(date_breaks = date_breaks,
                     date_labels = date_labels) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
    }
   else {
     gg <- gg +
       geom_col(aes(y= fct_rev(.data[[variable_to_use]]),
                   x=n_reads_pct, 
                   fill=.data[[color_col]]))
   }

  if(variable %in% c("month", "day")) {   # Add vertical limits
    gg <- gg +
      geom_vline(xintercept= as.numeric(as.Date(str_c(c(2000:2030), "-01-01"))))
  }
  
  if(color_coding == "taxonomy"){ 
    gg <- gg + scale_fill_viridis_d() 
    }
  if(color_coding == "ecological_function"){ 
    gg <- gg + scale_fill_manual(values = global$ecological_function_colors)
  }
  
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
                       taxo_level = global$taxo_levels[which(global$taxo_levels == taxo()$level) + 1]),
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



