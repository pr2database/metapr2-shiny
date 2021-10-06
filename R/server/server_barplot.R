
barplot <- function(df, variable, taxo_level) {
  
  gg <- df %>% 
    select(!!as.symbol(taxo_level), !!as.symbol(variable), n_reads) %>% 
    group_by(!!as.symbol(taxo_level), !!as.symbol(variable)) %>%
    summarize(n_reads = sum(n_reads)) %>%
    group_by(!!as.symbol(variable)) %>% 
    mutate(n_reads = n_reads/sum(n_reads)*100) %>% 
    ggplot() + 
    geom_col(aes(y=!!as.symbol(variable), x=n_reads, fill=!!as.symbol(taxo_level))) +
    scale_fill_viridis_d() +
    xlab("% of reads") + ylab("") +
    theme_bw()
  
  return(gg)
  
}


output$ui_barplot <- renderUI({
  tagList(
    
    fluidRow(
      column(4, radioButtons("barplot_variable", "Variable to use for barplots:", inline = TRUE,
                             choices = c("fraction_name", "depth_level","DNA_RNA"),
                             selected = c("depth_level")))
    ),
  )
})

output$graph_barplot <- renderUI({
  req(df_selected(), input$barplot_variable)
  tagList(
    renderPlot({
      barplot(df_selected(), 
             variable = input$barplot_variable,
             taxo_level = global$taxo_levels[which(global$taxo_levels == taxo()$level) + 1])
      
    }, height = 800, width=1000)
  )
})
