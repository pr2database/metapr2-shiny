output$treemap <- renderUI({
  req(df_selected_taxa_one())
  tagList(
    p(""),
    renderPlot({
      treemap(df_selected_taxa_one(), taxo_level = taxo()$level)
    },  height = 800, width = 800, res = 96)
  )  
})  
