# To do

# - Merge all ASVs that are similar and do map as % of eukaryotes or of other level...


asv_all <- eventReactive(input$button_match, {
  
  query_valid <- sequence_check(input$query)
  
  # Do not forget to include "shinyFeedback::useShinyFeedback()" at top of Page in ui
  shinyFeedback::feedback(inputId = "query", show = !query_valid, text = "Invalid sequence", color="red")
  
  req(query_valid, cancelOutput = TRUE)
  
  query_valid

  match_asv(top_n(asv_set$fasta, 2000), input$query)
  })


asv_filtered <- eventReactive(
  {input$button_match
    input$pct_id_min}, 
  {
  req(asv_all())
  asv_all() %>%
    dplyr::filter(pid >= input$pct_id_min)
})

output$query_valid <- renderText(asv_all())

output$asv_filtered <- renderUI({
  req(asv_filtered())
  tagList(
    p("Matching ASVs"),
    p(),
    DT::renderDT(asv_filtered() %>% 
                   mutate(sequence = gsub("(.{80})","\\1\n",sequence),
                          asv_code = str_c(str_sub(asv_code, 1, 8),"...")),
                 rownames = FALSE,
                 options = list(pageLength = 20,
                                autoWidth = TRUE,
                                scrollX=TRUE,
                                columnDefs = list(list(width = '10%', targets = c(1)),
                                                  list(width = '20%', targets = c(9:12)))
                                )
                  )

#     # downloadHandler(
#     #   filename = function() {str_c("primer_set_match_pr2_", Sys.Date(), ".tsv")},
#     #   content = function(path) {export(primer_set_match.df(), file=path)},
#     #   outputArgs = list(label = "Download results"),
#     # )
  )
})
