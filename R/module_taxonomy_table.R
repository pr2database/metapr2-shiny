# UI ----------------------------------------------------------------------

taxo_table_UI <- function(id) {
  ns <- NS(id)
  tagList(
    

    p(),
    # h4("Select and deselect datasets by clicking on the corresponding row."), 
    DT::DTOutput(ns('taxonomy_table'))
  )
}



# Server ------------------------------------------------------------------


taxo_table_Server <- function(id, fasta_all) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    options(warn=-1)
    
    
    
  # Panel with table of datasets -------------------------------------------------
    
    # See:
    # Column width: https://stackoverflow.com/questions/25205410/r-shiny-set-datatable-column-width
    # However does not real works....
    
    taxonomy_table <- reactive ({
      req(fasta_all())
      DT::datatable(fasta_all() %>%
                      select(-asv_code) %>% 
                      count(across(any_of(c(global$taxo_levels, "ecological_function"))), name = "Number of ASVs") %>%
                      arrange(across(any_of(global$taxo_levels))),
                    rownames = FALSE ,
                    options = list(
                      autoWidth = FALSE,
                      scrollX=FALSE,
                      pageLength = 50,
                      columnDefs = list(list(width = '10px', targets = 7)))
      ) 
    })
    
    output$taxonomy_table <- DT::renderDT(taxonomy_table())
    
    
   })
  
}  
