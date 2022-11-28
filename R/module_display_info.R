# UI ----------------------------------------------------------------------

display_info_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    uiOutput(outputId = ns('label'))
  )
}



# Server ------------------------------------------------------------------


display_info_server <- function(id, authentification, asv_clustered) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    version <- reactive({
      if(length(authentification$user) == 0) return("2.0") # Defaut case
      if(authentification$user == "v1") return( "1.0")
      if(authentification$user == "ge") return( "1.0 + Green edge")
      if(authentification$user == "ge2") return( "2.0 + Green edge")
      if(authentification$user == "private") return(  "2.0 + private")
      return("2.0")
    })
    
    output$label <- renderPrint(tagList(
      h4(str_c("metaPR2 database version ", version())),
      h4(str_c("ASVs: ", ifelse(asv_clustered, "clustered", "original")))
      
      )
    )
    
   })
  
}  
