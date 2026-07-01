# UI ----------------------------------------------------------------------

display_info_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    uiOutput(outputId = ns('label'))
  )
}



# Server ------------------------------------------------------------------


display_info_server <- function(id, authentification, asv_clustered, samples_selected) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    version <- reactive({
      if(length(authentification$user) == 0) return("4.0") # Defaut case
      if(authentification$user == "pacbio") return( "PacBio")
      if(authentification$user == "nansen") return( "Nansen legacy")
      if(authentification$user == "v2") return(  "2.0")
      if(authentification$user == "v3") return(  "3.0")
      if(authentification$user == "v4") return(  "4.0")
      if(authentification$user == "v5") return(  "5.0 - 16S plastid")
      if(authentification$user == "private") return(  "3.0 + private")
      return("4.0")
    })
  
    output$label <- renderPrint(tagList(
      h4(str_c("metaPR2 database version ", version())),
      h4(str_c("ASVs: ", ifelse(asv_clustered, "clustered", "original"))),
      h4(str_c("Samples selected: ", nrow(samples_selected()))),
      tags$em("Try to analyze less than 3000 samples")      
      )
    )
    
   })
  
}  
