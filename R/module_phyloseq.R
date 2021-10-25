
# UI ----------------------------------------------------------------------

phyloseq_alpha_UI <- function(id) {
  ns <- NS(id)
  
  if(!global$phyloseq_use) {
    return(
      tagList(h4("Too many datasets - Alpha diversity implemented later")))
  }
    
  tagList(
    uiOutput(ns('ui_ps_alpha')),
    shinycssloaders::withSpinner(uiOutput(ns('graph_ps_alpha')))
  )
}

phyloseq_beta_UI <- function(id) {
  ns <- NS(id)
  
  if(!global$phyloseq_use) {
    return(
      tagList(h4("Too many datasets - Beta diversity implemented later")))
  }
  
  tagList(
    uiOutput(ns('ui_ps_beta')),
    shinycssloaders::withSpinner(uiOutput(ns('graph_ps_beta')))
  )
}


# Server ------------------------------------------------------------------


phyloseqServer <- function(id, ps_selected, taxo, messages) {
  # stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    # Alpha UI ----------------------------------------------------------------
    
    output$ui_ps_alpha <- renderUI({
      tagList(
      
      includeMarkdown(system.file("readme", 'phyloseq.md', package = "metapr2")),
      checkboxGroupInput(ns("alpha_method"), "Diversity Measure", inline = TRUE,  
                         choices = c("Chao1", "Shannon", "Simpson", "Fisher"), 
                         selected = c("Chao1", "Shannon")),
      radioButtons(ns("alpha_x"), "X axis", inline = TRUE,
                   choices = c("latitude", "depth_level", "depth", "fraction_name","DNA_RNA", "temperature"),
                   selected = c("latitude")),
      )
    })
    
    output$graph_ps_alpha <- renderUI({
      
      req(ps_selected(), input$alpha_method, input$alpha_x)
      
      if (typeof(ps_selected())== "S4"){
      
          ps_alpha(ps= ps_selected(), 
                   measures=input$alpha_method,
                   x = input$alpha_x,
                   color="depth", shape = "fraction_name")
      } else {
        messages$no_data
      }
    })
    
    
    
    
    
    # Beta UI -----------------------------------------------------------------
    
        output$ui_ps_beta <- renderUI({
          
          tagList(
            
            includeMarkdown(system.file("readme", 'phyloseq.md', package = "metapr2")),    
            fluidRow(
              column(5, radioButtons(ns("beta_method"), "Ordination method", inline = TRUE,
                                      choices = c("NMDS", "CCA", "RDA", "MDS", "PCoA"),
                                      selected = c("NMDS"))),
              column(4, radioButtons(ns("beta_color_samples"), "Color varies with:", inline = TRUE,
                                     choices = c("latitude", "depth", "temperature"),
                                     selected = c("latitude"))),
              
              ),
            fluidRow(
              column(5, radioButtons(ns("beta_distance"), "Ordination distance", inline = TRUE,
                                     choiceNames = c("Bray-Curtis", "Gower", "Jensen-Shannon Divergence", "Jaccard"),
                                     choiceValues = c("bray", "gower", "jsd", "jaccard"),
                                     selected = c("bray"))),
              column(4, radioButtons(ns("beta_shape_samples"), "Shape varies with:", inline = TRUE,
                                     choices = c("fraction_name", "depth_level","DNA_RNA"),
                                     selected = c("fraction_name")))
            ),
        
          )
        })
    
  
        ps_ordinate <- reactive({
          req(ps_selected(), input$beta_method, input$beta_distance)
          
          # cat("PS ordinate: ")
          # print(pryr::mem_used())
          
          if (typeof(ps_selected())== "S4"){
            phyloseq::ordinate(ps_selected(), 
                               method = input$beta_method, 
                               distance = input$beta_distance, 
                               maxit=5)
            } else { "No data" }
          
        })

        output$graph_ps_beta <- renderUI({
          req(ps_ordinate(), input$beta_color_samples, input$beta_shape_samples)
          if (typeof(ps_selected())== "S4"){
            ps_beta(ps_selected(), ps_ordinate(), 
                    color_samples=input$beta_color_samples, 
                    shape_samples = input$beta_shape_samples,
                    color_taxa = global$taxo_levels[which(global$taxo_levels == taxo()$level) + 1])
          } else {
            messages$no_data
          }
        }) 


  })
  
}  
