
# UI ----------------------------------------------------------------------
phyloseq_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('ui_ps')),
  )
}

phyloseq_alpha_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('ui_ps_alpha_param')),
    shinycssloaders::withSpinner(uiOutput(ns('graph_ps_alpha')))
  )
}

phyloseq_beta_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('ui_ps_beta_param')),
    shinycssloaders::withSpinner(uiOutput(ns('graph_ps_beta')))
  )
}


# Server ------------------------------------------------------------------


phyloseqServer <- function(id, samples_selected, df_selected, fasta_selected, taxo, messages) {
  # stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    

  # Common to alpha and beta ------------------------------------------------

    
    ns <- NS(id)
    
    # Make sure that no more than 1000 samples selected
    
    n_samples_max = 1000
    
    n_samples_valid <- reactive({nrow(samples_selected()) <= n_samples_max})

    output$sample_number <- renderText({stringr::str_c("Number of samples: <b>", nrow(samples_selected()), 
                                                       if_else(n_samples_valid(), "</b>", " - Too many !!</b> - Must be below <b>1000 !</b>"),
                                                       sep=" ")})
    
    # Construct the phyloseq object for selected samples 
    
    ps_selected <- eventReactive(input$button_ps, {
      req(n_samples_valid())
      ps <- make_phyloseq(samples_selected(), df_selected(), fasta_selected())
      print(ps)
    })
    
    output$ui_ps <- renderUI({
      tagList(
        includeMarkdown(system.file("readme", 'phyloseq.md', package = "metapr2")),
        htmlOutput(ns("sample_number")),
        p(),
        actionButton(ns("button_ps"), "Compute diversity - Press again after updating samples", class = ifelse(n_samples_valid(), "btn-primary", "btn-danger")),
        p(),
      )
    })  
    
    
    # Alpha UI ----------------------------------------------------------------
    

    
    output$ui_ps_alpha_param <- renderUI({
      req(ps_selected())
      tagList(checkboxGroupInput(ns("alpha_method"), "Diversity Measure", inline = TRUE,  
                         choices = c("Chao1", "Shannon", "Simpson"), 
                         selected = c("Chao1", "Shannon", "Simpson")),
        fluidRow(
          column(3,tags$b("Discretize continuous Y variable")
                 ),         
          column(2, shinyWidgets::switchInput(ns("alpha_x_discretize"), value = FALSE, size = "mini")
                  )
      ),
      radioButtons(ns("alpha_x"), "Y variable", inline = TRUE,
                   choices = c( "ecosystem", "substrate","fraction_name","DNA_RNA",  "depth_level", "latitude","depth", "temperature", "salinity"),
                   selected = c("latitude")),
      
      fluidRow(
        column(4, radioButtons(ns("alpha_color"), "Color (only with continuous Y)", inline = TRUE,
                   choices = c("latitude", "depth","temperature", "salinity"),
                   selected = c("depth"))),
        column(6, radioButtons(ns("alpha_shape"), "Shape (only with continuous Y)", inline = TRUE,
                   choices = c( "fraction_name", "substrate","ecosystem", "depth_level", "DNA_RNA"),
                   selected = c("fraction_name")))
      )
      )
    })
    
    output$graph_ps_alpha <- renderUI({
      
      req(ps_selected(), input$alpha_method, input$alpha_x)
      
      if (typeof(ps_selected())== "S4"){
      
          gg <- ps_alpha(ps= ps_selected(), 
                   measures=input$alpha_method,
                   x = input$alpha_x,
                   color=input$alpha_color, 
                   shape = input$alpha_shape,
                   discretize = input$alpha_x_discretize)
        tagList(
          p(""),
          
          renderPrint(print(ps_selected())),
          
          renderPlot(gg,  height = 600*length(input$alpha_method), width = 1200, res = 96)
        )
      } else {
        messages$no_data
      }
    })
    
    
    
    
    
    # Beta UI -----------------------------------------------------------------
    
      output$ui_ps_beta_param <- renderUI({
        
        req(ps_selected())
        
        tagList(
          
          fluidRow(
            column(5, radioButtons(ns("beta_method"), "Ordination method", inline = TRUE,
                                    choices = c("NMDS", "CCA", "RDA", "MDS", "PCoA"),
                                    selected = c("NMDS"))),
            column(4, radioButtons(ns("beta_color_samples"), "Color varies with:", inline = TRUE,
                                   choices = c("latitude", "depth", "temperature", "salinity"),
                                   selected = c("latitude"))),
            
            ),
          fluidRow(
            column(5, radioButtons(ns("beta_distance"), "Ordination distance", inline = TRUE,
                                   choiceNames = c("Bray-Curtis", "Gower", "Jensen-Shannon Divergence", "Jaccard"),
                                   choiceValues = c("bray", "gower", "jsd", "jaccard"),
                                   selected = c("bray"))),
            column(4, radioButtons(ns("beta_shape_samples"), "Shape varies with:", inline = TRUE,
                                   choices = c("fraction_name", "substrate", "ecosystem", 
                                               "depth_level","DNA_RNA"),
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
