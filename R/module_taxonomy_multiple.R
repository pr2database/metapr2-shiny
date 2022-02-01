options_picker_taxo <- shinyWidgets::pickerOptions(
  # actionsBox = TRUE,
  selectedTextFormat = "count > 10",
  liveSearch = TRUE,
   noneSelectedText = "All"
)

options_picker_exclude <- shinyWidgets::pickerOptions(
  # actionsBox = TRUE,
  selectedTextFormat = "count > 10",
  liveSearch = TRUE,
  noneSelectedText = "None"
)

# =================================
# Small function to return the taxo number
# =================================

taxo_level_number <- function(taxo_level) {
  which(global$taxo_levels == taxo_level)
  }

# =================================
# Small function to return the taxo level and taxon name 
# =================================

taxo_selected <- function(supergroup, division, class, order, family, genus, species, asv_code) {
  
  taxo_1 <- c(supergroup[1], division[1], class[1], order[1], family[1], genus[1], species[1], asv_code[1])
  taxo_list <- list(supergroup = supergroup, division=division, class=class, order = order, family = family, genus = genus, species = species, asv_code=asv_code)
  
  # The levels for which nothing is selected return NULL and the length of the vector gives the first rank which is NULL
  
  taxo_level <- global$taxo_levels[length(taxo_1) + 1]
  
  taxo_name <- taxo_list[[taxo_level]]
  
  # Then the name(s) are just extracted from the list for the taxo_level
  
  # taxo_name <- taxo_list[[taxo_level]]
  
  if (taxo_level == "kingdom") taxo_name = "Eukaryota"
  
  message("Taxo level: ", taxo_level)
  message(taxo_name)
  
  # Very strange, the if one puts the name in the list then if there are more than one it is returned as several names (name1, name2 etc...) 
  # See: https://stackoverflow.com/questions/9031819/add-named-vector-to-a-list
  
  return( c(level = taxo_level, list(name = taxo_name), taxo_list)) 
}



# UI ----------------------------------------------------------------------

taxoUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Select Taxa"),
    
    div(actionButton(ns("validate_taxo"), "Validate Taxa", class = "btn-primary"), style="display:inline-block"),
    div(actionButton(ns("reset_taxo"), "Reset Taxa", class = "btn-primary"), style="display:inline-block"),
    p(),
    p("Press VALIDATE after changing taxonomy to update screen."),
    p("Press RESET to reset taxonomy to top level (need to press validate after reset)"),
    p(),
    
    shinyWidgets::pickerInput(ns("supergroup"), "Supergroup", choices = unique(global$pr2_taxo$supergroup), selected = NULL, multiple = TRUE, options= options_picker_taxo),
    
    # Use the purr map function to create the pickerInput
    purrr::map(global$taxo_levels[3:9], ~  shinyWidgets::pickerInput(ns(.x), str_to_title(.x) , choices = NULL, selected = NULL, multiple = TRUE, options= options_picker_taxo)),
    
    p(),
    
    h3("Exclude Taxa"),
    shinyWidgets::pickerInput(ns("taxa_excluded"), "", choices = c("Metazoa", "Streptophyta", "Fungi"), selected = NULL, multiple = TRUE, options= options_picker_exclude),
    p(),
    
    h3("Save/Load Taxa"),
    downloadButton(ns('save_taxo'), 'Save taxonomy'),
    p(),
    p(),
    fileInput(ns('load_taxo'), NULL, buttonLabel = 'Load taxonomy', multiple = FALSE, accept = ".yaml"),
    
    
    
    # verbatimTextOutput(ns("test1")),
    # verbatimTextOutput(ns("test2"))
    
  )
}

# Server ------------------------------------------------------------------


taxoServer <- function(id, fasta_selected) {

  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    # Reactive Value to prevent update of pickers when uploading-------------
    update_taxo_auto <- reactiveVal(TRUE) 
    

    # ===========================================
    # Update taxonomy selected
    # ===========================================
    
    taxo <- reactive({
      # Do not use req because if one member is NULL it will not be activated
      taxo_selected(input$supergroup, input$division, input$class, input$order, input$family, input$genus, input$species, input$asv_code)
      })
    

    taxo_final <- eventReactive(c(input$validate_taxo), {
      c(taxo(), list(taxa_excluded = input$taxa_excluded))
    }, ignoreNULL = F) # ignoreNULL is necessary so that the plots are created at initial time
 
    
    output$test1 <- renderPrint(taxo())
    output$test2 <- renderPrint(update_taxo_auto())
    
    # ===========================================
    # Update taxonomic lists (reactive variables)
    # ===========================================
    
    # See: https://mastering-shiny.org/action-dynamic.html#freezing-reactive-inputs
    #   In this case freezeReactiveValue does not work....
    

    
    # Define a function to update each Picker -------------
    
    update_taxo_picker <- function(taxo_level){
    
    observeEvent(input[[taxo_level]], { 
      
      # This line prevents automatic update when resetting and loading new values from yaml file
      req(update_taxo_auto())
      
      # The next line prevents update of taxonomy selector when loading new values from yaml file
      req((taxo_level_number(taxo_level) >= taxo_level_number(taxo()$level)) |
           (taxo_level == "kingdom"))
      
     taxo_level_below = global$taxo_levels[taxo_level_number(taxo_level) + 1]
      
      taxon_list <- fasta_selected() %>% 
        filter(.data[[taxo_level]] %in% input[[taxo_level]]) %>%
        pull(.data[[taxo_level_below]]) %>%
        unique()
      
      # cat("Update_taxo_picker: ", taxo_level,"\n")

      
      shinyWidgets::updatePickerInput(session = session,  inputId = taxo_level_below, choices = taxon_list)
    }, ignoreNULL = FALSE)
    
    }
    
    # Update all Pickers using the map function ----------------
    
    purrr::map(global$taxo_levels[2:8], ~ update_taxo_picker(.x))
    
    
    
    
    
    
    
    # Save settings --------
    
    output$save_taxo <- downloadHandler(
      
      filename = function() {str_c("metapr2_taxonomy", "_", Sys.Date(), ".yaml")},
      content = function(path) {
        yaml::write_yaml(taxo(), file = path)
        
      }
    ) 
    
    # Update settings -------- 
    
    # update_taxo_picker_upload <- function(taxo_level, taxo_new){
    #   
    #     taxo_level_below = global$taxo_levels[which(global$taxo_levels == taxo_level) + 1]
    # 
    #     taxon_list <- fasta_selected() %>%
    #       filter(.data[[taxo_level]] %in% taxo_new[[taxo_level]]) %>%
    #       pull(.data[[taxo_level_below]]) %>%
    #       unique()
    #     print(taxo_level_below)
    #     print(taxon_list)
    #     shinyWidgets::updatePickerInput(session = session,  inputId = taxo_level_below, selected = taxo_new[[taxo_level_below]], choices = taxon_list)
    #     isolate(input[[taxo_level_below]])
    #    
    # }
    
    
    observeEvent(input$load_taxo, {

      tryCatch(
        {taxo_new <- yaml::read_yaml(file = input$load_taxo$datapath)
        
        
        update_taxo_auto(FALSE)
        # shinyWidgets::updatePickerInput(session = session,  inputId = "supergroup", selected = taxo_new[["supergroup"]])
        # purrr::map(global$taxo_levels[2:8], ~ update_taxo_picker_upload(.x, taxo_new))
        # shinyWidgets::updatePickerInput(session = session,  inputId = "asv_code", selected = taxo_new[["asv_code"]])
        purrr::map(global$taxo_levels[2:9], ~ shinyWidgets::updatePickerInput(session = session,  inputId = .x, choices = taxo_new[[.x]], selected = taxo_new[[.x]]))

        shinyWidgets::updatePickerInput(session = session,  inputId = "taxa_excluded", selected = taxo_new[["taxa_excluded"]])
        update_taxo_auto(TRUE)

        },
        error=function(cond) {
          message("Invalid yaml file")
          showModal(modalDialog(title = "Loading settings", "Invalid YAML file", size = "s", easyClose = TRUE))
          return(NA)
        }
      )
    })
    
    # Reset settings -------- 
    
    observe({
      input$reset_taxo
      update_taxo_auto(FALSE)
      shinyWidgets::updatePickerInput(session = session,  inputId = "supergroup", choices = unique(global$pr2_taxo$supergroup), selected = character(0), )
      purrr::map(global$taxo_levels[3:9], ~ shinyWidgets::updatePickerInput(session = session,  inputId = .x, choices = character(0), selected = character(0)))
      update_taxo_auto(TRUE)
      # click(ns("validate_taxo"))
    })
      
    

    
    # Use taxo and not taxo(), ie return the function and not its value...
    return(taxo_final)
    
  })
  
} 


