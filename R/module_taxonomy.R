# =================================
# Small function to return the taxo level and taxon name 
# =================================

taxo_selected <- function(supergroup, division, class, order, family, genus, species= "All"){
  
  # Debug
  # browser()  
  
  # If the lowest level is "All" then it is the one selected then goes up one level
  if ( !(species %in% c("All", "")) ) {
    taxo_level = "species"
    taxo_name = species
  } else {
    if(!(genus %in% c("All", "")) ) {
      taxo_level = "genus"
      taxo_name = genus
    } else {
      if(!(family %in% c("All", "")) ) {
        taxo_level = "family"
        taxo_name = family
      } else {
        if(!(order %in% c("All", "")) ) {
          taxo_level = "order"
          taxo_name = order
        } else {
          if(!(class %in% c("All", "")) ) {
            taxo_level = "class"
            taxo_name = class
          } else {
            if(!(division %in% c("All", "")) ) {
              taxo_level = "division"
              taxo_name = division
            } else {
              if(!(supergroup %in% c("All", "")) ) {
                taxo_level = "supergroup"
                taxo_name = supergroup
              } else {
                taxo_level = "kingdom"
                taxo_name = "Eukaryota" 
              }
            }
          }
        }
      }  
    }  
  } 
  
  # Debug
  # browser()
  
  return( list(level = taxo_level, name = taxo_name))      
}


# UI ----------------------------------------------------------------------

taxoUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Select Taxa"),
    
    selectInput(ns("supergroup"), "Supergroup",
                choices = c("All", unique(global$pr2_taxo$supergroup))),
    selectInput(ns("division"), "Division",
                choices = "All"),
    selectInput(ns("class"), "Class",
                choices = "All"),
    selectInput(ns("order"), "Order",
                choices = "All"),
    selectInput(ns("family"), "Family",
                choices = "All"),
    selectInput(ns("genus"), "Genus",
                choices = "All"),
    selectInput(ns("species"), "Species",
                choices = "All")
  )
}



# Server ------------------------------------------------------------------


taxoServer <- function(id) {
  # stopifnot(is.reactive(taxo))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)

    # ===========================================
    # Update taxonomy selected
    # ===========================================
    
    taxo <- reactive({
      # req(input$supergroup, input$division, input$class, input$order, input$family, input$genus, input$species)
      taxo_selected(input$supergroup, input$division, input$class, input$order, input$family, input$genus, input$species)
      })
    
     
    # output$test1 <- renderText(taxo()$level)
    # output$test2 <- renderText(taxo()$name)
    
    # ===========================================
    # Update taxonomic lists (reactive variables)
    # ===========================================
    
    # See: https://mastering-shiny.org/action-dynamic.html#freezing-reactive-inputs
    #   In this case freezeReactiveValue does not work....
    
    
    # --- Division
    
    division_list <- reactive({
      req(input$supergroup)
      filter(global$pr2_taxo, supergroup == input$supergroup) %>%
        pull(division) %>%
        unique()
    })
    
    observeEvent(division_list(), {
      # Do not use ns() here for the InputId
      updateSelectInput(inputId = "division", choices = c("All", division_list()))  
    })
    
    # --- Class
    class_list <- reactive({
      req(input$division)
      filter(global$pr2_taxo, division == input$division) %>%
        pull(class) %>%
        unique()
    })
    
    observeEvent(class_list(), {
      updateSelectInput(inputId = "class", choices = c("All", class_list()))
    })
    
    # --- Orders
    order_list <- reactive({
      req(input$class)
      filter(global$pr2_taxo, class == input$class) %>%
        pull(order) %>%
        unique()
    })
    
    observeEvent(order_list(), {
      updateSelectInput(inputId = "order", choices = c("All", order_list()))
    })
    
    # --- Families
    family_list <- reactive({
      req(input$order)
      filter(global$pr2_taxo, order == input$order) %>%
        pull(family) %>%
        unique()
    })
    
    observeEvent(family_list(), {
      updateSelectInput(inputId = "family", choices = c("All", family_list()))
    })
    
    # --- Genera
    genus_list <- reactive({
      req(input$family)
      filter(global$pr2_taxo, family == input$family) %>%
        pull(genus) %>%
        unique()
    })
    
    observeEvent(genus_list(), {
      updateSelectInput(inputId = "genus", choices = c("All", genus_list()))
    })
    
    # --- Species
    species_list <- reactive({
      req(input$genus)
      filter(global$pr2_taxo, genus == input$genus) %>%
        pull(species) %>%
        unique()
    })
    
    observeEvent(species_list(), {
      updateSelectInput(inputId = "species", choices = c("All", species_list()))
    })
    
    # Use taxo and not taxo(), ie return the function and not its value...
    return(taxo)
    
  })
  
} 

    

