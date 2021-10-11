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
      updateSelectInput(inputId = ns("division"), choices = c("All", division_list()))
    })
    
    # --- Class
    class_list <- reactive({
      req(input$division)
      filter(global$pr2_taxo, division == input$division) %>%
        pull(class) %>%
        unique()
    })
    
    observeEvent(class_list(), {
      updateSelectInput(inputId = ns("class"), choices = c("All", class_list()))
    })
    
    # --- Orders
    order_list <- reactive({
      req(input$class)
      filter(global$pr2_taxo, class == input$class) %>%
        pull(order) %>%
        unique()
    })
    
    observeEvent(order_list(), {
      updateSelectInput(inputId = ns("order"), choices = c("All", order_list()))
    })
    
    # --- Families
    family_list <- reactive({
      req(input$order)
      filter(global$pr2_taxo, order == input$order) %>%
        pull(family) %>%
        unique()
    })
    
    observeEvent(family_list(), {
      updateSelectInput(inputId = ns("family"), choices = c("All", family_list()))
    })
    
    # --- Genera
    genus_list <- reactive({
      req(input$family)
      filter(global$pr2_taxo, family == input$family) %>%
        pull(genus) %>%
        unique()
    })
    
    observeEvent(genus_list(), {
      updateSelectInput(inputId = ns("genus"), choices = c("All", genus_list()))
    })
    
    # --- Species
    species_list <- reactive({
      req(input$genus)
      filter(global$pr2_taxo, genus == input$genus) %>%
        pull(species) %>%
        unique()
    })
    
    observeEvent(species_list(), {
      updateSelectInput(inputId = ns("species"), choices = c("All", species_list()))
    })
    
    return(taxo)
    
  })
  
} 

    

