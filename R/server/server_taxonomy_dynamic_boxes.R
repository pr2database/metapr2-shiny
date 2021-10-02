# ===========================================
# Update taxonomy selected
# ===========================================

taxo <- reactive({taxo_selected(input$supergroup, input$division, input$class, input$order, input$family, input$genus, input$species)})
# 
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
  # freezeReactiveValue(input, "division")
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
  # freezeReactiveValue(input, "class")
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
  # freezeReactiveValue(input, "order")
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
  # freezeReactiveValue(input, "family")
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
  # freezeReactiveValue(input, "genus")
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
  # freezeReactiveValue(input, "species")
  updateSelectInput(inputId = "species", choices = c("All", species_list()))
})


