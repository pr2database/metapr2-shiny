
# Filer ps with selected samples ------------------------------------------

ps_selected <- reactive({ 

  req(iv_samples$is_valid())
  
  ps_select (ps = asv_set$ps, 
             DNA_RNA = input$DNA_RNA, depth_level = input$depth_level, 
             fraction_name = input$fraction_name, substrate = input$substrate, 
             datasets_selected_id = input$datasets_selected_id,
             ps_reads_min = input$ps_reads_min, 
             taxo_level = taxo()$level, taxo_name = taxo()$name)
})


# Alpha UI ----------------------------------------------------------------

output$ui_ps_alpha <- renderUI({
  tagList(
  
  includeMarkdown(system.file("readme", 'phyloseq.md', package = "metapr2")),
  checkboxGroupInput("alpha_method", "Diversity Measure", inline = TRUE,  
                     choices = c("Chao1", "Shannon", "Simpson", "Fisher"), 
                     selected = c("Chao1", "Shannon")),
  radioButtons("alpha_x", "X axis", inline = TRUE,
               choices = c("latitude", "depth_level", "depth", "fraction_name","DNA_RNA", "temperature"),
               selected = c("latitude")),
  )
})

output$graph_ps_alpha <- renderUI({
  
  req(ps_selected())
  req(input$alpha_method)
  req(input$alpha_x)
  
  ps_alpha(ps= ps_selected(), measures=input$alpha_method,
           x = input$alpha_x,
           color="depth", shape = "fraction_name")
})


# Beta UI -----------------------------------------------------------------

output$ui_ps_beta <- renderUI({
  tagList(
    
    includeMarkdown(system.file("readme", 'phyloseq.md', package = "metapr2")),    
    fluidRow(
      column(5, radioButtons("beta_method", "Ordination method", inline = TRUE,
                              choices = c("NMDS", "CCA", "RDA", "MDS", "PCoA"),
                              selected = c("NMDS"))),
      column(4, radioButtons("beta_color_samples", "Color varies with:", inline = TRUE,
                             choices = c("latitude", "depth", "temperature"),
                             selected = c("latitude"))),
      
      ),
    fluidRow(
      column(5, radioButtons("beta_distance", "Ordination distance", inline = TRUE,
                             choiceNames = c("Bray-Curtis", "Gower", "Jensen-Shannon Divergence", "Jaccard"),
                             choiceValues = c("bray", "gower", "jsd", "jaccard"),
                             selected = c("bray"))),
      column(4, radioButtons("beta_shape_samples", "Shape varies with:", inline = TRUE,
                             choices = c("fraction_name", "depth_level","DNA_RNA"),
                             selected = c("fraction_name")))
    ),

  )
})

ps_ordinate <- reactive({
  req(ps_selected())
  phyloseq::ordinate(ps_selected(), method = input$beta_method, distance = input$beta_distance, maxit=5)
})

output$graph_ps_beta <- renderUI({
  req(ps_ordinate(), input$beta_color_samples, input$beta_shape_samples)
  ps_beta(ps_selected(), ps_ordinate(), 
          color_samples=input$beta_color_samples, shape_samples = input$beta_shape_samples,
          color_taxa = global$taxo_levels[which(global$taxo_levels == taxo()$level) + 1])
})  



# Bar plots ---------------------------------------------------------------

ps_barplot <- function(ps, variable, taxo_level) {
  ps <- merge_samples(ps, variable) 
  ps <- transform_sample_counts(ps, function(x) 100 * x/sum(x))
  
  plot_bar(ps, variable, fill = taxo_level) + 
    geom_bar(aes(color=!!as.symbol(variable), fill=!!as.symbol(variable)), stat="identity", position="stack")
  coord_flip() + 
    ylab("Percentage of Sequences")

  }

output$ui_ps_barplot <- renderUI({
  tagList(
    
    includeMarkdown("readme/phyloseq.md"),
    
    fluidRow(
      column(4, radioButtons("barplot_variable", "Variable to use for barplots:", inline = TRUE,
                             choices = c("fraction_name", "depth_level","DNA_RNA"),
                             selected = c("depth_level")))
    ),
  )
})

output$graph_ps_barplot <- renderUI({
  req(ps_ordinate(), input$barplot_variable)
  ps_barplot(ps_selected(), 
          variable = input$barplot_variable,
          taxo_level = global$taxo_levels[which(global$taxo_levels == taxo()$level) + 1])
})

