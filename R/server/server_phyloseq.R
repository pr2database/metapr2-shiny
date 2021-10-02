# Phyloseq filtration
# - Cannot the function 

ps_selected <- reactive({
  keepSamples = (get_variable(asv_set$ps, "DNA_RNA") %in% input$DNA_RNA ) & 
                (get_variable(asv_set$ps, "depth_level") %in% input$depth_level) & 
                (get_variable(asv_set$ps, "fraction_name") %in% input$fraction_name)& 
                (get_variable(asv_set$ps, "substrate") %in% input$substrate)& 
                (get_variable(asv_set$ps, "dataset_id") %in% input$datasets_selected_id)
  
  # Check some samples are left...
  if(length(keepSamples[keepSamples]) > 1 ) {  
    ps1 <- prune_samples(keepSamples, asv_set$ps) %>% 
      filter_taxa(function(x) sum(x) > 0, prune = TRUE)   # Need to remove the taxa that do not appear in any sample
    }
  else {
    print("Error in sample selection")
    ps1 <- asv_set$ps
  }
  
  TT = as(tax_table(ps1), "matrix")
  keepTaxa = TT[, taxo()$level] %in%taxo()$name
  
  if(length(keepTaxa) > 1){
    ps1 <- prune_taxa(keepTaxa, ps1) 
    ps1 <- prune_samples(sample_sums(ps1)>0, ps1)  # Need to remove the samples that do not have the taxon
  } else {
    warning("One or Zero taxon left")
    ps1
  }
})

output$ps_alpha <- renderUI({
  req(ps_selected())
  
  tagList(
    p(""),
    renderPrint(print(ps_selected())),
    renderPlot({
      phyloseq::plot_richness(ps_selected(), 
                              x="latitude", color = "depth", shape = "fraction_name" ,
                              measures=input$alpha_method) +
        geom_point(size=5, alpha=0.7) +
        xlim(-90,90) +
        scale_color_gradient(high = "darkblue", low = "lightblue1") 
    },  height = 600, width = 1200, res = 96)
  )  
})

output$ps_beta <- renderUI({
  req(ps_selected())

  ps_ordinate <- ordinate(ps_selected(), method = input$beta_method, distance = "bray")
  tagList(
    p(""),
    renderPrint(print(ps_selected())),
    renderPlot({
      phyloseq::plot_ordination(ps_selected(), ps_ordinate,
                                type="samples",
                                color="latitude", shape = "fraction_name") +
        geom_point(size=5, alpha=0.7) +
        # xlim(-90,90) +
        scale_color_gradient2(high = "darkblue", mid= "white", low = "darkred") +
        
      phyloseq::plot_ordination(ps_selected(), ps_ordinate,
                                  type="taxa",
                                  color="supergroup") +
        geom_point(size=5, alpha=0.7) +
        scale_color_viridis_d() +

        patchwork::plot_layout(ncol = 1)
      
    },  height = 1600, width = 1000, res = 96)
  )
})
