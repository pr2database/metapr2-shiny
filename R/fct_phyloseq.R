# Phyloseq filtration -----------------------------------------------------

# - Cannot the function select_samples because it is based on the base function select which is limited 


ps_select <- function(ps, gene_region, DNA_RNA, ecosystem, depth_level, fraction_name, substrate, datasets_selected_id,
                      ps_reads_min, 
                      taxo_level, taxo_name) {
  
# This function returns an error message ("character") if no samples selected and an "S4" object if selection is OK
  
  # if((length(DNA_RNA) == 0)) {
  #   validate("No DNA_RNA selected. Will use the whole dataset.")
  #   return(ps)
  # }

  keepSamples = 
    (phyloseq::get_variable(ps, "gene_region") %in% gene_region ) &
    (phyloseq::get_variable(ps, "DNA_RNA") %in% DNA_RNA ) & 
    (phyloseq::get_variable(ps, "ecosystem") %in% ecosystem)  & 
    (phyloseq::get_variable(ps, "depth_level") %in% depth_level) & 
    (phyloseq::get_variable(ps, "fraction_name") %in% fraction_name)& 
    (phyloseq::get_variable(ps, "substrate") %in% substrate)& 
    (phyloseq::get_variable(ps, "dataset_id") %in% datasets_selected_id)
  
  # Check some samples are left...
  if(sum(keepSamples[keepSamples]) > 1 ) {  
    ps1 <- phyloseq::prune_samples(keepSamples, ps) %>%  
      # Need to remove the taxa that do not have minimum number of reads
      phyloseq::filter_taxa(function(x) sum(x) >= ps_reads_min, prune = TRUE)
  }
  else {
    # validate("No samples selected. Will use the whole dataset.")
    return("No sample selected")
  }
  
  TT = as(phyloseq::tax_table(ps1), "matrix")
  keepTaxa = TT[, taxo_level] %in% taxo_name
  
  if(sum(keepTaxa) > 1){
    ps1 <- phyloseq::prune_taxa(keepTaxa, ps1) 
    # Need to remove the samples that do not have the taxon
    ps1 <- phyloseq::prune_samples(phyloseq::sample_sums(ps1)>0, ps1)
  } else {
    # validate("One or Zero taxon left - Will use all taxa.")
    return("No taxa selected")
  }

  return(ps1)
 
}


# Alpha diversity ---------------------------------------------------------

ps_alpha <- function(ps, measures = c("Shannon"), 
                     x="latitude" , color="depth", shape="fraction_name") {
  
  gg <-phyloseq::plot_richness(ps, 
                          x = x, color = color, shape = shape, 
                          measures=measures) +
    geom_point(size=5, alpha=0.85) +
    # xlim(xmin,xmax) +
    scale_color_gradient(high = "darkblue", low = "lightblue1") 
  if (x %in% c("depth_level", "fraction_name", "DNA_RNA")){
    gg <- gg +
      geom_boxplot(aes(x= .data[[x]], shape=NULL), alpha = 0)
  } else {
    gg <- gg +
      geom_smooth(aes(x= .data[[x]], shape=NULL), method = "gam", alpha = 0)  
    }
  
 tagList(
  p(""),

  renderPrint(print(ps)),

  renderPlot(gg,  height = 600, width = 1200, res = 96)
)
  
}


# Beta diversity ----------------------------------------------------------

ps_beta <- function(ps, ps_ordinate, 
                    color_samples="latitude", shape_samples = "fraction_name",
                    color_taxa) {
  
# showNotification(ui = "Computing ordination...", id="ps_ordinate_message")
  
# ps_ordinate <- phyloseq::ordinate(ps, method = method, distance = distance, maxit=5)

# removeNotification(id="ps_ordinate_message")

tagList(
  p(""),
  renderPrint(print(ps)),
  renderPlot({
    phyloseq::plot_ordination(ps, ps_ordinate,
                              type="samples",
                              color=color_samples, 
                              shape = shape_samples) +
      geom_point(size=5, alpha=0.85) +
      # xlim(-90,90) +
      scale_color_gradient2(high = "darkblue", mid= "white", low = "darkred") +
      
      phyloseq::plot_ordination(ps, ps_ordinate,
                                type="taxa",
                                color=color_taxa) +
      geom_point(size=5, alpha=0.8) +
      scale_color_viridis_d() +
      
      patchwork::plot_layout(ncol = 1)
    
  },  height = 1600, width = 1000, res = 96),
  renderPrint(print(ps_ordinate))
)

}
