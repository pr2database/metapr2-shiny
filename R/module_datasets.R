# UI ----------------------------------------------------------------------

data_reads_min_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(
      ns("reads_min"),
      "Minimum number of total reads per ASV",
      1000,
      min = 100,
      max = 10000,
      step = NA,
      width = NULL
    )
  )
}


data_datasets_UI <- function(id) {
  ns <- NS(id)
  
  choices = asv_set$datasets$dataset_id
  names(choices) = asv_set$datasets$dataset_code
  
  tagList(
    # shinyWidgets::multiInput(
    #   inputId = ns("datasets_selected_id"),
    #   label = h3("Select datasets"),
    #   # choices = asv_set$datasets$dataset_id,
    #   choiceNames = str_c(str_replace(asv_set$datasets$dataset_code, "_V4", ""), sep=" - "),
    #   choiceValues = asv_set$datasets$dataset_id,
    #   selected = asv_set$datasets$dataset_id,
    #   options= list(
    #     enable_search = TRUE,
    #     search_placeholder ="Search...",
    #     non_selected_header = "Available",
    #     selected_header = "Selected",
    #     actionsBox = TRUE)
    # )
    
    # See options: https://dreamrs.github.io/shinyWidgets/reference/pickerOptions.html
    
    shinyWidgets::pickerInput(
      inputId = ns("datasets_selected_id"),
      label = h3("Select datasets"),
      choices = choices,
      selected = asv_set$datasets$dataset_id,
      multiple = TRUE,
      options= shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        selectedTextFormat = "count > 1",
        liveSearch = TRUE
      )
    )
    
  )
}

data_datasets_table_UI <- function(id) {
  ns <- NS(id)
  tagList(
      # h4("Select and deselect datasets by clicking on the corresponding row."), 
      DT::DTOutput(ns('datasets_table'))
  )
}

data_samples_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # strong("Datasets selected:"),
    # textOutput(ns("datasets_selected_id")),
    
    h3("Select Samples"),
    
    checkboxGroupInput(ns("gene_region"), "Gene regions", inline = TRUE,  choices = global$gene_regions, selected = "V4"),
    checkboxGroupInput(ns("DNA_RNA"), "DNA or RNA", inline = TRUE,  choices = global$DNA_RNAs, selected = "DNA"),
    checkboxGroupInput(ns("substrate"), "Substrates", inline = TRUE,  choices = global$substrates, selected = global$substrates),
    checkboxGroupInput(ns("fraction_name"), "Size fractions", inline = TRUE,  choices = global$fraction_names, selected = global$fraction_names),
    checkboxGroupInput(ns("depth_level"), "Depth levels", inline = TRUE,  choices = global$depth_levels, selected = "surface"),
  )
}


# Server ------------------------------------------------------------------


dataServer <- function(id, taxo) {
# dataServer <- function(id, df_full, taxo) {
  # stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    options(warn=-1)



    # Validate sample selection -----------------------------------------------
    
    iv_samples <- shinyvalidate::InputValidator$new()
    
    iv_samples$add_rule("gene_region", shinyvalidate::sv_required(message = "Choose at least one gene region"))
    iv_samples$add_rule("DNA_RNA", shinyvalidate::sv_required(message = "Choose at least one DNA or RNA"))
    iv_samples$add_rule("substrate", shinyvalidate::sv_required(message = "Choose at least one substrate"))
    iv_samples$add_rule("fraction_name", shinyvalidate::sv_required(message = "Choose at least one fraction"))
    iv_samples$add_rule("depth_level", shinyvalidate::sv_required(message = "Choose at least one depth level"))
    iv_samples$add_rule("datasets_selected_id", shinyvalidate::sv_required(message = "Choose at least one dataset"))
    iv_samples$add_rule("reads_min", shinyvalidate::sv_required(message = "Must be at least 100"))
    iv_samples$add_rule("reads_min", ~ if (. < 100 ) "Must be at least 100")
    
    iv_samples$enable()
    
    
    # Create table of datasets -------------------------------------------------
    
    
    datasets_table <- reactive ({
      DT::datatable(asv_set$datasets %>% 
        select(dataset_id, dataset_name, region, paper_reference, sample_number, asv_number, n_reads_mean) %>%
        mutate(selected = ifelse(dataset_id %in% input$datasets_selected_id,TRUE, FALSE)),
        rownames = FALSE
      ) %>% DT::formatStyle("selected",  target = 'row',
                            backgroundColor = DT::styleEqual(c(FALSE, TRUE), c('white', 'lightcyan'))
                            )
      })
    
    # See:
    # Column width: https://stackoverflow.com/questions/25205410/r-shiny-set-datatable-column-width
    
    output$datasets_table <- DT::renderDT(datasets_table())
    
    
    # Update Sample Checkboxes With the values possible for the datasets selected ---
    
    update_checkbox <- function(variable, datasets_id) {
      values <- asv_set$samples %>% 
        filter(dataset_id %in% datasets_id) %>% 
        pull(.data[[variable]]) %>% 
        unique()
      
      updateCheckboxGroupInput(inputId = variable, 
                               choices = values,
                               selected = values,
                               inline = TRUE)
    }
    
    observeEvent(input$datasets_selected_id,{
      update_checkbox("gene_region", input$datasets_selected_id)
      update_checkbox("DNA_RNA", input$datasets_selected_id)
      update_checkbox("substrate", input$datasets_selected_id)
      update_checkbox("fraction_name", input$datasets_selected_id)
      update_checkbox("depth_level", input$datasets_selected_id)
      
      
    })
    
    
    # Update the datasets df -------------------------------------------------------
    
    datasets_selected <- reactive({
      req(iv_samples$is_valid())
      asv_set$datasets %>%
        filter(dataset_id %in% input$datasets_selected_id) 
      })
    
    # Display datasets selected
    
    
    output$datasets_selected_id = renderText(input$datasets_selected_id)
    
    # Select samples based on different parameters and datasets --------------------
    
    samples_selected <- reactive({
      
      # First check some samples are chosen
      req(iv_samples$is_valid())
      
      asv_set$samples %>%
        filter(gene_region %in% input$gene_region,
               DNA_RNA %in% input$DNA_RNA,
               depth_level %in% input$depth_level,
               fraction_name %in% input$fraction_name,
               substrate %in% input$substrate,
               dataset_id %in% input$datasets_selected_id
        ) })
    # Only keep the ASVs that are in df_slected ---------------
    
    fasta_selected <- reactive({
      req(taxo(), input$reads_min)
      asv_set$fasta %>%
        filter(.data[[taxo()$level]] %in% taxo()$name,
                sum_reads_asv >= input$reads_min
        ) 
      })
    
    # The full data frame filtered by samples, datasets and taxonomy ---------------
    
    # df_selected <- reactive({
    #   # First check some samples are chosen
    #   req(iv_samples$is_valid())
    #   
    #   df_full %>%
    #     filter(gene_region %in% input$gene_region,
    #       DNA_RNA %in% input$DNA_RNA,
    #       depth_level %in% input$depth_level,
    #       fraction_name %in% input$fraction_name,
    #       substrate %in% input$substrate,
    #       dataset_id %in% input$datasets_selected_id,
    #       .data[[taxo()$level]] %in% taxo()$name, 
    #       sum_reads_asv >= input$reads_min
    #     ) 
    #   })
    
    df_selected <- reactive({
      # First check some samples are chosen
      req(iv_samples$is_valid(), samples_selected(), fasta_selected ())
      
      asv_set$df %>%
        filter(file_code %in% samples_selected()$file_code,
               asv_code %in% fasta_selected()$asv_code) %>% 
        left_join(asv_set$samples) %>% 
        left_join(select(asv_set$fasta, asv_code, kingdom:species, sum_reads_asv)) %>%
        filter(!is.na(kingdom)) %>% # Some asvs are missing from the FASTA table... (to be checked)
        mutate(depth_level = forcats::fct_relevel(depth_level,
                                                  levels = c("bathypelagic", "mesopelagic", "euphotic", "surface")))
    })
    
    
    

    # Filter phyloseq by samples and taxon selected ----------------------------
    
    if (global$phyloseq_use){
    
        ps_selected <- reactive({ 
          
          req(iv_samples$is_valid())
          
          print("filtering PS")
          
          ps <- ps_select (ps = asv_set$ps, 
                     gene_region = input$gene_region,
                     DNA_RNA = input$DNA_RNA, depth_level = input$depth_level, 
                     fraction_name = input$fraction_name, substrate = input$substrate, 
                     datasets_selected_id = input$datasets_selected_id,
                     ps_reads_min = input$reads_min, 
                     taxo_level = taxo()$level, taxo_name = taxo()$name)
          return(ps)
        })
        
       
        return(list(datasets_selected = datasets_selected,
                    samples_selected = samples_selected,
                    df_selected = df_selected,
                    fasta_selected = fasta_selected,
                    ps_selected = ps_selected))
    }
    
    return(list(datasets_selected = datasets_selected,
                samples_selected = samples_selected,
                df_selected = df_selected,
                fasta_selected = fasta_selected))

  })
  
}  
