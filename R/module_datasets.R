# Options for picker-----------------------------------------------------------------

options_picker <- shinyWidgets::pickerOptions(
  actionsBox = TRUE,
  selectedTextFormat = "count > 10",
  liveSearch = TRUE
)

# UI ----------------------------------------------------------------------

data_reads_min_UI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Select ASVs"),
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
  tagList(
    uiOutput(ns('ui_datasets_selection'))
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
    
    # checkboxGroupInput(ns("gene_region"), "Gene regions", inline = TRUE,  choices = global$gene_regions, selected = "V4"),
    # checkboxGroupInput(ns("DNA_RNA"), "DNA or RNA", inline = TRUE,  choices = global$DNA_RNAs, selected = "DNA"),
    # checkboxGroupInput(ns("ecosystem"), "Ecosystems", inline = TRUE,  choices = global$ecosystems, selected = global$ecosystems),
    # checkboxGroupInput(ns("substrate"), "Substrates", inline = TRUE,  choices = global$substrates, selected = "water"),
    # checkboxGroupInput(ns("fraction_name"), "Size fractions", inline = TRUE,  choices = global$fraction_names, selected = c("pico", "total")),
    # checkboxGroupInput(ns("depth_level"), "Depth levels", inline = TRUE,  choices = global$depth_levels, selected = "surface"),
    shinyWidgets::pickerInput(ns("gene_region"), "Gene regions", choices = global$gene_regions, selected = "V4", multiple = TRUE, options= options_picker),
    shinyWidgets::pickerInput(ns("DNA_RNA"), "DNA or RNA", choices = global$DNA_RNAs, selected = "DNA", multiple = TRUE, options= options_picker),
    shinyWidgets::pickerInput(ns("ecosystem"), "Ecosystems", choices = global$ecosystems, selected = global$ecosystems, multiple = TRUE, options= options_picker),
    shinyWidgets::pickerInput(ns("substrate"), "Substrates", choices = global$substrates, selected = "water", multiple = TRUE, options= options_picker),
    shinyWidgets::pickerInput(ns("fraction_name"), "Size fractions", choices = global$fraction_names, selected = c("pico", "total"), multiple = TRUE, options= options_picker),
    shinyWidgets::pickerInput(ns("depth_level"), "Depth levels", choices = global$depth_levels, selected = "surface", multiple = TRUE, options= options_picker),
    
  )
}



# Server ------------------------------------------------------------------


dataServer <- function(id, taxo, authentification) {
  # dataServer <- function(id, df_full, taxo) {
  # stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    options(warn=-1)
    
    
    
    # Validate sample selection -----------------------------------------------
    
    iv_samples <- shinyvalidate::InputValidator$new()
    
    iv_samples$add_rule("gene_region", shinyvalidate::sv_required(message = "Choose at least one gene region"))
    iv_samples$add_rule("DNA_RNA", shinyvalidate::sv_required(message = "Choose at least one DNA or RNA"))
    iv_samples$add_rule("ecosystem", shinyvalidate::sv_required(message = "Choose at least one ecosystem"))
    iv_samples$add_rule("substrate", shinyvalidate::sv_required(message = "Choose at least one substrate"))
    iv_samples$add_rule("fraction_name", shinyvalidate::sv_required(message = "Choose at least one fraction"))
    iv_samples$add_rule("depth_level", shinyvalidate::sv_required(message = "Choose at least one depth level"))
    iv_samples$add_rule("datasets_selected_id", shinyvalidate::sv_required(message = "Choose at least one dataset"))
    iv_samples$add_rule("reads_min", shinyvalidate::sv_required(message = "Must be at least 100"))
    iv_samples$add_rule("reads_min", ~ if (. < 100 ) "Must be at least 100")
    
    iv_samples$enable()
    
    # Create menu for dataset selection ------------------------------------------
    
    output$ui_datasets_selection <- renderUI({
      req(!is.null(authentification$user))
      choices = asv_set$datasets$dataset_id
      names(choices) = asv_set$datasets$dataset_code
      choices <- choices[order(names(choices))]
      
      tagList(
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
    })
    
    
    # Read and filter datasets depending on user ---- THIS IS DONE ONLY AT START --------------
    
   #  user_datasets <- reactive({
   #    req(!is.null(authentification$user))
   #    
   #    if (authentification$user == "basic") {
   #       return( asv_set$datasets %>%
   #          filter(dataset_id %in% c(1, 34, 35, 205, 206))
   #          )
   #    }
   #    if (authentification$user == "public") {
   #      return(asv_set$datasets %>%
   #        filter(metapr2_version == "1.0")
   #      )
   #    }
   #    if (authentification$user == "private") {
   #      return(asv_set$datasets)
   #    }
   # })
    
    # Use observe to filter the different components of asv_set
    # Need to use <<- so that values are global
    
    observe({ 
      # req(user_datasets()) 
      req(!is.null(authentification$user))
      
      # asv_set <<-list()
      
      # Filtering the datasets depending on "user"
      
      if (authentification$user == "") {
         dir_asv_set <<- "data-qs"
      }
      if (authentification$user == "private") {
         dir_asv_set <<- "data-qs-private"
      }
      
      
      # Reading the data - Using the normal way ----------------------------------
      asv_set  <<- tryCatch(
        {
          qs::qread(system.file(dir_asv_set,  'asv_set.qs', package = "metapr2"))
        },
        error=function(cond) {
          message("Cannot use system.file")
          return(NA)
        }
      )

      # Reading the data - Using the explicit way ------------------------------

      if(is.na(asv_set)){
        asv_set <<- qs::qread(str_c("inst/", dir_asv_set, "/asv_set.qs"))
        print("Using full path")
      }
      
      # asv_set_full <- qs::qread("inst/data-qs/asv_set.qs")
      
      
      # Filtering samples, df and fasta depending on use -----------------------
      
      # asv_set$samples <<- asv_set_full$samples %>% 
      #   filter(dataset_id %in% asv_set$datasets$dataset_id)
      # 
      #  asv_set$df <<- asv_set_full$df %>% 
      #    filter(file_code %in% asv_set$samples$file_code)
      #  
      #  asv_set$fasta  <<- asv_set_full$fasta %>% 
      #    filter(asv_code %in% asv_set$df$asv_code) 
       
       cat("Data sets: ", nrow(asv_set$datasets), "\n")
       cat("Samples: ", nrow(asv_set$samples), "\n")
       cat("df: ",nrow(asv_set$df), "\n")
       cat("Fasta: ",nrow(asv_set$fasta), "\n")
       })

      
    # Create table of datasets -------------------------------------------------
    
    datasets_table <- reactive ({
      DT::datatable(asv_set$datasets %>% 
                      select(dataset_id, dataset_name, region, paper_reference, sample_number, asv_number, n_reads_mean) %>%
                      mutate(selected = ifelse(dataset_id %in% input$datasets_selected_id,TRUE, FALSE)) %>% 
                      arrange(dataset_name) ,
                    rownames = FALSE
      ) %>% DT::formatStyle("selected",  target = 'row',
                            backgroundColor = DT::styleEqual(c(FALSE, TRUE), c('white', 'lightcyan'))
      )
    })
    
    # See:
    # Column width: https://stackoverflow.com/questions/25205410/r-shiny-set-datatable-column-width
    
    output$datasets_table <- DT::renderDT(datasets_table())
    
    
    # Update Sample Checkboxes With the values possible for the datasets selected ---
    
    update_picker <- function(variable, datasets_id) {
      values <- asv_set$samples %>% 
        filter(dataset_id %in% datasets_id) %>% 
        arrange(.data[[variable]]) %>% 
        pull(.data[[variable]]) %>% 
        unique() %>% 
        as.character()
      values_selected <- values
      # print(values)
      if (variable == "DNA_RNA") values_selected <- "DNA"
      if (variable == "gene_region") values_selected <- "V4"
      if (variable == "depth_level") values_selected <- "surface"
      if (variable == "substrate") values_selected <- "water"
      if (variable == "fraction_name") values_selected <- c("pico", 'total')
      
      shinyWidgets::updatePickerInput( session = session,
                                       inputId = variable, 
                                       choices = values,
                                       selected = values_selected,
                                       options = options_picker)
    }
    
    update_checkbox <- function(variable, datasets_id) {
      values <- asv_set$samples %>% 
        filter(dataset_id %in% datasets_id) %>% 
        arrange(.data[[variable]]) %>% 
        pull(.data[[variable]]) %>% 
        unique() %>% 
        as.character()
      
      values_selected <- values
      if (variable == "DNA_RNA") values_selected <- "DNA"
      if (variable == "gene_region") values_selected <- "V4"
      if (variable == "depth_level") values_selected <- "surface"
      if (variable == "substrate") values_selected <- "water"
      if (variable == "fraction_name") values_selected <- c("pico", 'total')
      
      updateCheckboxGroupInput(inputId = variable, 
                               choices = values,
                               selected = values_selected,
                               inline = TRUE)
    }
    
    observeEvent(input$datasets_selected_id,{
      # update_checkbox("gene_region", input$datasets_selected_id)
      # update_checkbox("DNA_RNA", input$datasets_selected_id)
      # update_checkbox("ecosystem", input$datasets_selected_id)
      # update_checkbox("substrate", input$datasets_selected_id)
      # update_checkbox("fraction_name", input$datasets_selected_id)
      # update_checkbox("depth_level", input$datasets_selected_id)
      update_picker("gene_region", input$datasets_selected_id)
      update_picker("DNA_RNA", input$datasets_selected_id)
      update_picker("ecosystem", input$datasets_selected_id)
      update_picker("substrate", input$datasets_selected_id)
      update_picker("fraction_name", input$datasets_selected_id)
      update_picker("depth_level", input$datasets_selected_id)
      
      
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
               ecosystem %in% input$ecosystem,
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
      
      cols_to_remove <- c("reads_corrected_total" , "reads_corrected_photo" , 
                          "country" , "oceanic_region" , "cruise" , "station_id" , 
                          "bottom_depth" , "site_name" , 
                          "sum_reads_asv" , 
                          "Chla" , "NO3" , "NH4" , "PO4" , "Si")
      
      asv_set$df %>%
        filter(file_code %in% samples_selected()$file_code,
               asv_code %in% fasta_selected()$asv_code) %>% 
        left_join(asv_set$samples) %>% 
        left_join(select(asv_set$fasta, asv_code, kingdom:species, sum_reads_asv)) %>%
        filter(!is.na(kingdom)) %>% # Some asvs are missing from the FASTA table... (to be checked) %>% 
        select(-any_of(cols_to_remove))
    })
    
    
    
    
    # Filter phyloseq by samples and taxon selected ----------------------------
    
    # if (global$phyloseq_use){
    #   
    #   ps_selected <- reactive({ 
    #     
    #     req(iv_samples$is_valid())
    #     
    #     print("filtering PS")
    #     
    #     ps <- ps_select (ps = asv_set$ps, 
    #                      gene_region = input$gene_region,
    #                      DNA_RNA = input$DNA_RNA, 
    #                      ecosystem = input$ecosystem,
    #                      depth_level = input$depth_level, 
    #                      fraction_name = input$fraction_name, 
    #                      substrate = input$substrate, 
    #                      datasets_selected_id = input$datasets_selected_id,
    #                      ps_reads_min = input$reads_min, 
    #                      taxo_level = taxo()$level, taxo_name = taxo()$name)
    #     return(ps)
    #   })
    #   
    #   
    #   return(list(datasets_selected = datasets_selected,
    #               samples_selected = samples_selected,
    #               df_selected = df_selected,
    #               fasta_selected = fasta_selected,
    #               ps_selected = ps_selected))
    # }
    
    return(list(datasets_selected = datasets_selected,
                samples_selected = samples_selected,
                df_selected = df_selected,
                fasta_selected = fasta_selected))
    
  })
  
}  
