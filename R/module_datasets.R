# Options for picker-----------------------------------------------------------------

options_picker <- shinyWidgets::pickerOptions(
  actionsBox = TRUE,
  selectedTextFormat = "count > 10",
  liveSearch = TRUE
)

options_picker_datasets <- shinyWidgets::pickerOptions(
  actionsBox = TRUE,
  size = 10,
  selectedTextFormat = "count > 1",
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
      100,
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
    
    h4("Quick dataset selection."),
    checkboxGroupInput(ns("dataset_group"), "Dataset groups", inline = TRUE,  
                       choiceNames = global$datasets$group, choiceValues = global$datasets$filter),
    h4("Save/Load settings"),
    fluidRow(
      column(2, downloadButton(ns('save_settings'), 'Save settings')),
      column(4, fileInput(ns('load_settings'), NULL, buttonLabel = 'Load settings', multiple = FALSE, accept = ".yaml"))
    )  ,
    p(),
    # h4("Select and deselect datasets by clicking on the corresponding row."), 
    DT::DTOutput(ns('datasets_table'))
  )
}


data_samples_UI <- function(id) {
  ns <- NS(id)
  tagList(

    h3("Select Samples"),
    
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
      req(asv_set())
      choices = asv_set()$datasets$dataset_id
      names(choices) = asv_set()$datasets$dataset_code
      choices <- choices[order(names(choices))]
      
      tagList(
        # See options: https://dreamrs.github.io/shinyWidgets/reference/pickerOptions.html
        shinyWidgets::pickerInput(
          inputId = ns("datasets_selected_id"),
          label = h3("Select datasets"),
          choices = choices,
          selected = asv_set()$datasets$dataset_id,
          multiple = TRUE,
          options= options_picker_datasets
        )
        
      )
    })
    
      
    # Panel with table of datasets -------------------------------------------------
    
    # See:
    # Column width: https://stackoverflow.com/questions/25205410/r-shiny-set-datatable-column-width
    # However does not real works....
    
    datasets_table <- reactive ({
      req(asv_set())
      DT::datatable(asv_set()$datasets %>% 
                      select(dataset_id, dataset_name, region, paper_reference, sequencing_technology, sample_number, asv_number, n_reads_mean) %>%
                      mutate(selected = ifelse(dataset_id %in% input$datasets_selected_id,TRUE, FALSE)) %>% 
                      arrange(dataset_name) ,
                    rownames = FALSE ,
                    options = list(
                      autoWidth = FALSE,
                      scrollX=FALSE,
                      columnDefs = list(list(width = '10px', targets = 7)))
      ) %>% DT::formatStyle("selected",  target = 'row',
                            backgroundColor = DT::styleEqual(c(FALSE, TRUE), c('white', 'lightcyan'))
      )
    })
    
    output$datasets_table <- DT::renderDT(datasets_table())
    
    
    # Update the sets selected in case of Quick dataset selection --------------
    
    
    
    
    observeEvent(input$dataset_group, {
      req(asv_set())

      filter_datasets <- str_c(input$dataset_group , collapse = " | ")
      message(filter_datasets)
      
      selected <- asv_set()$datasets %>%
        filter(!! rlang::parse_expr(filter_datasets)) %>%
        # filter(str_detect(dataset_groups, 'global') & str_detect(dataset_groups, 'oceanic|coastal') & gene_region == 'V4') %>% 
        pull(dataset_id)
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "datasets_selected_id",
        selected = selected
      )
    })
    

    
    
    # Update Sample Checkboxes With the values possible for the datasets selected ---
    
    update_picker <- function(asv_set, variable, values_selected, datasets_id) {
      choices <- asv_set$samples %>% 
        filter(dataset_id %in% datasets_id) %>% 
        arrange(.data[[variable]]) %>% 
        pull(.data[[variable]]) %>% 
        unique() %>% 
        as.character()
      if (is.null(values_selected)) values_selected <- choices
      shinyWidgets::updatePickerInput( session = session,
                                       inputId = variable, 
                                       choices = choices,
                                       selected = values_selected,
                                       options = options_picker)
    }
    
    observeEvent(input$datasets_selected_id,{
      req(asv_set())
      update_picker(asv_set(), "gene_region", input$gene_region, input$datasets_selected_id)
      update_picker(asv_set(), "DNA_RNA", input$DNA_RNA, input$datasets_selected_id)
      update_picker(asv_set(), "ecosystem", input$ecosystem, input$datasets_selected_id)
      update_picker(asv_set(), "substrate", input$substrate, input$datasets_selected_id)
      update_picker(asv_set(), "fraction_name",  input$fraction_name, input$datasets_selected_id)
      update_picker(asv_set(), "depth_level", input$depth_level, input$datasets_selected_id)
    })
    
    # Create settings -----------------------------------------------------------
    
    settings  <- reactive({
         return(list(
                gene_region = input$gene_region,
                DNA_RNA = input$DNA_RNA,
                ecosystem = input$ecosystem,
                substrate = input$substrate,
                fraction_name = input$fraction_name,
                depth_level = input$depth_level,
                datasets_selected_id = input$datasets_selected_id,
                reads_min = input$reads_min)
           )
    })
    
    # Save settings --------
    output$save_settings <- downloadHandler(
      
      filename = function() {str_c("metapr2_settings", "_", Sys.Date(), ".yaml")},
      content = function(path) {
      yaml::write_yaml(settings(), file = path)
        
     }
    ) 
    
    # Update settings --------
    observeEvent(input$load_settings, {
      
      tryCatch( 
        {settings_new <- yaml::read_yaml(file = input$load_settings$datapath)                
          print(settings_new)
          
          choices = asv_set()$datasets$dataset_id
          names(choices) = asv_set()$datasets$dataset_code
          choices <- choices[order(names(choices))]
          
          shinyWidgets::updatePickerInput(
            session = session,
            inputId = "datasets_selected_id",
            choices = choices,
            selected =  settings_new$datasets_selected_id,
            options= options_picker_datasets
          )
          update_picker(asv_set(), "gene_region", settings_new$gene_region, settings_new$datasets_selected_id)
          update_picker(asv_set(), "DNA_RNA", settings_new$DNA_RNA, settings_new$datasets_selected_id)
          update_picker(asv_set(), "ecosystem", settings_new$ecosystem, settings_new$datasets_selected_id)
          update_picker(asv_set(), "substrate", settings_new$substrate, settings_new$datasets_selected_id)
          update_picker(asv_set(), "fraction_name", settings_new$fraction_name, settings_new$datasets_selected_id)
          update_picker(asv_set(), "depth_level", settings_new$depth_level, settings_new$datasets_selected_id)
        },
          error=function(cond) {
            message("Invalid yaml file")
            showModal(modalDialog(title = "Loading settings", "Invalid YAML file", size = "s", easyClose = TRUE))
            return(NA)
          }
      )

    })
    
# -----------------------------------------------------------------------------------------------------------------------    
    
    # ==================================================================
    #    Reading the data after authenitification at start
    # ==================================================================
    
    
    # Use observe to filter the different components of asv_set
    # Need to use <<- so that values are global
    
    asv_set <- reactive({ 
      # req(user_datasets()) 
      req(!is.null(authentification$user))
      
      # Filtering the datasets depending on "user"
      
      if (authentification$user == "") {
        dir_asv_set <- "data-qs"
      }
      if (authentification$user == "private") {
        dir_asv_set <- "data-qs-private"
      }
      
      
      message("User: ", authentification$user)
      
      # Reading the data - Using the normal way ----------------------------------
      asv_set_all  <- tryCatch(
        {
          qs::qread(system.file(dir_asv_set,  'asv_set.qs', package = "metapr2"))
        },
        error=function(cond) {
          message("Cannot use system.file")
          return(NA)
        }
      )

      
      # Reading the data - Using the explicit way ------------------------------
      
      if(is.na(asv_set_all)){
        asv_set_all <- qs::qread(str_c("inst/", dir_asv_set, "/asv_set.qs"))
        message("Using full path")
      }
      
      asv_set_all$samples <- asv_set_all$samples %>% 
        mutate(label = str_c(dataset_id, dataset_code,
                             str_replace_na(station_id, ""),
                             str_replace_na(depth_level, ""),
                             str_replace_na(substrate, ""),
                             sep = "-"))
      
      message("Data sets: ", nrow(asv_set_all$datasets))
      message("Samples: ", nrow(asv_set_all$samples))
      message("df: ",nrow(asv_set_all$df))
      message("Fasta: ",nrow(asv_set_all$fasta))
      message("Mem used (GB): ", pryr::mem_used()/10^9)
      
      return(asv_set_all)
      
    })
    
    
    # ==================================================================
    #    Filtering the data based on settings
    # ==================================================================
    
    
    # Update the datasets df -------------------------------------------------------
    
    datasets_selected <- reactive({
      req(iv_samples$is_valid())
      req(asv_set())
      asv_set()$datasets %>%
        filter(dataset_id %in% input$datasets_selected_id) 
    })
    

    
    samples_selected <- reactive({
      asv_set()$samples%>% 
        filter(gene_region %in% input$gene_region,
               DNA_RNA %in% input$DNA_RNA,
               ecosystem %in% input$ecosystem,
               depth_level %in% input$depth_level,
               fraction_name %in% input$fraction_name,
               substrate %in% input$substrate,
               dataset_id %in% input$datasets_selected_id
        )
    })
    

      
    cols_to_remove <- c("reads_corrected_total" , "reads_corrected_photo" , 
                          "country" , "oceanic_region" , "cruise" , "station_id" , 
                          "bottom_depth" , "site_name" ,
                          "Chla" , "NO3" , "NH4" , "PO4" , "Si")    
    
    df_selected <- reactive({
      # First check some samples are chosen
      req(iv_samples$is_valid())  
      asv_set()$df %>% 
        inner_join(samples_selected(), by = "file_code") %>% 
        left_join(select(asv_set()$fasta, asv_code, kingdom:species, ecological_function, sum_reads_asv), by="asv_code") %>%
        filter(!is.na(kingdom)) %>% # Some asvs are missing from the FASTA table... (to be checked) %>% 
        select(-any_of(cols_to_remove)) %>% 
        filter(.data[[taxo()$level]] %in% taxo()$name ,
               sum_reads_asv >= input$reads_min, 
               !(division %in% taxo()$taxa_excluded)
        )
    })
    
    fasta_selected <- reactive({
      message("Mem used (GB) after df_selected : ", pryr::mem_used()/10^9)
      asv_set()$fasta %>% 
        filter(asv_code %in% df_selected()$asv_code)
    })
      
     
    
    
    
      df_all <- reactive(asv_set()$df)
      fasta_all <- reactive(asv_set()$fasta)
    
      return(list(datasets_selected = datasets_selected,
                  samples_selected = samples_selected,
                  df_selected = df_selected,
                  fasta_selected = fasta_selected,
                  df_all = df_all,
                  fasta_all = fasta_all))

  })
  
}  
