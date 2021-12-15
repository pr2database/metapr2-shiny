
barplot <- function(df, variable, color_coding, taxo_level) {
  
  message("Computing barplot")
  
  variable_date <- c("year", "month", "day")
  
  if (variable == "year") {
    date_breaks = "1 year"
    date_labels = "%Y"
    date_angle = 0
  }
  if (variable %in%c( "month", "day")) { 
    date_breaks = "1 month"
    date_labels = "%Y-%m"
    date_angle = 45
  }
  
  
  
  if(color_coding == "taxonomy")  {
    color_col <- taxo_level }
  else {
    color_col <- color_coding
  }
  
  variable_to_use <- variable
  
  if(variable %in% variable_date) {
    variable_to_use <- "date"
  } 
  
  # https://statisticsglobe.com/aggregate-daily-data-to-month-year-intervals-in-r
  
  
  barplot_discretize <- function(df, x, reverse = TRUE, ...){
    if (length(unique(df[[x]])) > 1) {
      if (x == "depth") {
        df[[x]] = cut.default(df[[x]], breaks = c(seq(0,250,25), 500, 750, 1000, 9000), include.lowest = TRUE, dig.lab = 4)
      }
      else{
        if (reverse) df[[x]]  =  fct_rev(cut_width(df[[x]], ...))
        else df[[x]]  =  cut_width(df[[x]], ...)
      }
    } else{
      df[[x]] =  as.factor(df[[x]])
    }
    
    # print(df)
    return(df )
  }
  
  
  df <- df %>% 
    select(file_code, any_of(c(color_col, variable_to_use)), n_reads_pct)
  
  # Discretize the data (must make sure that there is more than one value)
  
  if (variable %in%  c("temperature", "salinity")) df <- barplot_discretize (df, variable,  width=5, boundary=0, reverse = TRUE)
  if (variable %in%  c("latitude")) df <- barplot_discretize (df, variable,  width=20, boundary=0, reverse = TRUE)
  if (variable %in%  c("depth")) df <- barplot_discretize (df, variable,  width=25, boundary=0, reverse = FALSE)
  
  # floor_date: Round date-times down.
  
  if(variable %in% variable_date) {
    df <- df %>%
      filter(!is.na(date)) %>% 
      mutate(date =  lubridate::floor_date(as.Date(date), variable))
  } 
  

  samples <- df %>% 
    select(file_code, any_of(c(variable_to_use))) %>% 
    distinct() %>% 
    group_by(across(any_of(variable_to_use))) %>% 
    count() 
  
  df <- df %>% 
    group_by(across(any_of(c(color_col, variable_to_use)))) %>%
    summarize(n_reads_pct = sum(n_reads_pct)) %>%
    group_by(across(any_of(variable_to_use))) %>% 
    mutate(n_reads_pct = n_reads_pct/sum(n_reads_pct)*100)
  
  
  # cat(variable_to_use, "\n")
  # print(df)
  
  gg <- df  %>% 
    ggplot() +
    xlab("% of reads") + ylab("") +
    theme_bw() 
  
  if(variable_to_use == "date") {
    gg <- gg +
      geom_col(aes (x= .data[[variable_to_use]],
                    y=n_reads_pct, 
                    fill=.data[[color_col]])) +
      scale_x_date(date_breaks = date_breaks,
                   date_labels = date_labels) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))  +
      scale_y_continuous(breaks = seq(0,100,20)) 
  }
  else {
    gg <- gg +
      geom_col(aes(y= fct_rev(.data[[variable_to_use]]),
                   x=n_reads_pct, 
                   fill=.data[[color_col]])) +
      # Add numberof samples
      geom_text(data = samples, 
                aes(x = 100, 
                    y = fct_rev(.data[[variable_to_use]]), 
                    label = glue::glue("n = {n}"),
                ),
                nudge_x = 3, 
                size = 3) +
      scale_x_continuous(breaks = seq(0,100,20))
  }
  
  if(variable %in% c("month", "day")) {   # Add vertical limits
    gg <- gg +
      geom_vline(xintercept= as.numeric(as.Date(str_c(c(2000:2030), "-01-01"))))
  }

  if(variable %in% c("year")) {   
    # Add numberof samples
    gg <- gg + geom_text( data = samples, 
                          aes(y = 100, 
                              x = .data[[variable_to_use]], 
                              label = glue::glue("n = {n}"),
                          ),
                          nudge_y = 3,
                          size = 3,
                          angle = 45) # Angle does not work with plotly
  }
  
  if(color_coding == "taxonomy"){ 
    gg <- gg + scale_fill_viridis_d() 
  }
  if(color_coding == "ecological_function"){ 
    gg <- gg + scale_fill_manual(values = global$ecological_function_colors)
  }
  
  return(gg)
  
}
