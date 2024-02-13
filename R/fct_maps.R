# Changes ------------------


# Leaflet - Function for size Legend -------------------------------------------

addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, legend_title=""){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", 
                           2*sizes, "px; height:", 2*sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, 
                   colors = colorAdditions, 
                   labels = labelAdditions, 
                   opacity = opacity,
                   title = legend_title))
}



# Leaflet - Function to start a  map ------------------------------------------


map_leaflet_init <- function(lng_center=0, lat_center=0, zoom = 3, 
                             width=1000,  height=1000) {
  
  lng_all = c(-5000,5000)
  lat_equator = c(0,0)
  lat_tropics = c(23.44, 23.44)
  lat_polar = c(66.6,66.6)
  dash_equator = NULL
  dash_tropics = c(20, 20)
  dash_polar = c(10, 30)
  line_weight = 0.5
  color = "blue"
  

  map <- leaflet(width = width, height = height,
                 options = leafletOptions(worldCopyJump = FALSE)) %>%
    addTiles() %>%
    addProviderTiles(providers$Esri.WorldStreetMap,
                     options = providerTileOptions(noWrap = FALSE)) %>%
    addPolylines(lat = lat_polar, lng = lng_all, dashArray = dash_polar, color = color, weight = line_weight) %>%
    addPolylines(lat = lat_tropics, lng = lng_all, dashArray = dash_tropics, color = color, weight = line_weight) %>%
    addPolylines(lat = lat_equator, lng = lng_all, dashArray = dash_equator, color = color, weight = line_weight) %>% 
    addPolylines(lat = -lat_tropics, lng = lng_all, dashArray = dash_tropics, color = color, weight = line_weight) %>%
    addPolylines(lat = -lat_polar, lng = lng_all, dashArray = dash_polar, color = color, weight = line_weight) %>%
    setView(lng=lng_center, lat=lat_center, zoom=zoom) 
}

# Leaflet - Function to draw a  map--------------------------------------------

# add size_factor as an argument to map_leaflet function

map_leaflet <- function(map, df, 
                        pct_max = 100,
                        size_factor = 30,
                        legend_title="% of eukaryotes",
                        map_type = "pie") {
  

# map_type can be "pie", "dominant"  
  
  cross <- makeIcon(
    iconUrl = "https://unpkg.com/ionicons@5.2.3/dist/svg/add-outline.svg",
    iconWidth = 10, iconHeight = 10,
    iconAnchorX = 5, iconAnchorY = 5
  )
  
  if(pct_max <= 0.1) pct_max = 0.1
  
  size_scale = c(1, 0.25, 0.10, 0.025, 0.01)
  size_labels = c(1, 0.25, 0.10, 0.025, 0.01)*pct_max
  
  df_taxa <- dplyr::select(df$present, -(file_code:dominant_taxon))
  
  pal_dominant <-  leaflet::colorFactor(palette = "viridis", df$present$dominant_taxon)
  pal_pie = viridis::viridis(ncol(df_taxa), option = "A")
  
  map <- map  %>% 
    addMarkers(data = df$absent,
               lat = ~ latitude, 
               lng = ~ longitude, 
               icon=cross,
               popup = ~ stringr::str_c(label, "<br/>","Taxon is absent"), 
               labelOptions = labelOptions(textsize = "10px", 
                                           noHide = F) 
    )
  
  # add checking if legend_title = "none" # to skip plotting legend on duplicate data
  if(legend_title != "none") {
    map <- map  %>% 
      addLegendCustom(colors = "black", 
                      labels = size_labels,
                      sizes = sqrt(size_scale)*size_factor,
                      legend_title = legend_title
      )
  }
  
  if(nrow(df$present) > 0) {
 
      if (map_type == "pie"){
       
        map <- map  %>% 
        leaflet.minicharts::addMinicharts(
          lng = df$present$longitude,
          lat = df$present$latitude,
          type = "pie",
          chartdata = df_taxa,
          colorPalette = pal_pie,
          width =  sqrt(df$present$pct/pct_max)*size_factor*1.8 ,
          opacity = 0.5,
          transitionTime = 0,
          # popup = stringr::str_c(df$present$label, " - ", sprintf(df$present$pct, fmt = '%.2f'), " %"),
          legendPosition = "bottomright"
        ) 
        } else  if (map_type == "dominant") {
    
        map <- map %>%
        addCircleMarkers(data = df$present,
                         lat = ~ latitude,
                         lng = ~ longitude,
                         radius = ~ sqrt(pct/pct_max)*size_factor,
                         popup = ~ stringr::str_c(label, "<br/>", sprintf(pct, fmt = '%.2f'), " % of euks", "<br/>", "Dominant taxon: ", dominant_taxon),
                         color = ~ pal_dominant(dominant_taxon),
                         weight = 0,  # No line
                         fillOpacity = 0.5, # Alpha factor
                         labelOptions = labelOptions(textsize = "10px",
                                                     noHide = F)) %>%
        addLegend("bottomright",
                  title = "Dominant taxon",
                  pal = pal_dominant,
                  values = df$present$dominant_taxon,
                  opacity = 1)}
  }  
  
  # cat("Map: ")
  # print(pryr::mem_used())
  
  return(map)
}

# Function to reformat data frame from sample counts ----------------------------

reformat_df_map <- function (df, samples, taxo_level) {
  
  # Only keep the taxa of interest
  
  # Debug
  # browser()
  
  # The next two lines are not necessary since df is already filtered for taxonomy but leave to be more general
  
  # df <- df%>% 
  #   filter(!!as.symbol(taxo_level)  %in% taxo_name) 
  
  message("Computing map")
  
  # Compute the level below the rank considered (e.g. species for genus) 
  
  if(taxo_level == "asv_code") taxo_level <- "species"
  
  if(taxo_level != "asv_code"){
  
       taxo_level_below = global$taxo_levels[which(global$taxo_levels == taxo_level) + 1]
      
      # Compute number for reads at taxo_level and taxo_level + 1
      
        samples_counts <- df %>%
          group_by(across(all_of(c("file_code", taxo_level, taxo_level_below)))) %>%
          mutate(n_reads_2 = sum(n_reads_pct, na.rm = TRUE)) %>%
          ungroup(.data[[taxo_level_below]]) %>%
          mutate(n_reads_1 = sum(n_reads_pct, na.rm = TRUE)) %>% 
          ungroup() %>% 
          select(all_of(c("file_code", taxo_level,taxo_level_below, "n_reads_1", "n_reads_2"))) %>% 
          distinct()
    
      df <- samples %>%             
        select(file_code) %>% 
        left_join(samples_counts)
      
      absent <- df %>%
        filter(is.na(n_reads_1))%>% 
        left_join(select(samples, file_code, latitude, longitude, label)) %>% 
        select(file_code, latitude, longitude, label) %>% 
        distinct()
      
      present <- df %>%   
        filter(!is.na(n_reads_1)) %>%  # Next line is necessary to include also the samples where the taxo group is absent...
        tidyr::expand(file_code, .data[[taxo_level]], .data[[taxo_level_below]]) %>% 
        left_join(samples_counts) %>% 
        mutate(n_reads_1 = tidyr::replace_na(n_reads_1, 0),  # This line is not necessary should there be no
               n_reads_2 = tidyr::replace_na(n_reads_2, 0)) %>% 
        filter(n_reads_1 != 0) %>% 
        left_join(select(samples, file_code, latitude, longitude, label)) %>% 
        distinct() %>% 
        mutate(pct = (n_reads_1/global$n_reads_tot_normalized)*100)
      
    # Debug
    # browser()
      
      dominant_taxon <- present %>%
        arrange(file_code, desc(n_reads_2)) %>%
        group_by(file_code) %>%
        dplyr::slice(1) %>%
        mutate(dominant_taxon = .data[[taxo_level_below]]) %>%
        select(file_code, dominant_taxon)
      
      present <- left_join(present, dominant_taxon) %>% 
        tidyr::pivot_wider(names_from = .data[[taxo_level_below]],
                    values_from = n_reads_2,
                    values_fill = 0) %>% 
        select(-n_reads_1)
      
  } else {
    absent <-samples %>%
      filter(!(file_code %in% df$file_code))%>% 
      select(file_code, latitude, longitude, label) %>%  
      distinct()
    
    present <- df %>%   
      filter(!is.na(n_reads_pct))  %>% 
      select(asv_code, file_code, latitude, longitude, label, n_reads_pct) %>% 
      mutate(pct = (n_reads_pct/global$n_reads_tot_normalized)*100) %>% 
      rename(dominant_taxon = asv_code)
    
  }
  
  return(list(present=present, absent = absent))
  
}

# Function to reformat df_map for duplicated data ------------------------------

reformat_df_map_dup <- function(ls_df) {
  
  # this function expect the input from df_map() which is a list of 2 df
  ls_df_dup = lapply(ls_df, function(df) {
    # create duplicate data shifted by 360 degrees
    df1 = df |>
      subset(subset = df$longitude < 0)
    df1$longitude = df1$longitude + 360
    # create duplicate date shifted by -360 degrees
    df2 = df|>
      subset(subset = df$longitude >= 0)
    df2$longitude = df2$longitude - 360
    # combine df1 and df2
    df_merge = rbind(df1, df2)
    return(df_merge)
  })
  return(ls_df_dup)
}


