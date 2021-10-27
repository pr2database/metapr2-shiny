# Libraries ---------------------------------------------------------------

#' @import shiny

# To display text boxes in Markdown
#' @import markdown 

#' @import dplyr
#' @import stringr
#' @import forcats
#' @import ggplot2
#' @import leaflet
#' @import patchwork

# Add the inst/img as a path called img. Images are then loaded as "img(src='img/metapr2_logo.png', width="80")"
# See: https://stackoverflow.com/questions/61276679/favicon-not-displaying-in-shiny

# The file should be in "inst/img" - See https://r-pkgs.org/inst.html

.onAttach <- function(libname, pkgname) {
      shiny::addResourcePath('img',system.file('img', package = 'metapr2'))
}

# Javascript function for timer -----------------------------------------------------

#  See: https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window
#   * Will close windows after x msec 60000 -> 1 min 600 000 -> 10 min

inactivity <- "function idleTimer() {
  var t = setTimeout(logout, 1800000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
    window.close();  //close the window
  }

  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, 1800000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"


# Data set type -----------------------------------------------------------

# set_type = "public"
# set_type = "basic"
set_type = "all"

# Read the data -----------------------------------------------------------


sub_dir = stringr::str_c("sets_", set_type)

## Using the normal way
asv_set  <- tryCatch(
  {
    qs::qread(system.file("data-qs", sub_dir, 'asv_set.qs', package = "metapr2"))
  },
  error=function(cond) {
    message("Cannot use system.file")
    return(NA)
  }
)

global  <- tryCatch(
  {
    qs::qread(system.file("data-qs", sub_dir, 'global.qs', package = "metapr2"))
  },
  error=function(cond) {
    message("Cannot use system.file")
    return(NA)
  }
)

## Using the explicit way

if(is.na(asv_set)){
  asv_set <- qs::qread(stringr::str_c("inst/data-qs/", sub_dir, "/asv_set.qs"))
  global <- qs::qread(stringr::str_c("inst/data-qs/", sub_dir, "/global.qs"))
  print("Using full path")
}

# Add ecosystem -----------------------------------------------------------


asv_set$samples <-   dplyr::left_join(asv_set$samples, 
                                      dplyr::select(asv_set$datasets, dataset_id, ecosystem))


# Reordering the variables ------------------------------------------------


depth_level_ordered <- c("under ice", "surface", "euphotic",
                      "bathypelagic", "mesopelagic", "land",
                      "composite" ,"bottom" )
fraction_name_ordered <- c("pico", "pico-nano", "nano",
                        "nano-micro", "micro",
                        "meso" ,"total" )
substrate_ordered <- c("water", "net", "ice", "sediment trap material", "sediment trap blank",
                      "epibiota", "sediment", "soil" )

ecosystems_ordered <- c( "oceanic", "coastal","estuarine","freshwater lakes","freshwater rivers","terrestrial")

asv_set$samples <- dplyr::mutate(asv_set$samples,
                                substrate = stringr::str_replace(substrate, "first year ice", "ice"),
                                depth_level = forcats::fct_relevel(depth_level, levels = depth_level_ordered),
                                fraction_name = forcats::fct_relevel(fraction_name, levels = fraction_name_ordered),
                                substrate = forcats::fct_relevel(substrate, levels = substrate_ordered),
                                ecosystem = forcats::fct_relevel(ecosystem, levels = ecosystems_ordered)
                                ) 


# Reorder for the check boxes ---------------------------------------------


update_order <- function(variable) {
  values <- dplyr::arrange(asv_set$samples, .data[[variable]])
  values <- dplyr::pull(values, .data[[variable]])
  unique(values)
}

global$depth_levels <- update_order("depth_level")
global$fraction_names <- update_order("fraction_name")
global$substrates <- update_order("substrate")
global$ecosystems <- update_order("ecosystem")


# Authentification (move to data_initialize later) ------------------------

global$credentials <- data.frame(
  user = c("basic", "public", "private"), # mandatory
  admin = c(FALSE, FALSE, FALSE),
  password = c(scrypt::hashPassword("12345"), scrypt::hashPassword("12345"), scrypt::hashPassword("meta2021")),
  is_hashed_password = TRUE,
  stringsAsFactors = FALSE
)


# Done --------------------------------------------------------------------


print("init.R done")

