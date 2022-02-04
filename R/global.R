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
#   * Will close windows after x msec 60 000 -> 1 min 600 000 -> 10 min

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


# function misc -----------------------------------------------------------

send_message <- function(type, ids, ...) {
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    for (id in ids) {
      session$sendCustomMessage(type = type, message = list(id = id, ...))
    }
  }
}

click <- function(ids) {
  send_message("clickElement", ids = ids)
}

# Read credentials ----------------------------------------------------------
# print("Read credentials")

credentials  <- tryCatch(
  {
    qs::qread(system.file("data-qs", 'credentials.qs', package = "metapr2"))
  },
  error=function(cond) {
    message("Cannot use system.file")
    return(NA)
  }
)

## Using the explicit way

if(is.na(credentials)){
  credentials <- qs::qread("inst/data-qs/credentials.qs")
  print("Using full path")
}

# Read global ----------------------------------------------------------

global  <- tryCatch(
  {
    qs::qread(system.file("data-qs", 'global.qs', package = "metapr2"))
  },
  error=function(cond) {
    message("Cannot use system.file")
    return(NA)
  }
)

## Using the explicit way

if(is.na(global)){
  global <- qs::qread("inst/data-qs/global.qs")
  print("Using full path")
}


# Change factors to character ----------------------------------------------------

global$ecosystems = as.character(global$ecosystems)
global$substrates = as.character(global$substrates)
global$fraction_names = as.character(global$fraction_names)
global$depth_levels = as.character(global$depth_levels)

global$datasets <- tibble::tribble(
  ~group, ~filter,
   "marine_global_V4", "(str_detect(dataset_groups, 'global') & str_detect(dataset_groups, 'oceanic|coastal') & gene_region == 'V4')",
   "oceanic", "str_detect(dataset_groups, 'oceanic') ",
   "coastal", "str_detect(dataset_groups, 'coastal') ", 
   "rivers", "str_detect(dataset_groups, 'river') ",
   "lakes", "str_detect(dataset_groups, 'lake') ",  
   "soils", "str_detect(dataset_groups, 'soils') ",
  # * Transitions
   "arctic", "str_detect(dataset_groups, 'arctic')",
   "antarctic", "str_detect(dataset_groups, 'antarctic')",
   "temperate", "str_detect(dataset_groups, 'temperate')",
   "tropical", "str_detect(dataset_groups, 'tropical')",
  "time series", "str_detect(dataset_groups, 'time series')"  
)

# Done --------------------------------------------------------------------


print("init.R done")

