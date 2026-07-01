# Libraries ---------------------------------------------------------------

#' @import shiny

# # To display text boxes in Markdown
# #' @import markdown 

#' @import dplyr
#' @import stringr
#' @import forcats
#' @import ggplot2
#' @import leaflet
#' @import patchwork

# Add the inst/img as a path called img. Images are then loaded as "img(src='img/metapr2_logo.png', width="80")"
# See: https://stackoverflow.com/questions/61276679/favicon-not-displaying-in-shiny

# The file should be in "inst/img" - See https://r-pkgs.org/inst.html

.onLoad <- function(libname, pkgname) {
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
print("Read credentials")

file_loaded  <- tryCatch(
  {
    credentials <- qs2::qs_read(system.file("data-qs/credentials.qs2", package = "metapr2", mustWork = TRUE))
    TRUE                # Returns true if loaded
  },
  error=function(cond) {
    message("Cannot use system.file")
    return(FALSE)
  }
)

## Using the explicit way

if(!file_loaded){
  credentials <- qs2::qs_read("inst/data-qs/credentials.qs2")
  print("Using full path")
}

# Read global ----------------------------------------------------------
print("Read globals")

file_loaded  <- tryCatch(
  {
    global <- qs2::qs_read(system.file("data-qs/global.qs2", package = "metapr2", mustWork = TRUE))
    TRUE              # Returns true if loaded
  },
  error=function(cond) {
    message("Cannot use system.file")
    return(FALSE)
  }
)

## Using the explicit way

if(!file_loaded){
  global <- qs2::qs_read("inst/data-qs/global.qs2")
  print("Using full path")
}


# Done --------------------------------------------------------------------


print("global.R done")

