
# Deploying to Shiny server -----------------------------------------------

# When deploying on ShinyApps this before at the console type at the console the following line
#        options(repos = BiocManager::repositories())
# See : https://aarthiramakrishnan.com/2018/01/09/deploying-shiny-apps-using-bioconductor.html
# To deploy app: rsconnect::deployApp("~/projects/shiny/app1", appName = "myapp", appTitle = "My Application")


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
#   * Will close windows after x msec 60000 -> 1 min 600000 -> 10 min

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
    t = setTimeout(logout, 600000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"

print("init.R done")