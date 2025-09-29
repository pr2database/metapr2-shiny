# Notes
# - cannot use rmarkdown::html_vignette for the moment (maybe with R version 4.0)
#
# - use devtools::build_vignettes() to build the vignettes
#
# - need to build source package to have the vignettes visible locally


library(pkgdown)

# Build the whole site - Best option so everything is updated
pkgdown::build_site()

# Preview site
pkgdown::preview_site()

#  when the README.md is modified (fails to work, use the build_site )
# pkgdown::build_home()

# Build articles for  the vignettes of the pr2database package web site
pkgdown::build_articles()

# Build news when the NEWS.md is changed
pkgdown::build_news()

# Build reference when the functions are modified (need to Build documentation before...)
pkgdown::build_reference()

# One also just use

pkgdown::build_article("vignette-shiny-presentation")
pkgdown::build_article("pr2_01_stats")

pkgdown::build_articles_index()
