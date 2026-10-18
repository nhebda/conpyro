## list of functions used for R package building and checking
## note that many of these commands can be executed using RStudio GUI.

# install.packages(devtools)

devtools::document()   ## creates/updates all the documentation files

devtools::build()  # Build the source package

devtools::check()

devtools::test()

devtools::load_all() ## useful to load your package without installing (basically sources all your funcitons, including internal ones)

## Online documentation with pkgdown (function calls to come when appropriate)
# install.packages("pkgdown")
