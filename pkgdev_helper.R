## list of functions used for R package building and checking
## note that many of these commands can be executed using RStudio GUI.

# install.packages(devtools)

devtools::document()   ## creates/updates all the documentation files

devtools::build(args = "--as-cran")   ## check with CRAN checks

devtools::check()

devtools::test()

devtools::load_all() ## useful to load your package wihtout installing (basically sources all your funcitons, including internal ones)

## Online documentation with pkgdown (funtion calls to come when apprpriate)
# install.packages("pkgdown")
