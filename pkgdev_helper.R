## list of functions used for R package building and checking
## note that many of these commands can be executed using RStudio GUI.

# install.packages(devtools)

devtools::document()   ## creates/updates all the documentation files

devtools::build()

devtools::test()

## Online documentation with pkgdown (funtion calls to come when apprpriate)
# install.packages("pkgdown")
