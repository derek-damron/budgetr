library(rmarkdown)
rmarkdown::render( "../budget/vignettes/introduction.Rmd"
                 , output_file = "index.html"
                 , output_dir = "."
                 )
