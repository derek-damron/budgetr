library(rmarkdown)
rmarkdown::render( "../budgetr/vignettes/introduction.Rmd"
                 , output_file = "index.html"
                 , output_dir = "."
                 )
