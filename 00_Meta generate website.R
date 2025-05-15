################################
# Meta generate website
################################

rmarkdown::render("01_Data preparation.Rmd", output_dir = "docs")
rmarkdown::render("index.Rmd", output_dir = "docs")
