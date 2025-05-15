################################
# Meta generate website
################################

rmarkdown::render("01_Data preparation.Rmd", output_dir = "docs")
rmarkdown::render("02_Reproducing the Explanandum and considering alternative satisfaction measures.Rmd", output_dir = "docs")
rmarkdown::render("03_Ordinal analysis.Rmd", output_dir = "docs")
rmarkdown::render("04_Add demographic variables.Rmd", output_dir = "docs")
rmarkdown::render("05_Mode effects and school fixed effects.Rmd", output_dir = "docs")
rmarkdown::render("06_Robustness ordinal.Rmd", output_dir = "docs")
rmarkdown::render("07_Check explanation for other outcomes.Rmd", output_dir = "docs")
rmarkdown::render("08_Misc.Rmd", output_dir = "docs")


rmarkdown::render("index.Rmd", output_dir = "docs")
