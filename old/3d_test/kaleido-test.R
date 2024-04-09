install.packages('reticulate')
reticulate::install_miniconda()
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')

# Save a single image
p <- plot_ly(x = 1:10)
tmp <- tempfile(fileext = ".png")
save_image(p, tmp)
file.show(tmp)

# Efficiently save multiple images
scope <- kaleido()
for (i in 1:5) {
  scope$transform(p, tmp)
}
# Remove and garbage collect to remove 
# R/Python objects and shutdown subprocesses
rm(scope); gc()


## REFERENCIA para usar en rmarkdown
#https://stackoverflow.com/questions/75679642/r-how-to-save-a-plotly-figure




reticulate::use_miniconda('r-reticulate')
save_image(fig, "test.svg")
