#Creamos la función que recibe los paquetes
install = function(pkg){
  #Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http:/cran.rstudio.com")
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

#Instalamos primero "foreach"
install("foreach")

#Seleccionamos los archivos que queremos instalar
archive = c("rJava", "shiny", "rmarkdown", "foreach", "caret", "e1071", "rpart", "tree", "RWeka", "C50")
foreach(i = archive) %do% install(i)
