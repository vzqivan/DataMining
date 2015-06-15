#Creamos la función que recibe los paquetes
install = function(pkg){
  #Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

#uso
install("RWeka")
