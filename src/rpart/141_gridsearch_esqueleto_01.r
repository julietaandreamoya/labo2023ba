
# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros
install.packages("doParallel")  # Si no tienes el paquete instalado

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection
require("doParallel")
require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(104311, 103903, 102871, 104287, 103319)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_ternaria", seed = semilla)

  # genero el modelo
  # quiero predecir clase_ternaria a partir del resto
  modelo <- rpart("clase_ternaria ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
      ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------



ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  ksemillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    PARAM$semillas, # paso el vector de semillas
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE#,
    #mc.cores = 1
  ) # se puede subir a 5 si posee Linux o Mac OS

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")  # Establezco el Working Directory
# cargo los datos

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020_01/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020_01/gridsearch.txt"

# Escribo los titulos al archivo donde van a quedar los resultados
# atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE,
#  y lo que estaba antes se pierde
# la forma que no suceda lo anterior es con append=TRUE
cat(
  file = archivo_salida,
  sep = "",
  "max_depth", "\t",
  "min_split", "\t",
  "cp_test", "\t",
  "minbucket", "\t",
  "ganancia_promedio", "\n"
)


# itero por los loops anidados para cada hiperparametro
##-----PRUEBA DE PARALELIZAR


num_cores <- detectCores()
cl <- makeCluster(num_cores)

registerDoParallel(cl)

# Ejemplo de uso con paquete doParallel y foreach
for (vmax_depth in c( 7, 8, 9, 10)) {
  for (vmin_split in c(500, 550, 600, 650, 700)) {
    for (minbucket in c(12, 13, 14)){
      for (cp_test in c(-1,-0.5,0,0.5,1)) {
        if(vmin_split / minbucket > 2) {
          param_basicos <- list(
            "cp" = cp_test, # complejidad minima
            "minsplit" = vmin_split,
            "minbucket" = minbucket, # minima cantidad de registros en una hoja
            "maxdepth" = vmax_depth
          ) # profundidad máxima del arbol
         
          # Un solo llamado, con la semilla 17
          ganancia_promedio <- ArbolesMontecarlo(semillas, param_basicos)
         
          # escribo los resultados al archivo de salida
          cat(
            file = archivo_salida,
            append = TRUE,
            sep = "",
            vmax_depth, "\t",
            vmin_split, "\t",
            cp_test, "\t",
            minbucket, "\t",
            ganancia_promedio, "\n"
          )
          # notar como se agrega
         
          # vminsplit  minima cantidad de registros en un nodo para hacer el split
        }
      }
    }
  }
}

stopCluster(cl)



##



# Al final del script, después de que todas las operaciones hayan terminado
print("El script ha terminado de ejecutarse con éxito.")

