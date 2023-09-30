# Corrida general del workflow

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})


# corrida de cada paso del workflow

# primeros pasos, relativamente rapidos
source("~/labo2023ba/src/workflow-inicial/z611_CA_reparar_dataset_ML.r")
source("~/labo2023ba/src/workflow-inicial/z621_DR_corregir_drifting_ML.r")
source("~/labo2023ba/src/workflow-inicial/z631_FE_historia_ML.r")
source("~/labo2023ba/src/workflow-inicial/z641_TS_training_strategy_ML.r")

# ultimos pasos, muy lentos
source("~/labo2023ba/src/workflow-inicial/z651_HT_lightgbm_ML.r")
source("~/labo2023ba/src/workflow-inicial/z661_ZZ_final_ML_10semillas.r")
