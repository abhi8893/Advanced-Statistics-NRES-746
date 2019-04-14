############################################################
#####################** Description **######################
# Miscellaneous helper functions
############################################################

my.source <- function(env){
  f <- rstudioapi::getSourceEditorContext()$path
  rm(list = ls(envir = env), envir = env)
  source(f, local=env)
}
