## ----setup, include=FALSE-----------------------------------------------------
# knitr::opts_chunk$set(echo = TRUE, message=FALSE, eval=FALSE)
# require(ggplot2)
# require(rhandsontable)
# require(flextable)
# require(ruminate)
# # Determining if ubiquity is installed
# if(system.file(package="ubiquity") == ""){
#   ubiquity_found = FALSE
# } else {
#   require(ubiquity)
#   ubiquity_found = TRUE
# }
# if(system.file(package="gridExtra") == ""){
#   gridExtra_found = FALSE
# } else {
#   require(gridExtra)
#   gridExtra_found = TRUE
# }
# 
# # The presim variable will contain presimulated data when eval is set to true
# presim_loaded = FALSE

## ----echo=FALSE, results=FALSE------------------------------------------------
# presim= list()
# if(file.exists("NCA_presim.RData")){
#   file.remove("NCA_presim.RData")
# }

## ----echo=FALSE, results=FALSE, eval=TRUE-------------------------------------
# if(file.exists("NCA_presim.RData")){
#   load("NCA_presim.RData")
#   presim_loaded = TRUE
# }
# 
# NCA_yaml = yaml::read_yaml(system.file(package="ruminate", "templates","NCA.yaml"))

## ----eval=FALSE---------------------------------------------------------------
#  formods.yaml  = system.file(package="formods",  "templates",  "formods.yaml")
#  ASM.yaml      = system.file(package="formods",  "templates",  "ASM.yaml")
#  UD.yaml       = system.file(package="formods",  "templates",  "UD.yaml")
#  DW.yaml       = system.file(package="formods",  "templates",  "DW.yaml")
#  FG.yaml       = system.file(package="formods",  "templates",  "FG.yaml")
#  NCA.yaml      = system.file(package="ruminate", "templates",  "NCA.yaml")
#  MB.yaml       = system.file(package="ruminate", "templates",  "MB.yaml")
#  CTS.yaml      = system.file(package="ruminate", "templates",  "CTS.yaml")

## ----eval=FALSE---------------------------------------------------------------
#  file.copy(from = system.file(package="ruminate", "templates",  "NCA.yaml"),
#            to   = "myNCA.yaml")

## ----eval=FALSE---------------------------------------------------------------
#  NCA.yaml      = "myNCA.yaml"

## ----eval=FALSE---------------------------------------------------------------
#  file.copy(from = system.file(package="formods", "templates",  "formods.yaml"),
#            to   = "myformods.yaml")

## ----eval=FALSE---------------------------------------------------------------
#  FM:
#    include:
#      files:
#      - file:
#          source: 'file.path(".", "templates", "report.docx")'
#          dest:   'file.path("config","report.docx")'
#      - file:
#          source: 'file.path(".", "templates", "report.pptx")'
#          dest:   'file.path("config","report.pptx")'
#      - file:
#          source: 'file.path(".", "templates", "report.yaml")'
#          dest:   'file.path("config","report.yaml")'

