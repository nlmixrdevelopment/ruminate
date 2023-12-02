## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, eval=FALSE)
require(ggplot2)
require(rhandsontable)
require(flextable)
require(ruminate)
require(dplyr)
# Determining if ubiquity is installed
if(system.file(package="ubiquity") == ""){
  require(ubiquity)
  ubiquity_found = FALSE
} else {
  require(ubiquity)
  ubiquity_found = TRUE
}
if(system.file(package="gridExtra") == ""){
  gridExtra_found = FALSE
} else {
  require(gridExtra)
  gridExtra_found = TRUE
}

# The presim variable will contain presimulated data when eval is set to true
presim_loaded = FALSE

## ----echo=FALSE, results=FALSE------------------------------------------------
#  presim= list()
#  if(file.exists("NCA_presim.RData")){
#    file.remove("NCA_presim.RData")
#  }

## ----echo=FALSE, results=FALSE, eval=TRUE-------------------------------------
if(file.exists("NCA_presim.RData")){
  load("NCA_presim.RData")
  presim_loaded = TRUE
}

NCA_yaml = yaml::read_yaml(system.file(package="ruminate", "templates","NCA.yaml"))

## ----warning=FALSE, message=FALSE, echo=FALSE, error=FALSE, results="hide", fig.width=8, fig.height=4----
#  polydf     = NULL
#  som_smooth = NULL
#  som_sample = NULL
#  
#  
#  
#  if(ubiquity_found){
#    system_new(system_file="mab_pk", overwrite=TRUE, output_directory=tempdir())
#  
#    cfg = build_system(system_file         =file.path(tempdir(), "system.txt"),
#                       output_directory    =file.path(tempdir(), "output"),
#                       temporary_directory =file.path(tempdir(), "transient"))
#  
#    parameters = system_fetch_parameters(cfg)
#    cfg = system_zero_inputs(cfg)
#    cfg = system_set_bolus(cfg, state   ="At",
#                                times   = c(  0.0),  #  day
#                                values  = c(400.0))  #  mg
#  
#    cfg=system_set_option(cfg, group  = "simulation",
#                               option = "output_times",
#                               linspace(0,30,100))
#  
#    som_smooth = run_simulation_ubiquity(parameters, cfg)
#    som_smooth$simout$time_C_ng_ml = som_smooth$simout$C_ng_ml*som_smooth$simout$ts.days
#  
#    cfg=system_set_option(cfg, group  = "simulation",
#                                option = "include_important_output_times",
#                                value  = "no")
#  
#    cfg=system_set_option(cfg, group  = "simulation",
#                               option = "output_times",
#                               c(0,.25, .5, 1, 2,7,14,21,28))
#    som_sample = run_simulation_ubiquity(parameters, cfg)
#    som_sample$simout$time_C_ng_ml = som_sample$simout$C_ng_ml*som_sample$simout$ts.days
#  
#  
#    for(tidx in 1:(nrow(som_sample$simout)-1)){
#      xv = c(som_sample$simout$ts.days[tidx], som_sample$simout$ts.days[tidx+1],  som_sample$simout$ts.days[tidx+1], som_sample$simout$ts.days[tidx] )
#      yvC = c(som_sample$simout$C_ng_ml[tidx], som_sample$simout$C_ng_ml[tidx+1], 0, 0)
#      yvTC = c(som_sample$simout$time_C_ng_ml[tidx], som_sample$simout$time_C_ng_ml[tidx+1], 0, 0)
#      tmpdf = data.frame(xv = xv, yvC=yvC, yvTC=yvTC, sp=tidx)
#      if(is.null(polydf)){
#        polydf = tmpdf
#      } else {
#        polydf = rbind(tmpdf, polydf)
#      }
#    }
#  }

## ----results="hide", warning=FALSE, echo=FALSE--------------------------------
#  # When eval is set to TRUE we save the presimulated results
#    presim$plots$som_smooth = som_smooth
#    presim$plots$som_sample = som_sample
#    presim$plots$polydf     = polydf

## ----results="hide", warning=FALSE, echo=FALSE, eval=TRUE---------------------
if(presim_loaded){
  som_smooth = presim$plots$som_smooth 
  som_sample = presim$plots$som_sample 
  polydf     = presim$plots$polydf
}

## ----warning=FALSE, message=FALSE, echo=FALSE, error=FALSE, eval=TRUE, results="hide", fig.width=8, fig.height=4----
if(ubiquity_found){
  p_C = ggplot()
  p_C = p_C +  geom_line(data=som_smooth$simout, aes(x=ts.days, y=C_ng_ml))
  p_C = p_C + geom_point(data=som_sample$simout, aes(x=ts.days, y=C_ng_ml), color="blue")
  p_C = p_C + xlab("Time") + ylab("Concentration") + ggtitle("AUC")
  p_C = p_C +  geom_polygon(data=polydf, aes(x=xv, y=yvC, group=sp), color="blue", linetype='dashed', fill="lightblue")
  p_C = p_C + theme(plot.title = element_text(hjust = 0.5))
  p_C = prepare_figure(fo=p_C, purpose="shiny")
  
  
  p_TC = ggplot()
  p_TC = p_TC +  geom_line(data=som_smooth$simout, aes(x=ts.days, y=time_C_ng_ml))
  p_TC = p_TC + geom_point(data=som_sample$simout, aes(x=ts.days, y=time_C_ng_ml), color="blue")
  p_TC = p_TC + xlab("Time") + ylab("Time x Concentration") + ggtitle("AUMC")
  p_TC = p_TC +  geom_polygon(data=polydf, aes(x=xv, y=yvTC, group=sp), color="blue", linetype='dashed', fill="lightblue")
  p_TC = p_TC + theme(plot.title = element_text(hjust = 0.5))
  p_TC = prepare_figure(fo=p_TC, purpose="shiny")
  
  
  p_AUC = p_C
  p_AUMC = p_TC
} else {
  p_AUC = ggplot2::ggplot()
  p_AUMC = ggplot2::ggplot()
}

## ----warning=FALSE, message=FALSE, echo=FALSE, error=FALSE, eval=TRUE, fig.width=8, fig.height=3.5----
if(gridExtra_found & ubiquity_found){
  gridExtra::grid.arrange(p_AUC, p_AUMC, ncol=2)
}

## ----warning=FALSE, message=FALSE, echo=FALSE---------------------------------
#  save(presim, file="NCA_presim.RData")

## ----warning=FALSE, message=FALSE, echo=FALSE, eval=TRUE----------------------
NCA_meta = NCA_fetch_PKNCA_meta()
NCA_meta = NCA_meta[["parameters"]] |>
  dplyr::select(-data_type) |>
  dplyr::rename("PKNCA Parameter"    = parameter)    |>
  dplyr::rename("App Parameter Name" = pretty_name)  |>
  dplyr::rename("Type of Units"      = unit_type)    |>
  dplyr::rename("Description"        = desc)      

DT::datatable(NCA_meta)

## ----echo=FALSE, message=FALSE, warning=FALSE, eval=TRUE,  style="max-height: 100px;", comment=""----
#yaml= file.path(system.file(package="ruminate"), "templates", "NCA.yaml")
cat(readLines(file.path(system.file(package="ruminate"), "templates", "NCA.yaml")), sep="\n")

