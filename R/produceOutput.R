require(data.table)
require(iamc)
require(rmndt)

## replace the model name with MODEL and produce output for all MIF files in directory
## based on the MAPPING
## the output file gets OUTPUT_PREFIX prepended to the file name.
## Warnings are appended to the given logfile.

# uncomment next blocks for REMIND transport submission
# MODEL <- "REMIND-Transport 2.1"
# MAPPING <- "~/git/project_interfaces/ar6/mapping_r21m42_AR6DB.csv"
# OUTPUT_PREFIX <- "AR6_"
#
# MIF_DIRECTORY <- "~/remind/testruns/ar6/runs_3006/"
# OUTPUT_DIRECTORY <- "~/remind/testruns/ar6/runs_3006/AR6_output/"
# LOGFILE <- file.path(OUTPUT_DIRECTORY, "missing.log")
# REMOVE_FROM_SCEN <- NULL
# ADD_TO_SCEN <- "Transport_"
#
# GENERATE_SINGLE_OUTPUT = TRUE
# OUTPUT_FILENAME = "AR6_data.mif"

# uncomment next blocks for SDP submission
MODEL <- "REMIND-MAgPIE 2.1-4.2"
MAPPING <- "mapping_r21m42_AR6DB.csv"
OUTPUT_PREFIX <- "AR6_"

MIF_DIRECTORY <- "SDP_mifs"
OUTPUT_DIRECTORY <- "AR6_output/"
LOGFILE <- file.path(OUTPUT_DIRECTORY, "missing.log")
REMOVE_FROM_SCEN <- "C_"
ADD_TO_SCEN <- "SusDev_"

GENERATE_SINGLE_OUTPUT = TRUE
OUTPUT_FILENAME = "SusDev_alldata.mif"

if(!file.exists(OUTPUT_DIRECTORY)){
  dir.create(OUTPUT_DIRECTORY)
}

set_model_and_scenario <- function(mif, model, scen_remove = NULL, scen_add = NULL){
  dt <- readMIF(mif)
  dt[, Model := model]
  if (!is.null(scen_remove)) dt[, Scenario := gsub(scen_remove,"",Scenario)]
  if (!is.null(scen_add)) {
    if(grepl(scen_add, unique(dt$Scenario), fixed=TRUE)){
      print(sprintf("Prefix %s already found in scenario name in %s.", scen_add, mif))
    }else{
      dt[, Scenario := paste0(scen_add,Scenario)]
    }
  }
  writeMIF(dt, mif)
}


flist <- list.files(MIF_DIRECTORY, "*.mif")
for(fl in flist){
  fl_path <- file.path(MIF_DIRECTORY, fl)
  set_model_and_scenario(
    fl_path, MODEL, REMOVE_FROM_SCEN, ADD_TO_SCEN)
  if(GENERATE_SINGLE_OUTPUT){
    iamc::write.reportProject(
            fl_path, MAPPING,
            file.path(OUTPUT_DIRECTORY, OUTPUT_FILENAME),
            append=TRUE,
            missing_log=LOGFILE)
  }else{
    iamc::write.reportProject(
            fl_path, MAPPING,
            file.path(OUTPUT_DIRECTORY, paste0(OUTPUT_PREFIX, fl)),
            missing_log=LOGFILE)
  }
}
