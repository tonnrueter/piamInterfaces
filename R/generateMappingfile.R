library(data.table)

## generate the mapping file from the template that can contain metadata

TEMPLATE <- "navigate/mapping_template_NAVIGATE.csv"
AR6_VAR <- "Variable_NAVIGATE"

# to use more than one input template, proceed as in the following example
# TEMPLATE <- c("navigate/mapping_template_NAVIGATE.csv", "navigate/mapping_template_SHAPE.csv")
# AR6_VAR <- c("Variable_NAVIGATE", "Variable_SHAPE")

MAPPING_FILE <- "mapping_r30m44_AR6DB.csv"
REMIND_VAR <- "r30m44"
REMIND_UNIT <- "r30m44_unit"
AR6_UNIT <- "Unit"
FACTOR_COL <- "r30m44_factor"
WEIGHT_COL <- NULL
SPATIAL_COL <- "r30m44_spatial"


##########

generateMapping <- function(template, file_name = NULL,
                            remind_var, remind_unit,
                            ar6_var, ar6_unit,
                            factor_col, weight_col,
                            spatial_col) {
  dt <- fread(template, sep = ";", )

  ## remove TODOs and empty mappings
  dt[get(remind_var) == "TODO", (remind_var) := ""]
  dt <- dt[get(remind_var) != ""]
  ## factor defaults to 1
  dt[is.na(get(factor_col)), (factor_col) := 1]
  
  ## spatial defaults to "reg+glo"
  
  # remove empty spatial column, as it is interpreted as logical column
  if (spatial_col %in% colnames(dt) && typeof(dt[, get(spatial_col)]) == "logical") {
    dt[,(spatial_col) := NULL]
  }
  
  if (spatial_col %in% colnames(dt)) {
    dt[get(spatial_col) == "", (spatial_col) := "reg+glo"]
  } else {
    dt[, (spatial_col) := "reg+glo"]
  }

  no_unit <- dt[get(remind_unit) == ""]
  if (nrow(no_unit)) {
    warning(sprintf("No unit found for variables %s", paste(no_unit[[remind_var]], collapse = ", ")))
  }

  dt <- dt[, c(remind_var, ar6_var, "factor", "weight", "spatial") :=
    list(
      sprintf("%s (%s)", get(remind_var), get(remind_unit)),
      sprintf("%s (%s)", get(ar6_var), get(ar6_unit)),
      get(factor_col),
      ifelse(is.null(weight_col), "NULL", get(weight_col)),
      get(spatial_col)
    )][
    , c(remind_var, ar6_var, "factor", "weight", "spatial"),
    with = FALSE
  ]

  if (!is.null(file_name)) {
    ## store mapping
    fwrite(dt, file = file_name, sep = ";")
  } else {
    return(dt)
  }
}

MODEL <- "REMIND-MAgPIE 3.0-4.4"
COMMENT_FILE <- "ar6-comments.csv"

storeComments <- function(template, remind_var, ar6_var, model, file_name = NULL) {
  dt <- fread(template)
  dt[get(remind_var) == "TODO", (remind_var) := ""]
  dt <- dt[get(remind_var) != ""]

  comments <- dt[Comment != ""]
  comments <- comments[, .(
    "Model" = model,
    "Scenario" = "All",
    "Region" = "All",
    "Variable" = get(ar6_var),
    "Year" = "All",
    Comment
  )]

  if (!is.null(file_name)) {
    fwrite(comments, file_name)
  } else {
    return(comments)
  }
}


if (length(TEMPLATE) == 1) {
  generateMapping(
    template = TEMPLATE,
    file_name = MAPPING_FILE,
    remind_var = REMIND_VAR,
    remind_unit = REMIND_UNIT,
    ar6_var = AR6_VAR,
    ar6_unit = AR6_UNIT,
    factor_col = FACTOR_COL,
    weight_col = WEIGHT_COL,
    spatial_col = SPATIAL_COL
  )

  storeComments(
    template = TEMPLATE,
    remind_var = REMIND_VAR,
    ar6_var = AR6_VAR,
    model = MODEL,
    file_name = COMMENT_FILE
  )
} else {
  tmp <- NULL
  tmp.comments <- NULL
  for (i in 1:length(TEMPLATE)) {
    m <- generateMapping(
      template = TEMPLATE[i],
      remind_var = REMIND_VAR,
      remind_unit = REMIND_UNIT,
      ar6_var = AR6_VAR[i],
      ar6_unit = AR6_UNIT,
      factor_col = FACTOR_COL,
      weight_col = WEIGHT_COL,
      spatial_col = SPATIAL_COL
    )
    setnames(m, AR6_VAR[i], "Variable")
    tmp <- rbind(tmp, m)
    
    c <- storeComments(
      template = TEMPLATE[i],
      remind_var = REMIND_VAR,
      model = MODEL,
      ar6_var = AR6_VAR[i]
    )
    tmp.comments <- rbind(tmp.comments, c)
  }
  
  fwrite(tmp, file = MAPPING_FILE, sep = ";")
  fwrite(tmp.comments, file = COMMENT_FILE, sep = ";")
}
