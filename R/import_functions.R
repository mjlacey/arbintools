### Importing functions

#' arbin_import
#'
#' This function takes an exported data file in Microsoft Excel format,
#' discards or includes certain data depending on the options chosen in the function,
#' and returns a list of two data frames: the complete raw data and an aggregated
#' statistics file. The statistics are aggregated from the raw data alone; the statistics
#' sheet as outputted to the Excel file is not read.
#' @param file The filename, which must end in .xls or .xlsx.
#' @param step.time Defaults to TRUE. Includes the step time variable from the data file if TRUE.
#' @param energy Defaults to TRUE. Includes the (dis)charge energy variables from the data file if TRUE.
#' @param cycles Defaults to 100. Determines the maximum number of cycles to be considered when
#' aggregating the statistics dataset.
#' @param mass Defaults to NULL. If an active material mass is specified - in MILLIGRAMS - the
#' capacities in the raw and statistics data frames will be converted to mAh/g.
#' @param meanE Defaults to FALSE. Will calculate a statistic for average charge and discharge voltage
#' if set to TRUE.
#' @keywords
#' @export
#' @examples
#' mydataset <- arbin_import("dataset.xlsx")
#' mydataset <- arbin_import("dataset.xlsx", step.time = FALSE, cycles = 200, mass = 2.55)
#' l=lapply(1:length(cellfile),function(x) arbinimport(cellfile[x],cycles=100,mass=mass[x]))

arbin_import <- function(file, step.time = TRUE, energy = TRUE, cycles = 100, mass = NULL, meanE = FALSE) {

  require(readxl)

  # All the "Channel*" sheets are read in. This function needs the readxl package.
  l <- lapply(grep("Channel_\\d", excel_sheets(file), value = TRUE),
              read_excel, path = file)

  # The list from the previous step is unlisted into a single data frame
  l <- do.call(rbind, l)

  # A new data frame for raw data is created using selected parts of the data.
  x <- data.frame(t = l$`Test_Time(s)`, # time (s)
                  step.n = l$Step_Index, # step number
                  cyc.n = l$Cycle_Index, # cycle number
                  I = l$`Current(A)`, # current (A)
                  E = l$`Voltage(V)`, # voltage (E)
                  Q.c = l$`Charge_Capacity(Ah)`, # charge capacity (Ah)
                  Q.d = l$`Discharge_Capacity(Ah)` # discharge capacity (Ah)
  )

  # Step time is included if specified.
  if(step.time == TRUE) {
    x$step.t <- l$`Step_Time(s)` # step time (s)
  }

  # (Dis)charge energy is included if specified.
  if(energy == TRUE) {
    x$En.d <- l$`Discharge_Energy(Wh)` # discharge energy (Wh)
    x$En.c <- l$`Charge_Energy(Wh)` # charge energy (Wh)
  }

  # Capacities are converted to mAh/g if active mass is specified.
  if(!is.null(mass) == TRUE) {
    x$Q.c <- x$Q.c * (1E6/mass)
    x$Q.d <- x$Q.d * (1E6/mass)
  }

  # Energies, if present, are converted to Wh/kg if active mass is specified.
  if(!is.null(mass) == TRUE & energy == TRUE) {
    x$En.d <- x$En.d * (1E6/mass)
    x$En.c <- x$En.c * (1E6/mass)
  }

  # Number of cycles to be included defaults to 100. In any case, the data is checked
  # and incomplete last cycles are discarded.
  cycles <- ifelse(max(x$cyc.n >= cycles), cycles, max(x$cyc.n) - 1)

  # Data frame of aggregated statistics is constructed.
  stats <- data.frame(
    cyc.n = c(1:cycles),
    Q.c = sapply(c(1:cycles), function(i) tail(x$Q.c[x$cyc.n == i], 1)),
    Q.d = sapply(c(1:cycles), function(i) tail(x$Q.d[x$cyc.n == i], 1)),
    CE = sapply(c(1:cycles), function(i) tail(x$Q.d[x$cyc.n == i], 1) / tail(x$Q.c[x$cyc.n == i], 1))
  )

  # Energy is included if specified.
  if(energy == TRUE) {
    stats$EE <- sapply(c(1:cycles), function(i) tail(x$En.d[x$cyc.n == i], 1) / tail(x$En.c[x$cyc.n == i], 1))
  }

  # Mean voltages are included if specified.
  if(meanE == TRUE) {
    stats$meanE.d <- sapply(c(1:cycles), function(i) mean(x$E[x$cyc.n == i & x$I < 0], 1))
    stats$meanE.c <- sapply(c(1:cycles), function(i) mean(x$E[x$cyc.n == i & x$I > 0], 1))
  }

  # Raw and statistics data frames are returned as a list.
  out <- list(raw = x, stats = stats)
  return(out)
}


#' arbin_import_raw
#'
#' This function is a simplified version of arbin_import which does not output a separate
#' statistics data frame. Consequently the output is a data frame rather than a list.
#' @param file The filename, which must end in .xls or .xlsx.
#' @param step.time Defaults to TRUE. Includes the step time variable from the data file if TRUE.
#' @param energy Defaults to TRUE. Includes the (dis)charge energy variables from the data file if TRUE.
#' @param mass Defaults to NULL. If an active material mass is specified - in MILLIGRAMS - the
#' capacities in the raw and statistics data frames will be converted to mAh/g.
#' @keywords
#' @export
#' @examples
#' mydataset <- arbin_import("dataset.xlsx")
#' mydataset <- arbin_import("dataset.xlsx", step.time = FALSE)

arbin_import_raw <- function(file, step.time = TRUE, energy = TRUE, mass = NULL) {

  require(readxl, quietly = TRUE)

  l <- lapply(grep("Channel_\\d", excel_sheets(file), value = TRUE),
              read_excel, path = file)

  l <- do.call(rbind, l)

  x <- data.frame(t = l$`Test_Time(s)`, # time (s)
                  step.n = l$Step_Index, # step number
                  cyc.n = l$Cycle_Index, # cycle number
                  I = l$`Current(A)`, # current (A)
                  E = l$`Voltage(V)`, # voltage (E)
                  Q.c = l$`Charge_Capacity(Ah)`, # charge capacity (Ah)
                  Q.d = l$`Discharge_Capacity(Ah)` # discharge capacity (Ah)
  )

  if(step.time == TRUE) {
    x$step.t <- l$`Step_Time(s)` # step time (s)
  }

  if(energy == TRUE) {
    x$En.d <- l$`Discharge_Energy(Wh)` # discharge energy (Wh)
    x$En.c <- l$`Charge_Energy(Wh)` # charge energy (Wh)
  }

  # Capacities are converted to mAh/g if active mass is specified.
  if(!is.null(mass) == TRUE) {
    x$Q.c <- x$Q.c * (1E6/mass)
    x$Q.d <- x$Q.d * (1E6/mass)
  }

  # Energies, if present, are converted to Wh/kg if active mass is specified.
  if(!is.null(mass) == TRUE & energy == TRUE) {
    x$En.d <- x$En.d * (1E6/mass)
    x$En.c <- x$En.c * (1E6/mass)
  }

  return(x)
}
