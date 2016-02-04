#' arbin_process_Rmap
#'
#' This function takes a data set for a "resistance mapping" experimentimported with either arbin_import
#' or arbin_import_raw and returns a processed data containing resistance information as well as
#' voltage, capacity, cycle number etc.
#' @param data.in The dataset to process. Can be either in list (arbin_import) or data.frame
#' (arbin_import_raw) form.
#' @keywords resistance map process
#' @export
#' @examples
#' processed <- arbin_process_Rmap(rawdata)
#'
arbin_process_Rmap <- function(data.in) {

  require(dplyr)
  require(parallel)

  # Check for data type, so it can handle data imported with either arbin_import function.
  # If "list", then it just takes the "raw" part.
  if (class(data.in) == "list") {
    raw <- data.in[[1]]
  } else if (class(data.in) == "data.frame") {
    raw <- data.in
  } else {
    stop("Data doesn't seem to be in a recognisable format! Was it imported with
         the arbin_import functions?")
  }

  # Message number of rows and estimated time.
  message("Now processing ", deparse(substitute(data.in)), ". There are ", nrow(raw),
          " rows, so this is estimated to take around ", round(3.14E-5 * nrow(raw), digits = 1),
          " seconds to complete.")

  # Assign the sign of the current as the "state".
  message("importing and filtering data... ", appendLF = FALSE)
  raw$state <- signCurrent(raw$I)

  # filter data, only complete cycles. Remove step 1 if it's a rest step.
  raw <- filter(raw, cyc.n %in% c(1:max(raw$cyc.n)-1))

  if (1 %in% unique(raw$step.n) && unique(raw$state[raw$step.n == 1][1]) == "R") {
    raw <- filter(raw, step.n != 1)
  }
  message("done, found ", max(raw$cyc.n) - 1, " cycles.")

  # Assign identify
  message("identifying current interruptions... ", appendLF = FALSE)
  raw$rests <- assignRest(raw$state)
  raw$rests[1] <- 1 ### bodge, because the first element is zero for some reason.
  message("done, found ", max(raw$rests), " interruptions")

  # Adjust capacity.
  if(raw$state[1] == "D") {
    raw$adjQ <- raw$Q.d - raw$Q.c
  } else if (raw$state[1] == "C") {
    raw$adjQ <- raw$Q.c - raw$Q.d
  } else {
    stop("problem! The data doesn't seem to start with a current pulse?")
  }

  message("Now assembling processed data, please wait a moment... ", appendLF = FALSE)
  proc <- mclapply(split(raw, raw$cyc.n), function(cycle) {

    if(tail(cycle$state, 1) != "R") {
      cycle <- filter(cycle, rests < max(cycle$rests))
    }

    out <- data.frame(
      rest = unique(cycle$rests),
      state = sapply(unique(cycle$rests), function(i) {
        last(cycle$state[cycle$rests == i & cycle$state != "R"])
      }),
      cyc.n = sapply(unique(cycle$rests), function(i) {
        last(cycle$cyc.n[cycle$rests == i])
      }),
      Q = sapply(unique(cycle$rests), function(i) {
        last(cycle$adjQ[cycle$rests == i])
      }),
      E = sapply(unique(cycle$rests), function(i) {
        last(cycle$E[cycle$rests == i & cycle$state != "R"])
      }),
      R = sapply(unique(cycle$rests), function(i) {
        (last(cycle$E[cycle$rests == i & cycle$state == "R"]) -
           last(cycle$E[cycle$rests == i & cycle$state != "R"])) /
          (-1 * last(cycle$I[cycle$rests == i & cycle$state != "R"]))
      })
    )
    return(out)
  }, mc.cores = detectCores())

  proc <- do.call(rbind, proc)
  message("done.")

  return(proc)

}
