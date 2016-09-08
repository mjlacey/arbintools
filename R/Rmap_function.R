### Importing functions

#' arbin_Rmap
#'
#' This function is for processing "galvanostatic cycling with current interruption" (GCCI, aka "resistance
#' mapping") data which has been imported with either of the arbin_import functions. This function is designed to work
#' with only very specific Arbin schedules. This function takes only an imported data file (as a data.frame or list of raw and
#' statistics) and returns a processed data file with calculated internal resistances and errors, among other required data.
#' @param data.in A data.frame or list imported with arbin_import or arbin_import_raw respectively.
#' @keywords Rmap
#' @useDynLib arbintools
#' @export
#' @examples
#' mydataset <- arbin_import("dataset.xlsx", mass = 3)
#' R_data <- arbin_Rmap(mydataset)

arbin_Rmap <- function(data.in) {

require(dplyr)
require(parallel)
require(purrr)

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
message("Now processing ", nrow(raw),
        " rows, which will probably take around ", ceiling(nrow(raw)/10000),
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

# Assign identifier
message("identifying current interruptions... ", appendLF = FALSE)
raw$rest <- assignRest(raw$state)
raw$rest[1] <- 1 ### this is a bodge, because the first element is zero for some reason.
message("done, found ", max(raw$rest), " interruptions")

# Adjust capacity.
if(raw$state[1] == "D") {
  raw$adjQ <- raw$Q.d - raw$Q.c
} else if (raw$state[1] == "C") {
  raw$adjQ <- raw$Q.c - raw$Q.d
} else {
  stop("problem! The data doesn't seem to start with a current pulse?")
}

# Now assemble the data.
# check if Windows, and set ncores to 1.
if(Sys.info()["sysname"] == "Windows") {
  cores <- 1 
} else {
  cores <- detectCores()
}

message("Now assembling processed data, please wait a moment... ", appendLF = FALSE)
proc <- mclapply(split(raw, raw$cyc.n), function(cycle) {
  
  # remove partial current-interruption cycle if it exists
  if(last(cycle$state) != "R") {
    cycle <- filter(cycle, rest < max(cycle$rest))
  }
  
  d1 <- cycle %>%
    select(rest, state, cyc.n, adjQ, E, I) %>%
    filter(state != "R") %>%
    slice_rows("rest") %>%
    by_slice(dmap, ~ last(.x), .collate = "rows")
  
  d2 <- cycle %>%                                 # take the single cycle
    select(rest, state, step.t, E, I) %>%         # select the rows we need
    filter(state == "R") %>%                      # filter only the rest periods
    slice_rows("rest") %>%                        
    by_slice(partial(lm, E ~ sqrt(step.t))) %>%   # for every rest period, fit E vs sqrt(t)
    .[[2]] %>%                                    
    map( ~ data.frame(                            # for each fit, make a one-row data frame of the...
      E0 = coef(summary(.))[1, 1],                # coefficient...
      E0_err = coef(summary(.))[1, 2]             # and error
      )
    ) %>%
    do.call(rbind, .)                             # and then bind it into a dataframe
  
  out <- cbind(d1, d2) %>%
    mutate(R = (E - E0) / I, R_err = E0_err / abs(I)) # calculate the resistances and error.
  
  return(out)
}, mc.cores = cores)

proc <- do.call(rbind, proc)
message("done.")

return(proc)

}