### Importing functions for Bio-Logic instruments

#' eclab_importall
#'
#' This function takes .mpt or .txt files exported from EC-Lab using the "Process Data" option. 
#' Cycle number must be included, else the function will stop prematurely. The function will
#' return *all* columns imported, but will rename common ones (E, I, Q.d etc) so as to be
#' consistent with the arbin_import functions. The function also checks whether Q is in coulombs,
#' and converts to mAh if so.
#' @param file The filename
#' @param mass Defaults to NULL. If an active material mass is specified - in MILLIGRAMS - the
#' capacities will be converted to mAh/g.
#' @keywords import
#' @export
#' @examples
#' mydataset <- eclab_importall("dataset.mpt", mass = 3.5)

eclab_importall <- function(file, mass = NULL) {
  
  require(dplyr)
  
  # Import raw data procedure
  data.in <- file %>% 
    readLines(n = 2) %>% 
    .[2] %>% # Read 2nd line and determine number of header lines
    regmatches(gregexpr('\\(?[0-9,.]+', .)) %>% # Extract number
    unlist %>%
    as.numeric %>%
    -1 %>% # Number of header lines to skip is the number minus one.
    read.delim(file, skip = .) # Read in whole file, skipping the header lines.
  
  if("X.Q.Qo..C" %in% colnames(data.in)) data.in$X.Q.Qo..C <- data.in$X.Q.Qo..C / 3.6
  
  # List of variable names and their replacements
  replace <- list(
    c("Ns", "step.n"),
    c("time.s", "t"),
    c("Ewe.V", "E"),
    c("X.I..mA", "I"),
    c("dq.mA.h", "dQ"),
    c("X.Q.Qo..mA.h", "Q"),
    c("X.Q.Qo..C", "Q"), # if it exists, C should be converted to mAh first.
    c("Ewe.Ece.V", "Ecell"),
    c("Ece.V", "Ece"),
    c("cycle.number", "cyc.n"),
    c("Q.charge.mA.h", "Q.c"),
    c("Q.discharge.mA.h", "Q.d"),
    c("Energy.charge.W.h", "En.c"),
    c("Energy.discharge.W.h", "En.d"),
    c("step.time.s", "step.t"),
    c("P.W", "P")
  )
  
  # Replace column names according to list above.
  for(i in 1:length(replace)) {
    colnames(data.in) <- gsub(replace[[i]][1], replace[[i]][2], colnames(data.in))
  }
  
  if("cyc.n" %in% colnames(data.in) == FALSE) { 
    stop("Can't proceed with import - cycle number data is missing. Please make sure
         you use EC-Lab's 'Process Data' function (Ctrl+F1) to export your data.")
  }
  
  # If step.n is missing, try and create it.
  if("step.n" %in% colnames(data.in) == FALSE) {
    
    # Split
    split.d <- data.in %>%
      split(factor(data.in$cyc.n))
    
    # For each cycle, look for step.n.changes and make a new step.n using a counter.
    split.d2 <- lapply(split.d, function(x) {
     
      # find out at which points the step.n changes
      changes <- which(x$step.n.changes == 1)
      
      # initialise an empty vector of the same length as the data for the cycle being processed
      step.n <- vector(mode = "numeric", length = nrow(x))
      
      # create the counter and set it to zero
      count.step <- 0
      
      # iterate over the new step.n vector and increase the counter at each value of i in changes
      for(i in 1:length(step.n)) {
        if(i %in% changes) {
          count.step <- count.step + 1
          step.n[i] <- count.step
        } else {
          step.n[i] <- count.step
        }
      }
      x <- cbind(x, step.n)
      return(x)
    })
  
  # Return processed data to data.in.  
  data.in <- do.call(rbind, split.d2)
    
  }
  
  # Convert capacities to mAh/g.
  if(!is.null(mass) == TRUE) {
    data.in$Q.c <- data.in$Q.c * (1E3/mass)
    data.in$Q.d <- data.in$Q.d * (1E3/mass)
  }
  
  return(data.in)
  
}

#' eclab_import_gcpl
#'
#' This function takes .mpt or .txt files exported from EC-Lab using the "Process Data" option. 
#' Cycle number must be included, else the function will stop prematurely. The function will
#' returns selected columns only (time, cycle number, step number, current, voltage, capacity), and
#' makes a separate 'stats' data frame similar to arbin_import.
#' @param file The filename
#' @param mass Defaults to NULL. If an active material mass is specified - in MILLIGRAMS - the
#' capacities will be converted to mAh/g.
#' @param cycles Defaults to 100. Determines the maximum number of cycles to be considered when
#' aggregating the statistics dataset.
#' @keywords import
#' @export
#' @examples
#' mydataset <- eclab_import_gcpl("dataset.mpt")
#' mydataset <- eclab_import_gcpl("dataset.mpt", mass = 3.5, cycles = 50)

eclab_import_gcpl <- function(file, mass = NULL, cycles = 100) {
  
  # Import raw data procedure
  data.in <- file %>% 
    readLines(n = 2) %>% 
    .[2] %>% # Read 2nd line and determine number of header lines
    regmatches(gregexpr('\\(?[0-9,.]+', .)) %>% # Extract number
    unlist %>%
    as.numeric %>%
    -1 %>% # Number of header lines to skip is the number minus one.
    read.delim(file, skip = .) # Read in whole file, skipping the header lines.
  
  # List of variable names and their replacements
  replace <- list(
    c("Ns", "step.n"),
    c("time.s", "t"),
    c("Ewe.V", "E"),
    c("X.I..mA", "I"),
    c("dq.mA.h", "dQ"),
    c("X.Q.Qo..mA.h", "Q"),
    c("Ewe.Ece.V", "Ecell"),
    c("Ece.V", "Ece"),
    c("cycle.number", "cyc.n"),
    c("Q.charge.mA.h", "Q.c"),
    c("Q.discharge.mA.h", "Q.d"),
    c("Energy.charge.W.h", "En.c"),
    c("Energy.discharge.W.h", "En.d"),
    c("step.time.s", "step.t"),
    c("P.W", "P")
  )
  
  # Replace column names according to list above.
  for(i in 1:length(replace)) {
    colnames(data.in) <- gsub(replace[[i]][1], replace[[i]][2], colnames(data.in))
  }
  
  if("cyc.n" %in% colnames(data.in) == FALSE) { 
    stop("Can't proceed with import - cycle number data is missing. Please make sure
         you use EC-Lab's 'Process Data' function (Ctrl+F1) to export your data.")
  }
  
  # If step.n is missing, try and create it.
  if("step.n" %in% colnames(data.in) == FALSE) {
    
    # Split
    split.d <- data.in %>%
      split(factor(data.in$cyc.n))
    
    # For each cycle, look for step.n.changes and make a new step.n using a counter.
    split.d2 <- lapply(split.d, function(x) {
      
      changes <- which(x$step.n.changes == 1)
      
      step.n <- vector(mode = "numeric", length = nrow(x))
      
      count.step <- 0
      
      for(i in 1:length(step.n)) {
        if(i %in% changes) {
          count.step <- count.step + 1
          step.n[i] <- count.step
        } else {
          step.n[i] <- count.step
        }
      }
      x <- cbind(x, step.n)
      return(x)
    })
    
    # Return processed data to data.in.  
    data.in <- do.call(rbind, split.d2)
    
  }
  
  # Make a new data frame.
  x <- select(data.in, t, step.n, cyc.n, I, E, Q.c, Q.d)
  
  # Convert masses to mAh/g.
  if(!is.null(mass) == TRUE) {
    x$Q.c <- x$Q.c * (1E3/mass)
    x$Q.d <- x$Q.d * (1E3/mass)
  }
  
  cycles <- ifelse(max(x$cyc.n >= cycles), cycles, max(x$cyc.n) - 1)
  
  # Data frame of aggregated statistics is constructed.
  stats <- data.frame(
    cyc.n = c(1:cycles),
    Q.c = sapply(c(1:cycles), function(i) max(x$Q.c[x$cyc.n == i])),
    Q.d = sapply(c(1:cycles), function(i) max(x$Q.d[x$cyc.n == i])),
 #   Q.c.err = sapply(c(1:cycles), function(i) last(x$Q.c.err[x$cyc.n == i])),
#  Q.d.err = sapply(c(1:cycles), function(i) last(x$Q.d.err[x$cyc.n == i])),
    CE = sapply(c(1:cycles), function(i) max(x$Q.d[x$cyc.n == i]) / max(x$Q.c[x$cyc.n == i]))
  )
  
  return(list(raw = x, stats = stats))
  
}

### Plotting function.

#' eclab_plotvp
#' 
#' Voltage profile plotting function for data imported with the eclab_import_gcpl function.
#' Fixes unwanted data points differently to the arbin_plotvp function and is purely experimental
#' at this point.
#' @param data dataset, which should be imported with eclab_import_gcpl.
#' @param cycles. The cycles to be plotted, expressed as a vector
#' @keywords
#' @export
#' @examples 
#' eclab_plotvp(mydataset, 1)
#' eclab_plotvp(mydataset, cycles = c(1,5,10))

eclab_plotvp <- function(data, cycles) {
  
  require(ggplot2)
  require(scales)
  require(grid)
  require(dplyr)
  
  # Data for the specified cycles is filtered off (uses filter() from
  # the dplyr package.) Checks first what format the data is in. If it sees
  # a list it assumes it should use the 'raw' data frame.
  if (class(data) == "list") {
    plotted.data <- filter(data$raw, cyc.n %in% cycles, I != 0)
  } else if (class(data) == "data.frame") {
    plotted.data <- filter(data, cyc.n %in% cycles, I != 0)
  } else {
    stop("Data doesn't seem to be in a recognisable format! Was it imported with
         the arbin_import functions?")
  }
  
  # Replaces data points for charge capacity while the cell is discharging, and vice versa.
  # Alternative is possibly using reshape2::melt() and group but I've not looked into it.

  #plotted.data$Q.c[plotted.data$Q.d != 0] <- NA
  #plotted.data$Q.d[plotted.data$Q.c != 0] <- NA
  plotted.data$Q.c[which(diff(plotted.data$Q.c) == 0)] <- NA
  plotted.data$Q.d[which(diff(plotted.data$Q.d) == 0)] <- NA
  
  # Basic plot setup. =============================================
  p <- ggplot(plotted.data) +
    geom_path(aes(x = Q.d, y = E, color = factor(cyc.n), group = factor(cyc.n)), size = 1) +
    geom_path(aes(x = Q.c, y = E, color = factor(cyc.n), group = factor(cyc.n)), size = 1) +
    xlab("Q / mAh g"^-1~"") +
    ylab("cell voltage / V") +
    guides(color = guide_legend(title = "cycle"))
  
  return(p)
  
  }
