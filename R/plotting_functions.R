### Plotting functions. The plotting functions here include arbin_quickplot,
### for quickly plotting any x or y variable; arbin_plotvp, for plotting voltage
### profiles, and arbin_qplot, for plotting capacity vs cycle number with
### multiple datasets.

#' arbin_quickplot
#' 
#' This function takes a data frame of either raw data or aggregated statistics,
#' any argument for x or y-axis plotting, and returns a ggplot with the desired
#' formatting and axis labels. Can also do things such as subsetting data with
#' dplyr.
#' @param data The dataset, which must be a dataframe.
#' @param x The variable to be plotted on the x-axis
#' @param y The variable to be plotted on the y-axis
#' @param geom The geom to be passed to ggplot; e.g. geom_point or geom_path
#' @param size The size of the geom.
#' @keywords
#' @export
#' @examples 
#' arbin_quickplot(mydataset$raw, x = t, y = E)
#' arbin_quickplot(filter(mydataset$raw, cyc.n == 1, x = t, y = E, geom = geom_path, size = 1)
#' arbin_quickplot(mydataset$stat, x = cyc.n, y = d.Q)

arbin_quickplot <- function(data, x, y, geom = geom_point, size = 4) {
  
  require(ggplot2)
  require(scales)
  require(grid)
  require(dplyr)
  
  # the x and y arguments are converted to strings so that they can
  # be evaluated correctly by ggplot using aes_string().
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  
  # labels is a list of labels with the element names corresponding to
  # variable names present in the data files. When plotting, a correctly
  # formatted axis label is selected from the list.
  labels <- list(
    t = "time / s",
    step.n = "step number",
    cyc.n = "cycle number",
    I = "I / A",
    E = "cell voltage / V",
    Q.c = "charge capacity / mAh g"^-1~"",
    Q.d = "discharge capacity / mAh g"^-1~"",
    step.t = "step time / s",
    En.d = "discharge energy / Wh kg"^-1~"",
    En.c = "charge energy / Wh kg"^-1~"",
    CE = expression("Q"[d] * " / Q"[c]),
    EE = expression("energy efficiency, E"[d] * " / E"[c]),
    meanE.d = "mean discharge voltage / V",
    meanE.c = "mean charge voltage / V"
  )
  
  # Basic plot setup. ================================================
  p <- ggplot(data) +
    geom(aes_string(x = x, y = y), size = size)
  
  # Labels looked up from the list of labels. ========================
  p <- p + xlab(labels[[x]]) + ylab(labels[[y]])
  
  # If scales and grid are installed, then a custom theme is added.
  # This does not seem to work as I thought so it's cut out for now, and scales
  # and grid are required packages.
#  if(!requireNamespace("scales") == FALSE | !requireNamespace("grid") == FALSE) {
    p <- p + theme_bw() +
      theme(text = element_text(size=21)) +
      theme(panel.border = element_rect(size=1,color = "black")) +
      theme(axis.ticks.length=unit(-0.25, "cm")) +
      theme(axis.text.x = element_text(margin = margin(0.5, 0, 0.2, 0, "cm"))) +
      theme(axis.text.y = element_text(margin = margin(0, 0.5, 0, 0.2, "cm"))) +
      theme(panel.grid.major = element_line(size=0.5))
    
    # If the y-axis shows capacity, the plot is rescaled from 0 to max capacity.
    # Also requires the scales package.
    if(y %in% c("Q.c", "Q.d")) {
      p <- p + scale_y_continuous(limits=c(0, max(data[y])))
    }
    
#  }
  
  return(p)
  
}


#' arbin_plotvp
#' 
#' This function takes a data frame of raw data and a specified cycle - or number
#' of cycles, as a vector - and outputs charge and discharge voltage profiles.
#' @param data The dataset, which can be the list as outputted by arbin_import or the data
#' frame as arbin_import_raw.
#' @param cycles The cycles to be plotted, expressed as a vector
#' @keywords
#' @export
#' @examples 
#' arbin_plotvp(mydataset, 1)
#' arbin_plotvp(mydataset, cycles = c(1,5,10))

arbin_plotvp <- function(data, cycles) {
  
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
  
  # The function tries to guess whether the discharge or charge
  # step is first in the cycle. This affects correct plotting.
  for (i in unique(plotted.data$step.n)) {
    testI <- mean(plotted.data$I[plotted.data$step.n == i])
    
    if (testI == 0) {
    } else if (testI < 0) {
      plotted.data$Q.d[plotted.data$Q.c != 0] <- NA
      plotted.data$Q.c[plotted.data$Q.d != 0] <- NA
      break
    } else if (testI > 0) {
      plotted.data$Q.c[plotted.data$Q.d != 0] <- NA
      plotted.data$Q.d[plotted.data$Q.c != 0] <- NA
      break
    }
  }
  
  # Basic plot setup. =============================================
  p <- ggplot(plotted.data) +
    geom_path(aes(x = Q.d, y = E, color = factor(cyc.n), group = factor(cyc.n)), size = 1) +
    geom_path(aes(x = Q.c, y = E, color = factor(cyc.n), group = factor(cyc.n)), size = 1) +
    xlab("Q / mAh g"^-1~"") +
    ylab("cell voltage / V") +
    guides(color = guide_legend(title = "cycle"))
  
  # If scales and grid are installed, then a custom theme is added.
  # This does not seem to work as I thought so it's cut out for now, and scales
  # and grid are required packages.
#  if (requireNamespace("scales", quietly = TRUE) & requireNamespace("grid", quietly = TRUE)) {
    p <- p + theme_bw() +
      theme(text = element_text(size=21)) +
      theme(panel.border = element_rect(size=1,color = "black")) +
      theme(axis.ticks.length=unit(-0.25, "cm")) +
      theme(axis.text.x = element_text(margin = margin(0.5, 0, 0.2, 0, "cm"))) +
      theme(axis.text.y = element_text(margin = margin(0, 0.5, 0, 0.2, "cm"))) +
      theme(panel.grid.major = element_line(size=0.5))
#  }
  
  return(p)
  
}


#' arbin_Qplot
#' 
#' This function takes a list of datasets, a vector of labels for those datasets and
#' returns a formatted capacity vs cycle number plot.
#' @param list A list of datasets, as exported from the arbin_import function - so each
#' list element is also a list
#' @param labels A vector of labels corresponding to the datasets in the list, in the correct order.
#' @keywords
#' @export
#' @examples 
#' arbin_Qplot(list(mydatasetA, mydatasetB), labels = c("dataset A", "dataset B"))

arbin_Qplot <- function(list, labels) {
  
  require(ggplot2)
  require(scales)
  require(grid)
  require(dplyr)
  
  # Check that there are the same number of datasets as labels, and stop if not.
  if (length(list) != length(labels)) {
    stop("Problem... it doesn't seem you've specified the same number of datasets as labels.")
  }
  
  # Pull out the statistics from the datasets. =================================
  stats <- lapply(list, function(x) x[[2]])
  
  # Attach the label to the statistics. ========================================
  stats <- lapply(seq_along(stats), function(i) {
    df <- stats[[i]]
    df$ident <- labels[i]
    return(df)
  })
  
  # Bind each statistics data frame into one data frame. =======================
  stats <- do.call(rbind, stats)
  
  # Basic plot setup. ==========================================================
  p <- ggplot(stats) +
    geom_point(aes(x = cyc.n, y = Q.d, color = ident), size = 4) + 
    xlab("cycle number") +
    ylab(expression("Q"[discharge] * " / mAh g"^-1~"")) +
    guides(color = guide_legend(title = ""))
  
  # If scales and grid are installed, then a custom theme is added. y-axis is
  # also rescaled.
  # This does not seem to work as I thought so it's cut out for now, and scales
  # and grid are required packages.
#  if(!requireNamespace("scales") == FALSE | !requireNamespace("grid") == FALSE) {
    p <- p + theme_bw() +
      theme(text = element_text(size=21)) +
      theme(panel.border = element_rect(size=1,color = "black")) +
      theme(axis.ticks.length=unit(-0.25, "cm")) +
      theme(axis.text.x = element_text(margin = margin(0.5, 0, 0.2, 0, "cm"))) +
      theme(axis.text.y = element_text(margin = margin(0, 0.5, 0, 0.2, "cm"))) +
      theme(panel.grid.major = element_line(size=0.5))
    
    p <- p + scale_y_continuous(limits=c(0, max(stats$Q.d)))
#  }
  
  return(p)
  
}