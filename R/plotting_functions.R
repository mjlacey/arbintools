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

arbin_quickplot <- function(data, x, y, geom = geom_point, size = 3) {
  
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
  
  # Replaces data points for charge capacity while the cell is discharging, and vice versa.
  # Alternative is possibly using reshape2::melt() and group but I've not looked into it.
  
  plotted.data$Q.c[plotted.data$step.n == unique(plotted.data$step.n[which(plotted.data$I < 0)])] <- NA
  plotted.data$Q.d[plotted.data$step.n == unique(plotted.data$step.n[which(plotted.data$I > 0)])] <- NA
  
  # Basic plot setup. =============================================
  p <- ggplot(plotted.data) +
    geom_path(aes(x = Q.d, y = E, color = factor(cyc.n), group = factor(cyc.n)), size = 1) +
    geom_path(aes(x = Q.c, y = E, color = factor(cyc.n), group = factor(cyc.n)), size = 1) +
    xlab("Q / mAh g"^-1~"") +
    ylab("cell voltage / V") +
    guides(color = guide_legend(title = "cycle"))
  
  return(p)
  
}

#' arbin_Qplot
#' 
#' This function takes a list of datasets, a vector of labels for those datasets, the size of the 
#' geom and whether or not to include error bars, and
#' returns a formatted capacity vs cycle number plot.
#' @param list A list of datasets, as exported from the arbin_import function - so each
#' list element is also a list
#' @param labels A vector of labels corresponding to the datasets in the list, in the correct order.
#' @keywords
#' @export
#' @examples 
#' arbin_Qplot(list(mydatasetA, mydatasetB), labels = c("dataset A", "dataset B"))

arbin_Qplot <- function(list, labels, size = 3, errors = FALSE) {
  
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
    geom_point(aes(x = cyc.n, y = Q.d, color = ident), size = size) +
    xlab("cycle number") +
    ylab(expression("Q"[discharge] * " / mAh g"^-1~"")) +
    guides(color = guide_legend(title = ""))
  
  p <- p + scale_y_continuous(limits=c(0, max(stats$Q.d)))
  
  if(errors) {
    p <- p + geom_errorbar(aes(x = cyc.n, ymin = Q.d - Q.d.err, ymax = Q.d + Q.d.err, color = ident), size = 0.6)
  }
  
  return(p)
  
}

#' theme_arbintools
#' 
#' This is a ggplot theme for the arbintools package. There are no arguments.
#' @keywords
#' @export
#' @examples 
#' p <- p + theme_arbintools()

theme_arbintools <- function(base_size=16) {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
    + theme(plot.title = element_text(size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour=NA),
            plot.background = element_rect(fill = "transparent", colour=NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(size = rel(1), colour="#333333"),
            axis.title.y = element_text(angle=90, colour="#333333"),
            axis.text = element_text(size = rel(0.8)), 
            axis.line.x = element_line(size=0.5, colour="#333333"),
            axis.line.y = element_line(size=0.5, colour="#333333"),
            axis.ticks.length=unit(-0.15, "cm"),
            axis.text.x = element_text(margin = margin(0.5, 0, 0.2, 0, "cm"), colour="#666666"),
            axis.text.y = element_text(margin = margin(0, 0.5, 0, 0.2, "cm"), colour="#666666"),
            panel.grid.major = element_line(colour="#eaeaea", size = 0.5),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.key.size = unit(0.6, "cm"),
            legend.margin = unit(0, "cm"),
            strip.background=element_rect(colour="#eaeaea",fill="#eaeaea"),
            strip.text = element_text(colour = "#333333", lineheight=0.7),
            legend.text = element_text(colour = "#333333")
    ))
  
}