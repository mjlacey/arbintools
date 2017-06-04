### Plotting functions. The plotting functions here include: arbin_quickplot,
### for quickly plotting any x or y variable; arbin_plotvp, for plotting voltage
### vs capacity profiles of one or more cycles of one cell; arbin_qplot, for plotting
### capacity vs cycle number with multiple datasets;arbin_dQdV, for plotting
### differential capacity plot of one or multiple cycles of one cell;arbin_dQdV_multi,
### for plotting differential capacity plot of multiple cells at one cycle number.

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

#' arbin_plotvp_multi
#'
#' Plot voltage vs capacity for multiple cells at one cycle. Used for rate capability
#' comparison and polarization comparison of multiple cells.
#'
#' @param list list containing list of data from multiple cells generated by arbinimport.
#' @param labels character vector containing the text to use for the plot legend. Must be as long as the number of cells being plotted
#' @param cycle The number of the cycle that is desired to be plotted. Default is cycle =1.
#' @export
#' @examples
#' arbin_plotvp_multi(l1[1:3],c("test","test2","test3"),cycle=5)
arbin_plotvp_multi<-function (list, labels, cycle=1)
{
  require(ggplot2)
  require(scales)
  require(grid)
  require(dplyr)
  if (length(list) != length(labels)) {
    stop("Problem... it doesn't seem you've specified the same number of datasets as labels.")
  }
  #need to add some check code to see if there are enough cycles to compare
  #remove the statistics sublist, keep raw data for Q vs V plot
  stats <- lapply(list, function(x) x[[1]])
  stats <- lapply(seq_along(stats), function(i) {
    df <- stats[[i]]
    #add column with label names to use for coloring plots.
    df$ident <- labels[i]
    #remove other cycles that you dont want.
    df<-filter(df, cyc.n == cycle)
    return(df)
  })
  stats <- do.call(rbind, stats)

  # Basic plot setup. ==========================================================
  p <- ggplot(stats) + geom_point(aes(x = Q.d, y = E, color = ident), size = 4)+
    geom_point(aes(x = Q.c, y = E, color = ident, group=ident), size = 4)+
    xlab(expression("Q"[discharge] * " / mAh g"^-1 ~ "")) + ylab("Voltage (V)") +
    guides(color = guide_legend("Cells"))

  # Plotting theme setup ==========================================================
  p <- p + theme_bw() + theme(text = element_text(size = 21)) + theme(legend.position = "bottom")+
    theme(panel.border = element_rect(size = 1, color = "black")) +
    theme(axis.ticks.length = unit(-0.25, "cm")) +
    theme(axis.text.x = element_text(margin = margin(0.5, 0, 0.2, 0, "cm"))) +
    theme(axis.text.y = element_text(margin = margin(0,0.5, 0, 0.2, "cm"))) +
    theme(panel.grid.major = element_line(size = 0.5))

  # Set y axis to continuous and set limits====================================
  p <- p + scale_y_continuous(limits = c(min(stats$E), max(stats$E)))
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
#' arbin_Qplot(l1[1:3],labels=c("Cell1", "Cell2","Cell3"))

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

#' arbin_dQdV
#'
#' Plot differential capacity plots (dQ/dV) for one cell at one cycle.
#'
#' @param list list of data generated from arbinimport script.
#' @param title input legend for the plot
#' @param cycle number of the cycle of interest, defaults to 1
#' @param ymin set ploting window range, defaults to 0.1
#' @param ymax set ploting window range, defaults to 1
#' @export
#' @examples
#' arbin_dQdV(list,cellfile,1,ymin,ymax)
arbin_dQdV<-function (list,title,cycle=1,ymin=0.1,ymax=1)
{
  require(ggplot2)
  require(scales)
  require(grid)
  require(dplyr)

  #remove the statistics sublist, keep raw data for dQ/dV vs V plot
  df <- list[1]

  #Combine discharge and charge capacity into continuous capacity Q.
  df$raw$Q<-df$raw$Q.c+df$raw$Q.d
  #Calculate dQ/dV
  x <- diff(df$raw$Q)/diff(df$raw$E)

  #Set non finite values to zero
  x[!is.finite(x)] <- 0

  #add zero to the beginning of the dQdV to recover the length of the vector after diff
  df$raw$dQdV<- c(0,x)
  df <- do.call(rbind, df)

  #remove other cycles than ones of interest.
  df<-filter(df, cyc.n == cycle)

  # Basic plot setup. ==========================================================
  p <- ggplot(df) + geom_point(aes(x = E, y = dQdV,color = factor(cyc.n), group = factor(cyc.n)),size = 1)+
  # Axis and legend titles set==================================================
    xlab(expression("Voltage / V")) + ylab("dQ/dV") +
    ggtitle(title)

  #Custom theme is added========================================================
  p <- p + theme_bw() + theme(text = element_text(size = 21)) + theme(legend.position = "bottom")+
    theme(panel.border = element_rect(size = 1, color = "black")) +
    theme(axis.ticks.length = unit(-0.25, "cm")) +
    theme(axis.text.x = element_text(margin = margin(0.5, 0, 0.2, 0, "cm"))) +
    theme(axis.text.y = element_text(margin = margin(0,0.5, 0, 0.2, "cm"))) +
    theme(panel.grid.major = element_line(size = 0.5))

  #Y axis is made to be continuous with limits set using ymin and ymax=========
  p <- p + scale_y_continuous(limits = c(ymin, ymax))
  return(p)
}

#' arbin_dQdV_multi
#'
#' Compare differential capacity plots from different cells, same cycle. .
#'
#' @param list list of data generated from arbinimport script.
#' @param title character vector containing the legend for each Cell for the legend
#' @param cycle Number of the cycle of interest, default is cycle=1
#' @param ymin set ploting window range for the dQ/dV axis, default is -1000
#' @param ymax set ploting window range for the dQ/dV axis, default is 1500
#' @export
#' @examples
#' arbin_dQdV_multi(l,title=c("Cell1", "Cell2","Cell3"),cycle=1,ymin=-1000,ymax=1500)
arbin_dQdV_multi<-function (list,title,cycle=1,ymin=-1000,ymax=1500)
{
  require(ggplot2)
  require(scales)
  require(grid)
  require(dplyr)

  #remove the statistics sublist, keep raw data for dQ/dV vs V plot
  stats <- lapply(list, function(x) x[[1]])
  stats <- lapply(seq_along(stats), function(i) {
    df<-stats[[i]]
    df$Q<-df$Q.c+df$Q.d
    x <- diff(df$Q)/diff(df$E)
    x[!is.finite(x)] <- 0
    df$dQdV<- c(0,x)
    #filter out/remove other cycles that you dont want.
    df<-filter(df, cyc.n %in% cycle)
    #add label to provide coloring for graphing
    df$ident <- title[i]
    return(df)
  })
  stats <- do.call(rbind, stats)
  # Basic plot setup. ==========================================================
  p <- ggplot(stats) +
    geom_point(aes(x = E, y = dQdV, color=ident))+

    # Axis and legend titles/colors set================================================
    xlab("Voltage (V)") + ylab("dQdV") +
    ggtitle(paste("Cycle",cycle))+
    labs(color="Sample")+

    #Custom theme is added========================================================
    theme_bw() +
    theme(text = element_text(size = 21)) +
    theme(legend.position = "right")+
    theme(panel.border = element_rect(size = 1, color = "black")) +
    theme(axis.ticks.length = unit(-0.25, "cm")) +
    theme(axis.text.x = element_text(margin = margin(0.5, 0, 0.2, 0, "cm"))) +
    theme(axis.text.y = element_text(margin = margin(0,0.5, 0, 0.2, "cm"))) +
    theme(panel.grid.major = element_line(size = 0.5))+

    #Y axis is made to be continuous with limits set using ymin and ymax=========
    scale_y_continuous(limits = c(ymin, ymax))
  return(p)
}
