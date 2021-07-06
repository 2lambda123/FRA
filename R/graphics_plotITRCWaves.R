#' plotFRC
#'
#' @description This functions return ggplot2 figure that visualise fractional response curve (FRC) that 
#' quantifies fractions of cells that exhibit different responses to a change in dose, or any other experimental condition.
#'
#' @param model FRAModel object return by FRA function
#' @param title_ character, specify title of plot, default \code{"Fractional Response Curve"}
#' @param xlab_ character, label of x axes, default \code{"Dose"}
#' @param ylab_ character, label of y axes and legend title, default \code{"Cumulative fraction of cells"}
#' @param ylimits_ TRUE FALSE or vector of minimum and maximum of y axes
#' @param fill.guide_ logical, specify if legend should be displayed
#' @param theme.signal optional, object returned by \code{GetRescaledSignalTheme}
#' @param plot.heterogeneity, logical, specify if heterogeneity should be added to FRC plot, default \code{TRUE}
#' @inheritDotParams rescaleSignalsValues
# #' @inheritDotParams GetPlotTheme
#' @export
plotFRC <-
  function(
    model,
    title_ =
      "Fractional Response Curve",
    xlab_ = "Dose",
    ylab_ = "Cumulative fraction of cells",
    fill.guide_ = "legend",
    ylimits_ = TRUE,
    alpha_ = 0.5,
    getScaleY = getScaleY.SCRC,
    theme.signal = NULL,
    plot.heterogeneity = TRUE,
    ...
  ){
    if(is.null(model)){
      stop("model must be an object of class FRAModel")
    } else if(class(model) !=  "FRAModel"){
      stop("model must be an object of class FRAModel")
    }
    signal_ <- as.name(model$signal)
    class_ <- as.name(model$class)
    model <-
      CalculateConfusion(
        model,
        ...)

    if(is.null(theme.signal)){
      theme.signal <-
        FRA::GetRescaledSignalTheme(
          model = model,
          ...
        )
    }
    signals.rescale.df <- theme.signal$signals.rescale.df
    colors <- theme.signal$colors
    col.rescaled <- theme.signal$col.rescaled
    col.to.rescale <- theme.signal$col.to.rescale
    
    g.plot <-
      ggplot2::ggplot() +
      GetPlotTheme(...) +
      ggplot2::ggtitle(title_)

    x.SCRC  <- "signal_rescaled"
    y.SCRC <- "scrc"
    group.SCRC <- "type"
    ggplot.data.SCRC <-
      model$confusion.waves %>%
      dplyr::filter(!!signal_ == !!class_) %>%
      dplyr::left_join(
        signals.rescale.df,
        by = model$signal
      ) %>%
      dplyr::mutate(
        type = "SCRC")

    g.plot +
      ggplot2::geom_point(
        data = ggplot.data.SCRC,
        mapping = ggplot2::aes_string(
          x = x.SCRC,
          y = y.SCRC)
      ) +
      ggplot2::geom_line(
        data = ggplot.data.SCRC,
        mapping = ggplot2::aes_string(
          x = x.SCRC,
          y = y.SCRC,
          group = group.SCRC)
      ) ->
      g.plot
    
    
    ylim_max <- 1.2*max(ggplot.data.SCRC[y.SCRC])
    
    if(plot.heterogeneity){
      x.waves  <- "signal_rescaled"
      y.waves <- "position"
      fill.waves <- paste("factor(", model$class, ")")
      group.waves <- paste("interaction(", model$class, ", type)")
      ggplot.data.waves <-
        model$confusion.waves.polygon %>%
        dplyr::left_join(
          signals.rescale.df,
          by = model$signal
        ) %>%
        dplyr::mutate(
          type = "SCRC")
  
      g.plot +
        ggplot2::geom_polygon(
          data = ggplot.data.waves,
          mapping = ggplot2::aes_string(
            x = x.waves,
            y = y.waves,
            fill = fill.waves,
            group = group.waves),
          alpha = alpha_
        ) ->
        g.plot
  
      g.plot +
        ggplot2::scale_fill_manual(
          guide = fill.guide_,
          name = xlab_,
          values = colors,
          labels = signals.rescale.df[[model$signal]]
        ) ->
        g.plot
      
      ylim_max <- 1.2*max(ggplot.data.waves$position)
    }
    
    if(!is.factor(signals.rescale.df[[col.rescaled]])){
      g.plot +
        getScaleXContinuous(
          xlab = xlab_,
          signals.rescale.df = signals.rescale.df,
          col.rescaled = col.rescaled,
          col.to.rescale = col.to.rescale
        ) -> g.plot
    } else {
      g.plot +
        getScaleXDiscrete(
            xlab = xlab_,
            signals.rescale.df = signals.rescale.df,
            col.rescaled = col.rescaled,
            col.to.rescale = col.to.rescale
          )->
        g.plot
    }

      if(is.logical(ylimits_)){
        if(ylimits_){
          ylimits_ <- c(1, ylim_max)
        } else {
          ylimits_ <- NULL
        }
      }
      if(!is.null(ylimits_)){
        if(length(ylimits_) != 2){
          warnings("Limits of y axes should be defined as vector that consists
                   minimal and maximal numeric value")
          ylimits_ <- NULL
        }
        ylimits_[2] <-
          max(ylimits_[2],
              (model$confusion.waves.polygon %>% dplyr::filter(position == max(position)))[["position"]])

        ylimits_[1] <-
          min(ylimits_[1],
              (model$confusion.waves.polygon %>% dplyr::filter(position == min(position)))[["position"]])
      }

      if(!is.null(getScaleY)){
        g.plot +
          getScaleY(
            ylab = ylab_,
            ylimits = ylimits_,
            ...
          ) ->
          g.plot
      }

    return(g.plot)

  }
