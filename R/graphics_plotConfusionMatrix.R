#' plotHeterogeneityPieCharts
#'
#' @description This functions return ggplot2 figure that visualise using pie charts what fraction of cells
#'  exposed to one dose exhibits responses in the range characteristic for other doses.
#'
#' @param model FRAModel object return by FRA function
#' @param title_ character, specify title of plot, default \code{"Cell-to-cel heterogeneity"}
#' @param xlab_ character, label of x axes, default \code{"dose for which response is typical"}
#' @param ylab_ character, label of y axes, default \code{"dose"}
#' @param max.signal maximal signal for which the cell-to-cell heterogeneity is plotted, default \code{confusion.signal == max(signal)}
#' @export
plotHeterogeneityPieCharts <-
  function(
    model,
    max.signal = NULL,
    save.plot = FALSE,
    title_ = "Cell-to-cel heterogeneity",
    ylab_ ="dose",
    xlab_ = "dose for which response is typical",
    theme.signal = NULL,
    fill.guide_ = "none",
    ...
  ){
    confusion.signal = 
    if(is.null(model)){
      stop("model must be an object of class FRAModel")
    } else if(class(model) != "FRAModel"){
      stop("model must be an object of class FRAModel")
    }

    if(is.null(model$confusion.table)){
      stop("model must be an object of class FRAModel and should contain confusion.table")
    }

    x_ = "1"
    y_ = "prob"
    fill_ = "fill"
    alpha_ = "alpha"
    # # ggplot2::model$confusion.table
    # # df.confusion.fill <- df.confusion$fill
    # # names(df.confusion.fill) <- df.confusion$fill
    #
    # summary_vars <-syms(experiments.df$response.cols)
    #
    # # Wrap the summary variables with mean()
    # summary_vars <- purrr::map(summary_vars, function(var) {
    #   expr(fun.summarise(!!var, na.rm = TRUE))
    # })
    #
    # # Prefix the names with `avg_`
    # names(summary_vars) <- experiments.df$response.protein
    confusion.signal.syms <- quo(max.signal == max(!!sym(model$signal)))
    if(!is.null(confusion.signal)){
      if(is.numeric(confusion.signal)){
        confusion.signal.syms <- quo(max.signal == confusion.signal)
      }
    }



    # df.confusion <-
    #   model$confusion.table %>%
    #   dplyr::filter(!!confusion.signal.syms) %>%
    #   dplyr::mutate(
    #     fill =
    #       dplyr::if_else(!!quo(!!sym(model$signal) == !!sym(model$class)),
    #                      ggthemes::canva_palettes[["Subdued and proffesional"]][2],
    #                      ggthemes::canva_palettes[["Subdued and proffesional"]][1]))
    df.confusion <-
      model$confusion.table %>%
      dplyr::filter(!!confusion.signal.syms) %>%
      dplyr::mutate(
        alpha =
          dplyr::if_else(!!quo(!!sym(model$signal) == !!sym(model$class)),
                         0.75, #ggthemes::canva_palettes[["Subdued and proffesional"]][2],
                         0.5#ggthemes::canva_palettes[["Subdued and proffesional"]][1]))
          ))
    alpha.values <- (df.confusion %>% dplyr::distinct(alpha))[["alpha"]]
    names(alpha.values) <- alpha.values
    
    alpha_ <- "factor(alpha)"
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
    
    
    fill_ <- paste("factor(", model$class, ")")
    #fill.values <- df.confusion$fill
    #names(fill.values) <- fill.values

    ggplot2::ggplot(
        df.confusion,
        mapping =
          ggplot2::aes_string(
            x = x_,
            y = y_,
            fill = fill_,
            alpha = alpha_ 
          )
      ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::facet_grid(paste(model$signal,model$class, sep = "~"),
                          switch = "both"
      ) +
      ggplot2::coord_polar(theta = "y", start = 0) +
      ggplot2::ylim(c(0,1)) +
      theme_scrc.confusion_matrix(theme.title_size = 12) +
      ggplot2::ylab(xlab_) +
      ggplot2::xlab(ylab_) +
      ggplot2::ggtitle(title_) +
      # ggplot2::scale_fill_manual(values = fill.values,
      #                            guide = "none"
      #                            )  + 
      ggplot2::scale_fill_manual(
        guide = fill.guide_,
        name = xlab_,
        values = colors,
        labels = signals.rescale.df[[model$signal]]
      ) +
      ggplot2::scale_alpha_manual(values =  alpha.values, guide = "none") %>%
    return()
  }
