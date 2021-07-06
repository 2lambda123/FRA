#' @importFrom stats na.omit
#' @importFrom stats predict
#' @importFrom stats sd
#' @importFrom stats setNames
#' @importFrom foreach "%do%"
#' @importFrom dplyr "%>%"
#' @export
CalculateConfusionWaves <-
  function(
    model,
    signal.list,
    signal.max = NULL,
    ...){

    if(is.null(signal.max)){
      signal.max <- (model$confusion.table %>% dplyr::summarise(max(max.signal)))[[1]]
    }
    signal_ <- as.name(model$signal)
    class_ <- as.name(model$class)
    max.signal_ <- as.name("max.signal")
    foreach::foreach(signal.val = signal.list) %do% {
      model$confusion.table %>%
        # dplyr::filter_(paste("max.signal == ", signal.max)) %>%
        # dplyr::filter_(
        #   paste(model$signal, "==", signal.val)) %>%
        # dplyr::mutate_(if_else_signal = paste("as.numeric(", model$signal, ")"),
        #                if_else_class = paste("as.numeric(", model$class, ")")) %>%
        dplyr::filter(!!max.signal_ == signal.max) %>%
        dplyr::filter(
           !!signal_ == signal.val) %>%
        dplyr::mutate(if_else_signal = as.numeric(!!signal_),
                        if_else_class = as.numeric(!!class_)) %>%
        dplyr::mutate(
          comparison =
            dplyr::if_else(
              condition = if_else_class < if_else_signal, #class < signal
              true  = "down",
              false =
                dplyr::if_else(
                  condition = if_else_class == if_else_signal, # class == signal,
                  true  = "equal",
                  false = "up"))) %>%
        dplyr::select(-if_else_class,
                      -if_else_signal) ->
        df.confusion.tmp


      foreach::foreach(class.val = signal.list) %do% {
        (if(class.val == signal.val){
          # df.confusion.tmp %>%
          #   dplyr::filter_(
          #     paste(model$class, ">=", class.val, "&", model$class, "<", signal.val)) 
          data.table::data.table( prob = 0 )
        } else {
          if(class.val < signal.val){
            df.confusion.tmp %>%
              # dplyr::filter_(
              #   paste(model$class, ">=", class.val, "&", model$class, "<", signal.val)) %>%
              dplyr::filter(
                (!!class_ >= class.val) & (!!class_ < signal.val)) %>%
              #dplyr::group_by(max.signal) %>%
              dplyr::summarise(
                prob = -sum(prob))
          } else {
            df.confusion.tmp %>%
              # dplyr::filter_(
              #   paste(model$class, "<=", class.val, "&", model$class, ">", signal.val)) %>%
              dplyr::filter(
                !!class_ <= class.val & !!class_ > signal.val) %>%
              #dplyr::group_by(max.signal) %>%
              dplyr::summarise(
                prob = sum(prob))
          }
        }) %>%
          # dplyr::mutate_(
          #   .dots =
          #     stats::setNames(
          #       nm = c(model$signal,
          #              model$class
          #              #, max.signal
          #       ),
          #       object = c(signal.val, class.val))) %>%
        dplyr::mutate(
          !!signal_ := signal.val,
          !!class_ := class.val) %>%
          return()
      } %>%
        do.call(
          what = rbind,
          args = .
        ) %>%
        return()
    } %>%
      do.call(
        what = rbind,
        args = .) %>%
      dplyr::left_join(
        y = model$scrc,
        by = model$signal) %>%
      dplyr::left_join(
        y = GetLevelsDf.Signal(
          model = model,
          signal.list = signal.list),
        by = model$signal) %>%
      dplyr::left_join(
        y = GetLevelsDf.Class(
          model = model,
          signal.list = signal.list),
        by = model$class) %>%
      return()
  }
