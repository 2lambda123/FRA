GetLogRegParameters <-
  function(
    data,
    model,
    signal.list = NULL,
    signal.factor.column = "X",
    response.factor.column = "Y",
    ...
  ){
    signal_ <-as.name(model$signal)
    data %>%
      dplyr::filter(!!signal_ %in% signal.list) ->
      data

    response.cols <-
      which(colnames(data) %in%
              model$response)
    response.factor <- paste(response.factor.column,
                             model$response,
                             sep = "_")
    colnames(data)[response.cols] <- response.factor

    select_columns_ <- c(signal.factor.column,response.factor)
    data %>%
      dplyr::rename(factor_signal_obj = model$signal) %>%
      dplyr::mutate(
        factor_signal_obj = factor_signal_obj#paste(signal.factor.column, factor_signal_obj, sep = "_")
      ) %>%
      dplyr::rename(!!signal.factor.column := "factor_signal_obj") %>%
      dplyr::select(!!!select_columns_) ->
      # dplyr::select_(
      #   paste("c(",
      #         signal.factor.column,
      #         ",",
      #         paste(response.factor, collapse = ","),
      #         ")")) ->
      data

    return(
      list(
        formula_string = paste(signal.factor.column,
                               paste(response.factor, collapse = "+"),
                               sep = "~"),
        data = data,
        signal = signal.factor.column,
        response = response.factor
      )
    )
  }
