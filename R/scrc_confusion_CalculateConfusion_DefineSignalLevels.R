GetLevelsDf.Class <-
  function(
    model,
    signal.list,
    ...){
    colnames = c()
    class_ <- as.name(model$class)
    data.frame(
      class_factor = signal.list,
      class_level  = 1:length(signal.list)) %>%
      dplyr::rename(
       !!class_ := class_factor) %>%
      return()
  }

GetLevelsDf.Signal <-
  function(
    model,
    signal.list,
    ...){
    colnames = c()
    signal_ <- as.name(model$signal)
    data.frame(
      signal_factor = signal.list,
      signal_level  = 1:length(signal.list)) %>%
      dplyr::rename(
          !!signal_ := signal_factor) %>%
      return()
  }
