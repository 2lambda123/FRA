ComputeRC <-
  function(model,
           parallel_cores,
           rc_type = "mean",
           rc.fun = mean,
           ...
  ){
    if(is.null(model)){
      return()
    }
    signal_ <- as.name(model$signal)
    response_ <- sapply(model$response, as.name)
    rc.name <- paste("rc", rc_type, sep = "_")
    rc.sum.name <- paste("rc.sum", rc_type, sep = "_")
    signal.list <- (model$data %>%
                      dplyr::arrange(!!signal_) %>%
                      dplyr::distinct(!!signal_))[[model$signal]]
    
    ### verify rc_type
    doParallel::registerDoParallel(parallel_cores)
    foreach::foreach(
      bootstrap.sample =
        model$bootstrap.samples) %dopar% {
          returnBootstrapData(
            model = model,
            bootstrap_ =
              bootstrap.sample) %>%
            dplyr::group_by(!!signal_) %>%
            dplyr::summarise_at(vars(!!!response_), rc.fun) %>% 
            # dplyr::summarise_(
            #   .dots =
            #     setNames(
            #       object = paste(rc_type, "(`", model$response, "`)", sep = ""),
            #       nm = model$response)) %>%
            dplyr::mutate(bootstrap = bootstrap.sample) %>%
            return(.)
        } %>%
      do.call(
        what = rbind,
        args = .
      ) ->
      model[[rc.name]]
    doParallel::registerDoParallel(parallel_cores)

    model[[rc.name]] %>%
      dplyr::group_by(!!signal_)%>%
      dplyr::summarise_at(vars(!!!response_), rc.fun) ->
      # dplyr::summarise_(
      #   .dots =
      #     setNames(
      #       object = paste(rc_type, "(`", model$response, "`)", sep = ""),
      #       nm = model$response)) ->
      model[[rc.sum.name]]
    return(model)
  }
