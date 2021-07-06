CalculateSCRC <-
  function(
    model,
    cols.list,
    ...
  ){
    signal_ <- as.name(model$signal)
    class_ <- as.name(model$class)
    max.signal_ <- as.name(cols.list$max.signal)
    bootstrap_ <- as.name(cols.list$bootstrap)
    computation_ <- as.name(cols.list$computation.task)
    model$confusion.bootstrap.table %>%
      # dplyr::filter_(
      #   paste(model$signal,
      #         "==",
      #         model$class)
      # ) ->
      dplyr::filter(!!signal_ == !!class_) ->
      confusion.bootstrap.diagonal.table

    foreach::foreach(scrc.i =
                       1:nrow(confusion.bootstrap.diagonal.table)) %do% {

      scrc_ <- confusion.bootstrap.diagonal.table[scrc.i,]
      groupby_columns_ <- sapply(c(model$signal,"max.signal"), as.name)
      prob_ <-
        (confusion.bootstrap.diagonal.table %>%
           dplyr::filter(
             !!max.signal_ == scrc_[["max.signal"]],
             !!bootstrap_ ==  scrc_[["bootstrap"]],
             !!computation_ ==  scrc_[["computation"]]
             ) %>%
           # dplyr::filter_(
           #   paste(model$signal,
           #         "<=",
           #         scrc_[[model$signal]])
           # ) %>%
           dplyr::filter(!!signal_ <= scrc_[[model$signal]]) %>%
           dplyr::summarise(prob.bootstrap = sum(prob.bootstrap)))[["prob.bootstrap"]]
      scrc_ %>%
        dplyr::mutate(prob.bootstrap = prob_)
    }  %>%
      do.call(what = rbind,
              args = .) %>%
      # dplyr::group_by_(
      #   model$signal,
      #   "max.signal"
      # ) %>%
      dplyr::group_by(!!!groupby_columns_) %>%
      dplyr::summarise(
        prob.mean = mean(prob.bootstrap),
        prob.sd = sd(prob.bootstrap)
      ) %>%
      dplyr::ungroup() %>%
      # dplyr::arrange_("max.signal",
      #                 model$signal) %>%
      dplyr::arrange(!!!groupby_columns_) %>%
      dplyr::mutate(scrc = prob.mean) ->
      model$scrc.table

    model$scrc.table[1,] %>%
      dplyr::mutate_all(.funs = function(x){0}) %>%
      dplyr::mutate(scrc = 1,
                    prob.mean = 1) %>%
      rbind(model$scrc.table) %>%
      dplyr::ungroup() ->
      model$scrc.table

    model$scrc.table %>%
      # dplyr::group_by_(
      #   model$signal
      # ) %>%
      dplyr::group_by(
        !!signal_
      ) %>%
      dplyr::summarise(
        scrc =
          max(prob.mean)
      ) ->
      model$scrc

    max.scrc <- 1
    for(i in 1:nrow(model$scrc)) {
      max.scrc <- max(max.scrc, model$scrc[i,]$scrc)
      model$scrc[i,]$scrc <- max.scrc
    }

    return(model)
  }
