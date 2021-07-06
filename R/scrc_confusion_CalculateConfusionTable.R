CalculateConfusionTable <-
  function(
    model,
    cols.list,
    signal.list,
    ...
  ){
    groupby_columns_ <- sapply(c(model$signal,
                          "max.signal",
                          "bootstrap",
                          cols.list$computation.task), as.name)
    (model$specifictiy.bootstrap.table %>%
       dplyr::group_by(
         !!!groupby_columns_) %>%
       dplyr::summarise(
         counts.sum = sum(counts)))

    group_columns_ <-  sapply(c(model$signal,
                                cols.list$max.signal,
                                cols.list$bootstrap,
                                cols.list$computation.task), as.name)
    model$specifictiy.bootstrap.table %>%
      dplyr::left_join(
        (model$specifictiy.bootstrap.table %>%
           dplyr::group_by(
             !!!group_columns_) %>%
           dplyr::summarise(
             counts.sum = sum(counts)
             # .dots =
             #   setNames(
             #     nm = cols.list$counts.sum,
             #     object = paste("sum(", cols.list$counts, ")")
             #   )
             )),
        by = c(model$signal,
               cols.list$max.signal,
               cols.list$bootstrap,
               cols.list$computation.task)) %>%
      dplyr::mutate(
        prob.bootstrap = counts/counts.sum
      ) ->
      model$confusion.bootstrap.table

    groupby_columns_ <- sapply(c(model$signal,
                                 model$class,
                                 cols.list$max.signal),
                               as.name)
    model$confusion.bootstrap.table %>%
      dplyr::group_by(
        !!!groupby_columns_
      ) %>%
      dplyr::summarise(
        prob = mean(prob.bootstrap),
        prob.sd = sd(prob.bootstrap)
      )  %>%
      dplyr::ungroup()  ->
      model$confusion.table

    ## add max.signal == 0
    model$confusion.table[1,] %>%
      dplyr::mutate_all(.funs = function(x){0}) %>%
      dplyr::mutate(prob = 1) %>%
      rbind(
        x = .,
        y = model$confusion.table) ->
      model$confusion.table

    ### add not existing points
    signal_class.df <-
      expand.grid(signal.list,
                  signal.list,
                  signal.list)
    colnames(signal_class.df)  <-
      c(model$signal,
        model$class,
        cols.list$max.signal)
    signal_ <- as.name(model$signal)
    class_ <- as.name(model$class)
    signal_class.df %>%
      # dplyr::filter_(paste(model$signal, "<=", "max.signal")) %>%
      # dplyr::filter_(paste(model$class, "<=", "max.signal")) ->
      dplyr::filter(!!signal_ <= max.signal ) %>%
      dplyr::filter(!!class_ <= max.signal ) ->
      signal_class.df
    signal_class.df$inner_join_id_ <-
      1:nrow(signal_class.df)

    signal_class.df %>%
      dplyr::inner_join(
        model$confusion.table,
        by = c(model$signal,
               model$class,
               cols.list$max.signal)) ->
      signal_class.inner_join.df

    signal_class.df[
      signal_class.df$inner_join_id_[
        -which(signal_class.df$inner_join_id_ %in%
                 signal_class.inner_join.df$inner_join_id_)],] ->
      signal_class.df

    select_columns_ <- sapply(c(model$signal,
                              model$class,
                              cols.list$max.signal), as.name)
    groupby_columns_ <- sapply(c(cols.list$max.signal), as.name)
    model$confusion.table %>%
      rbind(
        (signal_class.df %>%
           # dplyr::select_(
           #   paste("c(",
           #         paste(model$signal,
           #               model$class,
           #               cols.list$max.signal,
           #               sep = ","),
           #         ")")) %>%
           dplyr::select(!!!select_columns_) %>%
           dplyr::left_join(
             y = model$confusion.table %>%
               #dplyr::group_by_(cols.list$max.signal) %>%
               dplyr::group_by(!!!groupby_columns_) %>%
               dplyr::summarise(prob = 0, prob.sd = 0),
             by = cols.list$max.signal
           ))) ->
      model$confusion.table
    return(model)
  }
