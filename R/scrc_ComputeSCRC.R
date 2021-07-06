ComputeSCRC <-
  function(model,
           parallel_cores,
           cc_maxit = 100,
           lr_maxit = 1000,
           MaxNWts = 5000,
           SCRC.DEBUG = FALSE,
           ...
  ) {
    signal_ <- as.name(model$signal)
    class_ <- as.name(model$class)
    signal.list <- (model$data %>%
                     dplyr::arrange(!!signal_) %>%
                     dplyr::distinct(!!signal_))[[model$signal]]

    cols.list <-
      list(
        counts     = "counts",
        counts.sum = "counts.sum",
        # counts.total = "counts.total",
        # counts.total.sum = "counts.total.sum",
        max.signal = "max.signal",
        bootstrap  = "bootstrap",
        computation.task  = "computation"
      )

    ###TODO add cols list to computation tasks
    compuatations.task.list <-
      GetComputationsTasks(
        signal.list = signal.list,
        computations.bootstrap.samples = model$bootstrap.samples,
        #bootstrap.test.sample = TRUE, bootstrap.test.sample = TRUE,bootstrap.test.number = 4)
        ...)

    model$compuatations.task.list <- compuatations.task.list
    if(length(compuatations.task.list) < 1){
      stop("Fractional response analysis cannot be done if number of signals is higher than 1")
    }

    doParallel::registerDoParallel(parallel_cores)
    foreach::foreach(
      computation.task =
        compuatations.task.list) %dopar% {
          #computation.task = compuatations.task.list[[1]]
          df.res <-
            GetLogRegParameters(
              data =
                returnBootstrapData(
                  model = model,
                  bootstrap_ = computation.task$bootstrap),
              model = model,
              signal.list = computation.task$signal)

          lr_model <- nnet::multinom(formula = df.res$formula_string,
                                     data = df.res$data,
                                     na.action = na.omit,
                                     maxit = lr_maxit,
                                     MaxNWts = MaxNWts)#,  model = FALSE )
          attr(x =  lr_model$terms, ".Environment") <- NULL
          df.res.newdata <-
            GetLogRegParameters(
              data =
                returnBootstrapData(
                  model = model,
                  bootstrap_ =
                    computation.task$bootstrap.test
                    # model$bootstrap.samples[
                    #   -which(model$bootstrap.samples ==
                    #            computation.task$bootstrap
                    #            )]
                    ),
              model = model,
              signal.list = computation.task$signal)

          lr.fit <-
            predict(object  = lr_model,
                    newdata = df.res.newdata$data)

          df.res.newdata$data$class <- as.numeric(as.character(lr.fit))
          select_columns_ <- c(df.res.newdata$signal, model$class)
          group_columns_ <- sapply(c(df.res.newdata$signal, model$class), as.name)
          df.res.newdata$data %>%
            dplyr::select(!!!select_columns_) %>%
            dplyr::group_by(!!!group_columns_) %>%
            # dplyr::select_(paste("c(",
            #                      df.res.newdata$signal,
            #                      ",",
            #                      model$class,
            #                      ")")) %>%
            # dplyr::group_by_(
            #   df.res.newdata$signal,
            #   model$class) %>%
            dplyr::summarise(
              counts = n()
              # .dots = setNames(
              #   object = "n()",
              #   nm = cols.list$counts)
              ) %>%
            dplyr::rename(
               !!signal_ := !!df.res$signal
            ) %>%
            # dplyr::rename_(
            #   .dots = setNames(nm = model$signal,
            #                    object = df.res$signal)) %>%
            dplyr::ungroup() ->
            df.confusion

          
          expand.grid(signal.grid = computation.task$signal,
                      class.grid = computation.task$signal) %>%
            dplyr::rename(
              !!model$signal := signal.grid,
              !!model$class  := class.grid) ->
            # dplyr::rename_(
            #   .dots =
            #     setNames(
            #       nm = c(model$signal, model$class),
            #       object = c("signal_", "class_")
            #     ) ) ->
            signal_class.df
          signal_class.df$inner_join_id_ <- 1:nrow(signal_class.df)

          signal_class.df %>%
            dplyr::inner_join(df.confusion,
                              by = c(model$signal,
                                     model$class)) ->
            signal_class.inner_join.df

          signal_class.df[
            signal_class.df$inner_join_id_[
              -which(signal_class.df$inner_join_id_ %in%
                signal_class.inner_join.df$inner_join_id_)
            ],] %>%
            dplyr::select(-inner_join_id_)->
            signal_class.df
          rm(signal_class.inner_join.df)

          df.confusion %>%
            rbind(
             signal_class.df %>%
                 dplyr::mutate(
                   !!cols.list$counts :=  0
                     ))  ->
                df.confusion

          df.confusion[[cols.list$max.signal]] <-
            computation.task$max.signal
          df.confusion[[cols.list$bootstrap]] <-
            computation.task$bootstrap
          df.confusion[[cols.list$computation.task]] <-
            computation.task$id

          return(df.confusion)
        } %>%
      do.call(
        what = rbind,
        args = .
      ) ->
      model$specifictiy.bootstrap.table
    #doParallel::registerDoParallel(parallel_cores)
    doParallel::stopImplicitCluster()

   model <-
      CalculateConfusionTable(
        model = model,
        cols.list = cols.list,
        signal.list = signal.list)

   model <-
     CalculateConfusionMatrix(
       model = model,
       cols.list = cols.list)

    model <-
      CalculateSCRC(
        model = model,
        cols.list = cols.list
      )

    model$frc <- model$scrc
    colnames(model$frc) <- c("dose", "FRC")
    model$heterogeneity <- model$confusion.matrix
    
    return(model)
  }
