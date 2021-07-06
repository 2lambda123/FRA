GetComputationsTasks <-
  function(
    signal.list,
    computations.bootstrap.samples,
    computations.bootstrap.test.sample_ = FALSE,
    computations.bootstrap.test.number_ = NULL,
    computations.max_signal_only = FALSE,
    ...
  ){
#    print(as.list(match.call()))

    GetComputationsSignalTasks <-
      function(
        signal.list,
        ...){
        signal.list.max = signal.list
        if(computations.max_signal_only){
          signal.list.max = max(signal.list)
        }
        foreach::foreach(signal_ = signal.list.max) %do% {
          computation_task <- signal.list[which(signal.list <= signal_)]
          if(length(computation_task) <= 1 ){
            return(NULL)
          }
          return(
            list(
              max.signal = signal_,
              signal = computation_task))
        } ->
          computation_task.list
        computation.ids.nulls <- sapply(computation_task.list, is.null)
        if(sum(computation.ids.nulls) > 0){
          computation_task.list <-
            computation_task.list[-which(computation.ids.nulls)]
        }
        return(computation_task.list)
      }

    computation.signal.task.list <-
      GetComputationsSignalTasks(signal.list = signal.list)
    if(!computations.bootstrap.test.sample_){
      foreach::foreach(bootstrap_ = computations.bootstrap.samples) %do% {
        foreach::foreach(computation.signal.task = computation.signal.task.list) %do% {
          computation.signal.task$bootstrap <- bootstrap_
          computation.signal.task$bootstrap.test <-
            computations.bootstrap.samples[-which(computations.bootstrap.samples == bootstrap_)]
          return(computation.signal.task)
        } %>%
          return()
      } %>%
        unlist(., recursive = FALSE)  ->
        compuatations.task.list
    } else {
      foreach::foreach(bootstrap_ = computations.bootstrap.samples) %do% {
        foreach::foreach(computation.signal.task = computation.signal.task.list) %do% {
          computation.signal.task$bootstrap <- bootstrap_
          computations.bootstrap.samples.test <- computations.bootstrap.samples[-which(computations.bootstrap.samples == bootstrap_)]
          if(!is.null(computations.bootstrap.test.number_)){
            computations.bootstrap.samples.test <-
              computations.bootstrap.samples.test[
                sample.int(n = length(computations.bootstrap.samples.test),
                           size = computations.bootstrap.test.number_,
                           replace = TRUE)]
          }
          foreach::foreach(bootstrap.test_ = computations.bootstrap.samples.test
                             ) %do% {
                               computation.signal.task$bootstrap.test <- bootstrap.test_
                               return(computation.signal.task)
                             } %>%
            return()
        } %>%
          unlist(., recursive = FALSE)
      } %>%
        unlist(., recursive = FALSE) ->
        compuatations.task.list
    }
    foreach(compuatations.task.i = 1:length(compuatations.task.list)) %do% {
      compuatations.task <- compuatations.task.list[[compuatations.task.i]]
      compuatations.task$id <- compuatations.task.i
      compuatations.task
    } %>% return()
}
