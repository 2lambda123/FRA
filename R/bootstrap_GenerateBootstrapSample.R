#' GenerateNoBootstrapSample
#'
#' @param model FRAModel
#'
#' @description This function generate bootstrap samples
#'
GenerateNoBootstrapSample <-
  function(model){
    UseMethod("GenerateNoBootstrapSample")
  }

#' GenerateNoBootstrapSample.FRAModel
#'
#' @param model FRAModel
#'
#' @description This function generate bootstrap samples
#'
GenerateNoBootstrapSample.FRAModel <-
  function(
    model,
    ...
  ){
    data.table::data.table(
      bootstrap = c(rep(x = 1, times = nrow(model$data)),
                    rep(x = 2, times = nrow(model$data))),
      bootstrap.sample =
        c(paste(1,1:nrow(model$data), sep = "_"),
          paste(2, 1:nrow(model$data), sep = "_")),
      sample    = c(model$data[[model$sample]],model$data[[model$sample]])
    ) ->
      model$bootstrap.samples.df
    model$bootstrap.samples <-
      (model$bootstrap.samples.df %>%
         dplyr::distinct(bootstrap))[["bootstrap"]]
    return(model)
  }

#' GenerateBootstrapSample
#'
#' @description This function generate bootstrap samples
#'
#' @param model FRAModel
#'
GenerateBootstrapSample <-
  function(model,
           ...){
    UseMethod("GenerateBootstrapSample")
  }

#' GenerateBootstrapSample.FRAModel
#'
#' @param model FRAModel
#' @param bootstrap.number number of bootstrap sampling
#' @param bootstrap.sample_size size of one bootstrap sample
#'
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @importFrom dplyr %>%
#'
GenerateBootstrapSample.FRAModel <-
  function(
    model,
    bootstrap.number = 1,
    bootstrap.sample_size = NULL,
    parallel_cores = 1,
    ...
  ) {

    stopifnot(is.numeric(bootstrap.number))
    if(bootstrap.number < 2){
      bootstrap.number = 2
    }
    stopifnot(is.numeric(parallel_cores))

    if(is.null(bootstrap.sample_size)){
      bootstrap.sample_size <-
        GenerateDefaultBootstrapSampleSize(
          model,
          bootstrap.sample_size
        )
    }

    doParallel::registerDoParallel(parallel_cores)
    foreach::foreach(bootstrap.i = 1:bootstrap.number) %dopar% {
      signal_ <- as.name(model$signal)
      signal.list <- sort((model$data %>%
                  dplyr::distinct(!!signal_))[[model$signal]])
      foreach::foreach(
        signal.val =
          signal.list) %dopar% {
               df <-
                 data.frame(
                   bootstrap = bootstrap.i,
                   bootstrap.sample = paste(bootstrap.i,
                                            signal.val,
                                            1:bootstrap.sample_size,
                                            sep = "_"))
               df[[model$sample]] <-
                 sample(
                   x =
                     (model$data %>%
                        dplyr::filter(!!signal_ == signal.val))[[model$sample]],
                   size = bootstrap.sample_size,
                   replace = TRUE
                 )
               return(df)
             } %>%
        do.call(what = rbind,
                args = .) %>%
        return()
    } %>%
      do.call(what = rbind,
              args = .) %>%
      data.table::data.table() ->
      model$bootstrap.samples.df
    doParallel::stopImplicitCluster()

    model$bootstrap.samples <-
      (model$bootstrap.samples.df %>%
         dplyr::distinct(bootstrap))[["bootstrap"]]

    return(model)
  }

