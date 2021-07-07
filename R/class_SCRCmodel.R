#' Constructor of FRAModel S3 object
#'
#' @param data A data.frame or data.table object in a wide format that describe
#'  response (might be multidimmensional) of the samples to the signal
#'  (now only one dimmensional). Data.frame \code{data} consists columns of names
#'  defined by \code{sample}, \code{signal} (optional), and \code{response}.
#'   Each row represents a response of one sample to the input signal.
#'  Column \code{signal} define the input signal (now only in one-dimmension).
#'  Columns \code{response} define the multidimmensional (optional) response to
#'   the input signal.
#'  Column \code{sample} specify identifaction of sample. If \code{sample} is not
#'   defined then sample is identified by row number.
#' @param signal character, specify name of the column that represents the input signal
#' @param response vector of chartacters, that specify names of the columns that represents
#' the output response
#' @param sample character (optional), specify name of the column that consists identifiaction of
#' sample
#'
#' @return FRAModel S3 object with
new_FRAModel <-
  function(
    data,
    signal,
    response,
    sample
  ){
    if(!exists(x = "data")){
      stop("data not defined")
    } else {
      if(!("data.frame" %in% class(data))){
        stop("data should be data.frame or data.table")
      }
    }
    if(!exists(x = "signal")){
      stop("object signal not defined")
    } else {
      if(!(as.character(signal) %in% colnames(data))){
        stop(paste("data has not column named", signal))
      }
      if(!all(sapply(data[[signal]], is.numeric))){
        stop(paste("input signal must be numeric"))
      }
    }
    if(!exists(x = "response")){
      stop("data not defined")
    } else {
      if(!(all(as.character(response) %in% colnames(data)))){
        stop(paste("data has not column named",
                   paste(response, collapse = " ")))
      }
    }

    if(!exists(x = "sample")){
      data[["sample"]] <- 1:nrow(data)
      sample <- "sample"
    } else if (!(as.character(sample) %in% colnames(data))){
      data[["sample"]] <- 1:nrow(data)
      sample <- "sample"
    }
    sample_ <- as.name(sample)
    signal_ <- as.name(signal)
    model <-
      list(data =
             data %>%
             dplyr::arrange(!!sample_, !!signal_) %>%
             data.table::data.table(),
           signal = signal,
           class  = "class",
           response = response,
           sample = sample
      )
    class(model) <- "FRAModel"
    return(model)
  }


#' print.FRAModel
#'
#' @export
print.FRAModel <-
  function(x, ...) {
    #print(format(x, ...), "\n")
    cat(format(class(x), justify = "left"), "\n")
    cat("formula :",
        x$signal,
        "~",
        paste(x$response, collapse = "+"),
        "\n")
    cat("FRA :", "\n")
    SCRC_vec <- x$scrc$scrc
    names(SCRC_vec) <-x$scrc[[x$signal]]
    print(round(SCRC_vec, digits = 2))
    cat("confusion matrix :", "\n")
    print(round(as.matrix(x$confusion.matrix), digits = 2))
    # print(
    #   format(
    #     print(round(as.matrix(x$confusion.matrix.wide), digits = 2)),
    #           justify = "left",
    #     digits = TRUE),
    #   digits = 3)
  }

#' format.FRAModel
format.FRAModel <-
  function(
    model,
    ...
  ){
    paste(
      format(class(model)),
      "\n",
      paste("formula :",
            model$signal,
            "~",
            paste(model$response, collapse = "+")),
      "\n",
      "confusion matrix :\n",
      format(as.matrix(model$confusion.matrix)))
  }
