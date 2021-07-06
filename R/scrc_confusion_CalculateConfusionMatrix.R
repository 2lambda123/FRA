CalculateConfusionMatrix <-
  function(
    model,
    cols.list,
    ...
  ){
    class_ <- as.name(model$class)
    signal_ <- as.name(model$signal)
    max.signal_ <- as.name(cols.list$max.signal)
    max.signal.list <-
      (model$confusion.table %>%
         dplyr::distinct(!!max.signal_) %>%
         dplyr::arrange(!!max.signal_)
      )[[cols.list$max.signal]]
    model$confusion.matrix.list <-
      foreach(
        max.signal.val =
          max.signal.list
      ) %do% {
        model$confusion.table %>%
          #dplyr::arrange_(paste("as.numeric(", model$class, ")"))  %>%
          dplyr::arrange(as.numeric(!!class_))  %>%
          #dplyr::arrange_(paste("as.numeric(", model$signal, ")")) %>%
          dplyr::arrange(as.numeric(!!signal_)) %>%
          # dplyr::filter_(paste(cols.list$max.signal,
          #                      "==",
          #                      max.signal_
          # )) %>%
          dplyr::filter(!!max.signal_ == max.signal.val) %>%
          reshape2::dcast(
            formula = paste( model$signal, "~", model$class),
            value.var = "prob"#, fun.aggregate = max
            ) ->
          confusion.matrix
        confusion.matrix[which(is.na(confusion.matrix), arr.ind = TRUE)] <- 0
        signals <- confusion.matrix[,1]
        confusion.matrix <- confusion.matrix[,-1]
        if(is.null(dim(confusion.matrix))){
          names(confusion.matrix) <- signals
        } else {
          confusion.matrix <-
            confusion.matrix[
              order(as.numeric(signals)),
              order(as.numeric(colnames(confusion.matrix)))]
          rownames(confusion.matrix) <-
            signals[order(as.numeric(signals))]
        }
        return(confusion.matrix)
      }
    names(model$confusion.matrix.list) <- as.character(max.signal.list)
    model$confusion.matrix <-
      model$confusion.matrix.list[[as.character(max(max.signal.list))]]
    return(model)
  }
