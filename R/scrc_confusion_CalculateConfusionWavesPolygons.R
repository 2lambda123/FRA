CalculateConfusionWavesPolygons <-
  function(
    model,
    confusion.waves,
    signal.list,
    ...
  ){
    signal_ <- as.name(model$signal)
    class_ <- as.name(model$class)
    confusion.waves %>%
      dplyr::mutate(position  = scrc + prob) %>%
      dplyr::arrange(
        !!signal_,
        !!class_) %>%
      rbind(
        do.call(
          what = rbind,
          args = foreach::foreach(class_level_ =  1:length(signal.list)) %do% {
            (confusion.waves %>%
               dplyr::filter(
                   !!signal_ == !!class_ & (1 >= abs(class_level_ - signal_level))) %>%
               dplyr::mutate(position = scrc)) %>%
              rbind(
                (confusion.waves %>%
                   dplyr::filter((class_level == class_level_ - 1) &
                                   (signal_level < class_level_ - 1)) %>%
                   dplyr::mutate(position = scrc + prob))) %>%
              rbind(
                (confusion.waves %>%
                   dplyr::filter((class_level == class_level_ + 1) &
                                   (signal_level > class_level_ + 1)) %>%
                   dplyr::mutate(position = scrc + prob))) %>%
              dplyr::mutate(
                !!class_ := (GetLevelsDf.Class(model = model,
                                               signal.list = signal.list) %>%
                               dplyr::filter(class_level ==
                                               class_level_))[[model$class]]) %>%
              dplyr::arrange(-!!signal_)
          }
        )) %>%
      dplyr::select(!!signal_,
                     !!class_,
                     position) %>%
      dplyr::ungroup() %>%
      return()
  }

# CalculateConfusionWavesPolygons <-
#   function(
#     model,
#     confusion.waves,
#     signal.list,
#     ...
#   ){
#     confusion.waves %>%
#       dplyr::mutate(position  = scrc + prob) %>%
#       dplyr::arrange_(
#         model$signal,
#         model$class) %>%
#       rbind(
#         do.call(
#           what = rbind,
#           args = foreach::foreach(class_level_ =  1:length(signal.list)) %do% {
#             (confusion.waves %>%
#                dplyr::filter_(
#                  paste(
#                    model$signal, "==", model$class,
#                    "&", "(1 >= abs(class_level_ - signal_level))")) %>%
#                dplyr::mutate(position = scrc)) %>%
#               rbind(
#                 (confusion.waves %>%
#                    dplyr::filter((class_level == class_level_ - 1) &
#                                    (signal_level < class_level_ - 1)) %>%
#                    dplyr::mutate(position = scrc + prob))) %>%
#               rbind(
#                 (confusion.waves %>%
#                    dplyr::filter((class_level == class_level_ + 1) &
#                                    (signal_level > class_level_ + 1)) %>%
#                    dplyr::mutate(position = scrc + prob))) %>%
#               dplyr::mutate_(
#                 .dots = setNames(
#                   nm = model$class,
#                   object = ((GetLevelsDf.Class(model = model,
#                                                signal.list = signal.list) %>%
#                                dplyr::filter(class_level ==
#                                                class_level_))[[model$class]]))) %>%
#               dplyr::arrange_(paste("-",model$signal))
#           }
#         )) %>%
#       dplyr::select_(model$signal,
#                      model$class,
#                      "position") %>%
#       dplyr::ungroup() %>%
#       return()
#   }
