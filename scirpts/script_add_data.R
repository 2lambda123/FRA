### ###
### add data
### ###

input.path <- "../RCC/resources/input/"
output.path <- "/media/knt/sdb2//KN/RCC/resources/nfkb/bootstrap/" -> model.path
computation.path <- paste(output.path, "rcc_scc/", sep = "/")
confusion_matrix.path <- paste(computation.path, "confusion_matrix", sep = "/") ### full
output.analysis.path <- paste(output.path, "analysis", sep= "/")
rcc_clustering.path <-  paste(computation.path, "clustering", sep = "/") ### full

data.list <- list()
data.list[["nfkb"]] <-
  readRDS(
    file =
      paste(
        input.path,
        "nfkb_short_ts.rds", sep = "/")) %>%
  dplyr::filter(Stim %in% c(
    0,
    0.01,
    0.03,
    0.1,
    0.2,
    0.5,
    1,
    2,
    4,
    8,
    100
  ))

data.list[["nfkb"]] %>%
  dplyr::mutate(Stim = dplyr::if_else(Stim == 8, 10, Stim)) %>%
  dplyr::arrange(Stim) %>%
  data.table::data.table() ->
  data.list[["nfkb"]]



data <- data.list[["nfkb"]]  %>%
  reshape2::dcast(formula = "Stim+CellID2~Time", value.var = "intensity")
data %>% head()


data %>%
  dplyr::rename(signal = Stim,
                sample = CellID2) ->
  data

data.nfkb <-
  data %>%
  data.table::data.table()

# data.nfkb %>%
#   dplyr::filter(
#     signal %in% c(
#       0,
#       0.01,
#       0.1,
#       1,
#       10,
#       100)) ->
#   data.nfkb
#
# data.nfkb %>%
#   dplyr::filter(signal != 8) %>%
#   rbind(
#     (data.nfkb %>%
#        dplyr::filter(signal == 8) %>%
#        dplyr::mutate(signal = 10))) ->
#   data.nfkb

data.itrc.nfkb <- data.nfkb
data.itrc.nfkb.all <- data.nfkb
devtools::use_data(
  data.itrc.nfkb.all,
  overwrite = TRUE)


####
library(devtools)
data.fra.nfkb.all <- data.itrc.nfkb.all
data.fra.cytof <- FRA::data.fra.cytof %>% dplyr::filter(Stim != 50)
usethis::use_data(
  data.fra.cytof,
  overwrite = TRUE)
