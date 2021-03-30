### ###
### Main plots for CyTOF data presented in the Manuscript and SI
### ###

# Please download cytof data from : 10.5281/zenodo.4646713 
# and save the umder diredctory CYTOF_PATH
CYTOF_PATH <- "path/to/cytof/files" # define CYTOF_PATH
CYTOF_PATH <- "data" # define CYTOF_PATH

path <- paste(CYTOF_PATH, "data.fra.cytof.all_cell_types.rds", sep = "/")
data.cytof.all_cell_types <- readRDS(path)
head(data.cytof.all_cell_types)

path <- paste(CYTOF_PATH, "data.fra.cytof.list.rds", sep = "/")
data.cytof.list <- readRDS(path)
#### t-SNE plot ####
require(Rtsne)
require(tidyverse)
require(data.table)
require(foreach)

# markers used to identyfication of cell types 
gating.colnames <- c("HLA-DR", "CD11c", "CD123", "CD14",  "CD16",  "CD19", "CD20", "CD3", "CD38", "CD45", "CD56", "CD8")
samples_num <- 10000
data.cytof.all_cell_types[sample(samples_num, x = 1:nrow(data.cytof.all_cell_types)),] -> data.train 

data.train %>%
  dplyr::select(!!gating.colnames) %>% 
  as.matrix() -> data.train.matrix

# Rtsne returns tsne coordinates 
# coordinates are not deterministic and depends on methods parameters
tsne <- Rtsne(data.train.matrix, 
              dims = 2,
              perplexity=30, 
              verbose=TRUE, 
              max_iter = 500)

Y <- tsne$Y
colnames(Y) <- c("tsne1", "tsne2")
data.train %>% 
  cbind(Y) ->
  data.train
# tsne1 and tsne2 are used to plot cell position 

xlim_ <- c(-25, 25)
ylim_ <- xlim_
xlab_ <- "TSNE-1"
ylab_ <- "TSNE-2"

g.classes <- 
  ggplot(
    data.train, 
    aes(x = tsne1,
        y = tsne2)) + 
  coord_cartesian(xlim = xlim_, ylim = ylim_) +
  geom_point(
    alpha = 0.1,
    color = "gray") + 
  FRA::theme_scrc() + 
  geom_point(
    data = data.train %>% dplyr::filter(!is.na(cell_type)),
    mapping = aes(color = cell_type, fill = cell_type),
    alpha = 1) +
  xlab(xlab_) + 
  ylab(ylab_) +
  scale_fill_viridis(discrete = TRUE, option = "C", guide = FALSE
  ) +
  scale_color_viridis(discrete = TRUE,
                      option = "C",
                      name = "Cell Type"
  )
print(g.classes)

#### TSNE grid Sup Figure 2####
#variables.list <- c("pSTAT1", "pSTAT3", "pSTAT4", "pSTAT5", "pSTAT6", "STAT1", "STAT3", "IFNAR1", "IFNAR2")
variables.list <- c("pSTAT1", "pSTAT3", "pSTAT4", "pSTAT5", "pSTAT6")
stim.list <- (data.train %>% dplyr::distinct(Stim) %>% dplyr::arrange(Stim))[["Stim"]]
foreach(variable_ = variables.list) %do% {
  # color.limits_ <-
  #   as.numeric((data.train %>% dplyr::summarise_(
  #     min = paste("min(", variable_ ,")"),
  #     max = paste("quantile(", variable_ ,", prob = 0.975)")
  #                                    ))[c("min", "max")])
  
  color.limits_ <- c(0,1)
  variable.normalization <-
    as.numeric((data.train %>% dplyr::summarise_(
      min = 0, #paste("min(", variable_ ,")"),
      max = paste("quantile(", variable_ ,", prob = 0.975)")
    ))[c("min", "max")])
  
  mutate.expr <- quos((!!sym(variable_) - variable.normalization[1])/(variable.normalization[2] - variable.normalization[1]))
  names(mutate.expr) <- variable_
  
  g.list <- foreach(stim_ = stim.list)  %do% {
    ggplot(
      data.train %>%
        dplyr::filter(Stim == stim_) %>%
        dplyr::mutate(!!!mutate.expr) -> data.train.test, 
      aes_string(x = "tsne1",
                 y = "tsne2",
                 color = variable_,
                 fill = variable_)) + 
      coord_cartesian(xlim = xlim_, ylim = ylim_) +
      geom_point(
        alpha = 1) + 
      FRA::theme_scrc() +
      scale_color_viridis(
        #guide = "none",
        begin = 0.1,
        limits = color.limits_) +
      scale_fill_viridis(
        guide = "none",
        begin = 0.1,
        limits = color.limits_) +
      ggtitle(paste("IFNa:", stim_, "ng/ml"))
  }
  
  g.grid <- plot_grid(plotlist = g.list, ncol = 1)
  return(g.grid)
} %>%
  plot_grid(plotlist = ., 
            ncol = length(variables.list)) ->
  g.grid

#### Computing FRA ####
require(FRA)
model.list <- list()
frc.list <- list()
fra_pie_charts.list <- list()
parallel_cores = 2
bootstrap.number = 64
response_ = c("pSTAT1", "pSTAT3", "pSTAT4", "pSTAT5", "pSTAT6")
cell_type.list <- names(data.cytof.list)
for( cell_type in cell_type.list) {
  model.list[[cell_type]] <-
    FRA::FRA(
      data = data.cytof.list[[cell_type]],
      signal = "Stim",
      response = response_,
      parallel_cores = parallel_cores,
      bootstrap.number = bootstrap.number)
  print(model.list[[cell_type]])
  frc.list[[cell_type]] <- FRA::plotFRC(model = model.list[[cell_type]], title_ =  cell_type)
  fra_pie_charts.list[[cell_type]] <- FRA::plotHeterogeneityPieCharts(model = model.list[[cell_type]], title_ = cell_type)
}

plot_grid(plotlist = frc.list, 
          ncol = length(cell_type.list)) ->
  g.frc
print(g.frc)
plot_grid(plotlist = fra_pie_charts.list, 
          ncol = length(cell_type.list)) ->
  g.fra_pie_charts
print(g.fra_pie_charts)