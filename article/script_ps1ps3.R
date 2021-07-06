#### ps1 ps23
### ###
### manuscript U937 ps1 ps3
### ### 
library(tidyverse)
library(foreach)
library(data.table)

bootstrap.number <- 16
parallel_cores <- 2

#### U937 IFNG pSTAT1 ####

FRA::data.fra.ps1 %>% dplyr::group_by(stimulation) %>% 
  dplyr::summarise(response = mean(Intensity_MeanIntensity_Alexa)) ->
  df.ps1.sum

ps1.control <- (df.ps1.sum %>% dplyr::filter(stimulation == 0))[["response"]]
df.ps1.sum %>% 
  dplyr::mutate(response.nrm = response/ps1.control, response.nrm.log = log(response/ps1.control)) ->
  df.ps1.sum
rescale.fun = function(x){log(x = 100*x, base = 10)}
model.ps1 <- FRA::FRA(FRA::data.fra.ps1, 
                      signal = "stimulation",
                      response = "Intensity_MeanIntensity_Alexa",
                      bootstrap.number = bootstrap.number,
                      parallel_cores = parallel_cores)
g.ps1.frc <- FRA::plotFRC(model.ps1, rescale.fun= rescale.fun)
g.ps1.fra_pie_charts <- FRA::plotHeterogeneityPieCharts(model.ps1)

print(model.ps1)
print(g.ps1.frc)
print(g.ps1.fra_pie_charts)
#### U937 IL-10 pSTAT3 ####


FRA::data.fra.ps3 %>% dplyr::group_by(stimulation) %>% 
  dplyr::summarise(response = mean(Intensity_MeanIntensity_Alexa)) ->
  df.ps3.sum
ps3.control <- (df.ps3.sum %>% dplyr::filter(stimulation == 0))[["response"]]

df.ps3.sum %>% 
  dplyr::mutate(response.nrm = response/ps3.control, response.nrm.log = log(response/ps3.control)) ->
  df.ps3.sum

rescale.fun = function(x){log(x = x, base = 10)}
model.ps3 <- FRA::FRA(FRA::data.fra.ps3, 
                      signal = "stimulation",
                      response = "Intensity_MeanIntensity_Alexa",
                      bootstrap.number = bootstrap.number,
                      parallel_cores = parallel_cores)
g.ps3.frc <- FRA::plotFRC(model.ps3, rescale.fun= rescale.fun)
g.ps3.fra_pie_charts <- FRA::plotHeterogeneityPieCharts(model.ps3)

print(model.ps3)
print(g.ps3.frc)
print(g.ps3.fra_pie_charts)