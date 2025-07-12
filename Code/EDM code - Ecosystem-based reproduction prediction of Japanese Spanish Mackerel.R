#save.image(file = "my_workspace.RData")
#load("my_workspace.RData")


setwd("D:/01/剩余产量模型JABBA_SPiCT_BSM等/JABBA运行/黄渤海蓝点马鲛/EDM结果")

suppressPackageStartupMessages({
  library("dplyr")
  library("ggplot2")
  library("grid")
  library("gridExtra")
  library("stringr")
  library("purrr")
  library('png')
  library('rgl')
  library('parallel')
})

#devtools::install_github("SugiharaLab/rEDM", ref = "v1.7.1")
library('rEDM')#1.7.5  和1.7.1似乎没啥多大变化
library('EDM')#1.7.1   2选1  之前是用1.7.1
#vignette('rEDM-tutorial')
#source("D:/01/EDM/rEDM-master旧的/rEDM_1.7.1/R/zzz.R")
#source("D:/01/EDM/rEDM-master旧的/rEDM_1.7.1/R/LegacyInterface.R")
#source("D:/01/EDM/rEDM-master旧的/rEDM_1.7.1/R/EDM_AuxFuncs.R")
#source("D:/01/EDM/rEDM-master旧的/rEDM_1.7.1/R/EDM.R")
library(Rcpp)  
# 假设cpp_files是一个包含所有.cpp文件路径的字符向量  
#cpp_files <- list.files(path = "D:/01/EDM/rEDM-master旧的/rEDM_1.7.1/src", pattern = "\\.cpp$", full.names = TRUE)  

# 遍历每个文件并尝试使用sourceCpp  
#for (file in cpp_files) {  
  tryCatch({  
    sourceCpp(file)  
    print(paste("Successfully sourced:", file))  
  }, error = function(e) {  
    print(paste("Error sourcing:", file, ":", e$message))  
  })  
}
#source("D:/01/EDM/rEDM-master旧的/rEDM_1.7.1/R/redm1.7.1.R")
#Rcpp::loadModule("EDMInternal", TRUE) Rcpp::loadModule("lnlp_module", TRUE)Rcpp::loadModule("block_lnlp_module", TRUE) Rcpp::loadModule("xmap_module", TRUE)




#导入程序
source("faf12287-sup-0003-datas2.R")
#load('faf12287-sup-0002-datas1.Rdata')
#导入数据
Data_Yel <- read.table( "黄渤海蓝点马鲛BC指数和环境数据.csv", header=TRUE, sep="," )
Data_Bohai <- read.table( "黄渤海蓝点马鲛SSB_R指数与环境数据.csv", header=TRUE, sep="," )
head(Data_Yel)
head(Data_Bohai)
#蓝点马鲛BC指数
bio.Yel<-data.frame(Year=Data_Yel$Year,B=Data_Yel$B_000t,C=Data_Yel$C_000t)
#蓝点马鲛JABBA-B_R
bio.Bohai.raw<-data.frame(Year=Data_Bohai$Year,B=Data_Bohai$B,Recruitment=Data_Bohai$R)
#蓝点马鲛SSB_R
bio.Bohai.VAST<-data.frame(Year=Data_Bohai$Year,SSB=Data_Bohai$SSB_SpringCatch,Recruitment=Data_Bohai$R_Fall_plus_nextSpeingCatch)
#蓝点马鲛BC指数环境数据
phys.Yel<-data.frame(Year=Data_Yel$Year,sst_spawn=Data_Yel$sst_spawning_period,sst_nursery=Data_Yel$sst_nursery_feeding_period,sst_winter=Data_Yel$sst_overwintering_period,
                     Fishing_pressure=Data_Yel$F,summer_fishing_moratorium=Data_Yel$summer_fishing_moratorium,
                     NPGO_spawn=Data_Yel$NPGO_spawning_period,NPGO_nursery=Data_Yel$NPGO_nursery_feeding_period,NPGO_winter=Data_Yel$NPGO_overwintering_period,
                     NPA_spawn=Data_Yel$NPA_spawning_period,NPA_nursery=Data_Yel$NPA_nursery_feeding_period,NPA_winter=Data_Yel$NPA_overwintering_period,
                     WP_spawn=Data_Yel$WP_spawning_period,WP_nursery=Data_Yel$WP_nursery_feeding_period,WP_winter=Data_Yel$WP_overwintering_period,
                     NAO_spawn=Data_Yel$NAO_spawning_period,NAO_nursery=Data_Yel$NAO_nursery_feeding_period,NAO_winter=Data_Yel$NAO_overwintering_period,
                     SOI_spawn=Data_Yel$SOI_spawning_period,SOI_nursery=Data_Yel$SOI_nursery_feeding_period,SOI_winter=Data_Yel$SOI_overwintering_period,
                     Nino34_spawn=Data_Yel$Nino34_spawning_period,Nino34_nursery=Data_Yel$Nino34_nursery_feeding_period,Nino34_winter=Data_Yel$Nino34_overwintering_period,
                     PDO_spawn=Data_Yel$PDO_spawning_period,PDO_nursery=Data_Yel$PDO_nursery_feeding_period,PDO_winter=Data_Yel$PDO_overwintering_period,
                     NOI_spawn=Data_Yel$NOI_spawning_period,NOI_nursery=Data_Yel$NOI_nursery_feeding_period,NOI_winter=Data_Yel$NOI_overwintering_period,
                     NPI_spawn=Data_Yel$NPI_spawning_period,NPI_nursery=Data_Yel$NPI_nursery_feeding_period,NPI_winter=Data_Yel$NPI_overwintering_period,
                     AOI_spawn=Data_Yel$AOI_spawning_period,AOI_nursery=Data_Yel$AOI_nursery_feeding_period,AOI_winter=Data_Yel$AOI_overwintering_period,
                     AMO_spawn=Data_Yel$AMO_spawning_period,AMO_nursery=Data_Yel$AMO_nursery_feeding_period,AMO_winter=Data_Yel$AMO_overwintering_period)
#蓝点马鲛SSB_R指数环境数据
phys.Bohai<-data.frame(Year=Data_Bohai$Year,sst_spawn=Data_Bohai$sst_spawning_period,sst_nursery=Data_Bohai$sst_nursery_feeding_period,sst_winter=Data_Bohai$sst_overwintering_period,
                       Fishing_pressure=Data_Bohai$F,summer_fishing_moratorium=Data_Bohai$summer_fishing_moratorium,
                       NPGO_spawn=Data_Bohai$NPGO_spawning_period,NPGO_nursery=Data_Bohai$NPGO_nursery_feeding_period,NPGO_winter=Data_Bohai$NPGO_overwintering_period,
                       NPA_spawn=Data_Bohai$NPA_spawning_period,NPA_nursery=Data_Bohai$NPA_nursery_feeding_period,NPA_winter=Data_Bohai$NPA_overwintering_period,
                       WP_spawn=Data_Bohai$WP_spawning_period,WP_nursery=Data_Bohai$WP_nursery_feeding_period,WP_winter=Data_Bohai$WP_overwintering_period,
                       NAO_spawn=Data_Bohai$NAO_spawning_period,NAO_nursery=Data_Bohai$NAO_nursery_feeding_period,NAO_winter=Data_Bohai$NAO_overwintering_period,
                       SOI_spawn=Data_Bohai$SOI_spawning_period,SOI_nursery=Data_Bohai$SOI_nursery_feeding_period,SOI_winter=Data_Bohai$SOI_overwintering_period,
                       Nino34_spawn=Data_Bohai$Nino34_spawning_period,Nino34_nursery=Data_Bohai$Nino34_nursery_feeding_period,Nino34_winter=Data_Bohai$Nino34_overwintering_period,
                       PDO_spawn=Data_Bohai$PDO_spawning_period,PDO_nursery=Data_Bohai$PDO_nursery_feeding_period,PDO_winter=Data_Bohai$PDO_overwintering_period,
                       NOI_spawn=Data_Bohai$NOI_spawning_period,NOI_nursery=Data_Bohai$NOI_nursery_feeding_period,NOI_winter=Data_Bohai$NOI_overwintering_period,
                       NPI_spawn=Data_Bohai$NPI_spawning_period,NPI_nursery=Data_Bohai$NPI_nursery_feeding_period,NPI_winter=Data_Bohai$NPI_overwintering_period,
                       AOI_spawn=Data_Bohai$AOI_spawning_period,AOI_nursery=Data_Bohai$AOI_nursery_feeding_period,AOI_winter=Data_Bohai$AOI_overwintering_period,
                       AMO_spawn=Data_Bohai$AMO_spawning_period,AMO_nursery=Data_Bohai$AMO_nursery_feeding_period,AMO_winter=Data_Bohai$AMO_overwintering_period)

head(bio.Yel)
head(bio.Bohai.raw)
head(bio.Bohai.VAST)
head(phys.Yel)
head(phys.Bohai)

#Basic Plots
#plot the biological data as time series for the Yellow_Sea and the YBohai_Sea. First,
#we scale SSB and Recruitment by their standard deviations. 
#Then we convert the data from wide to long form for plotting.
df.plot <- bind_rows(
  bio.Yel %>%
    mutate(B = B / sd(B, na.rm = TRUE),
           C = C / sd(C, na.rm = TRUE)) %>%
    select(Year, B, C) %>%
    tidyr::gather(key = var, value = value, B, C) %>%
    mutate(region = "YellowandYBohai_Sea"),
  bio.Bohai.raw %>%
    mutate(B = B / sd(B, na.rm = TRUE),
           Recruitment_ind. = Recruitment / sd(Recruitment, na.rm = TRUE)) %>%
    select(Year, B, Recruitment_ind.) %>%
    tidyr::gather(key = var, value = value, B, Recruitment_ind.) %>%
    mutate(region = "Y&B_Sea"),
  bio.Bohai.VAST %>%
    mutate(`SSB` = SSB / sd(SSB, na.rm = TRUE),
           `Recruitment_bio` = Recruitment / sd(Recruitment, na.rm = TRUE)) %>%
    select(Year, `SSB`,`Recruitment_bio`) %>%
    tidyr::gather(key = var, value = value, `SSB`,`Recruitment_bio`) %>%
    mutate(region = "Y&B_Sea")
)


f1_a <- df.plot %>%
  filter(region == "YellowandYBohai_Sea") %>%
  ggplot(aes(x=Year,y=value,col=var)) +
  geom_line(lwd=1) +
  labs(y = "Normalized index") +
  theme_bw() +
  theme( legend.text = element_text(size = 8),
         legend.background = element_rect(color = 'black',size=.1),
         legend.key.height = unit(0.5, "cm"),
         legend.key.width = unit(0.5, "cm"),
         legend.justification=c(0.1,0.1), legend.position=c(0.05,0.1) )


f1_b <- bio.Yel %>%
  ggplot(aes(x=B / sd(B, na.rm = TRUE),y=C / sd(C, na.rm = TRUE))) +
  geom_point() +
  theme_bw()+ xlab("Normalized B ") +  ylab("Normalized C") 

f1_c <- df.plot %>%
  filter(region == "Y&B_Sea") %>%
  ggplot(aes(x=Year,y=value,col=var)) +
  geom_line(lwd=1) +
  labs(y = "Normalized index") +
  theme_bw() +
  theme( legend.text = element_text(size = 8),
         legend.background = element_rect(color = 'black',size=.1),
         legend.key.height = unit(0.5, "cm"),
         legend.key.width = unit(0.5, "cm"),
         legend.justification=c(0.1,0.1), legend.position=c(0.76,0.50) )


f1_d <- bio.Bohai.raw %>%
  ggplot(aes(x=B / sd(B, na.rm = TRUE),y=Recruitment/ sd(Recruitment, na.rm = TRUE))) +
  geom_point() +
  theme_bw()

f1_e <- f1_d+geom_point(aes(SSB/ sd(SSB, na.rm = TRUE),Recruitment/ sd(Recruitment, na.rm = TRUE)),data=bio.Bohai.VAST,col="red")+
     xlab("Normalized SSB(B) ") +  ylab("Normalized Recruitment") 
  
grid.arrange(grobs=list(f1_a,f1_b,f1_c,f1_d), ncol=2,widths = c(2.2,1))
#649 450
grid.arrange(grobs=list(f1_a,f1_b,f1_c,f1_e), ncol=2,widths = c(2.2,1))



#Univariate Analysis
do_univariate_analysis <- function(ts, E.list = 1:5, predict_diff = FALSE, ...)
{
  ts <- ts / sd(ts, na.rm = TRUE)
  if(predict_diff){
    simplex_out <- simplex_deltas(ts, E=E.list, ...)$delta_stats
  }else{
    simplex_out <- simplex(ts, E = E.list, ...)
  }
  E.star <- simplex_out$E[which.max(simplex_out$rho)]
  smap_out <- if(predict_diff){
    s_map_deltas(ts, E=E.star, ...)$delta_stats
  }else{
    s_map(ts, E = E.star, ...)
  }
  return(list(simplex = simplex_out,smap = smap_out))
}

#简单处理缺失值
#bio.Bohai.raw$Recruitment[8]=(bio.Bohai.raw$Recruitment[7]+bio.Bohai.raw$Recruitment[9])/2
#bio.Bohai.raw$B[6]=bio.Bohai.raw$B[7]

#Do univariate analysis on each abundance time series
results <- list(do_univariate_analysis(bio.Yel$C[!is.na(bio.Yel$C)], silent = TRUE) %>%
                  lapply(function(df) mutate(df,species = "Yellow_Sea",
                                             variable = "C", method = "normal")),
                do_univariate_analysis(bio.Bohai.VAST$Recruitment[!is.na(bio.Bohai.VAST$Recruitment)], silent = TRUE) %>%
                  lapply(function(df) mutate(df,species = "YBohai_Sea", variable
                                             = "Recruitment_bio", method = "normal")),
                do_univariate_analysis(bio.Bohai.raw$Recruitment[!is.na(bio.Bohai.raw$Recruitment)], silent = TRUE) %>%
                  lapply(function(df) mutate(df,species = "YBohai_Sea", variable
                                             = "Recruitment_ind.", method = "normal")),
                do_univariate_analysis(bio.Yel$B[!is.na(bio.Yel$B)], predict_diff = F, 
                                       silent = TRUE) %>%
                  lapply(function(df) mutate(df,species = "Yellow_Sea", 
                                             variable = "B", method = "normal")),
                do_univariate_analysis(bio.Bohai.VAST$SSB[!is.na(bio.Bohai.VAST$SSB)], predict_diff = F
                                       , silent = TRUE) %>%
                  lapply(function(df) mutate(df,species = "YBohai_Sea", variable
                                             = "SSB", method = "normal")),
                do_univariate_analysis(bio.Bohai.raw$B[!is.na(bio.Bohai.raw$B)], predict_diff = F
                                       , silent = TRUE) %>%
                  lapply(function(df) mutate(df,species = "YBohai_Sea", variable
                                             = "B", method = "normal")))

#如果不能成功 就把predict_diff = T 改为predict_diff = F

#do_univariate_analysis(bio.Yel$C, silent = TRUE)
#do_univariate_analysis(bio.Yel$C[!is.na(bio.Yel$C)], silent = TRUE)

####调整
#bio.Bohai.VAST$SSB1<-bio.Bohai.VAST$SSB
#bio.Bohai.VAST$SSB1[5]<-bio.Bohai.VAST$SSB[6]
#bio.Bohai.raw$B1<-bio.Bohai.raw$B
#bio.Bohai.raw$B1[5]<-bio.Bohai.raw$B[6]

results <- list(do_univariate_analysis(bio.Yel$C[!is.na(bio.Yel$C)], silent = TRUE) %>%
                  lapply(function(df) mutate(df,species = "Yellow_Sea",
                                             variable = "C", method = "normal")),
                do_univariate_analysis(bio.Bohai.VAST$Recruitment[!is.na(bio.Bohai.VAST$Recruitment)], silent = TRUE) %>%
                  lapply(function(df) mutate(df,species = "YBohai_Sea", variable
                                             = "Recruitment_bio", method = "normal")),
                do_univariate_analysis(bio.Bohai.raw$Recruitment[!is.na(bio.Bohai.raw$Recruitment)], silent = TRUE) %>%
                  lapply(function(df) mutate(df,species = "YBohai_Sea", variable
                                             = "Recruitment_ind.", method = "normal")),
                do_univariate_analysis(bio.Yel$B[!is.na(bio.Yel$B)], predict_diff = F, 
                                       silent = TRUE) %>%
                  lapply(function(df) mutate(df,species = "Yellow_Sea", 
                                             variable = "B", method = "normal")),
                do_univariate_analysis(bio.Bohai.VAST$SSB[!is.na(bio.Bohai.VAST$SSB)], predict_diff = F
                                       , silent = TRUE) %>%
                  lapply(function(df) mutate(df,species = "YBohai_Sea", variable
                                             = "SSB", method = "normal")),
                do_univariate_analysis(bio.Bohai.raw$B[!is.na(bio.Bohai.raw$B)], predict_diff = F
                                       , silent = TRUE) %>%
                  lapply(function(df) mutate(df,species = "YBohai_Sea", variable
                                             = "B", method = "normal")))

####调整结束









results <- list(
  simplex = do.call(rbind, lapply(results,function(L) L$simplex)),
  smap = do.call(rbind,lapply(results, function(L) {
    L$smap %>% mutate(drho = as.numeric(rho) - first(rho),
                      dmae = as.numeric(mae) - first(mae),
                      drmse = as.numeric(rmse) - first(rmse))
  }))
)
names(results)
results$simplex
results$smap
save(results,file="./results.Rdata")






do_null_s_map <- function(x, n.surr = 100, predict_diff = FALSE, n.core = 1, ...)
{
  do.call(rbind,
          mclapply(1:n.surr,function(idx) {
            x.surr <- make_surrogate_data(x[!is.na(x)],method='ebisuzaki')
            df.out <- do_univariate_analysis(x.surr,predict_diff = predict_diff, ...)
            return(mutate(df.out$smap,idx=idx))
          }, mc.cores = n.core)
  )
}

E.list = 1:5
#修改
do_null_s_map <- function(x, n.surr = 500, predict_diff = FALSE, n.core = 1, ...)
{
  do.call(rbind,
          mclapply(1:n.surr,function(idx) {
            x.surr <- make_surrogate_data(x[!is.na(x)],num_surr = n.surr,method='ebisuzaki')
            df.out <- do_univariate_analysis(x.surr[,idx],predict_diff = predict_diff, ...)
            return(mutate(df.out$smap,idx=idx))
          }, mc.cores = n.core)
  )
}

#E.list = 1:5
#lib = c(1, NROW(na.omit(x))-max(E)-0),




number_of_surrogates <- 500
results.null <- bind_rows(do_null_s_map(bio.Yel$C, silent = TRUE, n.surr = number_of_surrogates) %>%
                            mutate(species = "Yellow_Sea", variable = "C", method ="normal"),
                          do_null_s_map(bio.Bohai.VAST$Recruitment, silent = TRUE, n.surr = number_of_surrogates) %>%
                            mutate(species = "YBohai_Sea", variable = "Recruitment_bio",
                                   method = "normal"),
                          do_null_s_map(bio.Bohai.raw$Recruitment, silent = TRUE, n.surr = number_of_surrogates) %>%
                            mutate(species = "YBohai_Sea", variable = "Recruitment_ind.", method = "normal"),
                          do_null_s_map(bio.Yel$B, predict_diff = F, silent = TRUE, n.surr = number_of_surrogates) %>%
                            mutate(species = "Yellow_Sea", variable = "B", method ="normal"),
                          do_null_s_map(bio.Bohai.VAST$SSB, predict_diff = F, silent
                                        = TRUE, n.surr = number_of_surrogates) %>%
                            mutate(species = "YBohai_Sea", variable = "SSB", method = "normal"),
                          do_null_s_map(bio.Bohai.raw$B, predict_diff = F, silent
                                        = TRUE, n.surr = number_of_surrogates) %>%
                            mutate(species = "YBohai_Sea", variable = "B", method = "normal"))



results.null <- results.null %>% filter(theta==0) %>%
  rename(rho0=rho,mae0=mae,rmse0=rmse) %>%
  select(idx,species,variable,method,rho0,mae0,rmse0) %>%
  right_join(results.null,by=c("idx","species","variable","method") ) %>%
  mutate(drho=as.numeric(rho)-as.numeric(rho0),
         dmae=as.numeric(mae)-as.numeric(mae0),
         drmse=as.numeric(rmse)-as.numeric(rmse0))


save(results.null,file="./univariate_ebi_null.Rdata")



#Plots
load("univariate_ebi_null.Rdata")
plot_rows <- list( c("Yellow_Sea","C","normal"),
                   c("YBohai_Sea","Recruitment_ind.","normal"),
                   c("YBohai_Sea","Recruitment_bio","normal"),
                   c("Yellow_Sea","B","normal"),
                   c("YBohai_Sea","B","normal"),
                   c("YBohai_Sea","SSB","normal") )
g_rho <- vector(mode='list')
g_mae <- vector(mode='list')








#Plot
#shows Simplex, S-map,
results$simplex$rho<-as.numeric(results$simplex$rho)
results$smap$rho<-as.numeric(results$smap$rho)


for(rowdex in 1:length(plot_rows)){
  row.species <- plot_rows[[rowdex]][1]
  row.variable <- plot_rows[[rowdex]][2]
  row.method <- plot_rows[[rowdex]][3]
  h_A <- results$simplex %>%
    filter(species==row.species,variable==row.variable,method==row.method) %>%
  ggplot(aes(x=E,y=rho)) + geom_line(lwd = 1,col="blue") +
    coord_cartesian(ylim= c(0,1)) +
    xlab(expression("Embedding \nDimension (E)")) +
    ylab(expression(paste("Forecast \nSkill (",rho,")"))) +
    theme_bw()
  h_B1 <- results$smap %>%
    filter(species==row.species,variable==row.variable,method==row.method) %>%
  ggplot(aes(x=theta,y=rho)) + geom_line(lwd = 1,col="blue") +
    xlab(expression(paste("Nonlinearity (",theta,")"))) +
    ylab(expression(paste("Forecast \nSkill (",rho,")"))) +
    theme_bw()
  h_C1 <- results$smap %>%
    filter(species==row.species,variable==row.variable,method==row.method) %>%
  ggplot(aes(x=theta,y=drho)) + geom_line(lwd = 1,col="blue") +
    coord_cartesian(ylim= c(-.1,.7)) +
    xlab(expression(paste("Nonlinearity (",theta,")"))) +
    ylab(expression(paste("Nonlinear \nImprovement (",Delta*rho,")"))) +
    theme_bw()
  row.uni.null <- results.null %>%
    filter(species==row.species,variable==row.variable,method==row.method)
  row.uni.null$rho<-as.numeric(row.uni.null$rho)#我加的
  h_A <- h_A +
    theme(plot.margin=unit(c(0.5,0.5,2.0,1.0), "lines"),
          axis.title.x = element_text(vjust = 1,hjust=0.5))
  h_B <- h_B1 + stat_summary(data=row.uni.null, geom="ribbon", fill="grey80", alpha = .5,
                             fun.ymin = function(x) quantile(x, 0.05),
                             fun.ymax = function(x) quantile(x, 0.95)) +
    coord_cartesian(ylim= c(0,1), xlim = c(0,5)) +
    theme_bw() +
    theme(plot.margin=unit(c(0.5,0.5,2.0,1.0), "lines"))
  h_C <- h_C1 + stat_summary(data=row.uni.null, geom="ribbon", fill="grey80", alpha = .5,
                             fun.ymin = function(x) quantile(x, 0.05),
                             fun.ymax = function(x) quantile(x, 0.95)) +
    coord_cartesian(ylim = c(-.1,.7), xlim=c(0,5)) +
    theme_bw() +
    theme(plot.margin=unit(c(0.5,0.5,2.0,1.0), "lines"))
  h_C$layers <- rev(h_C$layers)
  g_rho[[rowdex]] <- grid.arrange(h_A, h_B, h_C, nrow=1,
                                  top=do.call(paste,as.list(plot_rows[[rowdex]])))
}

do.call(grid.arrange,c(g_rho,ncol=1))





#second set of figures that show results using (normalized) MAE to quantify
#forecast skill (or rather forecast error) instead of Pearson’s correlation
results$simplex$mae<-as.numeric(results$simplex$mae)
results$smap$mae<-as.numeric(results$smap$mae)
results$simplex$const_pred_mae<-as.numeric(results$simplex$const_pred_mae)
for(rowdex in 1:length(plot_rows)){
  row.species <- plot_rows[[rowdex]][1]
  row.variable <- plot_rows[[rowdex]][2]
  row.method <- plot_rows[[rowdex]][3]
  h_A <- results$simplex %>%
    filter(species==row.species,variable==row.variable,method==row.method) %>%
  ggplot(aes(x=E,y=mae)) + geom_line(lwd = 1,col='blue') +
    geom_line(aes(y=const_pred_mae),lty=2) +
    scale_y_reverse() +
    coord_cartesian(ylim= c(1.25,.25)) +
    theme_bw()
  h_B <- results$smap %>%
    filter(species==row.species,variable==row.variable,method==row.method) %>%
  ggplot(aes(x=theta,y=mae)) + geom_line(lwd = 1,col='blue') +
    scale_y_reverse() +
    coord_cartesian(ylim= c(1.25,.25)) +
    theme_bw()
  h_C1 <- results$smap %>%
    filter(species==row.species,variable==row.variable,method==row.method) %>%
  ggplot(aes(x=theta,y=dmae)) + geom_line(lwd = 1,col='blue') +
    scale_y_reverse() +
    coord_cartesian(ylim= c(-.6,.6)) +
    theme_bw()
  row.uni.null <- results.null %>%
    filter(species==row.species,variable==row.variable,method==row.method)
  row.uni.null$mae<-as.numeric(row.uni.null$mae)#我加的
  row.uni.null$const_pred_mae<-as.numeric(row.uni.null$const_pred_mae)#我加的
  h_C <- h_C1 + stat_summary(data=row.uni.null, geom="ribbon", fill="grey70",
                             fun.ymin = function(x) quantile(x, 0.05),
                             fun.ymax = function(x) quantile(x, 0.95)) +
    theme_bw()
  h_C$layers <- rev(h_C$layers)
  g_mae[[rowdex]] <- grid.arrange(h_A, h_B, h_C, nrow=1,
                                  top=do.call(paste,as.list(plot_rows[[rowdex]])))
}



grid.arrange(grobs=g_mae,ncol=1)





#CCM
do_ccm_runs <- function(block, ccm_runs,
                        E_list = 1:5, tp_fit = -1, tp_pred = 0,
                        lib_sizes = seq(from = 1, to = NROW(block), by = 1)
                        ,
                        random_libs = TRUE, replace = FALSE,
                        silent = TRUE, ...)
{
  return(do.call(rbind, lapply(1:NROW(ccm_runs), function(i) {
    out.temp <- do.call(rbind,
                        lapply(E_list, function(E) {
                          ccm(block, lib_column = ccm_runs$from[i], target_column =
                                ccm_runs$to[i],
                              E = E, random_libs = FALSE, lib_sizes = NROW(block), 
                              tp = tp_fit, ...,
                              silent = silent)
                        }))
    E.star <- out.temp[which.max(out.temp$rho), 'E']
    ccm(block, lib_column = ccm_runs$from[i],
        target_column = ccm_runs$to[i],
        E = E.star,
        lib_sizes = lib_sizes,
        random_libs = random_libs, replace = replace,
        tp = tp_pred, silent = silent, ...)
  })))
}

#我的修改
#lib_sizes = seq(from = max(E), to = NROW(block)-4, by = 1),

do_ccm_runs <- function(block, ccm_runs,
                        E_list = 1:5, tp_fit = -1, tp_pred = 0,
                        random_libs = TRUE, replace = FALSE,
                        silent = TRUE, ...)
{
  return(do.call(rbind, lapply(1:NROW(ccm_runs), function(i) {
    out.temp <- do.call(rbind,
                        lapply(E_list, function(E) {
                          ccm(block, lib=ccm_runs[i,],lib_column = 1, target_column =2,
                              E = E, random_libs = FALSE, lib_sizes = NROW(block), 
                              tp = tp_fit, ...,stats_only =F,#num_samples = 100,
                              silent = silent)$CCM1_PredictStat
                          #$CCM2_PredictStat
                        }))
    E.star <- out.temp[which.max(out.temp$rho), 'E']
    ccm(block, lib=ccm_runs[i,],lib_column = 1,
        target_column = 2,
        E = E.star,
        lib_sizes = seq(from = E.star+1, to = NROW(block)-5, by = 1),
        random_libs = random_libs, replace = replace,
        tp = tp_pred,  ...,silent = silent,#num_samples = 100,
        stats_only =F,
        )$CCM1_PredictStat  #forward mapped prediction
    #$CCM2_PredictStat    #reverse mapped prediction
    #$LibMeans
  })))
}

?ccm

# set up which effects to test using CCM
vars <- c("C", "B")
ccm_runs_ex1 <- expand.grid(from = vars, to = vars)
# don't run CCM from a variable to itself
ccm_runs_ex1 <- ccm_runs_ex1[ccm_runs_ex1$from != ccm_runs_ex1$to,]
vars1 <- c("Recruitment", "SSB ")
ccm_runs_ex11 <- expand.grid(from = vars1, to = vars1)
# don't run CCM from a variable to itself
ccm_runs_ex11 <- ccm_runs_ex11[ccm_runs_ex11$from != ccm_runs_ex11$to,]
vars2 <- c("Recruitment", "B ")
ccm_runs_ex12 <- expand.grid(from = vars2, to = vars2)
# don't run CCM from a variable to itself
ccm_runs_ex12 <- ccm_runs_ex12[ccm_runs_ex12$from != ccm_runs_ex12$to,]

results_CCM_ex1 <- do.call(rbind,
                           list(do_ccm_runs(bio.Yel[!is.na(bio.Yel$C),c("C", "B")], 
                                            ccm_runs_ex1[2,], silent = TRUE)
                                %>%
                                  mutate(species = "Yellow_Sea",label="B&C"),
                                do_ccm_runs(bio.Yel[!is.na(bio.Yel$C),c("B","C")], 
                                            ccm_runs_ex1[1,], silent = TRUE)
                                %>%
                                  mutate(species = "Yellow_Sea",label="B&C"),
                                do_ccm_runs(bio.Bohai.VAST[!is.na(bio.Bohai.VAST$Recruitment&bio.Bohai.VAST$SSB),c("Recruitment","SSB")], 
                                            ccm_runs_ex11[2,], silent = 
                                            TRUE) %>%
                                  mutate(species = "YBohai_Sea",label="SSB&R"
                                  ),
                                do_ccm_runs(bio.Bohai.VAST[!is.na(bio.Bohai.VAST$Recruitment&bio.Bohai.VAST$SSB),c("SSB","Recruitment")], 
                                            ccm_runs_ex11[1,], silent = 
                                              TRUE) %>%
                                  mutate(species = "YBohai_Sea",label="SSB&R"
                                  ),
                                do_ccm_runs(bio.Bohai.raw[!is.na(bio.Bohai.raw$Recruitment&bio.Bohai.raw$B),c("Recruitment","B")],
                                            ccm_runs_ex12[2,], silent = 
                                              TRUE) %>%
                                  mutate(species = "YBohai_Sea", label="JABBA_B&R"),
                                do_ccm_runs(bio.Bohai.raw[!is.na(bio.Bohai.raw$Recruitment&bio.Bohai.raw$B),c("B","Recruitment")],
                                            ccm_runs_ex12[1,], silent = 
                                              TRUE) %>%
                                  mutate(species = "YBohai_Sea", label="JABBA_B&R"))
)



#results_CCM_ex1
names(results_CCM_ex1)
write.csv(results_CCM_ex1,file = "results_CCM_ex1_causalitySSB_RFig3.csv")

results_CCM_ex1$lib_column<-results_CCM_ex1$lib
results_CCM_ex1$target_column<-results_CCM_ex1$target
results_CCM_ex1$lib_size<-results_CCM_ex1$LibSize
results_CCM_ex1$mae<-results_CCM_ex1$MAE
results_CCM_ex1$rmse<-results_CCM_ex1$RMSE

#Plot
df.plot <- results_CCM_ex1 %>%
  mutate(experiment = interaction(species,label, sep="; ")) %>%
  mutate(ccm_label = interaction(lib_column,target_column,sep="->")) %>%
  select(species,label,experiment,ccm_label,lib_size,rho,mae,rmse) %>%
  group_by(species,label,experiment,ccm_label,lib_size) %>%
  summarise_at(vars(rho,mae,rmse),funs(pmax(0,mean(.,na.rm=TRUE))))

labs_panels <- unique(df.plot$experiment)
labs_panels
labs_panels <- c("Yellow_Sea; B&C","YBohai_Sea; JABBA_B&R","YBohai_Sea; SSB&R")
n_panels <- length(labs_panels)
n_panels
h_panels <- vector(mode="list",n_panels)
h_panels



for(i_panel in 1:n_panels){
  h_panels[[i_panel]] <- df.plot %>%
    filter(experiment == labs_panels[[i_panel]]) %>%
    ggplot(aes(x=lib_size,y=rho,color=ccm_label)) + geom_line(lwd=1.5) +
    ylim(c(0,1.0)) +
    labs(title=labs_panels[[i_panel]],x='library size',y=expression(paste('crossmap
skill (',rho,')')),col="") +
    theme_bw() +
    theme(legend.position = "bottom")
} # i_pannel



g_legend<-function(a.gplot){
  g <- ggplotGrob(a.gplot + theme(legend.position = "bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  return(legend)}

mylegend<-g_legend(a.gplot=h_panels[[2]])
mylegend
lheight <- sum(mylegend$height)
lheight

grid.arrange(do.call(arrangeGrob, c(lapply(h_panels,
                                           function(h_i) h_i + theme(legend.position="none"
                                           )),
                                    nrow=1)),
             mylegend, nrow=2,heights = unit.c(unit(1, "npc") - lheight, lheight)
)



############################
#######################
#multiple lags
do_ccm_lag_analysis <- function(block, ccm_runs, lag_list = seq(-4,4,by=1), ...){
  do.call(rbind,
          lapply( lag_list, function(lag){
            do_ccm_runs1(block, ccm_runs, tp_fit = (lag-1),tp_pred = lag,
                        lib_sizes = NROW(block),random_libs = FALSE, 
                        num_samples = 1, ...)
          }))}

do_ccm_lag_analysis2 <- function(block, ccm_runs, lag_list = seq(-4,4,by=1), ...){
  do.call(rbind,
          lapply( lag_list, function(lag){
            do_ccm_runs2(block, ccm_runs, tp_fit = (lag-1),tp_pred = lag,
                         lib_sizes = NROW(block),random_libs = FALSE, 
                         num_samples = 1, ...)
          }))}



do_ccm_runs1 <- function(block, ccm_runs,
                        E_list = 1:5, tp_fit = -1, tp_pred = 0,
                        lib_sizes = seq(from = 2, to = NROW(block)-0, by = 1)
                        ,
                        random_libs = TRUE, replace = FALSE,
                        silent = TRUE, ...)
{
  return(do.call(rbind, lapply(1:NROW(ccm_runs), function(i) {
    out.temp <- do.call(rbind,
                        lapply(E_list, function(E) {
                          ccm(block, lib=ccm_runs[i,],lib_column = 1, target_column =2,
                              E = E, random_libs = FALSE, lib_sizes = NROW(block), 
                              tp = tp_fit, ...,stats_only =F,#num_samples = 100,
                              silent = silent)$CCM1_PredictStat
                          #$CCM2_PredictStat
                        }))
    E.star <- out.temp[which.max(out.temp$rho), 'E']
    
      ccm(block, lib=ccm_runs[i,],lib_column = 1,
        target_column = 2,
        E = E.star,
        lib_sizes = lib_sizes,
        random_libs = random_libs, replace = replace,
        tp = tp_pred,  ...,silent = silent,#num_samples = 100,
        stats_only =F,
    )$CCM1_PredictStat  #forward mapped prediction
    #$CCM2_PredictStat    #reverse mapped prediction
    #$LibMeans#
    
  })))
}

do_ccm_runs2 <- function(block, ccm_runs,
                         E_list = 1:5, tp_fit = -1, tp_pred = 0,
                         lib_sizes = seq(from = 2, to = NROW(block)-0, by = 1)
                         ,
                         random_libs = TRUE, replace = FALSE,
                         silent = TRUE, ...)
{
  return(do.call(rbind, lapply(1:NROW(ccm_runs), function(i) {
    out.temp <- do.call(rbind,
                        lapply(E_list, function(E) {
                          ccm(block, lib=ccm_runs[i,],lib_column = 1, target_column =2,
                              E = E, random_libs = FALSE, lib_sizes = NROW(block), 
                              tp = tp_fit, ...,stats_only =F,#num_samples = 100,
                              silent = silent)$CCM1_PredictStat
                          #$CCM2_PredictStat
                        }))
    E.star <- out.temp[which.max(out.temp$rho), 'E']
    
    ccm(block, lib=ccm_runs[i,],lib_column = 1,
        target_column = 2,
        E = E.star,
        lib_sizes = lib_sizes,
        random_libs = random_libs, replace = replace,
        tp = tp_pred,  ...,silent = silent,#num_samples = 100,
        stats_only =F,
    )$LibMeans#$CCM1_PredictStat  #forward mapped prediction
    #$CCM2_PredictStat    #reverse mapped prediction
    #$LibMeans#
    
  })))
}



results_lags_ex1 <- do.call(rbind,list(do_ccm_lag_analysis(bio.Yel[!is.na(bio.Yel$C),c("C", "B")],
                                                           ccm_runs_ex1[2,], lag_list = seq(-3,3,by
                                                                                        =1), silent = TRUE) %>%
                                         mutate(species = "Yellow_Sea",label="B&C"),
                                       do_ccm_lag_analysis(bio.Yel[!is.na(bio.Yel$C),c("B","C")],
                                                           ccm_runs_ex1[1,], lag_list = seq(-3,3,by
                                                                                        =1), silent = TRUE) %>%
                                         mutate(species = "Yellow_Sea",label="B&C"),
                                       do_ccm_lag_analysis(bio.Bohai.raw[!is.na(bio.Bohai.raw$Recruitment),c("Recruitment","B")],
                                                           ccm_runs_ex11[2,], lag_list = seq(-3,3,by
                                                                                        =1), silent = TRUE) %>%
                                         mutate(species = "YBohai_Sea",label="JABBA_B&R"),
                                       do_ccm_lag_analysis(bio.Bohai.raw[!is.na(bio.Bohai.raw$Recruitment),c("B","Recruitment")],
                                                           ccm_runs_ex11[1,], lag_list = seq(-3,3,by
                                                                                        =1), silent = TRUE) %>%
                                         mutate(species = "YBohai_Sea",label="JABBA_B&R"),
                                       do_ccm_lag_analysis(bio.Bohai.VAST[!is.na(bio.Bohai.VAST$Recruitment),c("Recruitment","SSB")],
                                                           ccm_runs_ex11[2,], lag_list = seq(-3,3,by
                                                                                             =1), silent = TRUE) %>%
                                         mutate(species = "YBohai_Sea",label="SSB&R"),
                                       do_ccm_lag_analysis(bio.Bohai.VAST[!is.na(bio.Bohai.VAST$Recruitment),c("SSB","Recruitment")],
                                                           ccm_runs_ex11[1,], lag_list = seq(-3,3,by
                                                                                             =1), silent = TRUE) %>%
                                         mutate(species = "YBohai_Sea",label="SSB&R")))

results_lags_ex2 <- do.call(rbind,list(do_ccm_lag_analysis2(bio.Yel[!is.na(bio.Yel$C),c("C", "B")],
                                                           ccm_runs_ex1[2,], lag_list = seq(-3,3,by
                                                                                        =1), silent = TRUE)[,c(-2:-3)] %>%
                                         mutate(species = "Yellow_Sea",label="B&C"),
                                       do_ccm_lag_analysis2(bio.Yel[!is.na(bio.Yel$C),c("B","C")],
                                                           ccm_runs_ex1[1,], lag_list = seq(-3,3,by
                                                                                        =1), silent = TRUE)[,c(-2:-3)] %>%
                                         mutate(species = "Yellow_Sea",label="B&C"),
                                       do_ccm_lag_analysis2(bio.Bohai.raw[!is.na(bio.Bohai.raw$Recruitment),c("Recruitment","B")],
                                                           ccm_runs_ex11[2,], lag_list = seq(-3,3,by
                                                                                        =1), silent = TRUE)[,c(-2:-3)] %>%
                                         mutate(species = "YBohai_Sea",label="JABBA_B&R"),
                                       do_ccm_lag_analysis2(bio.Bohai.raw[!is.na(bio.Bohai.raw$Recruitment),c("B","Recruitment")],
                                                           ccm_runs_ex11[1,], lag_list = seq(-3,3,by
                                                                                        =1), silent = TRUE)[,c(-2:-3)] %>%
                                         mutate(species = "YBohai_Sea",label="JABBA_B&R"),
                                       do_ccm_lag_analysis2(bio.Bohai.VAST[!is.na(bio.Bohai.VAST$Recruitment),c("Recruitment","SSB")],
                                                            ccm_runs_ex11[2,], lag_list = seq(-3,3,by
                                                                                              =1), silent = TRUE)[,c(-2:-3)] %>%
                                         mutate(species = "YBohai_Sea",label="SSB&R"),
                                       do_ccm_lag_analysis2(bio.Bohai.VAST[!is.na(bio.Bohai.VAST$Recruitment),c("SSB","Recruitment")],
                                                            ccm_runs_ex11[1,], lag_list = seq(-3,3,by
                                                                                              =1), silent = TRUE)[,c(-2:-3)] %>%
                                         mutate(species = "YBohai_Sea",label="SSB&R")))




names(results_lags_ex1)
names(results_lags_ex2)
names(results_lags_ex1)
results_lags_ex1<-cbind(results_lags_ex1,results_lags_ex2[,4])
names(results_lags_ex1)
results_lags_ex1$tp<-results_lags_ex1$`results_lags_ex2[, 4]`
names(results_lags_ex1)
results_lags_ex1<-results_lags_ex1%>%mutate(experiment = interaction(species,label, sep="; "))
#plot the ccm skill as a function of time lag.
h_lags <- vector(mode="list",3)
L_species <- c("Yellow_Sea; B&C","YBohai_Sea; JABBA_B&R","YBohai_Sea; SSB&R")
L_var <- c("JAI","LPUE")


results_lags_ex1$lib_column<-results_lags_ex1$lib
results_lags_ex1$target_column<-results_lags_ex1$target
names(results_lags_ex1)
results_lags_ex1$rho[results_lags_ex1$rho<0]=0


for(i_species in 1:length(L_species)){
  species_i <- L_species[[i_species]]
  df.i <- results_lags_ex1 %>%
    filter(experiment==species_i) %>%
    mutate(ccm_label = interaction(lib_column,target_column,sep="->"))
  phys_i <- df.i$target_colum[1]
  title_i <- species_i
  h_lags[[i_species]] <- ggplot(df.i,aes(x=tp,y=pmax(0,rho),color=ccm_label)) +
    geom_line(lwd=1.5) +
    labs(title=title_i,x='prediction lag (yrs)',y='cross-map skill (rho)') +
    ylim(c(0,1.0)) +
    theme_bw() +
    labs(col="") +
    theme(legend.position = "bottom")
}



g_legend<-function(a.gplot){
  g <- ggplotGrob(a.gplot + theme(legend.position = "bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  return(legend)}




mylegend<-g_legend(a.gplot=h_lags[[1]])
lheight <- sum(mylegend$height)*1.5
lheight
grid.arrange(do.call(arrangeGrob, c(lapply(h_lags,
                                           function(h_i) h_i + theme(legend.position="none"
                                           )),
                                    nrow=1)),
             mylegend, nrow=2,heights = unit.c(unit(1, "npc") - lheight, lheight)
)





#








################################
##########################
#4.2 Environmental Drivers
#We next use the same basic do_ccm_runs() to examine possible environmental drivers. We
#can use the same code, but with a different set of “from” and “to” variables.
#lib_sizes = seq(from = max(E), to = NROW(block)-0, by = 1),


do_ccm_runs3 <- function(block, ccm_runs,
                         E_list = 1:5, tp_fit = -1, tp_pred = 0,
                         random_libs = TRUE, replace = FALSE,
                         silent = TRUE, ...)
{
  return(do.call(rbind, lapply(1:NROW(ccm_runs), function(i) {
    out.temp <- do.call(rbind,
                        lapply(E_list, function(E) {
                          ccm(block, lib=ccm_runs[i,],lib_column = 1, target_column =2,
                              E = E, random_libs = FALSE, lib_sizes = NROW(block), 
                              tp = tp_fit, ...,stats_only =F,#num_samples = 100,
                              silent = silent)$CCM1_PredictStat
                          #$CCM2_PredictStat
                        }))
    E.star <- out.temp[which.max(out.temp$rho), 'E']
    
    ccm(block, lib=ccm_runs[i,],lib_column = 1,
        target_column = 2,
        E = E.star,
        lib_sizes = seq(from = E.star+1, to = NROW(block)-5, by = 1),
        random_libs = random_libs, replace = replace,
        tp = tp_pred,  ...,silent = silent,#num_samples = 100,
        stats_only =F,
    )$CCM1_PredictStat  #forward mapped prediction
    #$CCM2_PredictStat    #reverse mapped prediction
    #$LibMeans#
    
  })))
}






ccm_runs_ex2 <- expand.grid(from = c("Recruitment","SSB"), 
                            to = c("Fishing_pressure",
                                   "sst_spawn",
                                   "PDO_spawn",
                                   "NPGO_spawn",
                                   "NPA_spawn",
                                   "WP_spawn",
                                   "NAO_spawn",
                                   "SOI_spawn",
                                   "Nino34_spawn",
                                   "NOI_spawn",
                                   "NPI_spawn",
                                   "AOI_spawn",
                                   "AMO_spawn","summer_fishing_moratorium"))

ccm_runs_ex2

ccm_runs_ex2_nursery <- expand.grid(from = c("Recruitment","SSB"), 
                            to = c("Fishing_pressure",
                                   "sst_nursery",
                                   "PDO_nursery",
                                   "NPGO_nursery",
                                   "NPA_nursery",
                                   "WP_nursery",
                                   "NAO_nursery",
                                   "SOI_nursery",
                                   "Nino34_nursery",
                                   "NOI_nursery",
                                   "NPI_nursery",
                                   "AOI_nursery",
                                   "AMO_nursery","summer_fishing_moratorium"))

ccm_runs_ex2_nursery

ccm_runs_ex2_winter <- expand.grid(from = c("Recruitment","SSB"), 
                                    to = c("Fishing_pressure",
                                           "sst_winter",
                                           "PDO_winter",
                                           "NPGO_winter",
                                           "NPA_winter",
                                           "WP_winter",
                                           "NAO_winter",
                                           "SOI_winter",
                                           "Nino34_winter",
                                           "NOI_winter",
                                           "NPI_winter",
                                           "AOI_winter",
                                           "AMO_winter","summer_fishing_moratorium"))

ccm_runs_ex2_winter

block.temp <- full_join(bio.Yel,phys.Yel,by="Year")
block.temp1 <- full_join(bio.Bohai.raw,phys.Bohai,by="Year")
block.temp2 <- full_join(bio.Bohai.VAST,phys.Bohai,by="Year")
block.temp$SSB<-block.temp$B
block.temp$Recruitment<-block.temp$C
block.temp1$SSB<-block.temp1$B
block.temp1$Recruitment<-block.temp1$Recruitment
block.temp2$SSB<-block.temp2$SSB
block.temp2$Recruitment<-block.temp2$Recruitment

#block.temp
names(block.temp)
names(block.temp1)
names(block.temp2)

results_CCM_ex2 <- do.call(rbind,list(
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#1
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$Fishing_pressure),c("SSB","Fishing_pressure")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#2
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$sst_spawn),c("Recruitment","sst_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#3
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$sst_spawn),c("SSB","sst_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#4
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_spawn),c("Recruitment","PDO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#5
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$PDO_spawn),c("SSB","PDO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#6
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$NPGO_spawn),c("Recruitment","NPGO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#7
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$NPGO_spawn),c("SSB","NPGO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#8
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$NPA_spawn),c("Recruitment","NPA_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#9
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$NPA_spawn),c("SSB","NPA_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#10
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$WP_spawn),c("Recruitment","WP_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#11
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$WP_spawn),c("SSB","WP_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#12
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$NAO_spawn),c("Recruitment","NAO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#13
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$NAO_spawn),c("SSB","NAO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#14
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$SOI_spawn),c("Recruitment","SOI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#15
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$SOI_spawn),c("SSB","SOI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#16
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$Nino34_spawn),c("Recruitment","Nino34_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#17
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$Nino34_spawn),c("SSB","Nino34_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#18
  # do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_spawn),c("Recruitment","PDO_spawn")], 
  #             ccm_runs_ex2, silent = TRUE) %>%
  # mutate(species = "Yellow_Sea",label="B&C"),#19
  #do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$PDO_spawn),c("SSB","PDO_spawn")], 
  #            ccm_runs_ex2, silent = TRUE) %>%
  #mutate(species = "Yellow_Sea",label="B&C"),#20
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$NOI_spawn),c("Recruitment","NOI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#21
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$NOI_spawn),c("SSB","NOI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#22
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$NPI_spawn),c("Recruitment","NPI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#23
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$NPI_spawn),c("SSB","NPI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#24
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$AOI_spawn),c("Recruitment","AOI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#25
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$AOI_spawn),c("SSB","AOI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#26
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$AMO_spawn),c("Recruitment","AMO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#27
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$AMO_spawn),c("SSB","AMO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#28
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$summer_fishing_moratorium),c("Recruitment","summer_fishing_moratorium")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#29
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$summer_fishing_moratorium),c("SSB","summer_fishing_moratorium")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#30
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#1
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$Fishing_pressure),c("SSB","Fishing_pressure")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#2
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$sst_spawn),c("Recruitment","sst_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#3
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$sst_spawn),c("SSB","sst_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#4
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_spawn),c("Recruitment","PDO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#5
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_spawn),c("SSB","PDO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#6
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPGO_spawn),c("Recruitment","NPGO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#7
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPGO_spawn),c("SSB","NPGO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#8
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPA_spawn),c("Recruitment","NPA_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#9
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPA_spawn),c("SSB","NPA_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#10
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$WP_spawn),c("Recruitment","WP_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#11
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$WP_spawn),c("SSB","WP_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#12
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NAO_spawn),c("Recruitment","NAO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#13
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$NAO_spawn),c("SSB","NAO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#14
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$SOI_spawn),c("Recruitment","SOI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#15
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$SOI_spawn),c("SSB","SOI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#16
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Nino34_spawn),c("Recruitment","Nino34_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#17
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$Nino34_spawn),c("SSB","Nino34_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#18
  #do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_spawn),c("Recruitment","PDO_spawn")], 
  #            ccm_runs_ex2, silent = TRUE) %>%
  #mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  #do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_spawn),c("SSB","PDO_spawn")], 
  #            ccm_runs_ex2, silent = TRUE) %>%
  #mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NOI_spawn),c("Recruitment","NOI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$NOI_spawn),c("SSB","NOI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPI_spawn),c("Recruitment","NPI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#21
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPI_spawn),c("SSB","NPI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#22
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AOI_spawn),c("Recruitment","AOI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#23
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$AOI_spawn),c("SSB","AOI_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#24
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AMO_spawn),c("Recruitment","AMO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#25
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$AMO_spawn),c("SSB","AMO_spawn")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#26
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$summer_fishing_moratorium),c("Recruitment","summer_fishing_moratorium")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#27
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$summer_fishing_moratorium),c("SSB","summer_fishing_moratorium")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R")#28
))

save(results_CCM_ex2,file='./results_CCM_ex2.Rdata')


#[!is.na(block.temp$JAI),c("JAI","LPUE")]
#[,c("JAI","Fishing_pressure")]
#[,c("LPUE","Fishing_pressure")]
#[,c("JAI","SST")]
#[,c("LPUE","SST")]
#[,c("JAI","PDO")]
#[,c("LPUE","PDO")]











#4.2.0.1 NULL Analysis without Lags
#We now develop null distributions for the environmental CCM analysis. Here we use
#Ebisuzaki surrogates, which preserves the distribution of values of the time series, but
#destroys any dynamic relationship with the real data. There is a slight complication, which is
#that the code to generate Ebisuzaki surrogates in the ‘rEDM’ package uses fft() and cannot
#deal with NAs in the data. Thus we need to write a quick intermediate function to ignore the
#NAs.

make_surrogate_ignoreNA <- function(y,number_of_surrogates=1){
  I_good <- which(is.finite(y))
  Y <- matrix(NA,nrow = length(y),ncol=number_of_surrogates)
  Y[I_good,] <- make_surrogate_data(y[I_good],num_surr = number_of_surrogates,method='ebisuzaki')
  return(Y)}







#For ease, we define a function that repeats the do_ccm_runs 
#over a given number ofsurrogate realizations.
do_null_ccm_runs <- function(block, ccm_runs, n.surr = 100, n.core = 1, ...)
{
  do.call(rbind,
          mclapply(1:n.surr,function(idx) {
            # If this code is adapted to do ccm runs where "from" and "to" ar
            #en't disjoint,
# this next bit of code won't work quite as intended.
#放弃它   #
            block <- block %>%mutate_at(as.character(unique(ccm_runs[,'to'])),funs(make_surrogate_ignoreNA))
            df.out <- do_ccm_runs3(block, ccm_runs, ...)
            return(mutate(df.out,idx=idx))
          }, mc.cores = n.core)
  )
}




ccm_runs_ex2



block.temp
names(block.temp)
names(block.temp1)
names(block.temp2)

# ```{r}
number_of_surrogates <- 100
results_null_CCM_ex2 <- do.call(rbind,list(
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
                    ccm_runs_ex2[1,],
                    n.surr = number_of_surrogates, silent = TRUE) %>%
                    mutate(species = "Yellow_Sea",label="B&C"),#1
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$Fishing_pressure),c("SSB","Fishing_pressure")], 
                             ccm_runs_ex2[2,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#2
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$sst_spawn),c("Recruitment","sst_spawn")], 
                             ccm_runs_ex2[3,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#3
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$sst_spawn),c("SSB","sst_spawn")], 
                             ccm_runs_ex2[4,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#4
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_spawn),c("Recruitment","PDO_spawn")], 
                             ccm_runs_ex2[5,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#5
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$PDO_spawn),c("SSB","PDO_spawn")], 
                             ccm_runs_ex2[6,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#6
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$NPGO_spawn),c("Recruitment","NPGO_spawn")], 
                             ccm_runs_ex2[7,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#7
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$NPGO_spawn),c("SSB","NPGO_spawn")], 
                             ccm_runs_ex2[8,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#8
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$NPA_spawn),c("Recruitment","NPA_spawn")], 
                             ccm_runs_ex2[9,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#9
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$NPA_spawn),c("SSB","NPA_spawn")], 
                             ccm_runs_ex2[10,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#10
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$WP_spawn),c("Recruitment","WP_spawn")], 
                             ccm_runs_ex2[11,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#11
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$WP_spawn),c("SSB","WP_spawn")], 
                             ccm_runs_ex2[12,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#12
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$NAO_spawn),c("Recruitment","NAO_spawn")], 
                             ccm_runs_ex2[13,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#13
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$NAO_spawn),c("SSB","NAO_spawn")], 
                             ccm_runs_ex2[14,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#14
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$SOI_spawn),c("Recruitment","SOI_spawn")], 
                             ccm_runs_ex2[15,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#15
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$SOI_spawn),c("SSB","SOI_spawn")], 
                             ccm_runs_ex2[16,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#16
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$Nino34_spawn),c("Recruitment","Nino34_spawn")], 
                             ccm_runs_ex2[17,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#17
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$Nino34_spawn),c("SSB","Nino34_spawn")], 
                             ccm_runs_ex2[18,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#18
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$NOI_spawn),c("Recruitment","NOI_spawn")], 
                             ccm_runs_ex2[19,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#19
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$NOI_spawn),c("SSB","NOI_spawn")], 
                             ccm_runs_ex2[20,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#20
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$NPI_spawn),c("Recruitment","NPI_spawn")], 
                             ccm_runs_ex2[21,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#21
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$NPI_spawn),c("SSB","NPI_spawn")], 
                             ccm_runs_ex2[22,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#22
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$AOI_spawn),c("Recruitment","AOI_spawn")], 
                             ccm_runs_ex2[23,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#23
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$AOI_spawn),c("SSB","AOI_spawn")], 
                             ccm_runs_ex2[24,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#24
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$AMO_spawn),c("Recruitment","AMO_spawn")], 
                             ccm_runs_ex2[25,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#25
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$AMO_spawn),c("SSB","AMO_spawn")], 
                             ccm_runs_ex2[26,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#26
            do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$summer_fishing_moratorium),c("Recruitment","summer_fishing_moratorium")], 
                             ccm_runs_ex2[27,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#27
            do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$summer_fishing_moratorium),c("SSB","summer_fishing_moratorium")], 
                             ccm_runs_ex2[28,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "Yellow_Sea",label="B&C"),#28
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
                             ccm_runs_ex2[1,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#1
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$Fishing_pressure),c("SSB","Fishing_pressure")], 
                             ccm_runs_ex2[2,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#2
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$sst_spawn),c("Recruitment","sst_spawn")], 
                             ccm_runs_ex2[3,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#3
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$sst_spawn),c("SSB","sst_spawn")], 
                             ccm_runs_ex2[4,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#4
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_spawn),c("Recruitment","PDO_spawn")], 
                             ccm_runs_ex2[5,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#5
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_spawn),c("SSB","PDO_spawn")], 
                             ccm_runs_ex2[6,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#6
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPGO_spawn),c("Recruitment","NPGO_spawn")], 
                             ccm_runs_ex2[7,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#7
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPGO_spawn),c("SSB","NPGO_spawn")], 
                             ccm_runs_ex2[8,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#8
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPA_spawn),c("Recruitment","NPA_spawn")], 
                             ccm_runs_ex2[9,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#9
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPA_spawn),c("SSB","NPA_spawn")], 
                             ccm_runs_ex2[10,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#10
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$WP_spawn),c("Recruitment","WP_spawn")], 
                             ccm_runs_ex2[11,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#11
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$WP_spawn),c("SSB","WP_spawn")], 
                             ccm_runs_ex2[12,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#12
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NAO_spawn),c("Recruitment","NAO_spawn")], 
                             ccm_runs_ex2[13,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#13
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$NAO_spawn),c("SSB","NAO_spawn")], 
                             ccm_runs_ex2[14,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#14
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$SOI_spawn),c("Recruitment","SOI_spawn")], 
                             ccm_runs_ex2[15,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#15
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$SOI_spawn),c("SSB","SOI_spawn")], 
                             ccm_runs_ex2[16,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#16
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Nino34_spawn),c("Recruitment","Nino34_spawn")], 
                             ccm_runs_ex2[17,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#17
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$Nino34_spawn),c("SSB","Nino34_spawn")], 
                             ccm_runs_ex2[18,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#18
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NOI_spawn),c("Recruitment","NOI_spawn")], 
                             ccm_runs_ex2[19,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$NOI_spawn),c("SSB","NOI_spawn")], 
                             ccm_runs_ex2[20,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPI_spawn),c("Recruitment","NPI_spawn")], 
                             ccm_runs_ex2[21,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#21
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPI_spawn),c("SSB","NPI_spawn")], 
                             ccm_runs_ex2[22,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#22
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AOI_spawn),c("Recruitment","AOI_spawn")], 
                             ccm_runs_ex2[23,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#23
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$AOI_spawn),c("SSB","AOI_spawn")], 
                             ccm_runs_ex2[24,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#24
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AMO_spawn),c("Recruitment","AMO_spawn")], 
                             ccm_runs_ex2[25,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#25
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$AMO_spawn),c("SSB","AMO_spawn")], 
                             ccm_runs_ex2[26,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#26
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$summer_fishing_moratorium),c("Recruitment","summer_fishing_moratorium")], 
                             ccm_runs_ex2[27,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#27
            do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$summer_fishing_moratorium),c("SSB","summer_fishing_moratorium")], 
                             ccm_runs_ex2[28,],
                             n.surr = number_of_surrogates, silent = TRUE) %>%
              mutate(species = "YBohai_Sea",label="JABBA_B&R"),#28
            ))


save(results_null_CCM_ex2,file='./env_ccm_null.Rdata')
#ccm_runs_ex2
#!is.na(block.temp1$JAI&block.temp1$SLP)
#!is.na(block.temp1$LPUE&block.temp1$SLP)
#!is.na(block.temp1$JAI&block.temp1$SST)
#!is.na(block.temp1$LPUE&block.temp1$SST)
#!is.na(block.temp1$JAI&block.temp1$streamflow)
#!is.na(block.temp1$LPUE&block.temp1$streamflow)

#!is.na(block.temp$JAI&block.temp$SLP)
#!is.na(block.temp$LPUE&block.temp$SLP)
#!is.na(block.temp$JAI&block.temp$SST)
#!is.na(block.temp$LPUE&block.temp$SST)
#!is.na(block.temp$JAI&block.temp$streamflow)
#!is.na(block.temp$LPUE&block.temp$streamflow)

#!is.na(c$a&c$b)
#








#4.2.1 PLOT
load('./env_ccm_null.Rdata')
results_CCM_ex2$lib_column<-results_CCM_ex2$lib
results_CCM_ex2$target_column<-results_CCM_ex2$target
results_CCM_ex2$lib_size<-results_CCM_ex2$LibSize
results_CCM_ex2$mae<-results_CCM_ex2$MAE
results_CCM_ex2$rmse<-results_CCM_ex2$RMSE


df.plot <- results_CCM_ex2 %>%
  mutate(experiment = interaction(species,label, sep="; ")) %>%
  # mutate(ccm_label = interaction(lib_column,target_column,sep="->")) %>%
  select(species,lib_column,target_column,experiment,lib_size,rho,mae,rmse) %>%
  group_by(species,lib_column,target_column,experiment,lib_size) %>%
  summarise_at(vars(rho,mae,rmse),funs(pmax(0,median(.,na.rm=TRUE))))



labs_panels <- unique(df.plot$experiment)
n_panels <- length(labs_panels)
levels(unique(df.plot$experiment))
h_panels <- vector(mode="list",n_panels)
h_ccm_ex2 <- vector(mode="list",length(levels(unique(df.plot$experiment))))
L_species <- c("YBohai_Sea","Yellow_Sea")
L_var <- c("Recruitment","SSB")



for(i_species in 1:length(L_species)){
  for(i_var in 1:length(L_var)){
    i_1d <- length(L_species)*(i_species-1) + i_var
    var_i <- L_var[[i_var]]
    species_i <- L_species[[i_species]]
    ## JAI
    h_ccm_ex2[[i_1d]] <- df.plot %>%
      filter(species == species_i) %>%
      filter(lib_column == var_i) %>%
      # mutate(ccm_label = interaction(lib_column,target_column,sep="->"))%>%
      ggplot(aes(x=lib_size,y=rho,color=target_column)) + 
      geom_line(lwd=2)+
      ylim(c(0,1.0)) +
      labs(title=paste(labs_panels[[i_species]],var_i),
           col="") +
      theme_bw() +
      theme(legend.position = "bottom")+
      scale_color_manual(values=c("#5E4FA2","#00FFFF","#62BEA6","#F2FAAC",
                                 "#B8E2A1","#FEE695","#CC00FF","blue",
                                 "#FCA75E","#33FF00","#4198B6","yellow",
                                 "#FF0000","#9E0142"))
  } # i_var
} # i_species




g_legend<-function(a.gplot){
  g <- ggplotGrob(a.gplot + theme(legend.position = "bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  return(legend)}
mylegend<-g_legend(a.gplot=h_ccm_ex2[[1]])
lheight <- sum(mylegend$height)
# do.call(grid.arrange,c(h_ccm_ex2,nrow = 2))
grid.arrange(do.call(arrangeGrob, c(lapply(h_ccm_ex2,
                                           function(h_i) h_i + theme(legend.position="none"
                                           )),
                                    nrow=2)),
             mylegend, nrow=2,heights = unit.c(unit(1, "npc") - lheight, lheight)
)


#800  750




###############
#4.3 Lag Analysis







##############
###4.3 Lag Analysis
ccm_runs_ex2
#!is.na(block.temp1$JAI&block.temp1$SLP)
#!is.na(block.temp1$LPUE&block.temp1$SLP)
#!is.na(block.temp1$JAI&block.temp1$SST)
#!is.na(block.temp1$LPUE&block.temp1$SST)
#!is.na(block.temp1$JAI&block.temp1$streamflow)
#!is.na(block.temp1$LPUE&block.temp1$streamflow)

#!is.na(block.temp$JAI&block.temp$SLP)
#!is.na(block.temp$LPUE&block.temp$SLP)
#!is.na(block.temp$JAI&block.temp$SST)
#!is.na(block.temp$LPUE&block.temp$SST)
#!is.na(block.temp$JAI&block.temp$streamflow)
#!is.na(block.temp$LPUE&block.temp$streamflow)

#!is.na(c$a&c$b)
#



results_CCM_lags<- do.call(rbind,list(
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
                      ccm_runs_ex2[1,], lag_list = seq(-5,0,by=1),silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#1
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$Fishing_pressure),c("SSB","Fishing_pressure")], 
                      ccm_runs_ex2[2,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#2
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$sst_spawn),c("Recruitment","sst_spawn")], 
                      ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1),silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#3
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$sst_spawn),c("SSB","sst_spawn")], 
                      ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#4
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_spawn),c("Recruitment","PDO_spawn")], 
                      ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#5
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$PDO_spawn),c("SSB","PDO_spawn")], 
                      ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#6
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$NPGO_spawn),c("Recruitment","NPGO_spawn")], 
                      ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#7
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$NPGO_spawn),c("SSB","NPGO_spawn")], 
                      ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#8
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$NPA_spawn),c("Recruitment","NPA_spawn")], 
                      ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#9
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$NPA_spawn),c("SSB","NPA_spawn")], 
                      ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#10
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$WP_spawn),c("Recruitment","WP_spawn")], 
                      ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#11
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$WP_spawn),c("SSB","WP_spawn")], 
                      ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#12
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$NAO_spawn),c("Recruitment","NAO_spawn")], 
                      ccm_runs_ex2[13,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#13
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$NAO_spawn),c("SSB","NAO_spawn")], 
                      ccm_runs_ex2[14,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#14
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$SOI_spawn),c("Recruitment","SOI_spawn")], 
                      ccm_runs_ex2[15,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#15
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$SOI_spawn),c("SSB","SOI_spawn")], 
                      ccm_runs_ex2[16,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#16
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$Nino34_spawn),c("Recruitment","Nino34_spawn")], 
                      ccm_runs_ex2[17,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#17
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$Nino34_spawn),c("SSB","Nino34_spawn")], 
                      ccm_runs_ex2[18,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#18
  # do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_spawn),c("Recruitment","PDO_spawn")], 
  #             ccm_runs_ex2, silent = TRUE) %>%
  # mutate(species = "Yellow_Sea",label="B&C"),#19
  #do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$PDO_spawn),c("SSB","PDO_spawn")], 
  #            ccm_runs_ex2, silent = TRUE) %>%
  #mutate(species = "Yellow_Sea",label="B&C"),#20
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$NOI_spawn),c("Recruitment","NOI_spawn")], 
                      ccm_runs_ex2[19,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#21
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$NOI_spawn),c("SSB","NOI_spawn")], 
                      ccm_runs_ex2[20,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#22
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$NPI_spawn),c("Recruitment","NPI_spawn")], 
                      ccm_runs_ex2[21,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#23
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$NPI_spawn),c("SSB","NPI_spawn")], 
                      ccm_runs_ex2[22,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#24
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$AOI_spawn),c("Recruitment","AOI_spawn")], 
                      ccm_runs_ex2[23,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#25
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$AOI_spawn),c("SSB","AOI_spawn")], 
                      ccm_runs_ex2[24,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#26
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$AMO_spawn),c("Recruitment","AMO_spawn")], 
                      ccm_runs_ex2[25,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#27
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$AMO_spawn),c("SSB","AMO_spawn")], 
                      ccm_runs_ex2[26,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#28
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$summer_fishing_moratorium),c("Recruitment","summer_fishing_moratorium")], 
                      ccm_runs_ex2[27,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#29
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$summer_fishing_moratorium),c("SSB","summer_fishing_moratorium")], 
                      ccm_runs_ex2[28,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#30
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
                      ccm_runs_ex2[1,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#1
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$Fishing_pressure),c("SSB","Fishing_pressure")], 
                      ccm_runs_ex2[2,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#2
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$sst_spawn),c("Recruitment","sst_spawn")], 
                      ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#3
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$sst_spawn),c("SSB","sst_spawn")], 
                      ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#4
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_spawn),c("Recruitment","PDO_spawn")], 
                      ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#5
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_spawn),c("SSB","PDO_spawn")], 
                      ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#6
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPGO_spawn),c("Recruitment","NPGO_spawn")], 
                      ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#7
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPGO_spawn),c("SSB","NPGO_spawn")], 
                      ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#8
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPA_spawn),c("Recruitment","NPA_spawn")], 
                      ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#9
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPA_spawn),c("SSB","NPA_spawn")], 
                      ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#10
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$WP_spawn),c("Recruitment","WP_spawn")], 
                      ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#11
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$WP_spawn),c("SSB","WP_spawn")], 
                      ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#12
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NAO_spawn),c("Recruitment","NAO_spawn")], 
                      ccm_runs_ex2[13,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#13
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$NAO_spawn),c("SSB","NAO_spawn")], 
                      ccm_runs_ex2[14,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#14
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$SOI_spawn),c("Recruitment","SOI_spawn")], 
                      ccm_runs_ex2[15,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#15
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$SOI_spawn),c("SSB","SOI_spawn")], 
                      ccm_runs_ex2[16,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#16
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Nino34_spawn),c("Recruitment","Nino34_spawn")], 
                      ccm_runs_ex2[17,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#17
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$Nino34_spawn),c("SSB","Nino34_spawn")], 
                      ccm_runs_ex2[18,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#18
  #do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_spawn),c("Recruitment","PDO_spawn")], 
  #            ccm_runs_ex2, silent = TRUE) %>%
  #mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  #do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_spawn),c("SSB","PDO_spawn")], 
  #            ccm_runs_ex2, silent = TRUE) %>%
  #mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NOI_spawn),c("Recruitment","NOI_spawn")], 
                      ccm_runs_ex2[19,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$NOI_spawn),c("SSB","NOI_spawn")], 
                      ccm_runs_ex2[20,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPI_spawn),c("Recruitment","NPI_spawn")], 
                      ccm_runs_ex2[21,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#21
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPI_spawn),c("SSB","NPI_spawn")], 
                      ccm_runs_ex2[22,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#22
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AOI_spawn),c("Recruitment","AOI_spawn")], 
                      ccm_runs_ex2[23,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#23
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$AOI_spawn),c("SSB","AOI_spawn")], 
                      ccm_runs_ex2[24,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#24
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AMO_spawn),c("Recruitment","AMO_spawn")], 
                      ccm_runs_ex2[25,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#25
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$AMO_spawn),c("SSB","AMO_spawn")], 
                      ccm_runs_ex2[26,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#26
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$summer_fishing_moratorium),c("Recruitment","summer_fishing_moratorium")], 
                      ccm_runs_ex2[27,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#27
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$summer_fishing_moratorium),c("SSB","summer_fishing_moratorium")], 
                      ccm_runs_ex2[28,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R")#28
))





results_CCM_lags2<- do.call(rbind,list(
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
                       ccm_runs_ex2[1,], lag_list = seq(-5,0,by=1),silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#1
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$Fishing_pressure),c("SSB","Fishing_pressure")], 
                       ccm_runs_ex2[2,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#2
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$sst_spawn),c("Recruitment","sst_spawn")], 
                       ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1),silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#3
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$sst_spawn),c("SSB","sst_spawn")], 
                       ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#4
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_spawn),c("Recruitment","PDO_spawn")], 
                       ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#5
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$PDO_spawn),c("SSB","PDO_spawn")], 
                       ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#6
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$NPGO_spawn),c("Recruitment","NPGO_spawn")], 
                       ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#7
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$NPGO_spawn),c("SSB","NPGO_spawn")], 
                       ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#8
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$NPA_spawn),c("Recruitment","NPA_spawn")], 
                       ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#9
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$NPA_spawn),c("SSB","NPA_spawn")], 
                       ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#10
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$WP_spawn),c("Recruitment","WP_spawn")], 
                       ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#11
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$WP_spawn),c("SSB","WP_spawn")], 
                       ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#12
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$NAO_spawn),c("Recruitment","NAO_spawn")], 
                       ccm_runs_ex2[13,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#13
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$NAO_spawn),c("SSB","NAO_spawn")], 
                       ccm_runs_ex2[14,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#14
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$SOI_spawn),c("Recruitment","SOI_spawn")], 
                       ccm_runs_ex2[15,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#15
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$SOI_spawn),c("SSB","SOI_spawn")], 
                       ccm_runs_ex2[16,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#16
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$Nino34_spawn),c("Recruitment","Nino34_spawn")], 
                       ccm_runs_ex2[17,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#17
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$Nino34_spawn),c("SSB","Nino34_spawn")], 
                       ccm_runs_ex2[18,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#18
  # do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_spawn),c("Recruitment","PDO_spawn")], 
  #             ccm_runs_ex2, silent = TRUE)[,c(-2,-3)] %>%
  # mutate(species = "Yellow_Sea",label="B&C"),#19
  #do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$PDO_spawn),c("SSB","PDO_spawn")], 
  #            ccm_runs_ex2, silent = TRUE)[,c(-2,-3)] %>%
  #mutate(species = "Yellow_Sea",label="B&C"),#20
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$NOI_spawn),c("Recruitment","NOI_spawn")], 
                       ccm_runs_ex2[19,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#21
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$NOI_spawn),c("SSB","NOI_spawn")], 
                       ccm_runs_ex2[20,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#22
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$NPI_spawn),c("Recruitment","NPI_spawn")], 
                       ccm_runs_ex2[21,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#23
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$NPI_spawn),c("SSB","NPI_spawn")], 
                       ccm_runs_ex2[22,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)]%>%
    mutate(species = "Yellow_Sea",label="B&C"),#24
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$AOI_spawn),c("Recruitment","AOI_spawn")], 
                       ccm_runs_ex2[23,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#25
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$AOI_spawn),c("SSB","AOI_spawn")], 
                       ccm_runs_ex2[24,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#26
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$AMO_spawn),c("Recruitment","AMO_spawn")], 
                       ccm_runs_ex2[25,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#27
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$AMO_spawn),c("SSB","AMO_spawn")], 
                       ccm_runs_ex2[26,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#28
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$summer_fishing_moratorium),c("Recruitment","summer_fishing_moratorium")], 
                       ccm_runs_ex2[27,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#29
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$summer_fishing_moratorium),c("SSB","summer_fishing_moratorium")], 
                       ccm_runs_ex2[28,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#30
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
                       ccm_runs_ex2[1,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#1
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$Fishing_pressure),c("SSB","Fishing_pressure")], 
                       ccm_runs_ex2[2,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#2
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$sst_spawn),c("Recruitment","sst_spawn")], 
                       ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#3
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$sst_spawn),c("SSB","sst_spawn")], 
                       ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#4
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_spawn),c("Recruitment","PDO_spawn")], 
                       ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#5
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_spawn),c("SSB","PDO_spawn")], 
                       ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#6
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPGO_spawn),c("Recruitment","NPGO_spawn")], 
                       ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#7
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPGO_spawn),c("SSB","NPGO_spawn")], 
                       ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#8
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPA_spawn),c("Recruitment","NPA_spawn")], 
                       ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#9
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPA_spawn),c("SSB","NPA_spawn")], 
                       ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#10
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$WP_spawn),c("Recruitment","WP_spawn")], 
                       ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#11
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$WP_spawn),c("SSB","WP_spawn")], 
                       ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#12
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NAO_spawn),c("Recruitment","NAO_spawn")], 
                       ccm_runs_ex2[13,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#13
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$NAO_spawn),c("SSB","NAO_spawn")], 
                       ccm_runs_ex2[14,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#14
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$SOI_spawn),c("Recruitment","SOI_spawn")], 
                       ccm_runs_ex2[15,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#15
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$SOI_spawn),c("SSB","SOI_spawn")], 
                       ccm_runs_ex2[16,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#16
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Nino34_spawn),c("Recruitment","Nino34_spawn")], 
                       ccm_runs_ex2[17,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#17
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$Nino34_spawn),c("SSB","Nino34_spawn")], 
                       ccm_runs_ex2[18,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#18
  #do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_spawn),c("Recruitment","PDO_spawn")], 
  #            ccm_runs_ex2, silent = TRUE)[,c(-2,-3)] %>%
  #mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  #do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_spawn),c("SSB","PDO_spawn")], 
  #            ccm_runs_ex2, silent = TRUE)[,c(-2,-3)] %>%
  #mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NOI_spawn),c("Recruitment","NOI_spawn")], 
                       ccm_runs_ex2[19,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$NOI_spawn),c("SSB","NOI_spawn")], 
                       ccm_runs_ex2[20,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPI_spawn),c("Recruitment","NPI_spawn")], 
                       ccm_runs_ex2[21,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#21
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPI_spawn),c("SSB","NPI_spawn")], 
                       ccm_runs_ex2[22,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#22
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AOI_spawn),c("Recruitment","AOI_spawn")], 
                       ccm_runs_ex2[23,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#23
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$AOI_spawn),c("SSB","AOI_spawn")], 
                       ccm_runs_ex2[24,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#24
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AMO_spawn),c("Recruitment","AMO_spawn")], 
                       ccm_runs_ex2[25,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#25
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$AMO_spawn),c("SSB","AMO_spawn")], 
                       ccm_runs_ex2[26,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#26
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$summer_fishing_moratorium),c("Recruitment","summer_fishing_moratorium")], 
                       ccm_runs_ex2[27,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#27
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$summer_fishing_moratorium),c("SSB","summer_fishing_moratorium")], 
                       ccm_runs_ex2[28,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R")#28
))

#[,c(-2,-3)]


results_CCM_lags
results_CCM_lags2


names(results_CCM_lags)
names(results_CCM_lags2)

results_CCM_lags<-cbind(results_CCM_lags,results_CCM_lags2[,"tp"])
names(results_CCM_lags)
results_CCM_lags$tp<-results_CCM_lags$`results_CCM_lags2[, "tp"]`
names(results_CCM_lags)




results_CCM_lags$lib_column<-results_CCM_lags$lib
results_CCM_lags$target_column<-results_CCM_lags$target
results_CCM_lags$lib_size<-results_CCM_lags$LibSize
results_CCM_lags$mae<-results_CCM_lags$MAE
results_CCM_lags$rmse<-results_CCM_lags$RMSE









h_ccm_lags <- vector(mode="list",4)
L_species <- c("YBohai_Sea","Yellow_Sea")
L_var <- c("Recruitment","SSB")


for(i_species in 1:length(L_species)){
  for(i_var in 1:length(L_var)){
    i_1d <- length(L_species)*(i_species-1) + i_var
    var_i <- L_var[[i_var]]
    species_i <- L_species[[i_species]]
    df.i <- results_CCM_lags %>%
      filter(species==species_i) %>%
      filter(lib_column==var_i)
    phys_i <- df.i$target_colum[1]
    title_i <- paste(species_i,var_i)
    h_ccm_lags[[i_1d]] <- ggplot(df.i,aes(x=tp,y=pmax(0,rho),color=target_column)
    ) + geom_line(lwd=1.5) +
      labs(title=title_i,
           col="",
           x="prediction lag (yrs)",
           y=expression(paste("cross-map skill (",rho,")"))) +
      ylim(c(0,1.0)) +
      theme_bw() +
      theme(legend.position = "bottom")+
      scale_color_manual(values=c("#5E4FA2","#00FFFF","#62BEA6","#F2FAAC",
                                  "#B8E2A1","#FEE695","#CC00FF","blue",
                                  "#FCA75E","#33FF00","#4198B6","yellow",
                                  "#FF0000","#9E0142"))
  }}



g_legend<-function(a.gplot){
  g <- ggplotGrob(a.gplot + theme(legend.position = "bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  return(legend)}




mylegend<-g_legend(a.gplot=h_ccm_lags[[1]])
lheight <- sum(mylegend$height)
grid.arrange(do.call(arrangeGrob, c(lapply(h_ccm_lags,
                                           function(h_i) h_i + theme(legend.position="none"
                                           )),
                                    nrow=2)),
             mylegend, nrow=2,heights = unit.c(unit(1, "npc") - lheight, lheight)
)



#800  750










##########################################################
#############################################################
############################################################
#增加新变量#增加新变量#增加新变量
#增加新变量#增加新变量#增加新变量
#增加新变量#增加新变量#增加新变量
#增加新变量#增加新变量#增加新变量



names(block.temp)
names(block.temp1)
names(block.temp2)

results_CCM_ex2 <- do.call(rbind,list(
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#1
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$Fishing_pressure),c("SSB","Fishing_pressure")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#2
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$SST),c("Recruitment","SST")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#3
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$SST),c("SSB","SST")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#4
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO),c("Recruitment","PDO")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#5
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$PDO),c("SSB","PDO")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#6
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$Nino34),c("Recruitment","Nino34")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#7
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$Nino34),c("SSB","Nino34")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#8
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$AMO),c("Recruitment","AMO")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#9
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$AMO),c("SSB","AMO")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#10
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$AOI),c("Recruitment","AOI")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#11
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$AOI),c("SSB","AOI")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#12
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#1
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$Fishing_pressure),c("SSB","Fishing_pressure")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#2
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$SST),c("Recruitment","SST")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#3
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$SST),c("SSB","SST")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#4
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO),c("Recruitment","PDO")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#5
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO),c("SSB","PDO")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#6
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Nino34),c("Recruitment","Nino34")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#7
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$Nino34),c("SSB","Nino34")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#8
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AMO),c("Recruitment","AMO")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#9
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$AMO),c("SSB","AMO")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#10
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AOI),c("Recruitment","AOI")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#11
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$AOI),c("SSB","AOI")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#12
  do_ccm_runs3(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#1
  do_ccm_runs3(block.temp2[!is.na(block.temp2$SSB&block.temp2$Fishing_pressure),c("SSB","Fishing_pressure")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#2
  do_ccm_runs3(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$SST),c("Recruitment","SST")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#3
  do_ccm_runs3(block.temp2[!is.na(block.temp2$SSB&block.temp2$SST),c("SSB","SST")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#4
  do_ccm_runs3(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$PDO),c("Recruitment","PDO")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#5
  do_ccm_runs3(block.temp2[!is.na(block.temp2$SSB&block.temp2$PDO),c("SSB","PDO")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#6
  do_ccm_runs3(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$Nino34),c("Recruitment","Nino34")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#7
  do_ccm_runs3(block.temp2[!is.na(block.temp2$SSB&block.temp2$Nino34),c("SSB","Nino34")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#8
  do_ccm_runs3(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$AMO),c("Recruitment","AMO")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#9
  do_ccm_runs3(block.temp2[!is.na(block.temp2$SSB&block.temp2$AMO),c("SSB","AMO")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#10
  do_ccm_runs3(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$AOI),c("Recruitment","AOI")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#11
  do_ccm_runs3(block.temp2[!is.na(block.temp2$SSB&block.temp2$AOI),c("SSB","AOI")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST")#12
))











ccm_runs_ex2

block.temp <- full_join(bio.Yel,phys.Yel,by="Year")
block.temp1 <- full_join(bio.Bohai.raw,phys.Bohai,by="Year")
block.temp2 <- full_join(bio.Bohai.VAST,phys.Bohai,by="Year")
block.temp1$SSB<-block.temp1$Spring
block.temp1$Recruitment<-block.temp1$Summer
block.temp2$SSB<-block.temp2$Spring
block.temp2$Recruitment<-block.temp2$Summer

block.temp
names(block.temp)
names(block.temp1)
names(block.temp2)

# ```{r}
number_of_surrogates <- 100
results_null_CCM_ex2 <- do.call(rbind,list(
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
                   ccm_runs_ex2[1,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#1
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$Fishing_pressure),c("SSB","Fishing_pressure")], 
                   ccm_runs_ex2[2,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#2
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$SST),c("Recruitment","SST")], 
                   ccm_runs_ex2[3,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#3
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$SST),c("SSB","SST")], 
                   ccm_runs_ex2[4,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#4
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO),c("Recruitment","PDO")], 
                   ccm_runs_ex2[5,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#5
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$PDO),c("SSB","PDO")], 
                   ccm_runs_ex2[6,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#6
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$Nino34),c("Recruitment","Nino34")], 
                   ccm_runs_ex2[7,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#7
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$Nino34),c("SSB","Nino34")], 
                   ccm_runs_ex2[8,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#8
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$AMO),c("Recruitment","AMO")], 
                   ccm_runs_ex2[9,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#9
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$AMO),c("SSB","AMO")], 
                   ccm_runs_ex2[10,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#10
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$AOI),c("Recruitment","AOI")], 
                   ccm_runs_ex2[11,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#11
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$AOI),c("SSB","AOI")], 
                   ccm_runs_ex2[12,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#12
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Fishing_pressure),c("Recruitment","Fishing_pressure")],
                   ccm_runs_ex2[1,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#1
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$Fishing_pressure),c("SSB","Fishing_pressure")],
                   ccm_runs_ex2[2,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#2
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$SST),c("Recruitment","SST")],
                   ccm_runs_ex2[3,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#3
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$SST),c("SSB","SST")],
                   ccm_runs_ex2[4,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#4
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO),c("Recruitment","PDO")],
                   ccm_runs_ex2[5,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#5
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO),c("SSB","PDO")],
                   ccm_runs_ex2[6,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#6
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Nino34),c("Recruitment","Nino34")],
                   ccm_runs_ex2[7,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#7
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$Nino34),c("SSB","Nino34")],
                   ccm_runs_ex2[8,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#8
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AMO),c("Recruitment","AMO")],
                   ccm_runs_ex2[9,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#9
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$AMO),c("SSB","AMO")],
                   ccm_runs_ex2[10,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#10
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AOI),c("Recruitment","AOI")],
                   ccm_runs_ex2[11,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#11
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$AOI),c("SSB","AOI")],
                   ccm_runs_ex2[12,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#12
  do_null_ccm_runs(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$Fishing_pressure),c("Recruitment","Fishing_pressure")],
                   ccm_runs_ex2[1,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#1
  do_null_ccm_runs(block.temp2[!is.na(block.temp2$SSB&block.temp2$Fishing_pressure),c("SSB","Fishing_pressure")],
                   ccm_runs_ex2[2,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#2
  do_null_ccm_runs(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$SST),c("Recruitment","SST")],
                   ccm_runs_ex2[3,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#3
  do_null_ccm_runs(block.temp2[!is.na(block.temp2$SSB&block.temp2$SST),c("SSB","SST")],
                   ccm_runs_ex2[4,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#4
  do_null_ccm_runs(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$PDO),c("Recruitment","PDO")],
                   ccm_runs_ex2[5,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#5
  do_null_ccm_runs(block.temp2[!is.na(block.temp2$SSB&block.temp2$PDO),c("SSB","PDO")],
                   ccm_runs_ex2[6,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#6
  do_null_ccm_runs(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$Nino34),c("Recruitment","Nino34")],
                   ccm_runs_ex2[7,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#7
  do_null_ccm_runs(block.temp2[!is.na(block.temp2$SSB&block.temp2$Nino34),c("SSB","Nino34")],
                   ccm_runs_ex2[8,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#8
  do_null_ccm_runs(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$AMO),c("Recruitment","AMO")],
                   ccm_runs_ex2[9,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#9
  do_null_ccm_runs(block.temp2[!is.na(block.temp2$SSB&block.temp2$AMO),c("SSB","AMO")],
                   ccm_runs_ex2[10,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#10
  do_null_ccm_runs(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$AOI),c("Recruitment","AOI")],
                   ccm_runs_ex2[11,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#11
  do_null_ccm_runs(block.temp2[!is.na(block.temp2$SSB&block.temp2$AOI),c("SSB","AOI")],
                   ccm_runs_ex2[12,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST")#12
))


save(results_null_CCM_ex2,file='./env_ccm_null.Rdata')
#ccm_runs_ex2
#!is.na(block.temp1$JAI&block.temp1$SLP)
#!is.na(block.temp1$LPUE&block.temp1$SLP)
#!is.na(block.temp1$JAI&block.temp1$SST)
#!is.na(block.temp1$LPUE&block.temp1$SST)
#!is.na(block.temp1$JAI&block.temp1$streamflow)
#!is.na(block.temp1$LPUE&block.temp1$streamflow)

#!is.na(block.temp$JAI&block.temp$SLP)
#!is.na(block.temp$LPUE&block.temp$SLP)
#!is.na(block.temp$JAI&block.temp$SST)
#!is.na(block.temp$LPUE&block.temp$SST)
#!is.na(block.temp$JAI&block.temp$streamflow)
#!is.na(block.temp$LPUE&block.temp$streamflow)

#!is.na(c$a&c$b)
#








#4.2.1 PLOT
load('./env_ccm_null.Rdata')
results_CCM_ex2$lib_column<-results_CCM_ex2$lib
results_CCM_ex2$target_column<-results_CCM_ex2$target
results_CCM_ex2$lib_size<-results_CCM_ex2$LibSize
results_CCM_ex2$mae<-results_CCM_ex2$MAE
results_CCM_ex2$rmse<-results_CCM_ex2$RMSE


df.plot <- results_CCM_ex2 %>%
  mutate(experiment = interaction(species,label, sep="; ")) %>%
  # mutate(ccm_label = interaction(lib_column,target_column,sep="->")) %>%
  select(species,lib_column,target_column,experiment,lib_size,rho,mae,rmse) %>%
  group_by(species,lib_column,target_column,experiment,lib_size) %>%
  summarise_at(vars(rho,mae,rmse),funs(pmax(0,median(.,na.rm=TRUE))))



labs_panels <- unique(df.plot$experiment)
n_panels <- length(labs_panels)
levels(unique(df.plot$experiment))
h_panels <- vector(mode="list",n_panels)
h_ccm_ex2 <- vector(mode="list",6)
L_species <- c("YBohai_Sea","YBohai_Sea_Seasonal","Yellow_Sea")
L_var <- c("Recruitment","SSB")



for(i_species in 1:length(L_species)){
  for(i_var in 1:length(L_var)){
    #i_1d <- (length(L_species)-1)*(i_species-1) + i_var
    i_1d <- length(L_species)*(i_species-1) + i_var #length(L_species)=2
    var_i <- L_var[[i_var]]
    species_i <- L_species[[i_species]]
    ## JAI
    h_ccm_ex2[[i_1d]] <- df.plot %>%
      filter(species == species_i) %>%
      filter(lib_column == var_i) %>%
      # mutate(ccm_label = interaction(lib_column,target_column,sep="->"))%>%
      ggplot(aes(x=lib_size,y=rho,color=target_column)) + 
      geom_line(lwd=2)+
      ylim(c(0,1.0)) +
      labs(title=paste(labs_panels[[i_species]],var_i),
           col="") +
      theme_bw() +
      theme(legend.position = "bottom")
  } # i_var
} # i_species

h_ccm_ex2
aa<-h_ccm_ex2[c(-3,-6)]#3 和6是空的

g_legend<-function(a.gplot){
  g <- ggplotGrob(a.gplot + theme(legend.position = "bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  return(legend)}
mylegend<-g_legend(a.gplot=h_ccm_ex2[[1]])
mylegend<-g_legend(a.gplot=aa[[1]])
lheight <- sum(mylegend$height)
# do.call(grid.arrange,c(h,nrow = 2))
grid.arrange(do.call(arrangeGrob, c(lapply(h_ccm_ex2,
                                           function(h_i) h_i + theme(legend.position="none"
                                           )),
                                    nrow=3)),
             mylegend, nrow=2,heights = unit.c(unit(1, "npc") - lheight, lheight)
)

grid.arrange(do.call(arrangeGrob, c(lapply(aa,
                                           function(h_i) h_i + theme(legend.position="none"
                                           )),
                                    nrow=3))
             ,
             mylegend, nrow=2,heights = unit.c(unit(1, "npc") - lheight, lheight)
)





###############
#4.3 Lag Analysis







##############
###4.3 Lag Analysis
ccm_runs_ex2
#!is.na(block.temp1$JAI&block.temp1$SLP)
#!is.na(block.temp1$LPUE&block.temp1$SLP)
#!is.na(block.temp1$JAI&block.temp1$SST)
#!is.na(block.temp1$LPUE&block.temp1$SST)
#!is.na(block.temp1$JAI&block.temp1$streamflow)
#!is.na(block.temp1$LPUE&block.temp1$streamflow)

#!is.na(block.temp$JAI&block.temp$SLP)
#!is.na(block.temp$LPUE&block.temp$SLP)
#!is.na(block.temp$JAI&block.temp$SST)
#!is.na(block.temp$LPUE&block.temp$SST)
#!is.na(block.temp$JAI&block.temp$streamflow)
#!is.na(block.temp$LPUE&block.temp$streamflow)

#!is.na(c$a&c$b)
#



results_CCM_lags <- do.call(rbind,list(
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
                      ccm_runs_ex2[1,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#1
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$Fishing_pressure),c("SSB","Fishing_pressure")], 
                      ccm_runs_ex2[2,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#2
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$SST),c("Recruitment","SST")], 
                      ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#3
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$SST),c("SSB","SST")], 
                      ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#4
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO),c("Recruitment","PDO")], 
                      ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#5
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$PDO),c("SSB","PDO")], 
                      ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#6
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$Nino34),c("Recruitment","Nino34")], 
                      ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#7
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$Nino34),c("SSB","Nino34")], 
                      ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#8
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$AMO),c("Recruitment","AMO")], 
                      ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#9
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$AMO),c("SSB","AMO")], 
                      ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#10
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$AOI),c("Recruitment","AOI")], 
                      ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#11
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$AOI),c("SSB","AOI")], 
                      ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="none"),#12
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Fishing_pressure),c("Recruitment","Fishing_pressure")],
                      ccm_runs_ex2[1,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#1
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$Fishing_pressure),c("SSB","Fishing_pressure")],
                      ccm_runs_ex2[2,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#2
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$SST),c("Recruitment","SST")],
                      ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#3
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$SST),c("SSB","SST")],
                      ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#4
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO),c("Recruitment","PDO")],
                      ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#5
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO),c("SSB","PDO")],
                      ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#6
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Nino34),c("Recruitment","Nino34")],
                      ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#7
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$Nino34),c("SSB","Nino34")],
                      ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#8
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AMO),c("Recruitment","AMO")],
                      ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#9
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$AMO),c("SSB","AMO")],
                      ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#10
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AOI),c("Recruitment","AOI")],
                      ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#11
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$AOI),c("SSB","AOI")],
                      ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="raw"),#12
  do_ccm_lag_analysis(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$Fishing_pressure),c("Recruitment","Fishing_pressure")],
                      ccm_runs_ex2[1,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#1
  do_ccm_lag_analysis(block.temp2[!is.na(block.temp2$SSB&block.temp2$Fishing_pressure),c("SSB","Fishing_pressure")],
                      ccm_runs_ex2[2,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#2
  do_ccm_lag_analysis(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$SST),c("Recruitment","SST")],
                      ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#3
  do_ccm_lag_analysis(block.temp2[!is.na(block.temp2$SSB&block.temp2$SST),c("SSB","SST")],
                      ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#4
  do_ccm_lag_analysis(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$PDO),c("Recruitment","PDO")],
                      ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#5
  do_ccm_lag_analysis(block.temp2[!is.na(block.temp2$SSB&block.temp2$PDO),c("SSB","PDO")],
                      ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#6
  do_ccm_lag_analysis(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$Nino34),c("Recruitment","Nino34")],
                      ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#7
  do_ccm_lag_analysis(block.temp2[!is.na(block.temp2$SSB&block.temp2$Nino34),c("SSB","Nino34")],
                      ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#8
  do_ccm_lag_analysis(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$AMO),c("Recruitment","AMO")],
                      ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#9
  do_ccm_lag_analysis(block.temp2[!is.na(block.temp2$SSB&block.temp2$AMO),c("SSB","AMO")],
                      ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#10
  do_ccm_lag_analysis(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$AOI),c("Recruitment","AOI")],
                      ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST"),#11
  do_ccm_lag_analysis(block.temp2[!is.na(block.temp2$SSB&block.temp2$AOI),c("SSB","AOI")],
                      ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea_Seasonal",label="VAST")#12
))




results_CCM_lags2 <- do.call(rbind,list(
  (do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$Fishing_pressure),c("Recruitment","Fishing_pressure")], 
                        ccm_runs_ex2[1,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "Yellow_Sea",label="none"))[,c(-2,-3)],#1
  (do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$Fishing_pressure),c("SSB","Fishing_pressure")], 
                        ccm_runs_ex2[2,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "Yellow_Sea",label="none"))[,c(-2,-3)],#2
  (do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$SST),c("Recruitment","SST")], 
                        ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "Yellow_Sea",label="none"))[,c(-2,-3)],#3
  (do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$SST),c("SSB","SST")], 
                        ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "Yellow_Sea",label="none"))[,c(-2,-3)],#4
  (do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO),c("Recruitment","PDO")], 
                        ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "Yellow_Sea",label="none"))[,c(-2,-3)],#5
  (do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$PDO),c("SSB","PDO")], 
                        ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "Yellow_Sea",label="none"))[,c(-2,-3)],#6
  (do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$Nino34),c("Recruitment","Nino34")], 
                        ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "Yellow_Sea",label="none"))[,c(-2,-3)],#7
  (do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$Nino34),c("SSB","Nino34")], 
                        ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "Yellow_Sea",label="none"))[,c(-2,-3)],#8
  (do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$AMO),c("Recruitment","AMO")], 
                        ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "Yellow_Sea",label="none"))[,c(-2,-3)],#9
  (do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$AMO),c("SSB","AMO")], 
                        ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "Yellow_Sea",label="none"))[,c(-2,-3)],#10
  (do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$AOI),c("Recruitment","AOI")], 
                        ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "Yellow_Sea",label="none"))[,c(-2,-3)],#11
  (do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$AOI),c("SSB","AOI")], 
                        ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "Yellow_Sea",label="none"))[,c(-2,-3)],#12
  (do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Fishing_pressure),c("Recruitment","Fishing_pressure")],
                        ccm_runs_ex2[1,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea",label="raw"))[,c(-2,-3)],#1
  (do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$Fishing_pressure),c("SSB","Fishing_pressure")],
                        ccm_runs_ex2[2,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea",label="raw"))[,c(-2,-3)],#2
  (do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$SST),c("Recruitment","SST")],
                        ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea",label="raw"))[,c(-2,-3)],#3
  (do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$SST),c("SSB","SST")],
                        ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea",label="raw"))[,c(-2,-3)],#4
  (do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO),c("Recruitment","PDO")],
                        ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea",label="raw"))[,c(-2,-3)],#5
  (do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO),c("SSB","PDO")],
                        ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea",label="raw"))[,c(-2,-3)],#6
  (do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Nino34),c("Recruitment","Nino34")],
                        ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea",label="raw"))[,c(-2,-3)],#7
  (do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$Nino34),c("SSB","Nino34")],
                        ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea",label="raw"))[,c(-2,-3)],#8
  (do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AMO),c("Recruitment","AMO")],
                        ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea",label="raw"))[,c(-2,-3)],#9
  (do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$AMO),c("SSB","AMO")],
                        ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea",label="raw"))[,c(-2,-3)],#10
  (do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AOI),c("Recruitment","AOI")],
                        ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea",label="raw"))[,c(-2,-3)],#11
  (do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$AOI),c("SSB","AOI")],
                        ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea",label="raw"))[,c(-2,-3)],#12
  (do_ccm_lag_analysis2(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$Fishing_pressure),c("Recruitment","Fishing_pressure")],
                        ccm_runs_ex2[1,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea_Seasonal",label="VAST"))[,c(-2,-3)],#1
  (do_ccm_lag_analysis2(block.temp2[!is.na(block.temp2$SSB&block.temp2$Fishing_pressure),c("SSB","Fishing_pressure")],
                        ccm_runs_ex2[2,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea_Seasonal",label="VAST"))[,c(-2,-3)],#2
  (do_ccm_lag_analysis2(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$SST),c("Recruitment","SST")],
                        ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea_Seasonal",label="VAST"))[,c(-2,-3)],#3
  (do_ccm_lag_analysis2(block.temp2[!is.na(block.temp2$SSB&block.temp2$SST),c("SSB","SST")],
                        ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea_Seasonal",label="VAST"))[,c(-2,-3)],#4
  (do_ccm_lag_analysis2(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$PDO),c("Recruitment","PDO")],
                        ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea_Seasonal",label="VAST"))[,c(-2,-3)],#5
  (do_ccm_lag_analysis2(block.temp2[!is.na(block.temp2$SSB&block.temp2$PDO),c("SSB","PDO")],
                        ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea_Seasonal",label="VAST"))[,c(-2,-3)],#6
  (do_ccm_lag_analysis2(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$Nino34),c("Recruitment","Nino34")],
                        ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea_Seasonal",label="VAST"))[,c(-2,-3)],#7
  (do_ccm_lag_analysis2(block.temp2[!is.na(block.temp2$SSB&block.temp2$Nino34),c("SSB","Nino34")],
                        ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea_Seasonal",label="VAST"))[,c(-2,-3)],#8
  (do_ccm_lag_analysis2(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$AMO),c("Recruitment","AMO")],
                        ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea_Seasonal",label="VAST"))[,c(-2,-3)],#9
  (do_ccm_lag_analysis2(block.temp2[!is.na(block.temp2$SSB&block.temp2$AMO),c("SSB","AMO")],
                        ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea_Seasonal",label="VAST"))[,c(-2,-3)],#10
  (do_ccm_lag_analysis2(block.temp2[!is.na(block.temp2$Recruitment&block.temp2$AOI),c("Recruitment","AOI")],
                        ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea_Seasonal",label="VAST"))[,c(-2,-3)],#11
  (do_ccm_lag_analysis2(block.temp2[!is.na(block.temp2$SSB&block.temp2$AOI),c("SSB","AOI")],
                        ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
     mutate(species = "YBohai_Sea_Seasonal",label="VAST"))[,c(-2,-3)]#12
))


#[,c(-2,-3)]


results_CCM_lags
results_CCM_lags2


names(results_CCM_lags)
names(results_CCM_lags2)

results_CCM_lags<-cbind(results_CCM_lags,results_CCM_lags2[,"tp"])
names(results_CCM_lags)
results_CCM_lags$tp<-results_CCM_lags$`results_CCM_lags2[, "tp"]`
names(results_CCM_lags)




results_CCM_lags$lib_column<-results_CCM_lags$lib
results_CCM_lags$target_column<-results_CCM_lags$target
results_CCM_lags$lib_size<-results_CCM_lags$LibSize
results_CCM_lags$mae<-results_CCM_lags$MAE
results_CCM_lags$rmse<-results_CCM_lags$RMSE









h_ccm_lags <- vector(mode="list",6)
L_species <- c("YBohai_Sea","YBohai_Sea_Seasonal","Yellow_Sea")
L_var <- c("Recruitment","SSB")


for(i_species in 1:length(L_species)){
  for(i_var in 1:length(L_var)){
    i_1d <- length(L_species)*(i_species-1) + i_var
    var_i <- L_var[[i_var]]
    species_i <- L_species[[i_species]]
    df.i <- results_CCM_lags %>%
      filter(species==species_i) %>%
      filter(lib_column==var_i)
    phys_i <- df.i$target_colum[1]
    title_i <- paste(species_i,var_i)
    h_ccm_lags[[i_1d]] <- ggplot(df.i,aes(x=tp,y=pmax(0,rho),color=target_column)
    ) + geom_line(lwd=1.5) +
      labs(title=title_i,
           col="",
           x="prediction lag (yrs)",
           y=expression(paste("cross-map skill (",rho,")"))) +
      ylim(c(0,1.0)) +
      theme_bw() +
      theme(legend.position = "bottom")
  }}



g_legend<-function(a.gplot){
  g <- ggplotGrob(a.gplot + theme(legend.position = "bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  return(legend)}



aa<-h_ccm_lags[c(-3,-6)]
mylegend<-g_legend(a.gplot=h_ccm_lags[[1]])
mylegend<-g_legend(a.gplot=aa[[1]])
lheight <- sum(mylegend$height)
grid.arrange(do.call(arrangeGrob, c(lapply(h_ccm_lags,
                                           function(h_i) h_i + theme(legend.position="none"
                                           )),
                                    nrow=3)),
             mylegend, nrow=2,heights = unit.c(unit(1, "npc") - lheight, lheight)
)

grid.arrange(do.call(arrangeGrob, c(lapply(aa,
                                           function(h_i) h_i + theme(legend.position="none"
                                           )),
                                    nrow=3)),
             mylegend, nrow=2,heights = unit.c(unit(1, "npc") - lheight, lheight)
)


#




#############################################################
###########################################################
######################################################












##########################
##################
################################
#5 Multivariate EDM

results_multi_ex1 <- NULL







do_multivariate_E_analysis <- function(data,pred_col = 1,lag_col = 2,other_col =
                                         NULL, E = 1:5, first_column_time = FALSE, ...){
  if(first_column_time){
    tt <- data[,1]
    data <- data[,-1]
  }
  if(is.character(pred_col)) pred_col = match(pred_col,names(data))
  if(is.character(lag_col)) lag_col = match(lag_col,names(data))
  if(is.character(other_col)) other_col = match(other_col,names(data))
  block <- make_block(data = data, cols = c(pred_col,rep(lag_col,max(E)),
                                            other_col),
                      delays = c(0,0:-(max(E)-1),rep(0,length(other_col)))) %>%
    as.data.frame() %>%
    mutate_all(funs((. - mean(.,na.rm=TRUE))/sd(.,na.rm=TRUE)))
  if(first_column_time){
    block <- block %>% mutate(time = tt) %>% select(time,everything())
  }
  L_columns <- lapply(E,function(x) c(1+(1:x), 2+max(E)+seq(from=1,by = 1,length.out = length(other_col)) ))
  out <- block_lnlp(block, first_column_time = first_column_time,
                    method = "simplex", num_neighbors = "e+1",
                    columns = names(block)[ L_columns[[E]]], target_column = names(block)[2], stats_only = TRUE,
                    theta = NULL) %>%
    mutate(embedding=str_count(cols,",")+1) %>%
    rename(E1=embedding)
}














#修改

do_multivariate_E_analysis <- function(data,pred_col = 1,lag_col = 2,other_col =
                                         NULL, E = 1:5, first_column_time = FALSE, ...){
  if(first_column_time){
    tt <- data[,1]
    data <- data[,-1]
  }
  if(is.character(pred_col)) pred_col = match(pred_col,names(data))
  if(is.character(lag_col)) lag_col = match(lag_col,names(data))
  if(is.character(other_col)) other_col = match(other_col,names(data))
  block <- make_block(data = data, cols = c(pred_col,rep(lag_col,max(E)),
                                            other_col),
                      delays = c(0,0:-(max(E)-1),rep(0,length(other_col)))) %>%
    as.data.frame() %>%
    mutate_all(funs((. - mean(.,na.rm=TRUE))/sd(.,na.rm=TRUE)))
  if(first_column_time){
    block <- block %>% mutate(time = tt) %>% select(time,everything())
  }
  L_columns <- lapply(E,function(x) c(2+(1:x), 2+max(E)+seq(from=1,by = 1,length.out = length(other_col)) ))
  block = na.omit( block )
  out1 <- block_lnlp(block, first_column_time = first_column_time,
                    method = "simplex", num_neighbors = "e+1",
                    columns = names(block)[ L_columns[[1]]], target_column = names(block)[2], stats_only = TRUE,
                    theta = NULL)%>% mutate(embedding=1)# %>% mutate(embedding=str_count(cols,",")+1) %>% rename(E1=embedding)
  
  for(ax in 2:max(E)){
  out <- block_lnlp(block, first_column_time = first_column_time,
                    method = "simplex", num_neighbors = "e+1",
                    columns = names(block)[ L_columns[[ax]]], target_column = names(block)[2], stats_only = TRUE,
                    theta = NULL)%>% mutate(embedding=ax) 
    
  out1 <-do.call(rbind,
                 list(out1,out))
                          }
  return(out1)
  }


bio.Yel$SSB<-bio.Yel$B
bio.Yel$Recruitment<-bio.Yel$C
bio.Bohai.raw$SSB<-bio.Bohai.raw$B
bio.Bohai.raw$Recruitment<-bio.Bohai.raw$Recruitment
bio.Bohai.VAST$SSB<-bio.Bohai.VAST$SSB
bio.Bohai.VAST$Recruitment<-bio.Bohai.VAST$Recruitment

E.list <- 1:5
results_multi_ex1 <- do.call(rbind,
                             list(
                do_multivariate_E_analysis(data=bio.Yel,
                     pred_col= "Recruitment",lag_col = "SSB",
                        first_column_time = TRUE, E = E.list) %>%
                       mutate(species = "Yellow_Sea",pred_vars="SSB"),
                do_multivariate_E_analysis(data=bio.Bohai.raw,
                     pred_col = "Recruitment",lag_col = "SSB",
                        first_column_time = TRUE, E = E.list) %>%
                       mutate(species = "YBohai_Sea", pred_vars="SSB"),
                do_multivariate_E_analysis(data=bio.Bohai.VAST,
                                           pred_col = "Recruitment",lag_col = "SSB",
                                           first_column_time = TRUE, E = E.list) %>%
                  mutate(species = "YBohai_Sea_Seasonal", pred_vars="SSB"))
)



names(results_multi_ex1)
head(results_multi_ex1)


results_multi_ex1$rho<-as.numeric(results_multi_ex1$rho)
results_multi_ex1$mae<-as.numeric(results_multi_ex1$mae)
results_multi_ex1$rmse<-as.numeric(results_multi_ex1$rmse)
results_multi_ex1$const_pred_rho<-as.numeric(results_multi_ex1$const_pred_rho)
results_multi_ex1$const_pred_mae<-as.numeric(results_multi_ex1$const_pred_mae)
results_multi_ex1$const_pred_rmse<-as.numeric(results_multi_ex1$const_pred_rmse)


results_multi_ex1$mae[is.na(results_multi_ex1$mae)]<-1
results_multi_ex1$rho[results_multi_ex1$rho<0]<-0



#Make plots
h_mex1 <- vector(mode='list',3)
h_mex1




for(i_species in 1:3){
  species_i <- unique(results_multi_ex1$species)[i_species]
  df.i <- results_multi_ex1 %>%
    filter(species == species_i)
  p <- ggplot(df.i, aes(x = embedding))
  p <- p + geom_line(aes(y = pmax(0,rho), colour = "rho"))
  p <- p + geom_line(aes(y = 1 - pmin(1,mae), colour = "MAE"))
  # add constant predictors
  p <- p + geom_line(aes(y = pmax(0,const_pred_rho), colour = "rho"),lty=2,lwd=
                       0.75)
  p <- p + geom_line(aes(y = 1 - pmin(1,const_pred_mae), colour = "MAE"),lty=2,
                     lwd=0.75)
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(limits = c(-0.01,1),sec.axis = sec_axis(~1-., name = "MAE"))
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c( "red","blue"),labels=expression("MAE","rho"))
  p <- p + labs(y = expression(rho),
                x = "lags of SSB (E)",
                title = species_i,
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.8, 0.2)) + theme_bw()
  h_mex1[[i_species]] <- p
}


do.call(grid.arrange,c(h_mex1,nrow = 1))













h_A <- bio.Yel %>%
  mutate(Recruitment = lead(Recruitment,1)) %>%
  ggplot(aes(x=SSB,y=Recruitment)) + geom_point() +
  labs(x = "Stock(t) (SSB)",y = "Recruits(t+1) (Recruitment)") +
  theme_bw()
h_B <- results_multi_ex1 %>%
  filter(species=="Yellow_Sea") %>%
  filter(pred_vars==c("SSB")) %>%
  ggplot(aes(x=E,y=mae)) + geom_line(lwd=1,col="blue") +
  geom_line(aes(y=const_pred_mae),col="grey60",lty=2) +
  labs(x = "lags of SSB (E)") +
  theme_bw()
block_3d <- bio.Yel %>%
  mutate(Recruitment = lead(Recruitment,1)) %>%
  mutate(SSB_1 = SSB) %>%
  mutate(SSB_2 = lag(SSB,1)) %>%
  mutate_all(funs((. - min(.,na.rm=TRUE))/(max(.,na.rm=TRUE) - min(.,na.rm=TRUE
  ))))
rgl::plot3d(x=block_3d$SSB_1,y=block_3d$SSB_2,
            z=block_3d$Recruitment, 
            xlab = "Stock(t-1)", ylab = "Stock(t-2)",
            zlab = "Recruits(t)",type = "p",size=6.5
            )
#xlab = "x", ylab = "y", zlab = "z",
rgl::lines3d(x=block_3d$SSB_1,y=block_3d$SSB_2,
             z=block_3d$Recruitment,smooth= TRUE)
rgl.material(x=block_3d$SSB_1,y=block_3d$SSB_2,
             z=block_3d$Recruitment,smooth= TRUE)
rgl.postscript("3D S-R Atl6.pdf", fmt = "pdf",drawText = TRUE)
rgl.snapshot("3D S-R Atl6.png")
rgl.close()
## imports the png files
png.i <- readPNG("3D S-R Atl6.png")
h_C <- rasterGrob(png.i, x = unit(0.5, "npc"), 
                  #width = 1, height = 1,
                  y = unit(0.5, "npc"),interpolate=TRUE)
grid.arrange(h_A,h_B,h_C,nrow=1)




h_A <- bio.Bohai.raw %>%
  mutate(Recruitment = lead(Recruitment,1)) %>%
  ggplot(aes(x=SSB,y=Recruitment)) + geom_point() +
  labs(x = "Stock(t) (SSB)",y = "Recruits(t+1) (Recruitment)") +
  theme_bw()
h_B <- results_multi_ex1 %>%
  filter(species=="YBohai_Sea") %>%
  filter(pred_vars==c("SSB")) %>%
  ggplot(aes(x=E,y=mae)) + geom_line(lwd=1,col="blue") +
  geom_line(aes(y=const_pred_mae),col="grey60",lty=2) +
  labs(x = "lags of SSB (E)") +
  theme_bw()
grid.arrange(h_A,h_B,nrow=1)






















###SST lag  xuanzeshezhi

#5.2 Add in Environment
E.list <- 1:5
results_multi_ex2 <- do.call(rbind,
                             list(do_multivariate_E_analysis(data=full_join(bio.Yel
                                                                            ,phys.Yel,by="Year"),
                                                             pred_col = 'Recruitment',lag_col = 'SSB',other_col='Fishing_pressure',
                                                             first_column_time = TRUE, E = E.list) %>%
                                    mutate(species = "Yellow_Sea",pred_vars=paste("SSB","Fishing_pressure",sep = ";")),
                                  do_multivariate_E_analysis(data=full_join(bio.Yel
                                                                            ,phys.Yel,by="Year"),
                                                             pred_col = 'Recruitment',lag_col = 'SSB',other_col='sst_spawn',
                                                             first_column_time = TRUE, E = E.list) %>%
                                    mutate(species = "Yellow_Sea",pred_vars=paste("SSB","sst_spawn",sep = ";")),
                                  do_multivariate_E_analysis(data=full_join(bio.Yel
                                                                            ,phys.Yel,by="Year"),
                                                             pred_col = 'Recruitment',lag_col = 'SSB',other_col='summer_fishing_moratorium',
                                                             first_column_time = TRUE, E = E.list) %>%
                                    mutate(species = "Yellow_Sea",pred_vars=paste("SSB","summer_fishing_moratorium",sep = ";")),
                                  do_multivariate_E_analysis(data=full_join(bio.Bohai.raw,phys.Bohai,by="Year"),
                                                             pred_col = 'Recruitment',lag_col = 'SSB',other_col='Fishing_pressure',
                                                             first_column_time = TRUE, E = E.list) %>%
                                    mutate(species = "YBohai_Sea", pred_vars=paste("SSB","Fishing_pressure",sep = ";")),
                                  do_multivariate_E_analysis(data=full_join(bio.Bohai.raw,phys.Bohai,by="Year"),
                                                             pred_col = 'Recruitment',lag_col = 'SSB',other_col='sst_spawn',
                                                             first_column_time = TRUE, E = E.list) %>%
                                    mutate(species = "YBohai_Sea", pred_vars=paste("SSB","sst_spawn",sep = ";")),
                                  do_multivariate_E_analysis(data=full_join(bio.Bohai.raw,phys.Bohai,by="Year"),
                                                             pred_col = 'Recruitment',lag_col = 'SSB',other_col='summer_fishing_moratorium',
                                                             first_column_time = TRUE, E = E.list) %>%
                                    mutate(species = "YBohai_Sea", pred_vars=paste("SSB","summer_fishing_moratorium",sep = ";")),
                                  do_multivariate_E_analysis(data=full_join(bio.Bohai.VAST,phys.Bohai,by="Year"),
                                                             pred_col = 'Recruitment',lag_col = 'SSB',other_col='Fishing_pressure',
                                                             first_column_time = TRUE, E = E.list) %>%
                                    mutate(species = "YBohai_Sea_Seasonal", pred_vars=paste("SSB","Fishing_pressure",sep = ";")),
                                  do_multivariate_E_analysis(data=full_join(bio.Bohai.VAST,phys.Bohai,by="Year"),
                                                             pred_col = 'Recruitment',lag_col = 'SSB',other_col='summer_fishing_moratorium',
                                                             first_column_time = TRUE, E = E.list) %>%
                                    mutate(species = "YBohai_Sea_Seasonal", pred_vars=paste("SSB","summer_fishing_moratorium",sep = ";")),
                                  do_multivariate_E_analysis(data=full_join(bio.Bohai.VAST,phys.Bohai,by="Year"),
                                                             pred_col = 'Recruitment',lag_col = 'SSB',other_col='sst_spawn',
                                                             first_column_time = TRUE, E = E.list) %>%
                                    mutate(species = "YBohai_Sea_Seasonal", pred_vars=paste("SSB","sst_spawn",sep = ";"))
                                  ))




results_multi_ex2



results_multi_ex2$rho<-as.numeric(results_multi_ex2$rho)
results_multi_ex2$mae<-as.numeric(results_multi_ex2$mae)
results_multi_ex2$rmse<-as.numeric(results_multi_ex2$rmse)
results_multi_ex2$const_pred_rho<-as.numeric(results_multi_ex2$const_pred_rho)
results_multi_ex2$const_pred_mae<-as.numeric(results_multi_ex2$const_pred_mae)
results_multi_ex2$const_pred_rmse<-as.numeric(results_multi_ex2$const_pred_rmse)


results_multi_ex2$mae[is.na(results_multi_ex2$mae)]<-1







h_mex2 <- vector(mode='list',3)
plot_spec <- list(list('Yellow_Sea','SSB;Fishing_pressure')
                  ,list('YBohai_Sea','SSB;sst_spawn'),
                  list('YBohai_Sea_Seasonal','SSB;sst_spawn'))
for(i_plot in 1:length(plot_spec)){
  species_i <- plot_spec[[i_plot]][[1]]
  predvars_i <- plot_spec[[i_plot]][[2]]
  df.i <- results_multi_ex2 %>%
    filter(species == species_i) %>%
    filter(pred_vars == predvars_i)
  p <- ggplot(df.i, aes(x = E))
  p <- p + geom_line(aes(y = pmax(0,rho), colour = "rho"))
  p <- p + geom_line(aes(y = 1 - pmin(1,mae), colour = "MAE"))
  # add constant predictors
  p <- p + geom_line(aes(y = pmax(0,const_pred_rho), colour = "rho"),lty=2,lwd=
                       0.75)
  p <- p + geom_line(aes(y = 1 - pmin(1,const_pred_mae), colour = "MAE"),lty=2,
                     lwd=0.75)
  # now adding the secondary axis, following the example in the help file ?scal  e_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(limits = c(-0.01,1),sec.axis = sec_axis(~1-., name = "MAE"))
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c( "red","blue"),labels=expression("MAE",rho
                                                                           ))
  p <- p + labs(y = expression(rho),
                x = "lags of LPUE (E)",
                title = species_i,
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.8, 0.2)) + theme_bw()
  h_mex2[[i_plot]] <- p
}
do.call(grid.arrange,c(h_mex2,nrow = 1))

#Yel_Fishing-Bo_sst_spawn-BoV_sst_spawn




h_mex2 <- vector(mode='list',3)
plot_spec <- list(list('Yellow_Sea','SSB;Fishing_pressure')
                  ,list('YBohai_Sea','SSB;Fishing_pressure'),
                  list('YBohai_Sea_Seasonal','SSB;Fishing_pressure'))
for(i_plot in 1:length(plot_spec)){
  species_i <- plot_spec[[i_plot]][[1]]
  predvars_i <- plot_spec[[i_plot]][[2]]
  df.i <- results_multi_ex2 %>%
    filter(species == species_i) %>%
    filter(pred_vars == predvars_i)
  p <- ggplot(df.i, aes(x = E))
  p <- p + geom_line(aes(y = pmax(0,rho), colour = "rho"))
  p <- p + geom_line(aes(y = 1 - pmin(1,mae), colour = "MAE"))
  # add constant predictors
  p <- p + geom_line(aes(y = pmax(0,const_pred_rho), colour = "rho"),lty=2,lwd=
                       0.75)
  p <- p + geom_line(aes(y = 1 - pmin(1,const_pred_mae), colour = "MAE"),lty=2,
                     lwd=0.75)
  # now adding the secondary axis, following the example in the help file ?scal  e_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(limits = c(-0.01,1),sec.axis = sec_axis(~1-., name = "MAE"))
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c( "red","blue"),labels=expression("MAE",rho
  ))
  p <- p + labs(y = expression(rho),
                x = "lags of LPUE (E)",
                title = species_i,
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.8, 0.2)) + theme_bw()
  h_mex2[[i_plot]] <- p
}
do.call(grid.arrange,c(h_mex2,nrow = 1))

#Yel_Fishing-Bo_Fishing_pressure-BoV_Fishing_pressure



h_mex2 <- vector(mode='list',3)
plot_spec <- list(list('Yellow_Sea','SSB;sst_spawn')
                  ,list('YBohai_Sea','SSB;sst_spawn'),
                  list('YBohai_Sea_Seasonal','SSB;sst_spawn'))
for(i_plot in 1:length(plot_spec)){
  species_i <- plot_spec[[i_plot]][[1]]
  predvars_i <- plot_spec[[i_plot]][[2]]
  df.i <- results_multi_ex2 %>%
    filter(species == species_i) %>%
    filter(pred_vars == predvars_i)
  p <- ggplot(df.i, aes(x = E))
  p <- p + geom_line(aes(y = pmax(0,rho), colour = "rho"))
  p <- p + geom_line(aes(y = 1 - pmin(1,mae), colour = "MAE"))
  # add constant predictors
  p <- p + geom_line(aes(y = pmax(0,const_pred_rho), colour = "rho"),lty=2,lwd=
                       0.75)
  p <- p + geom_line(aes(y = 1 - pmin(1,const_pred_mae), colour = "MAE"),lty=2,
                     lwd=0.75)
  # now adding the secondary axis, following the example in the help file ?scal  e_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(limits = c(-0.01,1),sec.axis = sec_axis(~1-., name = "MAE"))
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c( "red","blue"),labels=expression("MAE",rho
  ))
  p <- p + labs(y = expression(rho),
                x = "lags of LPUE (E)",
                title = species_i,
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.8, 0.2)) + theme_bw()
  h_mex2[[i_plot]] <- p
}
do.call(grid.arrange,c(h_mex2,nrow = 1))

#Yel_sst_spawn-Bo_sst_spawn-BoV_sst_spawn




h_mex2 <- vector(mode='list',3)
plot_spec <- list(list('Yellow_Sea','SSB;Fishing_pressure')
                  ,list('Yellow_Sea','SSB;sst_spawn'),
                  list('Yellow_Sea','SSB;summer_fishing_moratorium'))
for(i_plot in 1:length(plot_spec)){
  species_i <- plot_spec[[i_plot]][[1]]
  predvars_i <- plot_spec[[i_plot]][[2]]
  df.i <- results_multi_ex2 %>%
    filter(species == species_i) %>%
    filter(pred_vars == predvars_i)
  p <- ggplot(df.i, aes(x = E))
  p <- p + geom_line(aes(y = pmax(0,rho), colour = "rho"))
  p <- p + geom_line(aes(y = 1 - pmin(1,mae), colour = "MAE"))
  # add constant predictors
  p <- p + geom_line(aes(y = pmax(0,const_pred_rho), colour = "rho"),lty=2,lwd=
                       0.75)
  p <- p + geom_line(aes(y = 1 - pmin(1,const_pred_mae), colour = "MAE"),lty=2,
                     lwd=0.75)
  # now adding the secondary axis, following the example in the help file ?scal  e_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(limits = c(-0.01,1),sec.axis = sec_axis(~1-., name = "MAE"))
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c( "red","blue"),labels=expression("MAE",rho
  ))
  p <- p + labs(y = expression(rho),
                x = "lags of LPUE (E)",
                title = species_i,
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.8, 0.2)) + theme_bw()
  h_mex2[[i_plot]] <- p
}
do.call(grid.arrange,c(h_mex2,nrow = 1))

#Yel_Fishing-Yel_sst_spawn-Yel_moratorium









h_mex2 <- vector(mode='list',3)
plot_spec <- list(list('YBohai_Sea','SSB;Fishing_pressure')
                  ,list('YBohai_Sea','SSB;sst_spawn'),
                  list('YBohai_Sea','SSB;summer_fishing_moratorium'))
for(i_plot in 1:length(plot_spec)){
  species_i <- plot_spec[[i_plot]][[1]]
  predvars_i <- plot_spec[[i_plot]][[2]]
  df.i <- results_multi_ex2 %>%
    filter(species == species_i) %>%
    filter(pred_vars == predvars_i)
  p <- ggplot(df.i, aes(x = E))
  p <- p + geom_line(aes(y = pmax(0,rho), colour = "rho"))
  p <- p + geom_line(aes(y = 1 - pmin(1,mae), colour = "MAE"))
  # add constant predictors
  p <- p + geom_line(aes(y = pmax(0,const_pred_rho), colour = "rho"),lty=2,lwd=
                       0.75)
  p <- p + geom_line(aes(y = 1 - pmin(1,const_pred_mae), colour = "MAE"),lty=2,
                     lwd=0.75)
  # now adding the secondary axis, following the example in the help file ?scal  e_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(limits = c(-0.01,1),sec.axis = sec_axis(~1-., name = "MAE"))
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c( "red","blue"),labels=expression("MAE",rho
  ))
  p <- p + labs(y = expression(rho),
                x = "lags of LPUE (E)",
                title = species_i,
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.8, 0.2)) + theme_bw()
  h_mex2[[i_plot]] <- p
}
do.call(grid.arrange,c(h_mex2,nrow = 1))

#JABBA-B_R_Fishing-JABBA-B_R_sst_spawn-JABBA-B_R_moratorium
















h_mex2 <- vector(mode='list',3)
plot_spec <- list(list('YBohai_Sea_Seasonal','SSB;Fishing_pressure')
                  ,list('YBohai_Sea_Seasonal','SSB;sst_spawn'),
                  list('YBohai_Sea_Seasonal','SSB;summer_fishing_moratorium'))
for(i_plot in 1:length(plot_spec)){
  species_i <- plot_spec[[i_plot]][[1]]
  predvars_i <- plot_spec[[i_plot]][[2]]
  df.i <- results_multi_ex2 %>%
    filter(species == species_i) %>%
    filter(pred_vars == predvars_i)
  p <- ggplot(df.i, aes(x = E))
  p <- p + geom_line(aes(y = pmax(0,rho), colour = "rho"))
  p <- p + geom_line(aes(y = 1 - pmin(1,mae), colour = "MAE"))
  # add constant predictors
  p <- p + geom_line(aes(y = pmax(0,const_pred_rho), colour = "rho"),lty=2,lwd=
                       0.75)
  p <- p + geom_line(aes(y = 1 - pmin(1,const_pred_mae), colour = "MAE"),lty=2,
                     lwd=0.75)
  # now adding the secondary axis, following the example in the help file ?scal  e_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(limits = c(-0.01,1),sec.axis = sec_axis(~1-., name = "MAE"))
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c( "red","blue"),labels=expression("MAE",rho
  ))
  p <- p + labs(y = expression(rho),
                x = "lags of LPUE (E)",
                title = species_i,
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.8, 0.2)) + theme_bw()
  h_mex2[[i_plot]] <- p
}
do.call(grid.arrange,c(h_mex2,nrow = 1))

#SSB_R_Fishing-SSB_R_sst_spawn-SSB_R_moratorium



















##We can also make plots that overlay the two multivariate experiments (EX1: only lags of
##LPUE, EX2: include environment).
h_mex2 <- vector(mode='list',3)
plot_spec <- list(list('Yellow_Sea','SSB;Fishing_pressure')
                  ,list('YBohai_Sea','SSB;SST'),
                  list('YBohai_Sea_Seasonal','SSB;SST'))
for(i_plot in 1:length(plot_spec)){
  species_i <- plot_spec[[i_plot]][[1]]
  predvars_i <- plot_spec[[i_plot]][[2]]
  df.i.ex1 <- results_multi_ex1 %>%
    filter(species == species_i) %>%
    filter(pred_vars == 'LPUE')
  df.i.ex2 <- results_multi_ex2 %>%
    filter(species == species_i) %>%
    filter(pred_vars == predvars_i)
  p <- ggplot(df.i.ex2, aes(x = E))
  p <- p + geom_line(aes(y = pmax(0,rho), colour = "rho"))
  # add the MAE line, transformed to match roughly the range of the temperature
  p <- p + geom_line(aes(y = 1 - pmin(1,mae), colour = "MAE"))
  # add constant predictors
  p <- p + geom_line(aes(y = pmax(0,rho),colour = "rho"), data = df.i.ex1 ,lty=
                       2,lwd=0.75)
  p <- p + geom_line(aes(y = 1 - pmin(1,mae),colour = "MAE"), data = df.i.ex1,
                     lty=2,lwd=0.75)
  # now adding the secondary axis, following the example in the help file ?scal  e_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(limits = c(-0.01,1),sec.axis = sec_axis(~1-., name = "MAE"))
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c("red", "blue"),labels=expression("MAE",rho
                                                                           ))
  p <- p + labs(y = expression(rho),
                x = "lags of LPUE (E)",
                title = species_i,
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.8, 0.2)) + theme_bw()
  h_mex2[[i_plot]] <- p
}

do.call(grid.arrange,c(h_mex2,nrow = 1))

