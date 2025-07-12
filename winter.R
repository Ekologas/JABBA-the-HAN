
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
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$sst_winter),c("Recruitment","sst_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#3
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$sst_winter),c("SSB","sst_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#4
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_winter),c("Recruitment","PDO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#5
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$PDO_winter),c("SSB","PDO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#6
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$NPGO_winter),c("Recruitment","NPGO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#7
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$NPGO_winter),c("SSB","NPGO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#8
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$NPA_winter),c("Recruitment","NPA_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#9
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$NPA_winter),c("SSB","NPA_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#10
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$WP_winter),c("Recruitment","WP_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#11
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$WP_winter),c("SSB","WP_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#12
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$NAO_winter),c("Recruitment","NAO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#13
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$NAO_winter),c("SSB","NAO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#14
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$SOI_winter),c("Recruitment","SOI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#15
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$SOI_winter),c("SSB","SOI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#16
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$Nino34_winter),c("Recruitment","Nino34_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#17
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$Nino34_winter),c("SSB","Nino34_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#18
  # do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_winter),c("Recruitment","PDO_winter")], 
  #             ccm_runs_ex2, silent = TRUE) %>%
  # mutate(species = "Yellow_Sea",label="B&C"),#19
  #do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$PDO_winter),c("SSB","PDO_winter")], 
  #            ccm_runs_ex2, silent = TRUE) %>%
  #mutate(species = "Yellow_Sea",label="B&C"),#20
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$NOI_winter),c("Recruitment","NOI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#21
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$NOI_winter),c("SSB","NOI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#22
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$NPI_winter),c("Recruitment","NPI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#23
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$NPI_winter),c("SSB","NPI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#24
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$AOI_winter),c("Recruitment","AOI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#25
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$AOI_winter),c("SSB","AOI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#26
  do_ccm_runs3(block.temp[!is.na(block.temp$Recruitment&block.temp$AMO_winter),c("Recruitment","AMO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#27
  do_ccm_runs3(block.temp[!is.na(block.temp$SSB&block.temp$AMO_winter),c("SSB","AMO_winter")], 
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
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$sst_winter),c("Recruitment","sst_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#3
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$sst_winter),c("SSB","sst_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#4
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_winter),c("Recruitment","PDO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#5
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_winter),c("SSB","PDO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#6
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPGO_winter),c("Recruitment","NPGO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#7
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPGO_winter),c("SSB","NPGO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#8
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPA_winter),c("Recruitment","NPA_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#9
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPA_winter),c("SSB","NPA_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#10
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$WP_winter),c("Recruitment","WP_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#11
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$WP_winter),c("SSB","WP_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#12
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NAO_winter),c("Recruitment","NAO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#13
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$NAO_winter),c("SSB","NAO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#14
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$SOI_winter),c("Recruitment","SOI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#15
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$SOI_winter),c("SSB","SOI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#16
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Nino34_winter),c("Recruitment","Nino34_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#17
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$Nino34_winter),c("SSB","Nino34_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#18
  #do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_winter),c("Recruitment","PDO_winter")], 
  #            ccm_runs_ex2, silent = TRUE) %>%
  #mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  #do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_winter),c("SSB","PDO_winter")], 
  #            ccm_runs_ex2, silent = TRUE) %>%
  #mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NOI_winter),c("Recruitment","NOI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$NOI_winter),c("SSB","NOI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPI_winter),c("Recruitment","NPI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#21
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPI_winter),c("SSB","NPI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#22
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AOI_winter),c("Recruitment","AOI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#23
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$AOI_winter),c("SSB","AOI_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#24
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AMO_winter),c("Recruitment","AMO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#25
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$AMO_winter),c("SSB","AMO_winter")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#26
  do_ccm_runs3(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$summer_fishing_moratorium),c("Recruitment","summer_fishing_moratorium")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#27
  do_ccm_runs3(block.temp1[!is.na(block.temp1$SSB&block.temp1$summer_fishing_moratorium),c("SSB","summer_fishing_moratorium")], 
               ccm_runs_ex2, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R")#28
))

save(results_CCM_ex2,file='./winterresults_CCM_ex2.Rdata')


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
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$sst_winter),c("Recruitment","sst_winter")], 
                   ccm_runs_ex2[3,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#3
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$sst_winter),c("SSB","sst_winter")], 
                   ccm_runs_ex2[4,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#4
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_winter),c("Recruitment","PDO_winter")], 
                   ccm_runs_ex2[5,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#5
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$PDO_winter),c("SSB","PDO_winter")], 
                   ccm_runs_ex2[6,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#6
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$NPGO_winter),c("Recruitment","NPGO_winter")], 
                   ccm_runs_ex2[7,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#7
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$NPGO_winter),c("SSB","NPGO_winter")], 
                   ccm_runs_ex2[8,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#8
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$NPA_winter),c("Recruitment","NPA_winter")], 
                   ccm_runs_ex2[9,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#9
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$NPA_winter),c("SSB","NPA_winter")], 
                   ccm_runs_ex2[10,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#10
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$WP_winter),c("Recruitment","WP_winter")], 
                   ccm_runs_ex2[11,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#11
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$WP_winter),c("SSB","WP_winter")], 
                   ccm_runs_ex2[12,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#12
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$NAO_winter),c("Recruitment","NAO_winter")], 
                   ccm_runs_ex2[13,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#13
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$NAO_winter),c("SSB","NAO_winter")], 
                   ccm_runs_ex2[14,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#14
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$SOI_winter),c("Recruitment","SOI_winter")], 
                   ccm_runs_ex2[15,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#15
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$SOI_winter),c("SSB","SOI_winter")], 
                   ccm_runs_ex2[16,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#16
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$Nino34_winter),c("Recruitment","Nino34_winter")], 
                   ccm_runs_ex2[17,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#17
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$Nino34_winter),c("SSB","Nino34_winter")], 
                   ccm_runs_ex2[18,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#18
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$NOI_winter),c("Recruitment","NOI_winter")], 
                   ccm_runs_ex2[19,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#19
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$NOI_winter),c("SSB","NOI_winter")], 
                   ccm_runs_ex2[20,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#20
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$NPI_winter),c("Recruitment","NPI_winter")], 
                   ccm_runs_ex2[21,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#21
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$NPI_winter),c("SSB","NPI_winter")], 
                   ccm_runs_ex2[22,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#22
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$AOI_winter),c("Recruitment","AOI_winter")], 
                   ccm_runs_ex2[23,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#23
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$AOI_winter),c("SSB","AOI_winter")], 
                   ccm_runs_ex2[24,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#24
  do_null_ccm_runs(block.temp[!is.na(block.temp$Recruitment&block.temp$AMO_winter),c("Recruitment","AMO_winter")], 
                   ccm_runs_ex2[25,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#25
  do_null_ccm_runs(block.temp[!is.na(block.temp$SSB&block.temp$AMO_winter),c("SSB","AMO_winter")], 
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
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$sst_winter),c("Recruitment","sst_winter")], 
                   ccm_runs_ex2[3,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#3
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$sst_winter),c("SSB","sst_winter")], 
                   ccm_runs_ex2[4,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#4
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_winter),c("Recruitment","PDO_winter")], 
                   ccm_runs_ex2[5,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#5
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_winter),c("SSB","PDO_winter")], 
                   ccm_runs_ex2[6,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#6
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPGO_winter),c("Recruitment","NPGO_winter")], 
                   ccm_runs_ex2[7,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#7
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPGO_winter),c("SSB","NPGO_winter")], 
                   ccm_runs_ex2[8,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#8
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPA_winter),c("Recruitment","NPA_winter")], 
                   ccm_runs_ex2[9,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#9
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPA_winter),c("SSB","NPA_winter")], 
                   ccm_runs_ex2[10,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#10
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$WP_winter),c("Recruitment","WP_winter")], 
                   ccm_runs_ex2[11,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#11
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$WP_winter),c("SSB","WP_winter")], 
                   ccm_runs_ex2[12,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#12
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NAO_winter),c("Recruitment","NAO_winter")], 
                   ccm_runs_ex2[13,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#13
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$NAO_winter),c("SSB","NAO_winter")], 
                   ccm_runs_ex2[14,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#14
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$SOI_winter),c("Recruitment","SOI_winter")], 
                   ccm_runs_ex2[15,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#15
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$SOI_winter),c("SSB","SOI_winter")], 
                   ccm_runs_ex2[16,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#16
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Nino34_winter),c("Recruitment","Nino34_winter")], 
                   ccm_runs_ex2[17,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#17
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$Nino34_winter),c("SSB","Nino34_winter")], 
                   ccm_runs_ex2[18,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#18
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NOI_winter),c("Recruitment","NOI_winter")], 
                   ccm_runs_ex2[19,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$NOI_winter),c("SSB","NOI_winter")], 
                   ccm_runs_ex2[20,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPI_winter),c("Recruitment","NPI_winter")], 
                   ccm_runs_ex2[21,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#21
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPI_winter),c("SSB","NPI_winter")], 
                   ccm_runs_ex2[22,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#22
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AOI_winter),c("Recruitment","AOI_winter")], 
                   ccm_runs_ex2[23,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#23
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$AOI_winter),c("SSB","AOI_winter")], 
                   ccm_runs_ex2[24,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#24
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AMO_winter),c("Recruitment","AMO_winter")], 
                   ccm_runs_ex2[25,],
                   n.surr = number_of_surrogates, silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#25
  do_null_ccm_runs(block.temp1[!is.na(block.temp1$SSB&block.temp1$AMO_winter),c("SSB","AMO_winter")], 
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


save(results_null_CCM_ex2,file='./winterenv_ccm_null.Rdata')
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
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$sst_winter),c("Recruitment","sst_winter")], 
                      ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1),silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#3
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$sst_winter),c("SSB","sst_winter")], 
                      ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#4
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_winter),c("Recruitment","PDO_winter")], 
                      ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#5
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$PDO_winter),c("SSB","PDO_winter")], 
                      ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#6
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$NPGO_winter),c("Recruitment","NPGO_winter")], 
                      ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#7
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$NPGO_winter),c("SSB","NPGO_winter")], 
                      ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#8
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$NPA_winter),c("Recruitment","NPA_winter")], 
                      ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#9
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$NPA_winter),c("SSB","NPA_winter")], 
                      ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#10
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$WP_winter),c("Recruitment","WP_winter")], 
                      ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#11
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$WP_winter),c("SSB","WP_winter")], 
                      ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#12
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$NAO_winter),c("Recruitment","NAO_winter")], 
                      ccm_runs_ex2[13,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#13
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$NAO_winter),c("SSB","NAO_winter")], 
                      ccm_runs_ex2[14,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#14
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$SOI_winter),c("Recruitment","SOI_winter")], 
                      ccm_runs_ex2[15,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#15
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$SOI_winter),c("SSB","SOI_winter")], 
                      ccm_runs_ex2[16,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#16
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$Nino34_winter),c("Recruitment","Nino34_winter")], 
                      ccm_runs_ex2[17,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#17
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$Nino34_winter),c("SSB","Nino34_winter")], 
                      ccm_runs_ex2[18,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#18
  # do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_winter),c("Recruitment","PDO_winter")], 
  #             ccm_runs_ex2, silent = TRUE) %>%
  # mutate(species = "Yellow_Sea",label="B&C"),#19
  #do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$PDO_winter),c("SSB","PDO_winter")], 
  #            ccm_runs_ex2, silent = TRUE) %>%
  #mutate(species = "Yellow_Sea",label="B&C"),#20
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$NOI_winter),c("Recruitment","NOI_winter")], 
                      ccm_runs_ex2[19,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#21
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$NOI_winter),c("SSB","NOI_winter")], 
                      ccm_runs_ex2[20,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#22
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$NPI_winter),c("Recruitment","NPI_winter")], 
                      ccm_runs_ex2[21,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#23
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$NPI_winter),c("SSB","NPI_winter")], 
                      ccm_runs_ex2[22,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#24
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$AOI_winter),c("Recruitment","AOI_winter")], 
                      ccm_runs_ex2[23,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#25
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$AOI_winter),c("SSB","AOI_winter")], 
                      ccm_runs_ex2[24,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#26
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$Recruitment&block.temp$AMO_winter),c("Recruitment","AMO_winter")], 
                      ccm_runs_ex2[25,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "Yellow_Sea",label="B&C"),#27
  do_ccm_lag_analysis(block.temp[!is.na(block.temp$SSB&block.temp$AMO_winter),c("SSB","AMO_winter")], 
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
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$sst_winter),c("Recruitment","sst_winter")], 
                      ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#3
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$sst_winter),c("SSB","sst_winter")], 
                      ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#4
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_winter),c("Recruitment","PDO_winter")], 
                      ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#5
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_winter),c("SSB","PDO_winter")], 
                      ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#6
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPGO_winter),c("Recruitment","NPGO_winter")], 
                      ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#7
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPGO_winter),c("SSB","NPGO_winter")], 
                      ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#8
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPA_winter),c("Recruitment","NPA_winter")], 
                      ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#9
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPA_winter),c("SSB","NPA_winter")], 
                      ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#10
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$WP_winter),c("Recruitment","WP_winter")], 
                      ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#11
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$WP_winter),c("SSB","WP_winter")], 
                      ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#12
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NAO_winter),c("Recruitment","NAO_winter")], 
                      ccm_runs_ex2[13,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#13
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$NAO_winter),c("SSB","NAO_winter")], 
                      ccm_runs_ex2[14,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#14
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$SOI_winter),c("Recruitment","SOI_winter")], 
                      ccm_runs_ex2[15,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#15
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$SOI_winter),c("SSB","SOI_winter")], 
                      ccm_runs_ex2[16,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#16
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Nino34_winter),c("Recruitment","Nino34_winter")], 
                      ccm_runs_ex2[17,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#17
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$Nino34_winter),c("SSB","Nino34_winter")], 
                      ccm_runs_ex2[18,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#18
  #do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_winter),c("Recruitment","PDO_winter")], 
  #            ccm_runs_ex2, silent = TRUE) %>%
  #mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  #do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_winter),c("SSB","PDO_winter")], 
  #            ccm_runs_ex2, silent = TRUE) %>%
  #mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NOI_winter),c("Recruitment","NOI_winter")], 
                      ccm_runs_ex2[19,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$NOI_winter),c("SSB","NOI_winter")], 
                      ccm_runs_ex2[20,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPI_winter),c("Recruitment","NPI_winter")], 
                      ccm_runs_ex2[21,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#21
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPI_winter),c("SSB","NPI_winter")], 
                      ccm_runs_ex2[22,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#22
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AOI_winter),c("Recruitment","AOI_winter")], 
                      ccm_runs_ex2[23,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#23
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$AOI_winter),c("SSB","AOI_winter")], 
                      ccm_runs_ex2[24,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#24
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AMO_winter),c("Recruitment","AMO_winter")], 
                      ccm_runs_ex2[25,], lag_list = seq(-5,0,by=1), silent = TRUE) %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#25
  do_ccm_lag_analysis(block.temp1[!is.na(block.temp1$SSB&block.temp1$AMO_winter),c("SSB","AMO_winter")], 
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
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$sst_winter),c("Recruitment","sst_winter")], 
                       ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1),silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#3
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$sst_winter),c("SSB","sst_winter")], 
                       ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#4
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_winter),c("Recruitment","PDO_winter")], 
                       ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#5
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$PDO_winter),c("SSB","PDO_winter")], 
                       ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#6
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$NPGO_winter),c("Recruitment","NPGO_winter")], 
                       ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#7
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$NPGO_winter),c("SSB","NPGO_winter")], 
                       ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#8
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$NPA_winter),c("Recruitment","NPA_winter")], 
                       ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#9
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$NPA_winter),c("SSB","NPA_winter")], 
                       ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#10
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$WP_winter),c("Recruitment","WP_winter")], 
                       ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#11
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$WP_winter),c("SSB","WP_winter")], 
                       ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#12
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$NAO_winter),c("Recruitment","NAO_winter")], 
                       ccm_runs_ex2[13,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#13
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$NAO_winter),c("SSB","NAO_winter")], 
                       ccm_runs_ex2[14,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#14
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$SOI_winter),c("Recruitment","SOI_winter")], 
                       ccm_runs_ex2[15,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#15
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$SOI_winter),c("SSB","SOI_winter")], 
                       ccm_runs_ex2[16,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#16
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$Nino34_winter),c("Recruitment","Nino34_winter")], 
                       ccm_runs_ex2[17,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#17
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$Nino34_winter),c("SSB","Nino34_winter")], 
                       ccm_runs_ex2[18,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#18
  # do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$PDO_winter),c("Recruitment","PDO_winter")], 
  #             ccm_runs_ex2, silent = TRUE)[,c(-2,-3)] %>%
  # mutate(species = "Yellow_Sea",label="B&C"),#19
  #do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$PDO_winter),c("SSB","PDO_winter")], 
  #            ccm_runs_ex2, silent = TRUE)[,c(-2,-3)] %>%
  #mutate(species = "Yellow_Sea",label="B&C"),#20
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$NOI_winter),c("Recruitment","NOI_winter")], 
                       ccm_runs_ex2[19,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#21
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$NOI_winter),c("SSB","NOI_winter")], 
                       ccm_runs_ex2[20,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#22
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$NPI_winter),c("Recruitment","NPI_winter")], 
                       ccm_runs_ex2[21,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#23
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$NPI_winter),c("SSB","NPI_winter")], 
                       ccm_runs_ex2[22,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)]%>%
    mutate(species = "Yellow_Sea",label="B&C"),#24
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$AOI_winter),c("Recruitment","AOI_winter")], 
                       ccm_runs_ex2[23,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#25
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$AOI_winter),c("SSB","AOI_winter")], 
                       ccm_runs_ex2[24,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#26
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$Recruitment&block.temp$AMO_winter),c("Recruitment","AMO_winter")], 
                       ccm_runs_ex2[25,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "Yellow_Sea",label="B&C"),#27
  do_ccm_lag_analysis2(block.temp[!is.na(block.temp$SSB&block.temp$AMO_winter),c("SSB","AMO_winter")], 
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
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$sst_winter),c("Recruitment","sst_winter")], 
                       ccm_runs_ex2[3,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#3
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$sst_winter),c("SSB","sst_winter")], 
                       ccm_runs_ex2[4,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#4
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_winter),c("Recruitment","PDO_winter")], 
                       ccm_runs_ex2[5,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#5
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_winter),c("SSB","PDO_winter")], 
                       ccm_runs_ex2[6,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#6
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPGO_winter),c("Recruitment","NPGO_winter")], 
                       ccm_runs_ex2[7,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#7
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPGO_winter),c("SSB","NPGO_winter")], 
                       ccm_runs_ex2[8,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#8
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPA_winter),c("Recruitment","NPA_winter")], 
                       ccm_runs_ex2[9,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#9
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPA_winter),c("SSB","NPA_winter")], 
                       ccm_runs_ex2[10,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#10
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$WP_winter),c("Recruitment","WP_winter")], 
                       ccm_runs_ex2[11,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#11
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$WP_winter),c("SSB","WP_winter")], 
                       ccm_runs_ex2[12,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#12
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NAO_winter),c("Recruitment","NAO_winter")], 
                       ccm_runs_ex2[13,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#13
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$NAO_winter),c("SSB","NAO_winter")], 
                       ccm_runs_ex2[14,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#14
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$SOI_winter),c("Recruitment","SOI_winter")], 
                       ccm_runs_ex2[15,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#15
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$SOI_winter),c("SSB","SOI_winter")], 
                       ccm_runs_ex2[16,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#16
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$Nino34_winter),c("Recruitment","Nino34_winter")], 
                       ccm_runs_ex2[17,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#17
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$Nino34_winter),c("SSB","Nino34_winter")], 
                       ccm_runs_ex2[18,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#18
  #do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$PDO_winter),c("Recruitment","PDO_winter")], 
  #            ccm_runs_ex2, silent = TRUE)[,c(-2,-3)] %>%
  #mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  #do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$PDO_winter),c("SSB","PDO_winter")], 
  #            ccm_runs_ex2, silent = TRUE)[,c(-2,-3)] %>%
  #mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NOI_winter),c("Recruitment","NOI_winter")], 
                       ccm_runs_ex2[19,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#19
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$NOI_winter),c("SSB","NOI_winter")], 
                       ccm_runs_ex2[20,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#20
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$NPI_winter),c("Recruitment","NPI_winter")], 
                       ccm_runs_ex2[21,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#21
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$NPI_winter),c("SSB","NPI_winter")], 
                       ccm_runs_ex2[22,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#22
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AOI_winter),c("Recruitment","AOI_winter")], 
                       ccm_runs_ex2[23,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#23
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$AOI_winter),c("SSB","AOI_winter")], 
                       ccm_runs_ex2[24,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#24
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$Recruitment&block.temp1$AMO_winter),c("Recruitment","AMO_winter")], 
                       ccm_runs_ex2[25,], lag_list = seq(-5,0,by=1), silent = TRUE)[,c(-2,-3)] %>%
    mutate(species = "YBohai_Sea",label="JABBA_B&R"),#25
  do_ccm_lag_analysis2(block.temp1[!is.na(block.temp1$SSB&block.temp1$AMO_winter),c("SSB","AMO_winter")], 
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


save(results_null_CCM_ex2,file='./winterenv_ccm_null.Rdata')
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
                                                             pred_col = 'Recruitment',lag_col = 'SSB',other_col='sst_winter',
                                                             first_column_time = TRUE, E = E.list) %>%
                                    mutate(species = "Yellow_Sea",pred_vars=paste("SSB","sst_winter",sep = ";")),
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
                                                             pred_col = 'Recruitment',lag_col = 'SSB',other_col='sst_winter',
                                                             first_column_time = TRUE, E = E.list) %>%
                                    mutate(species = "YBohai_Sea", pred_vars=paste("SSB","sst_winter",sep = ";")),
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
                                                             pred_col = 'Recruitment',lag_col = 'SSB',other_col='sst_winter',
                                                             first_column_time = TRUE, E = E.list) %>%
                                    mutate(species = "YBohai_Sea_Seasonal", pred_vars=paste("SSB","sst_winter",sep = ";"))
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
                  ,list('YBohai_Sea','SSB;sst_winter'),
                  list('YBohai_Sea_Seasonal','SSB;sst_winter'))
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

#Yel_Fishing-Bo_sst_winter-BoV_sst_winter




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
plot_spec <- list(list('Yellow_Sea','SSB;sst_winter')
                  ,list('YBohai_Sea','SSB;sst_winter'),
                  list('YBohai_Sea_Seasonal','SSB;sst_winter'))
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

#Yel_sst_winter-Bo_sst_winter-BoV_sst_winter




h_mex2 <- vector(mode='list',3)
plot_spec <- list(list('Yellow_Sea','SSB;Fishing_pressure')
                  ,list('Yellow_Sea','SSB;sst_winter'),
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

#Yel_Fishing-Yel_sst_winter-Yel_moratorium









h_mex2 <- vector(mode='list',3)
plot_spec <- list(list('YBohai_Sea','SSB;Fishing_pressure')
                  ,list('YBohai_Sea','SSB;sst_winter'),
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

#JABBA-B_R_Fishing-JABBA-B_R_sst_winter-JABBA-B_R_moratorium
















h_mex2 <- vector(mode='list',3)
plot_spec <- list(list('YBohai_Sea_Seasonal','SSB;Fishing_pressure')
                  ,list('YBohai_Sea_Seasonal','SSB;sst_winter'),
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

#SSB_R_Fishing-SSB_R_sst_winter-SSB_R_moratorium

