
# Installation
# install.packages(devtools)
# UPDATE Latest Version
# devtools:: install_github("jabbamodel/JABBA")

library(JABBA)
library(gplots)
library(rjags)
library("fitdistrplus")
library(reshape)
library(reshape2)

File = "D:/01/剩余产量模型JABBA_SPiCT_BSM等/JABBA运行/黄渤海蓝点马鲛/Output" # LINK to your folder of choice here
InputFiles="D:/01/剩余产量模型JABBA_SPiCT_BSM等/JABBA运行/黄渤海蓝点马鲛/Input" # LINK to your folder of choice here
setwd(InputFiles)

#><>><>><>><>><>><>><>><>><>><>><>
# 黄渤海蓝点马鲛
#><>><>><>><>><>><>><>><>><>><>><>

catch<-read.table("Catch.csv",header=TRUE,sep=",")
cpue<-read.table("cpue.csv",header=TRUE,sep=",")
#se<-read.table("se.csv",header=TRUE,sep=",")
se=NULL
head(catch)
head(cpue)
#head(se)
#cpue<-cpue[,-10]
#load("D:/01/剩余产量模型JABBA_SPiCT_BSM等/JABBA运行/黄渤海蓝点马鲛/Output/s1_Schaefer/s1_Schaefer_Schaefer_jabba.rdata")
#bet1<-jabba
#去除1963年前该渔业技术尚未发达时的CPUE
cpue[c(1:4),3]<-NA #去除1963年前该渔业技术尚未发达时的CPUE
cpue[4,9]<-NA #去除1963年前该渔业技术尚未发达时的CPUE
head(cpue)

assessment = "ss9_Schaefer"
output.dir = file.path(File,assessment)
dir.create(output.dir,showWarnings = T)
setwd(output.dir)
#------------------------------------------------------
# Simple example fit JABBA to Catch and CPUE with SEs
#-------------------------------------------------------

# Compile JABBA JAGS model and input object
?build_jabba
jbinput1 = build_jabba(catch=catch,cpue=cpue,se=NULL,
                       assessment=assessment,scenario = "Schaefer",
                       model.type = c("Schaefer", "Fox", "Pella", "Pella_m")[1],
                       sigma.est = FALSE, # estimate additional observation error
                       fixed.obsE = ifelse(is.null(se), 0.2, 0.01), # mimum observation error
                       igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                       #igamma=c(4,0.01)
                       r.dist = c("lnorm","range")[1],r.prior = c(0.5,0.2),
                       K.dist = c("lnorm","range")[1],K.prior =c(3233.538,0.46),
                       psi.dist =c("lnorm","beta")[1],psi.prior = c(0.7,0.12), # Initial depletion B/K
                       projection =T,pyrs =15,imp.yr = 2024,
                       TACs = seq(80,280,20),TACint = NULL,
                       P_bound= c(0.02,1.3),Plim = 0,
                       catch.metric = "000 t",
                       verbose=T)
jbinput1$jagsdata
jbinput1$settings
jbinput1$settings$params


# Check input
jbplot_indices(jbinput1)#?jbplot_indices
jbplot_indices(jbinput1, as.png = T)#legend.loc = "topleft",

# Fit JABBA (here mostly default value - careful)
?fit_jabba
bet1 = fit_jabba(jbinput1,quickmcmc = F,
                 save.jabba=T,
                 peels = 0,
                 save.all = T,
                 save.trj = T,
                 save.csvs = T,
                 verbose =T) #  run

names(bet1)
head(bet1$kobe)
write.csv(bet1$kobe,file = "kobe.csv")
bet1$timeseries
write.csv(bet1$timeseries,file = "timeseries.csv")

# Make individual plots width5 height3.5
jbplot_catch(bet1) #?jbplot_catch()   ,as.png = T,    add = T,
jbplot_catch(bet1,as.png = T)
jbplot_catcherror(bet1)
jbplot_ppdist(bet1)
jbplot_ppdist(bet1,as.png = T)#?jbplot_ppdist #width = 10,
jbplot_mcmc(bet1)
jbplot_mcmc(bet1,as.png = T)
jbplot_residuals(bet1)
jbplot_residuals(bet1,as.png = T)
jbplot_cpuefits(bet1)
jbplot_cpuefits(bet1,as.png = T)
jbplot_runstest(bet1)
jbplot_runstest(bet1,as.png = T)
jbplot_logfits(bet1)
jbplot_procdev(bet1)
jbplot_PPC(bet1) # Posterior Predictive Checks - Not great should 0.2-0.8
jbplot_spphase(bet1)  #jbplot_spphase(bet1,add=T)
jbplot_spphase(bet1,as.png = T) #?jbplot_spphase 5 4.5  pdf4.5 5
jbplot_spdyn(bet1)
jbplot_spdyn(bet1,as.png = T)#?jbplot_spdyn5 4.5  pdf4.5 5
jbplot_kobe(bet1)#?jbplot_kobe5 4.5  pdf4.5 5
jbplot_kobe(bet1,as.png = T)
jbplot_biplot(bet1)
jbplot_prj(bet1,type = c("BB0", "BBmsy", "FFmsy")[1])    
jbplot_prj(bet1,type = c("BB0", "BBmsy", "FFmsy")[2])  
jbplot_prj(bet1,type = c("BB0", "BBmsy", "FFmsy")[3])
jbplot_trj(bet1)#?jbplot_trj 5 3.5
jbplot_trj(bet1,as.png = T)#pdf3.5 4
jbplot_stdresiduals(bet1)
jbplot_stdresiduals(bet1,as.png = T)

# Status
jbplot_summary(bet1)      #pdf5.58 6.36 ?jbplot_summary()  jbplot_summary(bet1,as.png = T)
# combine plots  536 520  pdf5.58 5.36
jbpar(mfrow=c(2,2))
jbplot_summary(bet1,add=T,type = c("BBmsy", "FFmsy"))
jbplot_spphase(bet1,add=T)
jbplot_kobe(bet1,add=T)

jbpar(mfrow=c(3,2),plot.cex = 0.8)
jbplot_ensemble(bet1,xlim = NULL, legend.loc = "topleft",verbose = T)  
#?jbplot_ensemble 应该是000吨  绘图有错误啊
#536 596  pdf5.36 5.96  

#Plots plots JABBA ensemble models + projections - joint or by run
#?fw_jabba

# Try to improve runs test diagnostics by changing the variance settings
# Increase minimum obs error from 0.01 to 0.1 and remove SEs from CPUE model
jbinput2 = build_jabba(catch=catch,cpue=cpue,se=NULL,assessment=assessment,
                       scenario = "Run2",model.type = c("Schaefer", "Fox", "Pella", "Pella_m")[1],
                       sigma.est = TRUE,
                       fixed.obsE = ifelse(is.null(se), 0.25, 0.01), # mimum observation error
                       igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                       #igamma=c(4,0.01)
                       r.dist = c("lnorm","range")[1],r.prior = c(0.5,0.2),
                       K.dist = c("lnorm","range")[1],K.prior =c(3233.538,0.46),
                       psi.dist =c("lnorm","beta")[1],psi.prior = c(0.7,0.12), # Initial depletion B/K
                       projection =T,pyrs =15,imp.yr = 2024,
                       TACs = seq(80,280,20),TACint = NULL,
                       P_bound= c(0.02,1.3),Plim = 0,
                       catch.metric = "000 t",
                       verbose=T)


bet2= fit_jabba(jbinput2,quickmcmc = F,
                save.jabba=T,
                peels = 0,
                save.all = T,
                save.trj = T,
                save.csvs = T,
                verbose =T) #  run

# Check residual diags
jbplot_cpuefits(bet2)
jbplot_runstest(bet2)
jbplot_runstest(bet2,as.png = T)#?jbplot_runstest
jbplot_logfits(bet2)
jbplot_PPC(bet2,as.png = T)
jbplot_PPC(bet2)
jbplot_residuals(bet2)
jbplot_kobe(bet2)

# Improved
refinput = jbinput2 # Note as reference input 

# Compare
jbplot_summary(list(Run1=bet1,Run2=bet2))# combine plots  536 620  pdf6.58 5.36
jbplot_ensemble(list(Run1=bet1,Run2=bet2))#536 596  pdf6.58 5.36  

# Check parameters and convergence (p <0.05 is not fully converged)
bet2$pars 
# Make a long MCMC run with 3 chains
bet.full = fit_jabba(jbinput2,nc=3,quickmcmc = F,
                     save.jabba=T,
                     peels = 0,
                     save.all = T,
                     save.trj = T,
                     save.csvs = T,
                     verbose =T)

# MCMC convergence
bet.full$pars 
jbplot_mcmc(bet.full)

# get quantaties
bet.full$estimates
# FLR data.frame trajectories
bet.full$flqs
# fits
bet.full$diags


#------------------------------------------------------
# Estimate shape m as function of Bmsy/K
#-------------------------------------------------------

# Compile JABBA JAGS model and input object
jbinput3 = build_jabba(catch=catch,cpue=cpue,se=se,assessment=assessment,
                       scenario = "Est.Shape",model.type = "Pella_m", # Estimate shape
                       BmsyK=0.4, # mean 40%B0
                       shape.CV = 0.3, #CV
                       sigma.est = TRUE,
                       fixed.obsE = ifelse(is.null(se), 0.2, 0.01), # mimum observation error
                       igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                       #igamma=c(4,0.01)
                       r.dist = c("lnorm","range")[1],r.prior = c(0.5,0.2),
                       K.dist = c("lnorm","range")[1],K.prior =c(3233.538,0.46),
                       psi.dist =c("lnorm","beta")[1],psi.prior = c(0.7,0.12), # Initial depletion B/K
                       projection =T,pyrs =15,imp.yr = 2024,
                       TACs = seq(80,280,20),TACint = NULL,
                       P_bound= c(0.02,1.3),Plim = 0,
                       catch.metric = "000 t",
                       verbose=T)

bet3 = fit_jabba(jbinput3,quickmcmc = F,
                 save.jabba=T,
                 peels = 0,
                 save.all = T,
                 save.trj = T,
                 save.csvs = T,
                 verbose =T)

jbplot_ppdist(bet3) # check shape prior & posterior dist - not much information
# Compare
jbplot_summary(list(bet1,bet3))
jbplot_ensemble(list(bet1,bet3))

# also run model as Schaefer
#jbinput4 = build_jabba(catch=catch,cpue=cpue,se=NULL,assessment=assessment,
 #                      scenario = "Schaefer",model.type = "Schaefer", # Estimate shape
  #                     sigma.est = TRUE,fixed.obsE = 0.1,igamma = c(0.001,0.001),
   #                    psi.prior = c(1,0.1))

#bet4 = fit_jabba(jbinput4,quickmcmc=T)

# Compare 
jbpar(mfrow=c(3,2),plot.cex=0.7)
jbplot_ensemble(list(bet1,bet2,bet3))#,bet4
jbpar(mfrow=c(3,2),plot.cex=0.6)
jbplot_summary(list(bet1,bet2,bet3),add=T)#,bet4

#----------------------------------------------------
# Do some forecasting
#----------------------------------------------------

# F-based forecasting
# Relative Fmsy
# Single Forecast for Base-Case model - now works with imp.yr=1 
?fw_jabba#JABBA 的外部远期预测
#quant = c("Catch", "F")[2],type = c("ratio", "msy", "abs")[2],

fw1 = fw_jabba(bet1,nyears=5,imp.yr=2024,
               imp.values = seq(0.8,1.2,0.1),quant=c("Catch", "F")[2],
               type=c("ratio", "msy", "abs")[2],stochastic = c(TRUE, FALSE)[1])
#imp.values	vector Catch or F scenarios provide as absolute or ratios
#jbpar(mfrow=c(3,2))
jbpar(mfrow=c(3,2),plot.cex = 0.7)
jbplot_ensemble(fw1)
# Zoom-in
jbplot_ensemble(fw1,xlim=c(2010,2034))
abline(v=2023) # Check
# Forecast with AR1 process error
fw1.ar1 = fw_jabba(bet2,nyears=10,imp.yr=1,quant="F",type="msy",AR1=TRUE,stochastic = T)
# now compare
jbpar(mfrow=c(3,2),plot.cex = 0.6)
for(i in 1:3){
  jbplot_ensemble(fw1,subplots = c(1,2,5)[i],add=T,xlim=c(2010,2028),legend=ifelse(i==1,T,F))
  jbplot_ensemble(fw1.ar1,subplots = c(1,2,5)[i],add=T,xlim=c(2010,2028),legend=ifelse(i==1,T,F))
}
mtext(c("Default","AR1"),outer=T,at=c(0.27,0.77))

# IOTC-Style: Relative current catch (default mean 3 yrs)
# 10 years, 2 intermediate years, deterministic
fw.io = fw_jabba(bet2,nyears=10,imp.yr=3,imp.values = seq(0.6,1.2,0.1),quant="Catch",type="ratio",nsq=3,stochastic = F)
jbplot_ensemble(fw.io)
jbpar(mfrow=c(2,2))
jbplot_ensemble(fw.io,add=T,subplots = 1,legend.loc = "topright")
jbplot_ensemble(fw.io,add=T,subplots = 2,legend=F)
jbplot_ensemble(fw.io,add=T,subplots = 5,legend=F)
jbplot_ensemble(fw.io,add=T,subplots = 6,legend=F)

# ICCAT Style
Ccur = mean(tail(jbinput2$data$catch[,2],2))
TACs = c(75500,seq(60000,78000,2000))
fw.iccat= fw_jabba(bet2,nyears=10,imp.yr=3,initial = c(Ccur,76000),imp.values = TACs,quant="Catch",type="abs",nsq=3,stochastic = F,AR1=T)

jbpar(mfrow=c(2,2))
jbplot_ensemble(fw.iccat,legendcex = 0.4,xlim=c(2010,2027),subplots = c(1,2,5,6),add=T)
jbplot_ensemble(fw.iccat,legendcex = 0.4,xlim=c(2010,2027),subplots = c(1,2,5,6),plotCIs = F)

# Check if correct
jbpar()
jbplot_ensemble(fw.iccat,legendcex = 0.4,xlim=c(2010,2027),subplots = c(6),plotCIs = F,add=T)
abline(v=c(2017,2018,2019,2020)) # 2020 = imp.yr

# Do Ensemble modelling
jbplot_ensemble(list(bet2,bet3,bet4))
?jbplot_ensemble
# Joint all runs
ens = jbplot_ensemble(list(bet2,bet3,bet4),kbout=T,joint=T)

# Do ensemble forecast

fw.ens= fw_jabba(list(bet2,bet3,bet4),nyears=10,imp.yr=2,initial = Ccur,imp.values = TACs,quant="Catch",type="abs",nsq=3,stochastic = F,AR1=T,thin=3)
jbpar(mfrow=c(3,2),plot.cex = 0.6)
for(i in 1:6) jbplot_ensemble(fw.ens,add=T,subplots = i,legend = ifelse(i==2,T,F))


#----------------------------------------------------------------
# Conduct Retrospective Analysis and Hind-Cast Cross-Validation
#----------------------------------------------------------------
?hindcast_jabba
# Do hindcast cross-validation
hc1 = hindcast_jabba(jbinput1,bet1,peels=1:5)

# Show Retrospective Pattern
mohns= jbplot_retro(hc1)  #?jbplot_retro  536  736
jbplot_retro(hc1, as.png = T)

mohns
write.csv(mohns,"mohns.csv")
mohns[row.names(mohns)=="rho.mu",]

hindcasts = hc1
# Make alternative forecasts
hc2 = jbhcxval(hc1,AR1=T) # make forecasts with AR1

jbpar(mfrow=c(1,2))
for(i in 1:1){
  jbplot_hcxval(hc1,index=c(2)[i],add=T,minyr = 2020,legend.add = F)
  jbplot_hcxval(hc2,index=c(2)[i],add=T,minyr = 2007,legend.add = F)
}
mtext(c("Default","AR1"),outer=T,at=c(0.27,0.77))

jbmase(hc2)











########################################################################
##########################################################################
########################################################################
##########################################################################
########################################################################
###########################################################################
assessment = "ss10_Fox"
output.dir = file.path(File,assessment)
dir.create(output.dir,showWarnings = T)
setwd(output.dir)
#------------------------------------------------------
# Simple example fit JABBA to Catch and CPUE with SEs
#-------------------------------------------------------

# Compile JABBA JAGS model and input object
?build_jabba
jbinputFox = build_jabba(catch=catch,cpue=cpue,se=se,
                       assessment=assessment,scenario = "Fox",
                       model.type = c("Schaefer", "Fox", "Pella", "Pella_m")[2],
                       sigma.est = FALSE, # estimate additional observation error
                       fixed.obsE = ifelse(is.null(se), 0.2, 0.01), # mimum observation error
                       igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                       #igamma=c(4,0.01)
                       r.dist = c("lnorm","range")[1],r.prior = c(0.5,0.2),
                       K.dist = c("lnorm","range")[1],K.prior =c(3233.538,0.46),
                       psi.dist =c("lnorm","beta")[1],psi.prior = c(0.7,0.12), # Initial depletion B/K
                       projection =T,pyrs =15,imp.yr = 2024,
                       TACs = seq(80,280,20),TACint = NULL,
                       P_bound= c(0.02,1.3),Plim = 0,
                       catch.metric = "000 t",
                       verbose=T)
jbinputFox$jagsdata
jbinputFox$settings
jbinputFox$settings$params


# Check input
jbplot_indices(jbinputFox)


# Fit JABBA (here mostly default value - careful)
?fit_jabba
betFox = fit_jabba(jbinputFox,quickmcmc = F,
                 save.jabba=T,
                 peels = 0,
                 save.all = T,
                 save.trj = T,
                 save.csvs = T,
                 verbose =T) #  run

names(betFox)
head(betFox$kobe)
write.csv(betFox$kobe,file = "kobe.csv")
betFox$timeseries
write.csv(betFox$timeseries,file = "timeseries.csv")

# Make individual plots width5 height3.5
jbplot_catch(betFox) #?jbplot_catch()   ,as.png = T,    add = T,
jbplot_catcherror(betFox)
jbplot_ppdist(betFox)
jbplot_ppdist(betFox,width = 10,as.png = T)#?jbplot_ppdist
jbplot_mcmc(betFox)
jbplot_residuals(betFox)
jbplot_residuals(betFox,as.png = T)
jbplot_cpuefits(betFox)
jbplot_cpuefits(betFox,as.png = T)
jbplot_runstest(betFox)
jbplot_runstest(betFox,as.png = T)
jbplot_logfits(betFox)
jbplot_procdev(betFox)
jbplot_PPC(betFox) # Posterior Predictive Checks - Not great should 0.2-0.8
jbplot_spphase(betFox)  #jbplot_spphase(betFox,add=T)
jbplot_spphase(betFox,as.png = T) #?jbplot_spphase 5 4.5  pdf4.5 5
jbplot_spdyn(betFox)
jbplot_spdyn(betFox,as.png = T)#?jbplot_spdyn5 4.5  pdf4.5 5
jbplot_kobe(betFox)#?jbplot_kobe5 4.5  pdf4.5 5
jbplot_kobe(betFox,as.png = T)
jbplot_biplot(betFox)
jbplot_prj(betFox,type = c("BB0", "BBmsy", "FFmsy")[1])    
jbplot_prj(betFox,type = c("BB0", "BBmsy", "FFmsy")[2])  
jbplot_prj(betFox,type = c("BB0", "BBmsy", "FFmsy")[3])
jbplot_trj(betFox)#?jbplot_trj 5 3.5
jbplot_trj(betFox,as.png = T)#pdf3.5 4
jbplot_stdresiduals(betFox)
jbplot_stdresiduals(betFox,as.png = T)

# Status
jbplot_summary(betFox)      #pdf5.58 6.36 ?jbplot_summary()  jbplot_summary(betFox,as.png = T)
# combine plots  536 520  pdf5.58 5.36
jbpar(mfrow=c(2,2))
jbplot_summary(betFox,add=T,type = c("BBmsy", "FFmsy"))
jbplot_spphase(betFox,add=T)
jbplot_kobe(betFox,add=T)

jbpar(mfrow=c(3,2),plot.cex = 0.8)
jbplot_ensemble(betFox,xlim = NULL, legend.loc = "topleft",verbose = T)  
#?jbplot_ensemble    应该是000吨  绘图有错误啊
#536 596  pdf5.36 5.96  

#Plots plots JABBA ensemble models + projections - joint or by run
#?fw_jabba

# Try to improve runs test diagnostics by changing the variance settings
# Increase minimum obs error from 0.01 to 0.1 and remove SEs from CPUE model
jbinput2 = build_jabba(catch=catch,cpue=cpue,se=se,assessment=assessment,
                       scenario = "Run2",model.type = c("Schaefer", "Fox", "Pella", "Pella_m")[2],
                       sigma.est = FALSE,
                       fixed.obsE = ifelse(is.null(se), 0.25, 0.01), # mimum observation error
                       igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                       #igamma=c(4,0.01)
                       r.dist = c("lnorm","range")[1],r.prior = c(0.5,0.2),
                       K.dist = c("lnorm","range")[1],K.prior =c(3233.538,0.46),
                       psi.dist =c("lnorm","beta")[1],psi.prior = c(0.7,0.12), # Initial depletion B/K
                       projection =T,pyrs =15,imp.yr = 2024,
                       TACs = seq(80,280,20),TACint = NULL,
                       P_bound= c(0.02,1.3),Plim = 0,
                       catch.metric = "000 t",
                       verbose=T)


bet2= fit_jabba(jbinput2,quickmcmc = F,
                save.jabba=T,
                peels = 0,
                save.all = T,
                save.trj = T,
                save.csvs = T,
                verbose =T) #  run

# Check residual diags
jbplot_cpuefits(bet2)
jbplot_runstest(bet2)
jbplot_logfits(bet2)
jbplot_PPC(bet2)


# Improved
refinput = jbinput2 # Note as reference input 

# Compare
jbplot_summary(list(Run1=betFox,Run2=bet2))# combine plots  536 620  pdf6.58 5.36
jbplot_ensemble(list(Run1=betFox,Run2=bet2))#536 596  pdf6.58 5.36  

# Check parameters and convergence (p <0.05 is not fully converged)
bet2$pars 
# Make a long MCMC run with 3 chains
bet.full = fit_jabba(jbinput2,nc=3,quickmcmc = F,
                     save.jabba=T,
                     peels = 0,
                     save.all = T,
                     save.trj = T,
                     save.csvs = T,
                     verbose =T)

# MCMC convergence
bet.full$pars 
jbplot_mcmc(bet.full)

# get quantaties
bet.full$estimates
# FLR data.frame trajectories
bet.full$flqs
# fits
bet.full$diags


#------------------------------------------------------
# Estimate shape m as function of Bmsy/K
#-------------------------------------------------------

# Compile JABBA JAGS model and input object
jbinput3 = build_jabba(catch=catch,cpue=cpue,se=se,assessment=assessment,
                       scenario = "Est.Shape",model.type = "Pella_m", # Estimate shape
                       BmsyK=0.4, # mean 40%B0
                       shape.CV = 0.3, #CV
                       sigma.est = TRUE,
                       fixed.obsE = ifelse(is.null(se), 0.2, 0.01), # mimum observation error
                       igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                       #igamma=c(4,0.01)
                       r.dist = c("lnorm","range")[1],r.prior = c(0.5,0.2),
                       K.dist = c("lnorm","range")[1],K.prior =c(3233.538,0.46),
                       psi.dist =c("lnorm","beta")[1],psi.prior = c(0.7,0.12), # Initial depletion B/K
                       projection =T,pyrs =15,imp.yr = 2024,
                       TACs = seq(80,280,20),TACint = NULL,
                       P_bound= c(0.02,1.3),Plim = 0,
                       catch.metric = "000 t",
                       verbose=T)

bet3 = fit_jabba(jbinput3,quickmcmc = F,
                 save.jabba=T,
                 peels = 0,
                 save.all = T,
                 save.trj = T,
                 save.csvs = T,
                 verbose =T)

jbplot_ppdist(bet3) # check shape prior & posterior dist - not much information
# Compare
jbplot_summary(list(betFox,bet3))
jbplot_ensemble(list(betFox,bet3))

# also run model as Schaefer
#jbinput4 = build_jabba(catch=catch,cpue=cpue,se=NULL,assessment=assessment,
#                      scenario = "Schaefer",model.type = "Schaefer", # Estimate shape
#                     sigma.est = TRUE,fixed.obsE = 0.1,igamma = c(0.001,0.001),
#                    psi.prior = c(1,0.1))

#bet4 = fit_jabba(jbinput4,quickmcmc=T)

# Compare 
jbpar(mfrow=c(3,2),plot.cex=0.7)
jbplot_ensemble(list(betFox,bet2,bet3))#,bet4
jbpar(mfrow=c(3,2),plot.cex=0.6)
jbplot_summary(list(betFox,bet2,bet3),add=T)#,bet4

#----------------------------------------------------
# Do some forecasting
#----------------------------------------------------

# F-based forecasting
# Relative Fmsy
# Single Forecast for Base-Case model - now works with imp.yr=1 
?fw_jabba#JABBA 的外部远期预测
#quant = c("Catch", "F")[2],type = c("ratio", "msy", "abs")[2],

fw1 = fw_jabba(betFox,nyears=15,imp.yr=2024,
               imp.values = seq(0.8,1.2,0.1),quant=c("Catch", "F")[2],
               type=c("ratio", "msy", "abs")[2],stochastic = c(TRUE, FALSE)[1])
#imp.values	vector Catch or F scenarios provide as absolute or ratios
#jbpar(mfrow=c(3,2))
jbpar(mfrow=c(3,2),plot.cex = 0.7)
jbplot_ensemble(fw1)
# Zoom-in
jbplot_ensemble(fw1,xlim=c(2010,2034))
abline(v=2023) # Check
# Forecast with AR1 process error
fw1.ar1 = fw_jabba(bet2,nyears=10,imp.yr=1,quant="F",type="msy",AR1=TRUE,stochastic = T)
# now compare
jbpar(mfrow=c(3,2),plot.cex = 0.6)
for(i in 1:3){
  jbplot_ensemble(fw1,subplots = c(1,2,5)[i],add=T,xlim=c(2010,2028),legend=ifelse(i==1,T,F))
  jbplot_ensemble(fw1.ar1,subplots = c(1,2,5)[i],add=T,xlim=c(2010,2028),legend=ifelse(i==1,T,F))
}
mtext(c("Default","AR1"),outer=T,at=c(0.27,0.77))

# IOTC-Style: Relative current catch (default mean 3 yrs)
# 10 years, 2 intermediate years, deterministic
fw.io = fw_jabba(betFox,nyears=10,imp.yr=3,imp.values = seq(0.6,1.2,0.1),quant="Catch",type="ratio",nsq=3,stochastic = F)
jbplot_ensemble(fw.io)
jbpar(mfrow=c(2,2))
jbplot_ensemble(fw.io,add=T,subplots = 1,legend.loc = "topright")
jbplot_ensemble(fw.io,add=T,subplots = 2,legend=F)
jbplot_ensemble(fw.io,add=T,subplots = 5,legend=F)
jbplot_ensemble(fw.io,add=T,subplots = 6,legend=F)

# ICCAT Style
Ccur = mean(tail(jbinput2$data$catch[,2],2))
TACs = c(75500,seq(60000,78000,2000))
fw.iccat= fw_jabba(betFox,nyears=10,imp.yr=3,initial = c(Ccur,76000),imp.values = TACs,quant="Catch",type="abs",nsq=3,stochastic = F,AR1=T)

jbpar(mfrow=c(2,2))
jbplot_ensemble(fw.iccat,legendcex = 0.4,xlim=c(2010,2027),subplots = c(1,2,5,6),add=T)
jbplot_ensemble(fw.iccat,legendcex = 0.4,xlim=c(2010,2027),subplots = c(1,2,5,6),plotCIs = F)

# Check if correct
jbpar()
jbplot_ensemble(fw.iccat,legendcex = 0.4,xlim=c(2010,2027),subplots = c(6),plotCIs = F,add=T)
abline(v=c(2017,2018,2019,2020)) # 2020 = imp.yr

# Do Ensemble modelling
jbplot_ensemble(list(bet1,betFox))#,bet4
?jbplot_ensemble
# Joint all runs
ens = jbplot_ensemble(list(bet1,betFox),kbout=T,joint=T)#,bet4
save(ens, file = "bet1,betFox.RData")
# Do ensemble forecast

fw.ens= fw_jabba(list(betFox,bet1),nyears=10,imp.yr=2,initial = Ccur,imp.values = TACs,quant="Catch",type="abs",nsq=3,stochastic = F,AR1=T,thin=3)
jbpar(mfrow=c(3,2),plot.cex = 0.6)
for(i in 1:6) jbplot_ensemble(fw.ens,add=T,subplots = i,legend = ifelse(i==2,T,F))


#----------------------------------------------------------------
# Conduct Retrospective Analysis and Hind-Cast Cross-Validation
#----------------------------------------------------------------
?hindcast_jabba
# Do hindcast cross-validation
hc1 = hindcast_jabba(jbinputFox,betFox,peels=1:5)

# Show Retrospective Pattern
mohns= jbplot_retro(hc1)  #?jbplot_retro  536  736 mohns= jbplot_retro(hc1,as.png = T) 

mohns
write.csv(mohns,"mohns.csv")
mohns[row.names(mohns)=="rho.mu",]

hindcasts = hc1
# Make alternative forecasts
hc2 = jbhcxval(hc1,AR1=T) # make forecasts with AR1

jbpar(mfrow=c(1,2))
for(i in 1:1){
  jbplot_hcxval(hc1,index=c(2)[i],add=T,minyr = 2007,legend.add = F)
  jbplot_hcxval(hc2,index=c(2)[i],add=T,minyr = 2007,legend.add = F)
}
mtext(c("Default","AR1"),outer=T,at=c(0.27,0.77))

jbmase(hc2)














########################################################################
##########################################################################
########################################################################
##########################################################################
########################################################################
###########################################################################
assessment = "ss11_Pella"
output.dir = file.path(File,assessment)
dir.create(output.dir,showWarnings = T)
setwd(output.dir)
#------------------------------------------------------
# Simple example fit JABBA to Catch and CPUE with SEs
#-------------------------------------------------------

# Compile JABBA JAGS model and input object
?build_jabba
jbinputPella = build_jabba(catch=catch,cpue=cpue,se=se,
                           assessment=assessment,scenario = "Pella",
                           model.type = c("Schaefer", "Fox", "Pella", "Pella_m")[3],
                           sigma.est = FALSE, # estimate additional observation error
                           fixed.obsE = ifelse(is.null(se), 0.2, 0.01), # mimum observation error
                           igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                           #igamma=c(4,0.01)
                           r.dist = c("lnorm","range")[1],r.prior = c(0.5,0.2),
                           K.dist = c("lnorm","range")[1],K.prior =c(3233.538,0.46),
                           psi.dist =c("lnorm","beta")[1],psi.prior = c(0.7,0.12), # Initial depletion B/K
                           projection =T,pyrs =15,imp.yr = 2024,
                           TACs = seq(80,280,20),TACint = NULL,
                           P_bound= c(0.02,1.3),Plim = 0,
                           catch.metric = "000 t",
                           verbose=T)
jbinputPella$jagsdata
jbinputPella$settings
jbinputPella$settings$params


# Check input
jbplot_indices(jbinputPella)


# Fit JABBA (here mostly default value - careful)
?fit_jabba
betPella = fit_jabba(jbinputPella,quickmcmc = F,
                     save.jabba=T,
                     peels = 0,
                     save.all = T,
                     save.trj = T,
                     save.csvs = T,
                     verbose =T) #  run

names(betPella)
head(betPella$kobe)
write.csv(betPella$kobe,file = "kobe.csv")
betPella$timeseries
write.csv(betPella$timeseries,file = "timeseries.csv")

# Make individual plots width5 height3.5
jbplot_catch(betPella) #?jbplot_catch()   ,as.png = T,    add = T,
jbplot_catcherror(betPella)
jbplot_ppdist(betPella)
jbplot_ppdist(betPella,width = 10,as.png = T)#?jbplot_ppdist
jbplot_mcmc(betPella)
jbplot_residuals(betPella)
jbplot_residuals(betPella,as.png = T)
jbplot_cpuefits(betPella)
jbplot_cpuefits(betPella,as.png = T)
jbplot_runstest(betPella)
jbplot_runstest(betPella,as.png = T)
jbplot_logfits(betPella)
jbplot_procdev(betPella)
jbplot_PPC(betPella) # Posterior Predictive Checks - Not great should 0.2-0.8
jbplot_spphase(betPella)  #jbplot_spphase(betPella,add=T)
jbplot_spphase(betPella,as.png = T) #?jbplot_spphase 5 4.5  pdf4.5 5
jbplot_spdyn(betPella)
jbplot_spdyn(betPella,as.png = T)#?jbplot_spdyn5 4.5  pdf4.5 5
jbplot_kobe(betPella)#?jbplot_kobe5 4.5  pdf4.5 5
jbplot_kobe(betPella,as.png = T)
jbplot_biplot(betPella)
jbplot_prj(betPella,type = c("BB0", "BBmsy", "FFmsy")[1])    
jbplot_prj(betPella,type = c("BB0", "BBmsy", "FFmsy")[2])  
jbplot_prj(betPella,type = c("BB0", "BBmsy", "FFmsy")[3])
jbplot_trj(betPella)#?jbplot_trj 5 3.5
jbplot_trj(betPella,as.png = T)#pdf3.5 4
jbplot_stdresiduals(betPella)
jbplot_stdresiduals(betPella,as.png = T)

# Status
jbplot_summary(betPella)      #pdf5.58 6.36 ?jbplot_summary()  jbplot_summary(betPella,as.png = T)
# combine plots  536 520  pdf5.58 5.36
jbpar(mfrow=c(2,2))
jbplot_summary(betPella,add=T,type = c("BBmsy", "FFmsy"))
jbplot_spphase(betPella,add=T)
jbplot_kobe(betPella,add=T)

jbpar(mfrow=c(3,2),plot.cex = 0.8)
jbplot_ensemble(betPella,xlim = NULL, legend.loc = "topleft",verbose = T)  
#?jbplot_ensemblea    应该是000吨  绘图有错误啊
#536 596  pdf5.36 5.96  

#Plots plots JABBA ensemble models + projections - joint or by run
#?fw_jabba

# Try to improve runs test diagnostics by changing the variance settings
# Increase minimum obs error from 0.01 to 0.1 and remove SEs from CPUE model
jbinput2 = build_jabba(catch=catch,cpue=cpue,se=se,assessment=assessment,
                       scenario = "Run2",model.type = c("Schaefer", "Fox", "Pella", "Pella_m")[3],
                       sigma.est = FALSE,
                       fixed.obsE = ifelse(is.null(se), 0.2, 0.01), # mimum observation error
                       igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                       #igamma=c(4,0.01)
                       r.dist = c("lnorm","range")[1],r.prior = c(0.5,0.2),
                       K.dist = c("lnorm","range")[1],K.prior =c(3233.538,0.46),
                       psi.dist =c("lnorm","beta")[1],psi.prior = c(0.7,0.12), # Initial depletion B/K
                       projection =T,pyrs =15,imp.yr = 2024,
                       TACs = seq(80,280,20),TACint = NULL,
                       P_bound= c(0.02,1.3),Plim = 0,
                       catch.metric = "000 t",
                       verbose=T)


bet2= fit_jabba(jbinput2,quickmcmc = F,
                save.jabba=T,
                peels = 0,
                save.all = T,
                save.trj = T,
                save.csvs = T,
                verbose =T) #  run

# Check residual diags
jbplot_cpuefits(bet2)
jbplot_runstest(bet2)
jbplot_logfits(bet2)
jbplot_PPC(bet2)


# Improved
refinput = jbinput2 # Note as reference input 

# Compare
jbplot_summary(list(Run1=betPella,Run2=bet2))# combine plots  536 620  pdf6.58 5.36
jbplot_ensemble(list(Run1=betPella,Run2=bet2))#536 596  pdf6.58 5.36  

# Check parameters and convergence (p <0.05 is not fully converged)
bet2$pars 
# Make a long MCMC run with 3 chains
bet.full = fit_jabba(jbinput2,nc=3,quickmcmc = F,
                     save.jabba=T,
                     peels = 0,
                     save.all = T,
                     save.trj = T,
                     save.csvs = T,
                     verbose =T)

# MCMC convergence
bet.full$pars 
jbplot_mcmc(bet.full)

# get quantaties
bet.full$estimates
# FLR data.frame trajectories
bet.full$flqs
# fits
bet.full$diags


#------------------------------------------------------
# Estimate shape m as function of Bmsy/K
#-------------------------------------------------------

# Compile JABBA JAGS model and input object
jbinput3 = build_jabba(catch=catch,cpue=cpue,se=se,assessment=assessment,
                       scenario = "Est.Shape",model.type = "Pella_m", # Estimate shape
                       BmsyK=0.4, # mean 40%B0
                       shape.CV = 0.3, #CV
                       sigma.est = TRUE,
                       fixed.obsE = ifelse(is.null(se), 0.2, 0.01), # mimum observation error
                       igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                       #igamma=c(4,0.01)
                       r.dist = c("lnorm","range")[1],r.prior = c(0.5,0.2),
                       K.dist = c("lnorm","range")[1],K.prior =c(3233.538,0.46),
                       psi.dist =c("lnorm","beta")[1],psi.prior = c(0.7,0.12), # Initial depletion B/K
                       projection =T,pyrs =15,imp.yr = 2024,
                       TACs = seq(80,280,20),TACint = NULL,
                       P_bound= c(0.02,1.3),Plim = 0,
                       catch.metric = "000 t",
                       verbose=T)

bet3 = fit_jabba(jbinput3,quickmcmc = F,
                 save.jabba=T,
                 peels = 0,
                 save.all = T,
                 save.trj = T,
                 save.csvs = T,
                 verbose =T)

jbplot_ppdist(bet3) # check shape prior & posterior dist - not much information
# Compare
jbplot_summary(list(betPella,bet3))
jbplot_ensemble(list(betPella,bet3))

# also run model as Schaefer
#jbinput4 = build_jabba(catch=catch,cpue=cpue,se=NULL,assessment=assessment,
#                      scenario = "Schaefer",model.type = "Schaefer", # Estimate shape
#                     sigma.est = TRUE,fixed.obsE = 0.1,igamma = c(0.001,0.001),
#                    psi.prior = c(1,0.1))

#bet4 = fit_jabba(jbinput4,quickmcmc=T)

# Compare 
jbpar(mfrow=c(3,2),plot.cex=0.7)
jbplot_ensemble(list(betPella,bet2,bet3))#,bet4
jbpar(mfrow=c(3,2),plot.cex=0.6)
jbplot_summary(list(betPella,bet2,bet3),add=T)#,bet4

#----------------------------------------------------
# Do some forecasting
#----------------------------------------------------

# F-based forecasting
# Relative Fmsy
# Single Forecast for Base-Case model - now works with imp.yr=1 
?fw_jabba#JABBA 的外部远期预测
#quant = c("Catch", "F")[2],type = c("ratio", "msy", "abs")[2],

fw1 = fw_jabba(betPella,nyears=15,imp.yr=2024,
               imp.values = seq(0.8,1.2,0.1),quant=c("Catch", "F")[2],
               type=c("ratio", "msy", "abs")[2],stochastic = c(TRUE, FALSE)[1])
#imp.values	vector Catch or F scenarios provide as absolute or ratios
#jbpar(mfrow=c(3,2))
jbpar(mfrow=c(3,2),plot.cex = 0.7)
jbplot_ensemble(fw1)
# Zoom-in
jbplot_ensemble(fw1,xlim=c(2010,2034))
abline(v=2023) # Check
# Forecast with AR1 process error
fw1.ar1 = fw_jabba(bet2,nyears=10,imp.yr=1,quant="F",type="msy",AR1=TRUE,stochastic = T)
# now compare
jbpar(mfrow=c(3,2),plot.cex = 0.6)
for(i in 1:3){
  jbplot_ensemble(fw1,subplots = c(1,2,5)[i],add=T,xlim=c(2010,2028),legend=ifelse(i==1,T,F))
  jbplot_ensemble(fw1.ar1,subplots = c(1,2,5)[i],add=T,xlim=c(2010,2028),legend=ifelse(i==1,T,F))
}
mtext(c("Default","AR1"),outer=T,at=c(0.27,0.77))

# IOTC-Style: Relative current catch (default mean 3 yrs)
# 10 years, 2 intermediate years, deterministic
fw.io = fw_jabba(betPella,nyears=10,imp.yr=3,imp.values = seq(0.6,1.2,0.1),quant="Catch",type="ratio",nsq=3,stochastic = F)
jbplot_ensemble(fw.io)
jbpar(mfrow=c(2,2))
jbplot_ensemble(fw.io,add=T,subplots = 1,legend.loc = "topright")
jbplot_ensemble(fw.io,add=T,subplots = 2,legend=F)
jbplot_ensemble(fw.io,add=T,subplots = 5,legend=F)
jbplot_ensemble(fw.io,add=T,subplots = 6,legend=F)

# ICCAT Style
Ccur = mean(tail(jbinput2$data$catch[,2],2))
TACs = c(75500,seq(60000,78000,2000))
fw.iccat= fw_jabba(betPella,nyears=10,imp.yr=3,initial = c(Ccur,76000),imp.values = TACs,quant="Catch",type="abs",nsq=3,stochastic = F,AR1=T)

jbpar(mfrow=c(2,2))
jbplot_ensemble(fw.iccat,legendcex = 0.4,xlim=c(2010,2027),subplots = c(1,2,5,6),add=T)
jbplot_ensemble(fw.iccat,legendcex = 0.4,xlim=c(2010,2027),subplots = c(1,2,5,6),plotCIs = F)

# Check if correct
jbpar()
jbplot_ensemble(fw.iccat,legendcex = 0.4,xlim=c(2010,2027),subplots = c(6),plotCIs = F,add=T)
abline(v=c(2017,2018,2019,2020)) # 2020 = imp.yr

# Do Ensemble modelling
jbplot_ensemble(list(bet1,betFox,betPella))#,bet4
?jbplot_ensemble
# Joint all runs
ens = jbplot_ensemble(list(bet1,betFox,betPella),kbout=T,joint=T)#,bet4
save(ens, file = "bet1,betFox,betPella.RData")
# Do ensemble forecast
jbplot_summary(list(bet1,betFox,betPella),add=T)#,bet4


fw.ens= fw_jabba(list(betPella,bet1),nyears=10,imp.yr=2,initial = Ccur,imp.values = TACs,quant="Catch",type="abs",nsq=3,stochastic = F,AR1=T,thin=3)
jbpar(mfrow=c(3,2),plot.cex = 0.6)
for(i in 1:6) jbplot_ensemble(fw.ens,add=T,subplots = i,legend = ifelse(i==2,T,F))


#----------------------------------------------------------------
# Conduct Retrospective Analysis and Hind-Cast Cross-Validation
#----------------------------------------------------------------
?hindcast_jabba
# Do hindcast cross-validation
hc1 = hindcast_jabba(jbinputPella,betPella,peels=1:5)

# Show Retrospective Pattern
mohns= jbplot_retro(hc1)  #?jbplot_retro  536  736 mohns= jbplot_retro(hc1,as.png = T) 

mohns
write.csv(mohns,"mohns.csv")
mohns[row.names(mohns)=="rho.mu",]

hindcasts = hc1
# Make alternative forecasts
hc2 = jbhcxval(hc1,AR1=T) # make forecasts with AR1

jbpar(mfrow=c(1,2))
for(i in 1:1){
  jbplot_hcxval(hc1,index=c(2)[i],add=T,minyr = 2007,legend.add = F)
  jbplot_hcxval(hc2,index=c(2)[i],add=T,minyr = 2007,legend.add = F)
}
mtext(c("Default","AR1"),outer=T,at=c(0.27,0.77))

jbmase(hc2)
































########################################################################
##########################################################################
########################################################################
##########################################################################
########################################################################
###########################################################################
assessment = "ss12_Pella_m"
output.dir = file.path(File,assessment)
dir.create(output.dir,showWarnings = T)
setwd(output.dir)
#------------------------------------------------------
# Simple example fit JABBA to Catch and CPUE with SEs
#-------------------------------------------------------

# Compile JABBA JAGS model and input object
?build_jabba
jbinputPella_m = build_jabba(catch=catch,cpue=cpue,se=se,
                             assessment=assessment,scenario = "Pella_m",
                             model.type = c("Schaefer", "Fox", "Pella", "Pella_m")[4],
                             sigma.est = FALSE, # estimate additional observation error
                             fixed.obsE = ifelse(is.null(se), 0.2, 0.01), # mimum observation error
                             igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                             #igamma=c(4,0.01)
                             r.dist = c("lnorm","range")[1],r.prior = c(0.5,0.2),
                             K.dist = c("lnorm","range")[1],K.prior =c(3233.538,0.46),
                             psi.dist =c("lnorm","beta")[1],psi.prior = c(0.7,0.12), # Initial depletion B/K
                             projection =T,pyrs =15,imp.yr = 2024,
                             TACs = seq(80,280,20),TACint = NULL,
                             P_bound= c(0.02,1.3),Plim = 0,
                             catch.metric = "000 t",
                             verbose=T)
jbinputPella_m$jagsdata
jbinputPella_m$settings
jbinputPella_m$settings$params


# Check input
jbplot_indices(jbinputPella_m)


# Fit JABBA (here mostly default value - careful)
?fit_jabba
betPella_m = fit_jabba(jbinputPella_m,quickmcmc = F,
                       save.jabba=T,
                       peels = 0,
                       save.all = T,
                       save.trj = T,
                       save.csvs = T,
                       verbose =T) #  run

names(betPella_m)
head(betPella_m$kobe)
write.csv(betPella_m$kobe,file = "kobe.csv")
betPella_m$timeseries
write.csv(betPella_m$timeseries,file = "timeseries.csv")

# Make individual plots width5 height3.5
jbplot_catch(betPella_m) #?jbplot_catch()   ,as.png = T,    add = T,
jbplot_catcherror(betPella_m)
jbplot_ppdist(betPella_m)
jbplot_ppdist(betPella_m,width = 10,as.png = T)#?jbplot_ppdist
jbplot_mcmc(betPella_m)
jbplot_residuals(betPella_m)
jbplot_residuals(betPella_m,as.png = T)
jbplot_cpuefits(betPella_m)
jbplot_cpuefits(betPella_m,as.png = T)
jbplot_runstest(betPella_m)
jbplot_runstest(betPella_m,as.png = T)
jbplot_logfits(betPella_m)
jbplot_procdev(betPella_m)
jbplot_PPC(betPella_m) # Posterior Predictive Checks - Not great should 0.2-0.8
jbplot_spphase(betPella_m)  #jbplot_spphase(betPella_m,add=T)
jbplot_spphase(betPella_m,as.png = T) #?jbplot_spphase 5 4.5  pdf4.5 5
jbplot_spdyn(betPella_m)
jbplot_spdyn(betPella_m,as.png = T)#?jbplot_spdyn5 4.5  pdf4.5 5
jbplot_kobe(betPella_m)#?jbplot_kobe5 4.5  pdf4.5 5
jbplot_kobe(betPella_m,as.png = T)
jbplot_biplot(betPella_m)
jbplot_prj(betPella_m,type = c("BB0", "BBmsy", "FFmsy")[1])    
jbplot_prj(betPella_m,type = c("BB0", "BBmsy", "FFmsy")[2])  
jbplot_prj(betPella_m,type = c("BB0", "BBmsy", "FFmsy")[3])
jbplot_trj(betPella_m)#?jbplot_trj 5 3.5
jbplot_trj(betPella_m,as.png = T)#pdf3.5 4
jbplot_stdresiduals(betPella_m)
jbplot_stdresiduals(betPella_m,as.png = T)

# Status
jbplot_summary(betPella_m)      #pdf5.58 6.36 ?jbplot_summary()  jbplot_summary(betPella_m,as.png = T)
# combine plots  536 520  pdf5.58 5.36
jbpar(mfrow=c(2,2))
jbplot_summary(betPella_m,add=T,type = c("BBmsy", "FFmsy"))
jbplot_spphase(betPella_m,add=T)
jbplot_kobe(betPella_m,add=T)

jbpar(mfrow=c(3,2),plot.cex = 0.8)
jbplot_ensemble(betPella_m,xlim = NULL, legend.loc = "topleft",verbose = T)  
#?jbplot_ensemblea    应该是000吨  绘图有错误啊
#536 596  pdf5.36 5.96  

#Plots plots JABBA ensemble models + projections - joint or by run
#?fw_jabba

# Try to improve runs test diagnostics by changing the variance settings
# Increase minimum obs error from 0.01 to 0.1 and remove SEs from CPUE model
jbinput2 = build_jabba(catch=catch,cpue=cpue,se=se,assessment=assessment,
                       scenario = "Run2",model.type = c("Schaefer", "Fox", "Pella", "Pella_m")[4],
                       sigma.est = T,
                       fixed.obsE = 0.01,
                       igamma = c(0.001,0.001),
                       #igamma=c(4,0.01),
                       r.dist = c("lnorm","range")[1],r.prior = c(0.5,0.2),
                       K.dist = c("lnorm","range")[1],K.prior =c(2513.715,0.46),
                       psi.dist =c("lnorm","beta")[1],psi.prior = c(0.4,0.21), # Initial depletion B/K
                       projection =T,pyrs =15,imp.yr = 2024,
                       TACs = seq(80,280,20),TACint = NULL,
                       P_bound= c(0.02,1.3),Plim = 0,
                       catch.metric = "000 t",
                       verbose=T)


bet2= fit_jabba(jbinput2,quickmcmc = F,
                save.jabba=T,
                peels = 0,
                save.all = T,
                save.trj = T,
                save.csvs = T,
                verbose =T) #  run

# Check residual diags
jbplot_cpuefits(bet2)
jbplot_runstest(bet2)
jbplot_logfits(bet2)
jbplot_PPC(bet2)


# Improved
refinput = jbinput2 # Note as reference input 

# Compare
jbplot_summary(list(Run1=betPella_m,Run2=bet2))# combine plots  536 620  pdf6.58 5.36
jbplot_ensemble(list(Run1=betPella_m,Run2=bet2))#536 596  pdf6.58 5.36  

# Check parameters and convergence (p <0.05 is not fully converged)
bet2$pars 
# Make a long MCMC run with 3 chains
bet.full = fit_jabba(jbinput2,nc=3,quickmcmc = F,
                     save.jabba=T,
                     peels = 0,
                     save.all = T,
                     save.trj = T,
                     save.csvs = T,
                     verbose =T)

# MCMC convergence
bet.full$pars 
jbplot_mcmc(bet.full)

# get quantaties
bet.full$estimates
# FLR data.frame trajectories
bet.full$flqs
# fits
bet.full$diags


#------------------------------------------------------
# Estimate shape m as function of Bmsy/K
#-------------------------------------------------------

# Compile JABBA JAGS model and input object
jbinput3 = build_jabba(catch=catch,cpue=cpue,se=se,assessment=assessment,
                       scenario = "Est.Shape",model.type = "Pella_m", # Estimate shape
                       BmsyK=0.4, # mean 40%B0
                       shape.CV = 0.3, #CV
                       sigma.est = TRUE,
                       fixed.obsE = 0.1,
                       igamma = c(0.001,0.001),
                       #igamma=c(4,0.01)
                       r.dist = c("lnorm","range")[1],r.prior = c(0.5,0.2),
                       K.dist = c("lnorm","range")[1],K.prior =c(2513.715,0.46),
                       psi.dist =c("lnorm","beta")[1],psi.prior = c(0.4,0.21), # Initial depletion B/K
                       projection =T,pyrs =15,imp.yr = 2024,
                       TACs = seq(80,280,20),TACint = NULL,
                       P_bound= c(0.02,1.3),Plim = 0,
                       catch.metric = "000 t",
                       verbose=T)

bet3 = fit_jabba(jbinput3,quickmcmc = F,
                 save.jabba=T,
                 peels = 0,
                 save.all = T,
                 save.trj = T,
                 save.csvs = T,
                 verbose =T)

jbplot_ppdist(bet3) # check shape prior & posterior dist - not much information
# Compare
jbplot_summary(list(betPella_m,bet3))
jbplot_ensemble(list(betPella_m,bet3))

# also run model as Schaefer
#jbinput4 = build_jabba(catch=catch,cpue=cpue,se=NULL,assessment=assessment,
#                      scenario = "Schaefer",model.type = "Schaefer", # Estimate shape
#                     sigma.est = TRUE,fixed.obsE = 0.1,igamma = c(0.001,0.001),
#                    psi.prior = c(1,0.1))

#bet4 = fit_jabba(jbinput4,quickmcmc=T)

# Compare 
jbpar(mfrow=c(3,2),plot.cex=0.7)
jbplot_ensemble(list(betPella_m,bet2,bet3))#,bet4
jbpar(mfrow=c(3,2),plot.cex=0.6)
jbplot_summary(list(betPella_m,bet2,bet3),add=T)#,bet4

#----------------------------------------------------
# Do some forecasting
#----------------------------------------------------

# F-based forecasting
# Relative Fmsy
# Single Forecast for Base-Case model - now works with imp.yr=1 
?fw_jabba#JABBA 的外部远期预测
#quant = c("Catch", "F")[2],type = c("ratio", "msy", "abs")[2],

fw1 = fw_jabba(betPella_m,nyears=15,imp.yr=2024,
               imp.values = seq(0.8,1.2,0.1),quant=c("Catch", "F")[2],
               type=c("ratio", "msy", "abs")[2],stochastic = c(TRUE, FALSE)[1])
#imp.values	vector Catch or F scenarios provide as absolute or ratios
#jbpar(mfrow=c(3,2))
jbpar(mfrow=c(3,2),plot.cex = 0.7)
jbplot_ensemble(fw1)
# Zoom-in
jbplot_ensemble(fw1,xlim=c(2010,2034))
abline(v=2023) # Check
# Forecast with AR1 process error
fw1.ar1 = fw_jabba(bet2,nyears=10,imp.yr=1,quant="F",type="msy",AR1=TRUE,stochastic = T)
# now compare
jbpar(mfrow=c(3,2),plot.cex = 0.6)
for(i in 1:3){
  jbplot_ensemble(fw1,subplots = c(1,2,5)[i],add=T,xlim=c(2010,2028),legend=ifelse(i==1,T,F))
  jbplot_ensemble(fw1.ar1,subplots = c(1,2,5)[i],add=T,xlim=c(2010,2028),legend=ifelse(i==1,T,F))
}
mtext(c("Default","AR1"),outer=T,at=c(0.27,0.77))

# IOTC-Style: Relative current catch (default mean 3 yrs)
# 10 years, 2 intermediate years, deterministic
fw.io = fw_jabba(betPella_m,nyears=10,imp.yr=3,imp.values = seq(0.6,1.2,0.1),quant="Catch",type="ratio",nsq=3,stochastic = F)
jbplot_ensemble(fw.io)
jbpar(mfrow=c(2,2))
jbplot_ensemble(fw.io,add=T,subplots = 1,legend.loc = "topright")
jbplot_ensemble(fw.io,add=T,subplots = 2,legend=F)
jbplot_ensemble(fw.io,add=T,subplots = 5,legend=F)
jbplot_ensemble(fw.io,add=T,subplots = 6,legend=F)

# ICCAT Style
Ccur = mean(tail(jbinput2$data$catch[,2],2))
TACs = c(75500,seq(60000,78000,2000))
fw.iccat= fw_jabba(betPella_m,nyears=10,imp.yr=3,initial = c(Ccur,76000),imp.values = TACs,quant="Catch",type="abs",nsq=3,stochastic = F,AR1=T)

jbpar(mfrow=c(2,2))
jbplot_ensemble(fw.iccat,legendcex = 0.4,xlim=c(2010,2027),subplots = c(1,2,5,6),add=T)
jbplot_ensemble(fw.iccat,legendcex = 0.4,xlim=c(2010,2027),subplots = c(1,2,5,6),plotCIs = F)

# Check if correct
jbpar()
jbplot_ensemble(fw.iccat,legendcex = 0.4,xlim=c(2010,2027),subplots = c(6),plotCIs = F,add=T)
abline(v=c(2017,2018,2019,2020)) # 2020 = imp.yr

# Do Ensemble modelling
jbplot_ensemble(list(bet1,betFox,betPella,betPella_m))#,bet4
?jbplot_ensemble
# Joint all runs
ens = jbplot_ensemble(list(bet1,betFox,betPella,betPella_m),kbout=T,joint=T)#,bet4
save(ens, file = "bet1,betFox,betPella,betPella_m.RData")
# Do ensemble forecast
jbplot_summary(list(bet1,betFox,betPella,betPella_m),add=T)#,bet4


fw.ens= fw_jabba(list(betPella_m,bet1),nyears=10,imp.yr=2,initial = Ccur,imp.values = TACs,quant="Catch",type="abs",nsq=3,stochastic = F,AR1=T,thin=3)
jbpar(mfrow=c(3,2),plot.cex = 0.6)
for(i in 1:6) jbplot_ensemble(fw.ens,add=T,subplots = i,legend = ifelse(i==2,T,F))


#----------------------------------------------------------------
# Conduct Retrospective Analysis and Hind-Cast Cross-Validation
#----------------------------------------------------------------
?hindcast_jabba
# Do hindcast cross-validation
hc1 = hindcast_jabba(jbinputPella_m,betPella_m,peels=1:5)

# Show Retrospective Pattern
mohns= jbplot_retro(hc1)  #?jbplot_retro  536  736 mohns= jbplot_retro(hc1,as.png = T) 

mohns
write.csv(mohns,"mohns.csv")
mohns[row.names(mohns)=="rho.mu",]

hindcasts = hc1
# Make alternative forecasts
hc2 = jbhcxval(hc1,AR1=T) # make forecasts with AR1

jbpar(mfrow=c(1,2))
for(i in 1:1){
  jbplot_hcxval(hc1,index=c(2)[i],add=T,minyr = 2007,legend.add = F)
  jbplot_hcxval(hc2,index=c(2)[i],add=T,minyr = 2007,legend.add = F)
}
mtext(c("Default","AR1"),outer=T,at=c(0.27,0.77))

jbmase(hc2)


























#------------------------------------------------------
# Catch-Only with biomass prior in 2007 as type B/Bmsy
#------------------------------------------------------
# Compile JABBA JAGS model and input object for Catch Only
# Add biomass prior based on B/Bmsy guestimate
jbinput5 = build_jabba(catch=catch,model.type = "Fox",
                       assessment=assessment,scenario =  "CatchOnly" ,
                       b.prior=c(0.5,0.2,2007,"bbmsy"),
                       psi.prior = c(1,0.1))


# Fit JABBA
bet5 = fit_jabba(jbinput5,save.jabba=TRUE,output.dir=output.dir)

# Check depletion prior vs posterior
jbplot_bprior(bet5)
# Compare
jbplot_summary(list(bet2,bet5))



#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# Original South Atlantic Swordfish example here
# Winker et al. (2018). JABBA: Just Another Biomass Assessment
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
swos = iccat$swos

assessment = "SWOSiccat"
scenario = "Base"
output.dir = file.path(File,assessment)
dir.create(output.dir,showWarnings = F)
setwd(output.dir)

# Compile JABBA JAGS model and input object
jbswos = build_jabba(catch=swos$catch,cpue=normIndex(swos$cpue),se=swos$se,assessment=assessment,scenario = scenario,
                     model.type = "Pella",
                     BmsyK = 0.4,
                     r.prior=c(0.42,0.37),
                     K.prior = c(250000,1),
                     psi.prior = c(1,0.25),
                     fixed.obsE = 0.2,
                     add.catch.CV = FALSE,
                     proc.dev.all = FALSE, 
                     igamma=c(4,0.01), # default process error (moderately informative)
                     P_bound = c(0.02,1.1),verbose=T)

jbplot_indices(jbswos)

# fit JABBA
fit.swos = fit_jabba(jbswos,save.jabba=TRUE,output.dir=output.dir)

jbplot_cpuefits(fit.swos)
jbplot_logfits(fit.swos)
jbplot_residuals(fit.swos)
jbplot_runstest(fit.swos)
jbplot_ppdist(fit.swos)

jbpar(mfrow=c(1,2),plot.cex = 0.7)
jbplot_spphase(fit.swos,add=T)
jbplot_kobe(fit.swos,add=T)

# Project
# ICCAT Style
Ccur = 10056
TACs = seq(10000,18000,1000)
fw.swos= fw_jabba(fit.swos,nyears=10,imp.yr=4,initial = Ccur,imp.values = TACs,quant="Catch",type="abs",nsq=3,stochastic = T,AR1=F)
jbpar(mfrow=c(2,2),plot.cex = 0.7)
jbplot_ensemble(fw.swos,legendcex = 0.4,xlim=c(2015,2025),subplots = c(1:4),add=T)




















