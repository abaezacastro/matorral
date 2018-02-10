require(poweRlaw)
require(caspr)

RUNM_CONFIG_DIR = Sys.getenv()['RUNM_CONFIG_DIR']
source(sprintf('%s/matorral.R', RUNM_CONFIG_DIR))

source('~/Vegetation_project/plfit_sc.R')
source('~/Vegetation_project/simfunctions.r')
load('~/Vegetation_project/obj_tipping_point')

####################################################
#initial parametes of landscape dynamics 
#par_ed<-parameters_desert.csv

par_ed<-read.csv('parameters_desert.csv',header=FALSE)
#rotational schaduale parameter
#t_r<-par_ed[2,2]

#generate a boolean vector with rotation shaduale of time t_r
#vec_rot<-rep(c(0,1),each=t_r,length.out=2000)
L_eq=18
matorral$parms$aridity<-1

RT=5 #par_ed[2,1]
L_bound=10#par_ed[2,2]


H_bound=20#par_ed[2,3]

PT=5#par_ed[4,2]

matorral$parms$herd_size<-10 #rep(c(rep(L_eq+H_bound,PT),rep(L_eq-L_bound,RT)),length.out=1010)

parameters_list<-parms_timeseries(matorral$parms,1010)

  #################################################################
Rsult_perturbation<-list()

     mIL<-init_landscape(c("D","G","A","S"),c(0.2,0.2,0.3,0.3),width = 200)

Rsult_perturbation<-ca(mIL,matorral,saveeach=500,t_max = 500,seed=4000)


save(Rsult_perturbation,file=(sprintf('%s/obj_result',Sys.getenv()['RUNM_RUN_DIR'])))


require(ggplot2)

ggplot(fortify(Rsult_perturbation)) +
  geom_raster(aes(x,y,fill=state)) +
  scale_fill_manual(values=grazing$cols)

