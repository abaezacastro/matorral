require(poweRlaw)
require(caspr)
require(plot3D)
require(scatterplot3d)

require(ggplot2)
rot=c("5","10","15","20","25","30")
Aridity=c("0.2","0.4","0.6","0.8","1.0")
A_num<-seq(0.2,1,0.2)
L<-seq(0,40,2)
Lf<-formatC(L,digits=1,format='d',flag='0')


table_rest_H<-matrix(ncol=5,nrow=length(L))
table_rest_L<-matrix(ncol=5,nrow=length(L))
table_rest_H_min<-matrix(ncol=5,nrow=length(L))
table_rest_L_min<-matrix(ncol=5,nrow=length(L))
table_rest_H_max<-matrix(ncol=5,nrow=length(L))
table_rest_L_max<-matrix(ncol=5,nrow=length(L))
scater_table=matrix(ncol=4,nrow=5*length(L))


for( i in 1:5){
  for(j in 1:length(L)){
    count=count+1
setwd(sprintf("~/Vegetation_project/Aridity/run_1/2016.01.28-16.16.41/ini_herd=%s-Aridity=%s",Lf[j],Aridity[i]))

if(file.exists('obj_result')==TRUE){
load("obj_result")
  table_rest_L[j,i]<-result_saved[[2]][2]
  table_rest_H[j,i]<-result_saved[[3]][2]
  
  
  
}
    
    
  }
}




table_rest_L[18:21,5]<-0
table_rest_H[18:21,5]<-0
table_rest_L[1,5]<-table_rest_H[1,5]
table_rest_L[2,5]<-table_rest_H[2,5]
table_rest_L[1,4]<-table_rest_H[1,4]
table_rest_L[2,4]<-table_rest_H[2,4]
table_rest_L[1,3]<-table_rest_H[1,3]
table_rest_L[1,2]<-table_rest_H[1,2]

table_rest_L[1,1]<-table_rest_H[1,1]
table_rest_L[2,1]<-table_rest_H[2,1]
table_rest_L[3,1]<-table_rest_H[3,1]


count=0
for( i in 1:5){
  for(j in 1:length(L)){
    count=count+1
    scater_table[count,]=c(table_rest_L[j,i],table_rest_H[j,i],L[j],A_num[6-i])
  }}



colnames(scater_table)<-c("L_IC","H_IC","Livestock","Aridity")


scater_table<-as.data.frame(scater_table)
scater_table$Aridity<-factor(scater_table$Aridity)
tiff(filename ="~/Vegetation_project/Alternative_stabe_States.tiff",width = 24,height = 16,units = "cm",res = 300)

P<-ggplot(subset(scater_table,H_IC>0.1),aes(x=Livestock,y=H_IC,col=Aridity,size=Aridity))+
  geom_point()+
  expand_limits(x = 40)+
  geom_point(data =scater_table, aes(x=Livestock,y=L_IC))+
  scale_colour_brewer(palette ="RdYlBu")+
  scale_size_discrete(range=c(2,7))+
  labs(title="Alternative Stable States",x="Livestock [# Animals]",y="% Area covered by forest   [F*]")+
  theme(axis.title.y = element_text(size = rel(1.6)))+
  theme(axis.title.x = element_text(size = rel(1.6)))+
  theme(plot.title = element_text(size = rel(1.8)))+
  theme(legend.title= element_text(size = rel(1.4)))+
  theme(legend.text= element_text(size = rel(1.4)))+
  theme(axis.text= element_text(size = rel(1),colour = 'black'))
P
dev.off()





L<-c(14,16,18,20,22)
Ar<-c("1.0","0.8","0.6","0.4")
Lf<-formatC(L,digits=1,format='d',flag='0')
par(mfrow=c(3,3))
plotss<-list()
lines_pl<-list()
lines_exp<-list()
lines_ln<-list()
alpha<-rep(0,length(L))
ks_st<-matrix(ncol=3,nrow=length(L))
for(j in 1:length(L)){
  
  setwd(sprintf("~/Vegetation_project/Aridity/run_1/2016.01.28-16.16.41/ini_herd=%s-Aridity=%s",Lf[j],Ar[1]))
  if(file.exists('obj_result')==TRUE){
    load("obj_result")
    est<-estimate_xmin(result_saved[[1]][[1]])
    result_saved[[1]][[1]]$setXmin(est)
   : ks_st[j,1]<-est$KS
    est<-estimate_xmin(result_saved[[1]][[2]])
    result_saved[[1]][[2]]$setXmin(est)
    ks_st[j,2]<-est$KS
    est<-estimate_xmin(result_saved[[1]][[3]])
    result_saved[[1]][[3]]$setXmin(est)
    ks_st[j,3]<-est$KS
    
    plotss[[j]]<-plot(result_saved[[1]][[1]])
    lines_pl[[j]]<-lines(result_saved[[1]][[1]],col='red')
    lines_exp[[j]]<-lines(result_saved[[1]][[2]],col='magenta')
    lines_ln[[j]]<-lines(result_saved[[1]][[3]],col='green')
    alpha[j]<-result_saved[[1]][[1]]$getPars()
  }
}
plot(L,alpha,type="l")




P<-ggplot(subset(scater_table,Aridity==0.2),aes(x=Livestock,y=H_IC))+
  geom_point()+
  expand_limits(x = 40)+
  geom_point(aes(x=Livestock,y=L_IC))+
  labs(title="Alternative Stable States",x="Livestock [# Animals]",y="% Area covered by forest   [F*]")+
  theme(axis.title.y = element_text(size = rel(1.6)))+
  theme(axis.title.x = element_text(size = rel(1.6)))+
  theme(plot.title = element_text(size = rel(1.8)))+
  theme(legend.title= element_text(size = rel(1.4)))+
  theme(legend.text= element_text(size = rel(1.4)))+
  theme(axis.text= element_text(size = rel(1),colour = 'black'))
P





for(i in 1:length(L)){
 ppp<-cbind(plotss[[i]],rep(L[i],length(plotss[[i]]$x)))
 ppln<-cbind(lines_ln[[i]],rep(L[i],length(lines_ln[[i]]$x)))
 ppexp<-cbind(lines_exp[[i]],rep(L[i],length(lines_exp[[i]]$x)))
 pppl<-cbind(lines_pl[[i]],rep(L[i],length(lines_pl[[i]]$x)))

   if(i==1) { 
     pdat<-ppp
     pdat_Lln<-ppln
     pdat_Lexp<-ppexp
     pdat_Lpl<-pppl
   }
 if(i>1) {
   
   pdat<-rbind(pdat,ppp)
   pdat_Lln<-rbind(pdat_Lln,ppln)
   pdat_Lexp<-rbind(pdat_Lexp,ppexp)
   pdat_Lpl<-rbind(pdat_Lpl,pppl)
   } 

}


colnames(pdat)<-c("p_size","CDF","N_An")
colnames(pdat_Lln)<-c("p_size","CDF","N_An")
colnames(pdat_Lexp)<-c("p_size","CDF","N_An")
colnames(pdat_Lpl)<-c("p_size","CDF","N_An")

pdat<-as.data.frame(pdat)
pdat_Lln<-as.data.frame(pdat_Lln)
pdat_Lexp<-as.data.frame(pdat_Lexp)

pdat_Lexp<-pdat_Lexp[which(log(pdat_Lexp$CDF)!=-Inf & log(pdat_Lexp$CDF)>-10),]
pdat_Lpl<-as.data.frame(pdat_Lpl)

#
tiff(filename ="~/Vegetation_project/patch_size_distribution.tiff",width = 30,height = 10,units = "cm",res = 300)
ggplot(pdat,aes(x=p_size,y=CDF))+
  geom_point()+
  geom_line(data=pdat_Lln,aes(x=p_size,y=CDF),colour='red')+
  geom_line(data=pdat_Lpl,aes(x=p_size,y=CDF),colour='green')+
  geom_line(data=pdat_Lexp,aes(x=p_size,y=CDF),colour='blue')+
  scale_y_log10()+
  scale_x_log10()+
  labs(x="Patch Size [# cells]",y="CDF")+
  theme(axis.title.y = element_text(size = rel(1.6)))+
  theme(axis.title.x = element_text(size = rel(1.6)))+
  facet_grid(. ~ N_An)+
  scale_color_manual(breaks=c('Log-Normal','Power-Law','Exponential'),values=c("red","green","blue"))
  dev.off()
###################################################################
  #################################################################3
  ###################################################################
  ####################################################################
#  ROTATIONS
  
  RT=seq(1,25,5)
  RTf<-formatC(RT,digits=1,format='d',flag='0')
  L_B=seq(0,20,5)
  L_Bf<-formatC(L_B,digits=1,format='d',flag='0')
  H_B=seq(0,12,3)
  H_Bf<-formatC(H_B,digits=1,format='d',flag='0')
  PT=seq(1,25,5)
  PTf<-formatC(PT,digits=1,format='d',flag='0')
  
  res_rotation<-matrix(nrow=length(PT)*length(RT)*length(L_B)*length(H_B),ncol=6)
  count=0
  for( i in 1:length(RTf)){
   for( j in 1:length(L_Bf)){
   for( h in 1:length(H_Bf)){
     for(l in 1:length(PTf)){
      count=count+1
      setwd(sprintf("~/Vegetation_project/Rotation/run_3/2016.02.01-16.27.45/RT=%s-L_bound=%s-H_bound=%s-PT=%s",RTf[i],L_Bf[j],H_Bf[h],PTf[l]))
      
        load("obj_result")
       # print(mean(tail(result_saved[[3]]$S[500:1000],100)))
        
                herd_size<-rep(c(rep(L_eq-L_B[j],RT[i]),rep(L_eq+H_B[h],PT[l])),length.out=1000)
        S_eq<-mean(tail(result_saved[[3]]$S[500:1000],100))
        Val<-mean(tail(S_eq_tipping$cover$S[500:1000],100))
        res_rotation[count,]=c(RT[i],L_B[j],H_B[h],PT[l],S_eq/Val,sum(herd_size)/(1000*L_eq))
  rm(result_saved)
           }
    }
   }
  }
  
  
  colnames(res_rotation)<-c("RestingTime","Low_bound","High_bound","PressureTime","S_eq","herd_size")
  
  res_rotation<-as.data.frame(res_rotation)
  
  ggplot(res_rotation,aes(x=Low_bound,y=High_bound,fill=herd_size))+
    geom_tile()+
    scale_fill_gradient(limits = c(0.13, 1.53),low = "black",high = "white")+
    facet_grid(RestingTime~PressureTime)
  
  
  
  #############################
  #rotation at the lower point power law formation
  
  
  
  RT=seq(5,25,5)
  RTf<-formatC(RT,digits=1,format='d',flag='0')
  L_B=seq(3,18,3)
  L_Bf<-formatC(L_B,digits=1,format='d',flag='0')
  H_B=seq(0,8,2)
  H_Bf<-formatC(H_B,digits=0,format='d',flag='0')
  PT=seq(5,25,5)
  PTf<-formatC(PT,digits=1,format='d',flag='0')
  
  IC_A<-seq(0.25,0.45,0.05)
  IC_Af<-formatC(IC_A,digits=2,format='f',flag='0')
  IC_S<-seq(0.25,0.45,0.05)
  IC_Sf<-formatC(IC_S,digits=2,format='f',flag='0')
  L_eq=18
  res_rotation_low<-matrix(nrow=length(H_B)*length(PT)*length(RT)*length(L_B)*length(IC_A)*length(IC_S), ncol=9)
 
  
  ini_C<-c(0.1,0.4,0.7)
  count=0
  for(m in 1:length(IC_A)){
    for(n in 1:length(IC_S)){
  for( i in 1:length(RTf)){
    for( j in 1:length(L_Bf)){
      for( h in length(H_Bf)){
        for(l in 1:length(PTf)){
          
             setwd(sprintf("~/Vegetation_project/Rotation_atpowerLawPoint/dif_IC/2016.02.10-14.52.58/RT=%s-L_bound=%s-H_bound=%s-PT=%s-IC_A=%s-IC_S=%s",RTf[i],L_Bf[j],H_Bf[h],PTf[l],IC_Af[m],IC_Sf[n]))
          
              if (count!=1538){
              load("obj_result")
}
            count=count+1
            
          print(count)
          
               herd_size<-rep(c(rep(L_eq-L_B[j],RT[i]),rep(L_eq+H_B[h],PT[l])),length.out=1000)
               Val<-mean(tail(S_eq_pl$cover$S[100:300],100))
          
               S_eq<-mean(tail(Rsult_perturbation$cover$S[900:1000],50))
               A_eq<-mean(tail(Rsult_perturbation$cover$A[900:1000],50))
               res_rotation_low[count,]=c(RT[i],L_B[j],H_B[h],PT[l],S_eq,A_eq,sum(herd_size)/(1000*(L_eq+6)),IC_A[m],IC_S[n])
          
                 ts_series= cbind(Rsult_perturbation$cover$S,Rsult_perturbation$cover$A,seq(1,1001,1),rep(RT[i],1001),rep(L_B[j],1001),rep(H_B[h],1001),rep(PT[l],1001),rep(S_eq,1001),rep(sum(herd_size)/(1000*(L_eq+6)),1001),rep(IC_A[m],1001),rep((IC_S[n]),1001))
          
           if(count==1){ts_results<-ts_series}
               else
               {
               ts_results<-rbind(ts_results,ts_series)
                }
           

       #   rm(Rsult_perturbation)
            }
      }
    }
  }
    }
  }  
  
  
  
  colnames(res_rotation_low)<-c("RestingTime","Low_bound","High_bound","PressureTime","S_eq","A_eq","herd_size","IC_S","IC_A")
  colnames(ts_results)<-c("TS","TS_A","time","RestingTime","Low_bound","High_bound","PressureTime","S_eq","herd_size","IC_S","IC_A")
  res_rotation<-as.data.frame(res_rotation)
  ts_results<-as.data.frame(ts_results)

    ggplot(subset(res_rotation,IC_S==0.7),aes(x=Low_bound,y=High_bound,fill=herd_size*S_eq))+
    geom_tile()+
    scale_fill_gradient(limits = c(0, 1),low = "black",high = "green")+
    facet_grid(PressureTime~RestingTime)
  
  
  
  
  ggplot(subset(ts_results,IC_S==0.4 & High_bound == 6),aes(x=time,y=TS,colour=factor(Low_bound)))+
    geom_line()+
    facet_grid(PressureTime~RestingTime)
  
  results_list<-list (table_rest_H,
                      table_rest_L,
                      table_rest_H_min,
                      table_rest_L_min,
                      table_rest_H_max,
                      table_rest_L_max,
                      scater_table,
                      plotss,
                      lines_pl,
                      lines_exp,
                      lines_ln,
                      alpha,
                      res_rotation,
                      ts_results,
                      res_rotation_low)
  
  save(results_list,file = "~/Vegetation_project/results_fin_object")
  