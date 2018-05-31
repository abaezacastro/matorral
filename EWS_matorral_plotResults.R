
#Figure 2:
setwd("C:/Users/abaezaca/Dropbox (ASU)/Vegetation_project/")
load("results_fin_object")
load("results_ASS_rev1")
colores=c("#d7191c",
"#fdae61",
"lightgreen",
"#a6d96a",
"#1a9641")

res_ASS<-as.data.frame(res_ASS)
colnames(res_ASS)<-c("L_IC","H_IC","Livestock","Aridity")
res_ASS$Aridity=as.factor(res_ASS$Aridity)
P<-ggplot()+
scale_colour_manual(name = "Aridity",labels = c("0","0.2", "0.4", "0.6","0.8"),values = rev(colores))+
scale_size_manual(name = "Aridity",labels = c("0","0.2", "0.4", "0.6","0.8"),values = c(2,3,4,5,6))+
  scale_x_continuous(limits=c(0,40))+
  geom_point(data=subset(res_ASS,L_IC<0.1),aes(x=rev(Livestock),y=rev(L_IC),colour=rev(Aridity),size=rev(Aridity)))+
  geom_point(data =subset(res_ASS,H_IC>0.1), aes(x=rev(Livestock),y=rev(H_IC),colour=rev(Aridity),size=rev(Aridity)))+
  labs(title="",x="Livestock Population Size",y="Area [% total] covered by forest [F*]")+
  theme(axis.title.y = element_text(size = rel(1.6)))+
  theme(axis.title.x = element_text(size = rel(1.6)))+
  theme(plot.title = element_text(size = rel(1.8)))+
  theme(legend.title= element_text(size = rel(1.4)))+
  theme(legend.text= element_text(size = rel(1.4)))+
  theme(axis.text= element_text(size = rel(1),colour = 'black'))+
guides(colour = guide_legend("Aridity"),size = guide_legend("Aridity"))+
  theme_bw()

P
tiff(filename ="Alternative_stabe_States_rev1B.tiff",width = 18,height = 16,units = "cm",res = 300)

P
dev.off()

load("results_EWSTest_C")

#Figure 3
P_V<-as.data.frame(Result[[2]])
AICs<-as.data.frame(Result[[1]])
colnames(P_V)=1:20
colnames(AICs)=1:20

PVR=reshape(P_V,direction="long", varying=colnames(P_V),times=colnames(P_V),v.names ="pvalue",timevar ="repetition",idvar="Livestock")
AIC_I=reshape(AICs,direction="long", varying=colnames(AICs),times=colnames(AICs),v.names ="AIC_I",timevar ="repetition",idvar="Livestock")


PVR$Livestock=PVR$Livestock+9
AIC_I$Livestock=AIC_I$Livestock+9
require(ggplot2)

aicp=ggplot()+geom_smooth(data=AIC_I,aes(x=Livestock,y=AIC_I),level=0.99,colour="black")+geom_hline(yintercept = 0, linetype = "longdash")+scale_y_continuous(name="I_AIC",limits=c(-0.01,0.01))+scale_x_continuous(name="Livestock Population Size")+theme_bw()
#+geom_vline(xintercept = 26, color='coral', linetype = "longdash")

pvp=ggplot()+geom_smooth(data=PVR,aes(x=Livestock,y=pvalue),level=0.99,colour="black")+scale_y_continuous(name="p-value",limits=c(0,0.6))+scale_x_continuous(name="Livestock Population Size")+theme_bw()
#+geom_vline(xintercept = 26,color="red", linetype = "longdash")
gridExtra::grid.arrange(aicp,pvp,ncol=2)


tiff(filename ="statisticsEWS_rev1.tiff",width = 16,height = 8,units = "cm",res = 300)

gridExtra::grid.arrange(aicp,pvp,ncol=2)
dev.off()

#figure 5

setwd("C:/Users/abaezaca/Dropbox (ASU)/Vegetation_project/")
load("results_Rotational_rev1b")
res_ASS_Rot<-as.data.frame(res_ASS_Rot)
colnames(res_ASS_Rot)<-c("5","15","Livestock","Aridity")
res_ASS_Rot$Aridity=as.factor(res_ASS_Rot$Aridity)
PVR=reshape(res_ASS_Rot,direction="long", varying=colnames(res_ASS_Rot)[1:2],times=colnames(res_ASS_Rot)[1:2],v.names ="forest_cover",timevar ="Rotation",idvar=c("Livestock","Aridity"))

colores=c("#d7191c",
"#fdae61",
"lightgreen",
"#a6d96a",
"#1a9641")


P<-ggplot(data=subset(PVR,Aridity==levels(PVR$Aridity)[1] & Rotation =="5"),aes(x=Livestock,y=forest_cover))+
  scale_x_continuous(limits=c(0,50))+
  scale_y_continuous(limits=c(0,1))+	
  geom_line(colour=colores[5], size = 1.5)+
  geom_line(data =subset(res_ASS,Aridity == "0" & Livestock <= 26), aes(x=Livestock,y=H_IC),colour =colores[1], size = 1.5)+  
  geom_line(data =subset(res_ASS,Aridity == "0" & Livestock > 26), aes(x=Livestock,y=H_IC),colour =colores[1], size = 1.5)+  
  geom_line(data=subset(PVR,Aridity==levels(PVR$Aridity)[1] & Rotation =="15"),aes(x=Livestock,y=forest_cover),colour =colores[5], linetype = 2, size = 1.5)+  
  geom_hline(yintercept = 0.7, linetype = "longdash")+
  labs(title="",x="Livestock Population Size",y="[F*]")+
  scale_colour_manual(name = "Livestock Strategy",labels = c("Constant","Rotation"),values = colores[c(5,1)])+
  theme(axis.title.y = element_text(size = rel(1.6)))+
  theme(axis.title.x = element_text(size = rel(1.6)))+
  theme(plot.title = element_text(size = rel(1.8)))+
  theme(legend.title= element_text(size = rel(1.4)))+
  theme(legend.text= element_text(size = rel(1.4)))+
  theme(axis.text= element_text(size = rel(1),colour = 'black'))+
  theme_bw()

load("C:/Users/abaezaca/Dropbox (ASU)/Vegetation_project/matorral/results_resting_and_rotation")
res_ASS_TPpertu=as.data.frame(res_ASS_TPpertu)
colnames(res_ASS_TPpertu)=c("F","A","D","rot")

P_rot1=ggplot(data=res_ASS_TPpertu,aes(x=factor(rot),y=F))+geom_boxplot()+
labs(title="",x="Rotation [years]",y="[F*]")+
geom_hline(yintercept = 0.2, linetype = "longdash")+
theme_bw()
#+
#geom_smooth(aes(x=rot,y=F))



load("C:/Users/abaezaca/Dropbox (ASU)/Vegetation_project/matorral/results_Rotation_timeseries")
colnames(ts_res_ASS_TPpertu)=c("F","time","rot")
 ts_res_ASS_TPpertu=as.data.frame(ts_res_ASS_TPpertu)

P_rot2=ggplot()+
geom_line(data=subset(ts_res_ASS_TPpertu,rot==33),aes(y=F,x=time,colour="darkgreen"),size=1)+
geom_line(data=subset(ts_res_ASS_TPpertu,rot==21),aes(y=F,x=time,colour="green"),size=1)+  
geom_line(data=subset(ts_res_ASS_TPpertu,rot==13),aes(y=F,x=time,colour="lightgreen"),size=1)+  
geom_line(data=subset(ts_res_ASS_TPpertu,rot==5),aes(y=F,x=time,colour="yellow"),size=1)+  
labs(title="",x="Time [years]",y="Area covered by forest, F")+
scale_colour_manual(name = "Rotation [years]",labels = rev(c("5","13","21","33")),values = rev(c("yellow","lightgreen","green","darkgreen")))+
theme_bw()

gridExtra::grid.arrange(P_rot2,gridExtra::arrangeGrob(P, P_rot1, ncol = 2), ncol=1)


tiff(filename ="C:/Users/abaezaca/Dropbox (ASU)/Vegetation_project/Rotation_fig5_rev1.tiff",width = 16,height = 12,units = "cm",res = 900)
gridExtra::grid.arrange(P_rot2,gridExtra::arrangeGrob(P, P_rot1, ncol = 2), ncol=1)
dev.off()


################################################
#supporting material
################################################
#sensitivity to parameter l


load("C:/Users/abaezaca/Dropbox (ASU)/Vegetation_project/matorral/res_l_sensitivity")
colnames(res_l_sensitivity)=c("F","A","D","l")
 res_l_sensitivity=as.data.frame(res_l_sensitivity)

tiff(filename ="C:/Users/abaezaca/Dropbox (ASU)/Vegetation_project/FigureS1.tiff",width = 12,height = 8,units = "cm",res = 900)

ggplot()+geom_smooth(data=res_l_sensitivity,aes(x=l,y=F),colour="black")+
geom_point(data=res_l_sensitivity,aes(x=l,y=F))+
labs(title="",x="half-saturation livestock constant",y="Area covered by forest, F*")+
theme_bw()
dev.off()
#sensitivity to parameter mL_x for x={F,A,G}

load("C:/Users/abaezaca/Dropbox (ASU)/Vegetation_project/matorral/res_mus_sensitivity")
colnames(res_mus_sensitivity)=c("F","A","D","muL_F","muL_A","muL_G")
 res_mus_sensitivity=as.data.frame(res_mus_sensitivity)

require(sensitivity)
x=pcc(X=res_mus_sensitivity[,c(4,5,6)],y=res_mus_sensitivity[,1],rank=F)    #calculate patial correlation
tiff(filename ="C:/Users/abaezaca/Dropbox (ASU)/Vegetation_project/FigureS2.tiff",width = 12,height = 8,units = "cm",res = 900)

barplot(as.vector(x$PCC[[1]]),names.arg = c(expression(m[F]^L),expression(m[A]^L),expression(m[G]^L)),cex.names=0.7,main='',ylab=expression(rho))   # barplot 1
dev.off()

tiff(filename ="C:/Users/abaezaca/Dropbox (ASU)/Vegetation_project/FigureS3.tiff",width = 12,height = 8,units = "cm",res = 900)

ggplot()+geom_smooth(data=res_mus_sensitivity, aes(x=muL_F,y=F),colour="black")+
geom_point(data=res_mus_sensitivity, aes(x=muL_F,y=F))+
geom_point(data=res_mus_sensitivity, aes(x=muL_F,y=A),colour="yellow")+
xlab(expression(m[F]^L))+
ylab(expression(F))+
theme_bw()
dev.off()


 
