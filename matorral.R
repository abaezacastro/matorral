#' Chilean matorral Model


matorral <- list()
class(matorral) <- "ca_model"  
matorral$name <- "Andres' Chilean matorral Model"
matorral$ref <- NA  # a bibliographic reference
matorral$states <- c("D","G","A","S")
matorral$cols <- terrain.colors(4)[c(4,3,2,1)]
# a list of default model parameters, used to validate input parameters. 
matorral$parms <- list(
  ### vegetation model parameters: 
  m_s = 0.01,   # intrinsic mortality sclerophylls
  m_a = 0.01,    # intrinsic mortality Acacia
  m_g = 1,    # intrinsic mortality grass
  betaS = 4, # colonization rate sclerophylls 
  betaA = 7,  # colonization rate Acacia
  R = 1,      # regeneration rate of grass
  fas =1,      # local fascilitation on Sclerophylls 
  a = 7,      # half saturation constant of effect of livestock on Acacia recrutment
  aridity = 1.0,  # Aridity
  del = 0.9,  	# seeds dispersed; (1-del) seeds on nearest neighbourhood 
  fg = 0.3, 		# local fascilitation grass
  fss = 1,      # local facilitation of sclerophyllus forest on its own recolonisation
  m_LG=0.1,     # livestock induced mortality grass
  m_LS=0.015,    # livestock induced mortality sclerophylls
  m_LA=0.005,    # livestock induced mortality Acacia
  n_A=1, ##params of density dependent mortality in acacia
  ic_A=2.7,# 2.7 
  ### livestock model parameters: 
  DM_I=2,  
  ME_Ac=10, #29 #mega-Joules/Kg DM
  ME_S=49, 
  ME_G=29, 
  herd_size = 5, # number of animals per hectare
  K_mr = 0.31*2, #kids annual mortality rate
  RS_mr= 0.12,
  OS_mr= 0.125,
  B_f_mr=0.12,
  B_m_mr=0.1,
  ratio_mf=0.1,
  fer_kids=0.9,
  prolificacy=1.0,
  kidding_rate=1.1,
  E_m_factor=5,
  A_Pk = 1
)
# matorral$livestock <- function(x, l = NULL, herd_size = matorral$parms$herd_size) { #for now a null model returning always the same value
#               structure(
#                 data.frame(sex = rep("F", herd_size), 
#                            age = rep(17, herd_size), 
#                            type = rep("S", herd_size), 
#                            d_rate = rep(12, herd_size)
#                 ),
#                 class = c("livestock", "data.frame")
#               )
#               }
# an update function
#browser()
matorral$update <- function(x_old, parms=matorral$parms, subs = 12, livestock = matorral$parms$herd_size, ...) {

   for(ss in 1:subs) {
    # creates variable "pod_used" in environment of function ca()
    
  x_new <- x_old
  
  # define update procedures depending on parms 
  
  # model specific part:
  
  # 2 - drawing random numbers
  rnum <- runif(prod(x_old$dim)) # one random number between 0 and 1 for each cell

  rho <- as.list(summary(x_old)$cover)
  # count local density of occupied fields of S and A for each cell: 
  parms$Q_A <- count(x_old, "A")/4 
  parms$Q_S <- count(x_old, "S")/4 
  parms$Q_AS <- parms$Q_A + parms$Q_S
  #parms$perhectare <- prod(x_old$dim)/(200*200)
  parms$Live <-livestock #*parms$perhectare
  # calculate recolonisation rates of A cells
  recolonisation_A <- with(parms, betaA*aridity*(del*rho$A*(Live/(a+Live))+(1-del)*Q_A)/subs)
  
  # calculate recolonisation rates of S cells
  recolonisation_S <- with(parms, betaS*aridity*fss*Q_S/subs)
  
  # calculate death rates
  death_S <- with(parms, (m_LS*Live+m_s)/subs)
  death_A <- with(parms, (m_LA*Live+m_a*(n_A+ic_A*Q_A))/subs)
  
  # correct for overshooting death prob
  #death[death > 1] <- 1
  
  regeneration_G <- with(parms, aridity*(R + fg*Q_A)/subs)
  degradation <- with(parms, ((m_LG*Live+ m_g) /subs))
  
  # check for sum of probabilities to be inferior 1 and superior 0
  if(any(c(recolonisation_S+recolonisation_A+degradation,recolonisation_S + recolonisation_A, death_A,death_S, regeneration_G) > 1 )) warning(paste("a set probability is exceeding 1 in run", 3, "time step", i, "! decrease delta!!!")) 
  if(any(c(recolonisation_S,recolonisation_A,degradation, death_A,death_S, regeneration_G) < 0)) warning(paste("a set probability falls below 0 in run", 3, "in time step", i, "! balance parameters!!!")) 
  
  if(any(c(recolonisation_S+recolonisation_A+degradation,recolonisation_S + recolonisation_A, death_A,death_S, regeneration_G) > 1 )) browser()
  # 5 - applying the rules to fill the cells in x_new
  
  x_new$cells[which(x_old$cells == "G" & rnum <= recolonisation_S + recolonisation_A & recolonisation_S > recolonisation_A )] <- "S"
  x_new$cells[which(x_old$cells == "G" & rnum <= recolonisation_S + recolonisation_A & recolonisation_S <= recolonisation_A )] <- "A"
  x_new$cells[which(x_old$cells == "A"  & rnum <= death_A)] <- "D"
  x_new$cells[which(x_old$cells == "S"  & rnum <= death_S)] <- "D"
  x_new$cells[which(x_old$cells == "G"  & rnum > recolonisation_S + recolonisation_A & rnum <= recolonisation_S + recolonisation_A+degradation)] <- "D"  
  x_new$cells[which(x_old$cells == "D"   & rnum <= regeneration_G)] <- "G"
  
  
  }
  
  return(x_new)
}







