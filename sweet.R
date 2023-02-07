pacotes <- c("tidyverse","ggrepel","fastDummies","knitr", "splines",
             "reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools",
             "ggstance","cowplot","beepr","factoextra","neuralnet",
             "ggpubr","GGally", "viridis", "plyr", "ggforce")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

rsq <- function (x, y) cor(x, y) ^ 2

#-----------------------------DATASET -----------------------------------------------#
soils <- read.csv("data/dataset.csv", sep = ";", header = T, dec = ",")
#Removing gamma superior to 50
soils <- soils[soils$gamma<=50,]
#normalizing measurements to a lower limit equal to 0
soils$fs[soils$fs<0] <- 0
soils$u[soils$u<0] <-0 
# removing NAs
soils<-na.omit(soils)

soils <- within(soils, 
                   Soil <- factor(Soil, 
                                      levels=names(sort(table(Soil), 
                                                        decreasing=TRUE))))

#plotting soils information
ggplot(soils) +
 geom_bar(aes(x=Soil)) +
  labs(x="",y="Number of occurrences")+
  theme_bw()+
  #theme(text = element_text(family = "Arial"))+
  facet_zoom(ylim = c(0, 300))+
  theme(plot.title = element_blank(), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=14),text = element_text(size = 14))   

ggsave("soils.pdf",height=8,width = 16)

# Histograma do G
ggplot(soils, aes(G))+
  geom_histogram()+
  xlab("G") +ylab("Frequency") +
  theme_bw()+
  theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   
ggsave("Histogram of G.png",height=8,width = 8)

# Histograma do G por tipo de solo:

soils %>%
  ggplot(aes(x=G, color=Soil, fill=Soil)) +
  labs(y="Probability distribution function (%)")+
 # geom_density()+
  #geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
  geom_density(alpha=.5) +
  scale_fill_viridis(discrete = T )+
  scale_color_viridis(discrete = T)+
  #guides(fill=guide_legend(title="Tipo de solo"),color=guide_legend(title="Tipo de solo"))+
  theme_bw()+
  theme(text = element_text(family = "Arial"))+
  theme(legend.title = element_blank(), legend.position=c(0.85,0.75) )+
  theme(plot.title = element_blank(),text = element_text(size = 14)) 
ggsave("Density function of G by soil type - before outliers treatment.png",height=8,width = 8)

#-----------------------------OUTLIERS TREATMENT-------------------------------#
outliers_removal <- function(dataset){
  tipos_soils <- unique(dataset[,1])
  for(tipo in tipos_soils){
    ##############FS############################
    qnt <- quantile(dataset[dataset$Soil==tipo,"fs"], probs=c(.25, .75), na.rm = T)
    caps <- quantile(dataset[dataset$Soil==tipo,"fs"], probs=c(.05, .95), na.rm = T)
    H <- 1.5 * IQR(dataset[dataset$Soil==tipo,"fs"], na.rm = T)
    dataset[dataset$Soil == tipo & dataset$fs  > qnt[2]+H,"fs"] <- caps[2]
    dataset[dataset$Soil == tipo & dataset$fs  < qnt[1]-H,"fs"] <- caps[1]
    ##############G############################
    qnt <- quantile(dataset[dataset$Soil==tipo,"G"], probs=c(.25, .75), na.rm = T)
    caps <- quantile(dataset[dataset$Soil==tipo,"G"], probs=c(.05, .95), na.rm = T)
    H <- 1.5 * IQR(dataset[dataset$Soil==tipo,"G"], na.rm = T)
    dataset[dataset$Soil == tipo & dataset$G  > qnt[2]+H,"G"] <- caps[2]
    dataset[dataset$Soil == tipo & dataset$G  < qnt[1]-H,"G"] <- caps[1]
    ##############u############################
    qnt <- quantile(dataset[dataset$Soil==tipo,"u"], probs=c(.25, .75), na.rm = T)
    caps <- quantile(dataset[dataset$Soil==tipo,"u"], probs=c(.05, .95), na.rm = T)
    H <- 1.5 * IQR(dataset[dataset$Soil==tipo,"u"], na.rm = T)
    dataset[dataset$Soil == tipo & dataset$u  > qnt[2]+H,"u"] <- caps[2]
    dataset[dataset$Soil == tipo & dataset$u  < qnt[1]-H,"u"] <- caps[1]
    ##############qt############################
    qnt <- quantile(dataset[dataset$Soil==tipo,"qt"], probs=c(.25, .75), na.rm = T)
    caps <- quantile(dataset[dataset$Soil==tipo,"qt"], probs=c(.05, .95), na.rm = T)
    H <- 1.5 * IQR(dataset[dataset$Soil==tipo,"qt"], na.rm = T)
    dataset[dataset$Soil == tipo & dataset$qt  > qnt[2]+H,"qt"] <- caps[2]
    dataset[dataset$Soil == tipo & dataset$qt  < qnt[1]-H,"qt"] <- caps[1]
  }
  return(dataset)
}
summary(soils)
soils <- outliers_removal(soils)

summary(soils)
soils %>%
  ggplot(aes(x=G, color=Soil, fill=Soil)) +
  labs(y="Probability distribution function (%)")+
  # geom_density()+
  #geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
  geom_density(alpha=.5) +
  scale_fill_viridis(discrete = T )+
  scale_color_viridis(discrete = T)+
  #guides(fill=guide_legend(title="Tipo de solo"),color=guide_legend(title="Tipo de solo"))+
  theme_bw()+
  theme(text = element_text(family = "Arial"))+
  theme(legend.title = element_blank(), legend.position=c(0.85,0.75) )+
  theme(plot.title = element_blank(),text = element_text(size = 14)) 
ggsave("Density function of G by soil type - after outliers treatment.png",height=8,width = 8)
#1861 --> ajustou os outliers
##################################################################################
#                               ESTUDO DAS CORRELAÇÕES                           #
##################################################################################
#A função correlation do pacote correlation faz com que seja estruturado um
#diagrama interessante que mostra a inter-relação entre as variáveis e a
#magnitude das correlações entre elas
#soils<-filter(soils, qt<10000 & gamma<25)
f<-ggpairs(soils[2:6], aes( alpha = 0.4))
f +
  theme_bw()+
  theme(text = element_text(family = "Arial"))+
  theme(plot.title = element_blank(),axis.text = element_text(size = 14), axis.title = element_text(size = 14),text = element_text(size = 14))   
ggsave("scatter.png",height=8,width = 8)

##################################################################################
#     ESTIMANDO UM model MÚLTIPLO COM AS VARIÁVEIS DA BASE DE DADOS soils    
#     TRATA-SE DE UMA REGRESSÃO LINEAR
##################################################################################
#Estimando a Regressão Múltipla

model_soils <- lm(formula = gamma ~ . - Soil -mq -Rf,
                      data = soils)
summary(model_soils)
#################################################################################
#     LOGARITIMIC FUNCTION APPLIED TO FS, QT AND U
#############################################################################

soils$logFs <- log10(soils$fs+1)
soils$logqt <- log10(soils$qt+1)
soils$UTransf <- log10(soils$u+1)
model_log <- lm(gamma~G+logqt+logFs+UTransf  ,
                 data = soils)
summary(model_log)
saveRDS(model_log, "model_log.rds")
soils$yhat_lm_log <- model_log$fitted.values

#################################################################################
#     Robertson & Cabal (2010)
#############################################################################
soils$yhat_gamma_r_cabal <- 0.24*(0.27 * log(abs(soils$Rf)) + 0.36*log(soils$qt*100) +1.236)*10*soils$G/2.65
summary(lm(formula=gamma~yhat_gamma_r_cabal, data=soils))
plot(soils$yhat_gamma_r_cabal,soils$gamma)
rsq(soils$yhat_gamma_r_cabal,soils$gamma)

#################################################################################
# Mayne & Peuchen (2012)
#############################################################################
soils$yhat_gamma_m_peuchen <- (0.636*(soils$qt)^0.072) * (10 + soils$mq/8)
plot(soils$yhat_gamma_m_peuchen,soils$gamma)
rsq(soils$yhat_gamma_m_peuchen,soils$gamma)

#################################################################################
# Mayne (2014)
#############################################################################
soils$yhat_gamma_mayne <- 12 + 1.5*log(soils$fs+1)
plot(soils$yhat_gamma_mayne,soils$gamma)
rsq(soils$yhat_gamma_mayne,soils$gamma)

###############NEURAL NETWORK##############################
soils_nn <- select(soils, c(gamma, G,qt,fs,u,yhat_lm_log))

#Scaling data for performance
set.seed(42)
max_data <- apply(soils_nn, 2, max) 
min_data <- apply(soils_nn, 2, min)
scaled <- scale(soils_nn,center = min_data, scale = max_data - min_data)

index = sample(1:nrow(soils_nn),round(0.70*nrow(soils_nn)))
train_data <- as.data.frame(scaled[index,])
test_data <- as.data.frame(scaled[-index,])

set.seed(42)
nn <- neuralnet(gamma~.,data=train_data,hidden=c(32,16,8,4),linear.output=T)
saveRDS(nn, "model_nn_train_data.rds")
nn_all <- neuralnet(gamma~.,data=scaled,hidden=c(32,16,8,4),linear.output=T)
saveRDS(nn_all, "model_nn_all_data.rds")

pr.nn <- compute(nn,test_data[,2:7])
pr.all <- compute(nn,scaled[,2:7])

test_data$yhat_nn <- pr.nn$net.result

##########################################################################
# PLOTTING RESULTS
##########################################################################

soils$yhat_lm <- model_soils$fitted.values
soils$yhat_lm_log <- model_log$fitted.values
soils$yhat_nn <- pr.all$net.result*(max(soils$gamma)-min(soils$gamma))+min(soils$gamma)

soils$diff_nn <- abs(soils$gamma - soils$yhat_nn)/soils$gamma
soils$diff_lm <- abs(soils$gamma - soils$yhat_lm)/soils$gamma
soils$diff_lm_log <- abs(soils$gamma - soils$yhat_lm_log)/soils$gamma

#scatter plot - linear regression
eqn <- sprintf(
  "italic(gamma) == %.3g + %.3g * italic(G) + %.3g*qt + %.3g*fs +%.3g*u * ',' ~~ italic(r)^2 ~ '=' ~ %.2g",
  coef(model_soils)[1],
  coef(model_soils)[2],
  coef(model_soils)[3],
  coef(model_soils)[4],
  coef(model_soils)[5],
  summary(model_soils)$r.squared
)
soils %>%
  ggplot() +
  geom_point(aes(x = gamma, y = yhat_lm),
             color = "black", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = gamma, y = gamma), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  annotate("text",x = Inf, y = -Inf,label = eqn, parse = TRUE,hjust = 1.1, vjust = -.5, size=4  )+
  labs(x = "Soil unit weight (kN/m³)", y = "Estimated soil unit weight (kN/m³)") +
  theme_bw() +
  theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   

ggsave("linear_model.png",height=8,width = 8)

#linear regression with log
rsq(soils$gamma,soils$yhat_lm_log)
eqn <- sprintf(
  "italic(gamma) ==  %.3g + %.3g * italic(G) + %.3g*log(qt) + %.3g*log(fs) + %.3g*log(u) * ',' ~~ italic(r)^2 ~ '=' ~ %.2g",
  coef(model_log)[1],
  coef(model_log)[2],
  coef(model_log)[3],
  coef(model_log)[4],
  coef(model_log)[5],
  rsq(soils$gamma,soils$yhat_lm_log)
)

soils %>%
  ggplot() +
  geom_point(aes(x = gamma, y = yhat_lm_log),
             color = "black", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = gamma, y = gamma), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  annotate("text",x = Inf, y = -Inf,label = eqn, parse = TRUE,hjust = 1.1, vjust = -.5, size=4  )+
  labs(x = "Soil unit weight (kN/m³)", y = "Estimated soil unit weight (kN/m³)") +
  theme_bw() +
  theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   

ggsave("linear_model_log.png",height=8,width = 8)


#  Robertson & Cabal (2010)
rsq(soils$gamma,soils$yhat_gamma_r_cabal)
eqn <- sprintf(
  "  'Robertson & Cabal (2010)'   ~~ italic(r)^2 ~ '=' ~ %.2g",
  rsq(soils$gamma,soils$yhat_gamma_r_cabal)
)

soils %>%
  ggplot() +
  geom_point(aes(x = gamma, y = yhat_gamma_r_cabal),
             color = "black", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = gamma, y = gamma), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  annotate("text",x = Inf, y = -Inf,label = eqn, parse = TRUE,hjust = 1.1, vjust = -.5, size=4  )+
  labs(x = "Soil unit weight (kN/m³)", y = "Estimated soil unit weight (kN/m³)") +
  theme_bw() +
  theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   

ggsave("robertson_and_cabal.png",height=8,width = 8)



#gerando um gráfico do model de regressão RNA
eqn <- sprintf(
  "Hidden ~ layers:~ 4 * ',' ~~ italic(r)^2 ~ '=' ~ %.2g",
  rsq(test_data$gamma,test_data$yhat_nn)
)

test_data %>%
  ggplot() +
  geom_point(aes(x = gamma, y = yhat_nn),
             color = "black", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = gamma, y = gamma), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  annotate("text",x = Inf, y = -Inf,label = eqn, parse = TRUE,hjust = 1.1, vjust = -.5, size=4  )+
  labs(x = "Soil unit weight (kN/m³)", y = "Estimated soil unit weight (kN/m³)") +
  theme_bw() +
  theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   

ggsave("test_data_nn_scaled_entrada_log_4_camadas.png",height=8,width = 8)


#gerando um gráfico do model de regressão RNA
eqn <- sprintf(
  "Hidden ~ layers: ~ 4* ',' ~~ italic(r)^2 ~ '=' ~ %.2g",
  rsq(soils$gamma,soils$yhat_nn)
)

soils %>%
  ggplot() +
  geom_point(aes(x = gamma, y = yhat_nn),
             color = "black", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = gamma, y = gamma), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  annotate("text",x = Inf, y = -Inf,label = eqn, parse = TRUE,hjust = 1.1, vjust = -.5, size=4  )+
  labs(x = "Soil unit weight (kN/m³)", y = "Estimated soil unit weight (kN/m³)") +
  theme_bw() +
  theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   

ggsave("neural_network_entrada_log_4_camadas.png",height=8,width = 8)

