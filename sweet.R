pacotes <- c("tidyverse","ggrepel","fastDummies","knitr", "splines",
             "reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools",
             "ggstance","cowplot","beepr","factoextra","neuralnet",
             "ggpubr","GGally", "viridis", "plyr", "ggforce","randomForest")

#install.packages("devtools")
library("devtools")
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

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
  #theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   
ggsave("Histogram of G.pdf",height=8,width = 8)

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
  #theme(text = element_text(family = "Arial"))+
  theme(legend.title = element_blank(), legend.position=c(0.85,0.75) )+
  theme(plot.title = element_blank(),text = element_text(size = 14)) 
ggsave("Density function of G by soil type - before outliers treatment.pdf",height=8,width = 8)

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
  #theme(text = element_text(family = "Arial"))+
  theme(legend.title = element_blank(), legend.position=c(0.85,0.75) )+
  theme(plot.title = element_blank(),text = element_text(size = 14)) 
ggsave("Density function of G by soil type - after outliers treatment.pdf",height=8,width = 8)
#1861 --> ajustou os outliers

##################################################################################
# CLUSTERING ANALYSIS
#################################################################################
soilscluster <- select(soils, c("G","gamma","qt","fs","u"))

#removendo a coluna com o nome dos soils
#soils <- soils[,-1]
#realizando o scale para padronizar as variáveis
soils.scaled <- scale(soilscluster)

#matriz de distâncias eucledianas
distance = dist(soils.scaled, method = "euclidean")

#Análise de Elbow, quantos clusters?
f <- fviz_nbclust(soils.scaled, FUN = hcut, method = "wss")
f + xlab("Number of clusters") +ylab("Total within sum of square (wss)") +
  theme_bw()+
  theme(plot.title = element_blank(),legend.text = element_text( size = 14),legend.position=c(0.92,0.85),axis.text = element_text(size = 14), axis.title = element_text(size = 14),text = element_text(size = 14))   
ggsave("wss.pdf",height=8,width = 8)

f <- fviz_nbclust(soils.scaled, kmeans, method = "silhouette")
f + xlab("Number of clusters") +ylab("average silhouette width") +
  theme_bw()+
  theme(plot.title = element_blank(),legend.text = element_text( size = 14),legend.position=c(0.92,0.85),axis.text = element_text(size = 14), axis.title = element_text(size = 14),text = element_text(size = 14))   
ggsave("silhuette.pdf",height=8,width = 8)

# soils with 2 clusters
set.seed(42)
soils.k2 <- kmeans(soils.scaled, centers = 2)
soils$cluster <- soils.k2$cluster
#png(file="Cluster K2.pdf",width=800, height=600)
f <- fviz_cluster(soils.k2, data = soils.scaled, main = "Cluster K2")
f + 
  #xlab("Dimensão 1 (44.7%)") +ylab("Dimensão 2 (24%)") +
  theme_bw()+
  theme(plot.title = element_blank(),legend.text = element_text( size = 14),legend.position=c(0.92,0.85),axis.text = element_text(size = 14), axis.title = element_text(size = 14),text = element_text(size = 14))   
ggsave("Cluster K2.pdf",height=8,width = 8)
#dev.off()

soils$cluster <- as.factor(soils$cluster)
levels(soils$cluster) <- c("Cluster 1","Cluster 2")
ggplot(soils, aes(gamma))+
  geom_histogram()+
  facet_wrap(~cluster)+  
  xlab("gamma (kN/m³)") +ylab("Number of occurrences") +
  theme_bw()+
  theme(legend.text = element_text( size = 14),legend.position=c(0.92,0.85),axis.text = element_text(size = 14), axis.title = element_text(size = 14),text = element_text(size = 14))   
ggsave("gamma vs clusters K2.pdf",height=8,width = 8)

#visualizando em cores os clusters
soils %>% ggplot() +
  #scale_colour_manual(values = c("#5ec962", "#21918c"))+
  scale_color_viridis(discrete = T)+
  geom_point(aes(x = G,
                 y = gamma,
                 colour = cluster),
             size = 3)+
  xlab("G") +ylab("gamma (kN/m³)") +
  theme_bw()+
  theme(legend.position="none", axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   
ggsave("cluster G vs gamma k=2.pdf",height=8,width = 8)
summary(soils$Soil[soils$cluster=="Cluster 1"])
summary(soils$Soil[soils$cluster=="Cluster 2"])

# soils with 3 clusters
set.seed(42)
soils.k3 <- kmeans(soils.scaled, centers = 3)
soils$cluster <- soils.k3$cluster
#png(file="Cluster K2.pdf",width=800, height=600)
f <- fviz_cluster(soils.k3, data = soils.scaled, main = "Cluster K3")
f + 
  #xlab("Dimensão 1 (44.7%)") +ylab("Dimensão 2 (24%)") +
  theme_bw()+
  theme(plot.title = element_blank(),legend.text = element_text( size = 14),legend.position=c(0.92,0.85),axis.text = element_text(size = 14), axis.title = element_text(size = 14),text = element_text(size = 14))   
ggsave("Cluster K3.pdf",height=8,width = 8)
#dev.off()

soils$cluster <- as.factor(soils$cluster)
levels(soils$cluster) <- c("Cluster 1","Cluster 2","Cluster 3")
ggplot(soils, aes(gamma))+
  geom_histogram()+
  facet_wrap(~cluster)+  
  xlab("gamma (kN/m³)") +ylab("Number of occurrences") +
  theme_bw()+
  theme(legend.text = element_text( size = 14),legend.position=c(0.92,0.85),axis.text = element_text(size = 14), axis.title = element_text(size = 14),text = element_text(size = 14))   
ggsave("gamma vs clusters K3.pdf",height=8,width = 8)

#visualizando em cores os clusters
soils %>% ggplot() +
  scale_color_viridis(discrete = T)+
  geom_point(aes(x = G,
                 y = gamma,
                 color = cluster),
             size = 3)+
  xlab("G") +ylab("gamma (kN/m³)") +
  theme_bw()+
  theme(legend.position="none", axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   
ggsave("cluster G vs gamma k=3.pdf",height=8,width = 8)

summary(soils$Soil[soils$cluster=="Cluster 1"])
summary(soils$Soil[soils$cluster=="Cluster 2"])
summary(soils$Soil[soils$cluster=="Cluster 3"])


# soils with 4 clusters
set.seed(42)
soils.k4 <- kmeans(soils.scaled, centers = 4)
soils$cluster <- soils.k4$cluster
f <- fviz_cluster(soils.k4, data = soils.scaled, main = "Cluster K6")
f + 
  theme_bw()+
  theme(plot.title = element_blank(),legend.text = element_text( size = 14),legend.position=c(0.92,0.85),axis.text = element_text(size = 14), axis.title = element_text(size = 14),text = element_text(size = 14))   
ggsave("Cluster K4.pdf",height=8,width = 8)
#dev.off()

soils$cluster <- as.factor(soils$cluster)
levels(soils$cluster) <- c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5")
ggplot(soils, aes(gamma))+
  geom_histogram()+
  facet_wrap(~cluster)+  
  xlab("gamma (kN/m³)") +ylab("Number of ocurrences") +
  theme_bw()+
  theme(legend.text = element_text( size = 14),legend.position=c(0.92,0.85),axis.text = element_text(size = 14), axis.title = element_text(size = 14),text = element_text(size = 14))   
ggsave("gamma vs clusters K4.pdf",height=8,width = 8)

#visualizando em cores os clusters
soils %>% ggplot() +
  scale_color_viridis(discrete=T)+
  geom_point(aes(x = G,
                 y = gamma,
                 color = cluster),
             size = 3)+
  xlab("G") +ylab("gamma (kN/m³)") +
  theme_bw()+
  theme(legend.position="none", axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   
ggsave("cluster G vs gamma k=4.pdf",height=8,width = 8)

summary(soils$Soil[soils$cluster=="Cluster 1"])
summary(soils$Soil[soils$cluster=="Cluster 2"])
summary(soils$Soil[soils$cluster=="Cluster 3"])
summary(soils$Soil[soils$cluster=="Cluster 4"])

# soils with 5 clusters
set.seed(42)
soils.k5 <- kmeans(soils.scaled, centers = 5)
soils$cluster <- soils.k5$cluster
#png(file="Cluster K2.pdf",width=800, height=600)
f <- fviz_cluster(soils.k5, data = soils.scaled, main = "Cluster K6")
f + 
  #xlab("Dimensão 1 (44.7%)") +ylab("Dimensão 2 (24%)") +
  theme_bw()+
  theme(plot.title = element_blank(),legend.text = element_text( size = 14),legend.position=c(0.92,0.85),axis.text = element_text(size = 14), axis.title = element_text(size = 14),text = element_text(size = 14))   
ggsave("Cluster K5.pdf",height=8,width = 8)
#dev.off()

soils$cluster <- as.factor(soils$cluster)
levels(soils$cluster) <- c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5")
ggplot(soils, aes(gamma))+
  geom_histogram()+
  facet_wrap(~cluster)+  
  xlab("gamma (kN/m³)") +ylab("Number of ocurrences") +
  theme_bw()+
  theme(legend.text = element_text( size = 14),legend.position=c(0.92,0.85),axis.text = element_text(size = 14), axis.title = element_text(size = 14),text = element_text(size = 14))   
ggsave("gamma vs clusters K5.pdf",height=8,width = 8)


#visualizando em cores os clusters
soils %>% ggplot() +
  scale_color_viridis(discrete=T)+
  geom_point(aes(x = G,
                 y = gamma,
                 color = cluster),
             size = 3)+
  xlab("G") +ylab("gamma (kN/m³)") +
  theme_bw()+
  theme(legend.position="none", axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   
ggsave("cluster G vs gamma k=5.pdf",height=8,width = 8)

summary(soils$Soil[soils$cluster=="Cluster 1"])
summary(soils$Soil[soils$cluster=="Cluster 2"])
summary(soils$Soil[soils$cluster=="Cluster 3"])
summary(soils$Soil[soils$cluster=="Cluster 4"])
summary(soils$Soil[soils$cluster=="Cluster 5"])

##################################################################################
#                               CORRELATION STUDY                               #
##################################################################################

f<-ggpairs(soils[2:6], aes( alpha = 0.4))
f +
  theme_bw()+
  theme(plot.title = element_blank(),axis.text = element_text(size = 7), axis.title = element_text(size = 14),text = element_text(size = 14))   
ggsave("scatter.pdf")

##################################################################################
#     ESTIMANDO UM model MÚLTIPLO COM AS VARIÁVEIS DA BASE DE DADOS soils    
#     TRATA-SE DE UMA REGRESSÃO LINEAR
##################################################################################
#Estimando a Regressão Múltipla

model_soils <- lm(formula = gamma ~ . - Soil -mq -Rf -fs -cluster,
                      data = soils)
summary(model_soils)
#################################################################################
#     LOGARITIMIC FUNCTION APPLIED TO QT AND U
#############################################################################

#soils$logFs <- log10(soils$fs+1)
soils$logqt <- log10(soils$qt+1)
soils$UTransf <- log10(soils$u+1)
model_log <- lm(gamma~G+logqt+UTransf  ,
                 data = soils)
summary(model_log)
saveRDS(model_log, "model_log.rds")
soils$yhat_lm_log <- model_log$fitted.values

#################################################################################
#     Robertson & Cabal (2010)
#############################################################################
soils$yhat_gamma_r_cabal_1 <- (0.27 * log(abs(soils$Rf)) + 0.36*log(soils$qt*100) +1.236)*10
summary(lm(formula=gamma~yhat_gamma_r_cabal_1, data=soils))
plot(soils$yhat_gamma_r_cabal_1,soils$gamma)
rsq(soils$yhat_gamma_r_cabal_1,soils$gamma)


soils$yhat_gamma_r_cabal_2 <- 3.72+ 0.24*(0.27 * log(abs(soils$Rf)) + 0.36*log(soils$qt*100) +1.236)*10*soils$G/2.65
summary(lm(formula=gamma~yhat_gamma_r_cabal_2, data=soils))
plot(soils$yhat_gamma_r_cabal_2,soils$gamma)
rsq(soils$yhat_gamma_r_cabal_2,soils$gamma)

#################################################################################
# Mayne & Peuchen (2012)
#############################################################################
soils$yhat_gamma_m_peuchen_1 <- 10+(soils$mq/8)
plot(soils$yhat_gamma_m_peuchen_1,soils$gamma)
rsq(soils$yhat_gamma_m_peuchen_1,soils$gamma)

soils$yhat_gamma_m_peuchen_2 <- (0.636*(soils$qt)^0.072) * (10 + soils$mq/8)
plot(soils$yhat_gamma_m_peuchen_2,soils$gamma)
rsq(soils$yhat_gamma_m_peuchen_2,soils$gamma)
#soils$yhat_gamma_m_peuchen <- (0.886*(soils$qt/100)^0.072) * (1 + 0.125*soils$mq/10) *10
#################################################################################
# Mayne (2014)
#############################################################################
soils$yhat_gamma_mayne_1 <- 26 - (14/(1+(0.5*log10(soils$fs+1))^2))
plot(soils$yhat_gamma_mayne_1,soils$gamma)
rsq(soils$yhat_gamma_mayne_1,soils$gamma)


soils$yhat_gamma_mayne_2 <- 12 + 1.5*log(soils$fs+1)
plot(soils$yhat_gamma_mayne_2,soils$gamma)
rsq(soils$yhat_gamma_mayne_2,soils$gamma)

###############NEURAL NETWORK##############################
soils_nn <- select(soils, c(gamma, G,qt,fs,u))
#soils_nn <- select(soils, c(gamma, G,logqt,UTransf,yhat_lm_log))

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
#plot.nnet(nn,alpha.val = 0.5, bord.col = 'black',max.sp= T, size=10)
saveRDS(nn, "model_nn_train_data.rds")
nn_all <- neuralnet(gamma~.,data=scaled,hidden=c(32,16,8,4),linear.output=T)
saveRDS(nn_all, "model_nn_all_data.rds")

pr.nn <- compute(nn,test_data[,2:5])
pr.all <- compute(nn,scaled[,2:5])

test_data$yhat_nn <- pr.nn$net.result

##########################################################
#RANDOM FOREST TO PREDICT THE SOIL TYPE
##########################################################
#scaled <- cbind(scaled, select(soils,Soil))
#model_RF <- randomForest(Soil~., data = scaled, ntree = 100, mtry = 3, importance = TRUE)
#model_RF


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
  "italic(gamma) == %.3g + %.3g * italic(G) + %.3g*qt +%.3g*u * ',' ~~ italic(r)^2 ~ '=' ~ %.2g",
  coef(model_soils)[1],
  coef(model_soils)[2],
  coef(model_soils)[3],
  coef(model_soils)[4],
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
  #theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   

ggsave("linear_model.pdf",height=8,width = 8)

#linear regression with log
rsq(soils$gamma,soils$yhat_lm_log)
eqn <- sprintf(
  "italic(gamma) ==  %.3g + %.3g * italic(G) + %.3g*log(qt)+ %.3g*log(u) * ',' ~~ italic(r)^2 ~ '=' ~ %.2g",
  coef(model_log)[1],
  coef(model_log)[2],
  coef(model_log)[3],
  coef(model_log)[4],
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
  #theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   

ggsave("linear_model_log.pdf",height=8,width = 8)


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
 #theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   

ggsave("robertson_and_cabal.pdf",height=8,width = 8)


# Mayne & Peuchen (2012)
rsq(soils$gamma,soils$yhat_gamma_m_peuchen)
eqn <- sprintf(
  "  'Mayne & Peuchen (2012)'   ~~ italic(r)^2 ~ '=' ~ %.2g",
  rsq(soils$gamma,soils$yhat_gamma_m_peuchen)
)

soils %>%
  ggplot() +
  geom_point(aes(x = gamma, y = yhat_gamma_m_peuchen),
             color = "black", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = gamma, y = gamma), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  annotate("text",x = Inf, y = -Inf,label = eqn, parse = TRUE,hjust = 1.1, vjust = -.5, size=4  )+
  labs(x = "Soil unit weight (kN/m³)", y = "Estimated soil unit weight (kN/m³)") +
  theme_bw() +
 # theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   

ggsave("Mayne_and_peuchen.pdf",height=8,width = 8)


# Mayne (2014)
rsq(soils$gamma,soils$yhat_gamma_mayne)
eqn <- sprintf(
  "  'Mayne (2014)'   ~~ italic(r)^2 ~ '=' ~ %.2g",
  rsq(soils$gamma,soils$yhat_gamma_mayne)
)

soils %>%
  ggplot() +
  geom_point(aes(x = gamma, y = yhat_gamma_mayne),
             color = "black", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = gamma, y = gamma), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  annotate("text",x = Inf, y = -Inf,label = eqn, parse = TRUE,hjust = 1.1, vjust = -.5, size=4  )+
  labs(x = "Soil unit weight (kN/m³)", y = "Estimated soil unit weight (kN/m³)") +
  theme_bw() +
  #theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   

ggsave("Mayne_2014.pdf",height=8,width = 8)



#gerando um gráfico do model de regressão RNA
eqn <- sprintf(
  "Hidden ~ layers:~ 4 (32-16-8-4)* ',' ~~ italic(r)^2 ~ '=' ~ %.2g",
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
  #theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   

ggsave("test_data_nn_scaled_entrada_log_4_camadas.pdf",height=8,width = 8)


#gerando um gráfico do model de regressão RNA
eqn <- sprintf(
  "Hidden ~ layers: ~ 4 (32-16-8-4)* ',' ~~ italic(r)^2 ~ '=' ~ %.2g",
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
  #theme(text = element_text(family = "Arial"))+
  theme(legend.text = element_text( size = 16),legend.position=c(0.92,0.85),axis.text = element_text(size = 20), axis.title = element_text(size = 20),text = element_text(size = 20))   

ggsave("neural_network_entrada_log_4_camadas.pdf",height=8,width = 8)

