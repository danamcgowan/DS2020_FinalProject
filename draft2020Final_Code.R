#read in libraries
library(haven)
library(ggcorrplot)
library(dplyr)
library(usdm)
library(tree)
library(ISLR2)
library(randomForest) 
library(caret)
library(Metrics)

#read in year 1 data
data <- read_dta('/Users/danarohde/Desktop/Data2020/Final Project/FFdata/wave2/FF_wave2_2020v2.dta')

#data$m2h9c1 how much money did you receive last month from welfare/TANF?
#data$m2h9c2 how much money from food stamps?
#cm2povco: Poverty ratio - household income/poverty threshold
#cm2hhinc: income used in ^
#make target 
data$cm2hhinc <- ifelse(data$cm2hhinc >= 0, data$cm2hhinc, NA)
data$cm2povco <- ifelse(data$cm2povco >= 0, data$cm2povco, NA)
data$m2h9c1 <- ifelse(data$m2h9c1 >= 0, data$m2h9c1, NA)
data$pov_threshold <- data$cm2hhinc/data$cm2povco
data$y <- (data$m2h9c1)/(data$pov_threshold-data$cm2hhinc) #target: ((govt aid)/(poverty line-income))
data <- data[!is.na(data$y) & is.finite(data$y), ]

#overall EDA - cm2natsm, cm2povca, cm2povco, cm2hhinc, m2l1, m2h9c1, m2c38, m2b32, m2h12, cm2gdad, m2h9a3>.1 correlation
numeric_data <- data[, sapply(data, is.numeric)] #get numeric columns
variances <- apply(numeric_data, 2, var, na.rm = TRUE) #get their variances
correlations <- cor(numeric_data[, which(variances>0)]) #extract correlations of numeric variables with variance>0
sort(abs(correlations[, "y"]), decreasing = TRUE) #get top variables for y

#Demographics EDA - none with correlation>.1
m <- model.matrix(~0+., data=data[,c('y', 'f2a6c', 'f2c1a', 'f2f2b1', 'f2f2b2', 'f2f2b3', 'f2f2b4', 'f2f2b5', 'f2f2b6', 'f2f2b7', 'f2f2b8', 'f2f2b9', 'f2f2b10', 'f2g1a', 'f2g1b', 'f2g1c', 'f2g1d', 'f2g1x', 'f2g5', 'f2g5a', 'f2g5b', 'f2g12d', 'f2g14a', 'f2g15a', 'f2k7', 'cf2span', 'm2a7c', 'cm2b_age', 'm2c1a', 'm2e2a1', 'm2f2b1', 'm2f2b2', 'm2f2b3', 'm2f2b4', 'm2f2b5', 'm2f2b6', 'm2f2b7', 'm2f2b8', 'm2f2b9', 'm2f2b10', 'm2g1a', 'm2g1b', 'm2g1c', 'm2g1d', 'm2g1x', 'm2g10d', 'm2g12a', 'm2g13a', 'cm2span')]) %>% 
  cor(use="pairwise.complete.obs") 
sort(abs(m[,'y']), decreasing = TRUE)

#Employment EDA - none with correlation>.1
m <- model.matrix(~0+., data=data[,c('y', 'f2f2e1', 'f2f2e2', 'f2f2e3', 'f2f2e4', 'f2f2e5', 'f2f2e6', 'f2f2e7', 'f2f2e8', 'f2f2e9', 'f2f2e10', 'f2k6', 'f2k7c', 'f2k7d', 'f2k7e', 'f2k8', 'f2k9', 'f2k9a', 'f2k9b', 'f2k10a', 'f2k11', 'f2k12', 'f2k12a', 'f2k13', 'f2k14a', 'f2k15bc', 'f2k16a', 'f2k16b', 'f2k16c', 'f2k16d', 'f2k17', 'f2k18a', 'f2k18b', 'f2k18c', 'f2k19', 'f2k22a', 'f2k22a1', 'f2k23a', 'f2k23a1', 'f2k24a', 'f2k24d', 'f2k24e1', 'f2k25a', 'f2k25a1', 'm2c33', 'm2c35', 'm2f2e1', 'm2f2e2', 'm2f2e3', 'm2f2e4', 'm2f2e5', 'm2f2e6', 'm2f2e7', 'm2f2e8', 'm2f2e9', 'm2f2e10', 'm2k4', 'm2k5', 'm2k6', 'm2k6a', 'm2k6b', 'm2k7a', 'm2k8', 'm2k8a', 'm2k8b', 'm2k8c', 'm2k9a', 'm2k10bc', 'm2k11a', 'm2k11b', 'm2k11c', 'm2k11d', 'm2k12', 'm2k13a', 'm2k13b', 'm2k13c', 'm2k14', 'm2k17a', 'm2k17a1', 'm2k18a', 'm2k18a1', 'm2k19a', 'm2k19d', 'm2k20a', 'm2k20a1')]) %>% 
  cor(use="pairwise.complete.obs") 
sort(abs(m[,'y']), decreasing = TRUE)

#Attitudes and expectations EDA - none with correlation>.1
model_matrix_ae <- model.matrix(~0+., data=data[,c('y', 'f2g6a', 'f2g6b', 'm2c5a', 'm2g4a', 'm2g4b')]) %>% 
  cor(use="pairwise.complete.obs") 
sort(abs(m[,'y']), decreasing = TRUE)

#Childcare, Cognitive and behavioral development, Education and school EDA - none with correlation>.1
data_child <- select(data, c(f2b16a, f2b16b, f2b16c, f2b16d, f2b16e, f2b16f, f2b19, f2b19a1, f2b21a1, f2b21a2, f2b21a3, f2b21a4, f2b21a5, f2b21a6, f2b21a7, f2b21a8, f2b21a9, f2b21a10, f2b21a11, f2b21a12, f2b21a13, f2b21b, f2b22, f2b24, f2b26a, f2b26b, f2b26c, f2b26d, f2b26e, f2b26f, f2b26g, f2b27cp, f2b27dp, f2b27ep, f2b27fp, f2b27gp, f2b29, f2b29b, f2b29c, f2b30b, f2b31, f2b33b, f2b34, f2b34a, f2b37a, f2b37b, f2b37c, f2b37d, f2b37e, f2b37f, f2g2, f2g2c, f2g3, f2g3c, f2j21, f2j22, f2j23, f2j24, f2j25, f2j26, f2k1a, f2k2, f2k3a, f2k3b, f2k3c, f2k3d, f2k3e, f2k3f, f2k3g, f2k3h, f2k3i, f2k3j, f2k3k, f2k3l, f2k3m, f2k3n, f2k3o, f2k5, f2k5a1, f2k5a1c, f2k5a2, f2k5a3, f2k5a4, f2k5a5, f2k5a6, f2k5a7, f2k5a8, f2k5a9, f2k5a10, f2k5a11, f2k5a12, f2k5a13, f2k5a14, f2k5a15, f2k5a16, f2k7a, f2k7b1, f2k7b2, f2k7b3, f2k7b4, f2k7b5, f2k7b6, f2k7b7, f2k7b8, f2k7b9, f2k7b10, f2k7b11, f2k7b12, f2k7b13, f2k7b14, f2k7b15, cf2edu, m2b17a, m2b17b, m2b17c, m2b17d, m2b17e, m2b17f, m2b21, m2b22, m2b23, m2b24a2, m2b25a1, m2b25a2, m2b25a3, m2b25a4, m2b25a5, m2b25a6, m2b25a7, m2b25a8, m2b25a9, m2b25a10, m2b25a11, m2b25a12, m2b25a13, m2b25b, m2b26, m2b27, m2b28, m2b28b, m2b30a, m2b30b, m2b30c, m2b30d, m2b30e, m2b30f, m2b30g, m2b31cp, m2b31dp, m2b31ep, m2b31fp, m2b31gp, m2b33, m2b33a, m2b34, m2b34a, m2b35b, m2b36, m2b39, m2b40, m2b40a, m2b43a, m2b43b, m2b43c, m2b43d, m2b43e, m2b43f, m2d3a, m2d3b1, m2d3b2, m2d3b3, m2d3b4, m2d3b5, m2d3b6, m2d3b7, m2g2, m2g2c1, m2g3, m2g3c1, m2g6c, m2h8c, m2h8d, m2k1, m2k2a, m2k2b, m2k2c, m2k2d, m2k2e, m2k2f, m2k2g, m2k2h, m2k2i, m2k2j, m2k2k, m2k2l, m2k2m, m2k2n, m2k2o, m2k3, m2k3a1, m2k3a1c, m2k3a2, m2k3a3, m2k3a4, m2k3a5, m2k3a6, m2k3a7, m2k3a8, m2k3a9, m2k3a10, m2k3a11, m2k3a12, m2k3a13, m2k3a14, m2k3a15, m2k3a16, cm2edu, y))
m<-model.matrix(~0+., data=data_child) %>% 
  cor(use="pairwise.complete.obs") 
sort(abs(m[,'y']), decreasing = TRUE)

#paradata and weights EDA - cm2natsm (flag indicating whether in national sample) best variable
m<-model.matrix(~0+., data=data[,c('y','cf2intmon', 'cf2twoc', 'cf2new12', 'cf2samp', 'cf2natsm', 'cf2natsmx', 'cf2citsm', 'f2g4', 'f2k1', 'f2k4', 'cf2hhimp', 'cf2hhimpb', 'cf2tele', 'cf2span', 'cq2natsm', 'cq2natsmx', 'cq2citsm', 'cm2intmon', 'cm2twoc', 'cm2tdiff', 'cm2fdiff', 'cm2samp', 'cm2natsm', 'cm2natsmx', 'cm2citsm', 'cm2b_age', 'cm2stflg', 'cm2hhimp', 'cm2tele', 'cm2span')]) %>% 
  cor(use="pairwise.complete.obs") 
sort(abs(m[,'y']), decreasing = TRUE)

#parenting variables EDA - none with correlation>.1
m<-model.matrix(~0+., data=data[,c('y','f2a3b', 'f2a3c', 'f2b12', 'f2b14a5', 'f2b17a', 'f2b17b', 'f2b17c', 'f2b17d', 'f2b17e', 'f2b17f', 'f2b17g', 'f2b17h', 'f2b17i', 'f2b17i1', 'f2b18a', 'f2b18b', 'f2b18c', 'f2b18d', 'f2b20ax', 'f2b20bx', 'f2b20cx', 'f2b33', 'f2b33a', 'f2b34a', 'f2b35', 'f2b36a', 'f2b36b', 'f2b36c', 'f2b36d', 'f2b36e', 'f2b36f', 'f2b36g', 'f2b36h', 'f2b36i', 'f2b36i1', 'f2b38a', 'f2b38b', 'f2b38c', 'f2b38d', 'f2c2', 'f2c2b1', 'f2c2e', 'f2c2g', 'f2c3a', 'f2c3b', 'f2c3c', 'f2c3d', 'f2c3e', 'f2c3f', 'f2c3g', 'f2c3h', 'f2c3i', 'f2c3j', 'f2c3k', 'f2c3k1', 'f2c4', 'f2c13d', 'f2d1a', 'f2d2a', 'f2d2b', 'f2d2c', 'f2d2d', 'f2d2e', 'f2e3a', 'f2e3b', 'f2e3c', 'f2e3d', 'f2e3e', 'f2e3f', 'f2e3g', 'f2e3h', 'f2e3i', 'f2e3j', 'f2e4', 'f2e4a', 'f2k12', 'f2k12a', 'f2k18b', 'f2k18c', 'f2l8a', 'm2b12', 'm2b15a5', 'm2b18a', 'm2b18b', 'm2b18c', 'm2b18d', 'm2b18e', 'm2b18f', 'm2b18g', 'm2b18h', 'm2b19', 'm2b19a', 'm2b20a', 'm2b20b', 'm2b20c', 'm2b20d', 'm2b20ax', 'm2b20bx', 'm2b20cx', 'm2b38', 'm2b38a', 'm2b40a', 'm2b41', 'm2b42a', 'm2b42b', 'm2b42c', 'm2b42d', 'm2b42e', 'm2b42f', 'm2b42g', 'm2b42h', 'm2b42i', 'm2b42i1', 'm2b44a', 'm2b44b', 'm2b44c', 'm2b44d', 'm2c2', 'm2c2b1', 'm2c2e', 'm2c2g', 'm2c3a', 'm2c3b', 'm2c3c', 'm2c3d', 'm2c3e', 'm2c3f', 'm2c3g', 'm2c3h', 'm2c3i', 'm2c3j', 'm2c2cx', 'm2c4', 'm2c4a', 'm2c5', 'm2c5a', 'm2c5b', 'm2c3ax', 'm2c6a', 'm2c6b', 'm2c6c', 'm2c6d', 'm2c18', 'cm2finst', 'm2d2', 'm2d2a', 'm2d2b', 'm2d2c', 'm2d2d', 'm2d2e', 'm2d2f', 'm2d3', 'm2d4a', 'm2e2cx', 'm2e4a', 'm2e4b', 'm2e4c', 'm2e4d', 'm2e4e', 'm2e4f', 'm2e4g', 'm2e4h', 'm2e4i', 'm2e4j', 'm2e5', 'm2e5a', 'm2e6a', 'm2e7a', 'm2e7b', 'm2e7c', 'm2e7d', 'm2e7e', 'm2e7f', 'm2e7g', 'm2k8a', 'm2k8b', 'm2k13b', 'm2k13c', 'm2l9a')]) %>% 
  cor(use="pairwise.complete.obs") 
sort(abs(m[,'y']), decreasing = TRUE)

#romantic relationships EDA - none with correlation>.1
m<-model.matrix(~0+., data=data[,c('y','f2a5', 'f2a5a', 'f2a6', 'f2a6a', 'f2a6a1', 'f2a6a2', 'f2a7a', 'f2a7a1a', 'f2a7b', 'f2a7b1a', 'f2a7c', 'f2a7c1a', 'f2a7d', 'f2a7d1', 'f2a7e', 'f2a7e1a', 'f2a8a', 'f2a8b', 'f2a8e', 'f2a8f', 'f2a8g', 'f2a8h', 'f2a9', 'f2a10', 'cf2marm', 'cf2cohm', 'f2b1a', 'f2c5', 'f2c8', 'f2c11a', 'f2c13c3', 'f2c14', 'f2c17', 'f2d1', 'f2d1x', 'f2d2c', 'f2d2d', 'f2d2e', 'f2d3', 'f2d3a', 'f2d4', 'f2d5a', 'f2d5b', 'f2d5c', 'f2d5d', 'f2d5e', 'f2d5f', 'f2d5g', 'f2d5h', 'f2d5i', 'f2d6', 'f2d7a', 'f2d7b', 'f2d7c', 'f2d7d', 'f2d7e', 'f2d7f', 'f2d7g', 'f2d7h', 'f2d7i', 'f2e1', 'f2e2', 'f2e2a2', 'f2e2b', 'f2e2c', 'cf2marp', 'cf2cohp', 'f2f0', 'f2h16d', 'f2l3', 'f2l6d', 'f2l7', 'f2l8', 'f2l8b', 'm2a6', 'm2a6b', 'm2a6c', 'm2a7', 'm2a7a', 'm2a7a1', 'm2a7a2', 'm2a8a', 'm2a8a1a', 'm2a8b', 'm2a8b1a', 'm2a8c', 'm2a8c1a', 'm2a8d', 'm2a8d1', 'm2a8e', 'm2a8e1a', 'm2a9a', 'm2a9b', 'm2a9e', 'm2a9f', 'm2a9g', 'm2a9h', 'm2a10', 'm2a11', 'cm2relf', 'cm2marf', 'cm2cohf', 'm2c1', 'm2c1b', 'm2c6a', 'm2c6b', 'm2c6c', 'm2c6d', 'm2c7', 'm2c9', 'm2c13', 'm2c17', 'm2c19', 'm2c22', 'm2c30', 'm2c30a', 'm2c31', 'm2d1', 'm2d1x', 'm2d2a', 'm2d2b', 'm2d2c', 'm2d2d', 'm2d2e', 'm2d2f', 'm2d3', 'm2d4', 'm2d4a', 'm2d5', 'm2d6a', 'm2d6b', 'm2d6c', 'm2d6d', 'm2d6e', 'm2d6f', 'm2d6g', 'm2d6h', 'm2d6i', 'm2d6j', 'm2d6k', 'm2d6l', 'm2d7', 'm2d8a', 'm2d8b', 'm2d8c', 'm2d8d', 'm2d8e', 'm2d8f', 'm2d8g', 'm2d8h', 'm2d8i', 'm2d8j', 'm2d8k', 'm2d8l', 'm2d9', 'm2d9a1', 'm2d9a2', 'm2d9a3', 'm2d9b', 'm2d9c', 'm2e1', 'm2e2', 'm2e2a2', 'm2e2b', 'm2e2c', 'm2e6a', 'm2e6b', 'm2e6c', 'm2e6d', 'm2e7a', 'm2e7b', 'm2e7c', 'm2e7d', 'm2e7e', 'm2e7f', 'm2e7g', 'm2e8a', 'm2e8b', 'm2e8c', 'm2e8d', 'm2e8e', 'm2e8f', 'm2e8g', 'm2e8h', 'm2e8i', 'm2e8j', 'm2e8k', 'm2e8l', 'm2e9', 'm2e9a1', 'm2e9a2', 'm2e9a3', 'm2e9b', 'm2e9c', 'cm2marp', 'cm2cohp', 'm2l3', 'm2l7', 'm2l8', 'm2l9', 'm2l9a', 'm2l10')]) %>% 
  cor(use="pairwise.complete.obs") 
sort(abs(m[,'y']), decreasing = TRUE)

#housing and neighborhood EDA - cm2gdad
housing_vars <- c('y', 'f2a3', 'f2a3b', 'f2a4a', 'f2a4c', 'f2b1', 'f2b14a1', 'f2b14a2', 'f2b14a3', 'f2b14a4', 'f2b14a5', 'f2b14a6', 'f2b14a7', 'f2b14a8', 'f2b15', 'f2c1b', 'f2c7', 'f2e2d', 'f2f2b1', 'f2f2b2', 'f2f2b3', 'f2f2b4', 'f2f2b5', 'f2f2b6', 'f2f2b7', 'f2f2b8', 'f2f2b9', 'f2f2b10', 'f2f2d1', 'f2f2d2', 'f2f2d3', 'f2f2d4', 'f2f2d5', 'f2f2d6', 'f2f2d7', 'f2f2d8', 'f2f2d9', 'f2f2d10', 'f2f2e1', 'f2f2e2', 'f2f2e3', 'f2f2e4', 'f2f2e5', 'f2f2e6', 'f2f2e7', 'f2f2e8', 'f2f2e9', 'f2f2e10', 'cf2gdad', 'cf2gmom', 'f2h1', 'f2h2', 'f2h3c', 'f2h5', 'f2h5a', 'f2h17e', 'f2h17g', 'f2h17h', 'f2h17j', 'f2h17k', 'm2a3', 'm2a4a', 'm2a4c', 'm2b1', 'm2b15a1', 'm2b15a2', 'm2b15a3', 'm2b15a4', 'm2b15a5', 'm2b15a6', 'm2b15a7', 'm2b15a8', 'm2b16', 'm2e3', 'm2f1', 'm2f2b1', 'm2f2b2', 'm2f2b3', 'm2f2b4', 'm2f2b5', 'm2f2b6', 'm2f2b7', 'm2f2b8', 'm2f2b9', 'm2f2b10', 'm2f2d1', 'm2f2d2', 'm2f2d3', 'm2f2d4', 'm2f2d5', 'm2f2d6', 'm2f2d7', 'm2f2d8', 'm2f2d9', 'm2f2d10', 'm2f2e1', 'm2f2e2', 'm2f2e3', 'm2f2e4', 'm2f2e5', 'm2f2e6', 'm2f2e7', 'm2f2e8', 'm2f2e9', 'm2f2e10', 'cm2adult', 'cm2kids', 'cm2gdad', 'cm2gmom', 'm2h1', 'm2h1a', 'm2h2', 'm2h3c', 'm2h5', 'm2h6', 'm2h19e', 'm2h19g', 'm2h19h', 'm2h19j', 'm2h19k')
m<-model.matrix(~0+., data=data[,housing_vars]) %>% 
  cor(use="pairwise.complete.obs") 
sort(abs(m[,'y']), decreasing = TRUE)

# legal system EDA - none with correlation>.1
legal_vars <- c('y', 'f2a8c', 'f2a8d', 'f2c1b', 'f2c6', 'f2c6a', 'f2c6b', 'f2c6c', 'f2c6d', 'f2c7', 'f2h18', 'f2h19', 'f2h20', 'f2h20b1', 'f2h20b2', 'f2h20b3', 'f2h20b4', 'f2h20b5', 'f2h20b6', 'f2h20b7', 'f2h20b8', 'f2h20b9', 'f2h20b10', 'f2h20b11', 'f2h20b12', 'f2h20b13', 'f2h20b14', 'f2h20b15', 'f2h20b16', 'f2h20b17', 'f2h20b18', 'f2h20b19', 'f2h21', 'f2h21d1', 'f2h21d2', 'f2h21d3', 'f2h21d4', 'f2h21d5', 'f2h21d6', 'f2h21d7', 'f2h21d8', 'f2h21d9', 'f2h21d10', 'f2h21d11', 'f2h21d12', 'f2h21d13', 'f2h21d14', 'f2h21d15', 'f2h21d16', 'f2h21d17', 'f2h21d18', 'f2h21d19', 'f2h22', 'f2h23', 'f2h24', 'f2h27', 'cf2finjail', 'cm2ffinjail', 'cf2fevjail', 'cm2ffevjail', 'm2a9c', 'm2a9d', 'm2c8a', 'm2c8b', 'm2c8c', 'm2c8d', 'm2c8e', 'm2c33', 'm2c33b', 'm2c36', 'm2c36a1', 'm2c36a2', 'm2c36a3', 'm2c36a4', 'm2c36a5', 'm2c36a6', 'm2c36a7', 'm2c36a8', 'm2c36a9', 'm2c36a10', 'm2c36a11', 'm2c36a12', 'm2c36a13', 'm2c36a14', 'm2c36a15', 'm2c36a16', 'm2c36a17', 'm2c36a18', 'm2c36a19', 'cm2finjail', 'cm2fevjail', 'm2e10')
m<-model.matrix(~0+., data=data[,legal_vars]) %>% 
  cor(use="pairwise.complete.obs") 
sort(abs(m[,'y']), decreasing = TRUE)

#health and health behavior EDA - none with correlation>.1
health <- c('y','f2a8f', 'f2b2', 'f2b3', 'f2b4a', 'f2b4b', 'f2b4c', 'f2b4d', 'f2b4e', 'f2b4f', 'f2b4g', 'f2b4h', 'f2b5a1', 'f2b6', 'f2b9', 'f2b10a', 'f2b11', 'f2b11a', 'f2b11b', 'f2b12', 'f2b13', 'f2b14a7', 'f2b32', 'f2b33', 'f2b33a', 'f2c10', 'f2c20', 'f2h17l', 'f2j1', 'f2j2', 'f2j3', 'f2j3a', 'f2j4', 'f2j4a', 'f2j4b1', 'f2j4b2', 'f2j4b3', 'f2j4b4', 'f2j4b5', 'f2j4b6', 'f2j5', 'f2j5a', 'f2j6', 'f2j7', 'f2j7a', 'f2j8', 'f2j8a', 'f2j9', 'f2j10', 'f2j11', 'f2j12', 'f2j13', 'f2j13a', 'f2j13b', 'f2j13c1', 'f2j13c2', 'f2j13c3', 'f2j14', 'f2j14a', 'f2j14b', 'f2j15a', 'f2j15b', 'f2j15c', 'f2j15cx', 'f2j15c1', 'f2j15d', 'f2j15e', 'f2j15f', 'f2j16', 'f2j16a', 'f2j16b', 'f2j17', 'f2j18a', 'f2j18b', 'f2j18c', 'f2j18d', 'f2j18e', 'f2j19', 'f2j19a', 'f2j20a', 'f2j20b', 'f2j20c', 'f2j20d', 'f2j20e', 'f2j20f', 'f2j20g', 'cf2gad_case', 'cf2md_case_con', 'cf2md_case_lib', 'm2a9f', 'm2a12', 'm2b2', 'm2b3', 'm2b4a', 'm2b4b', 'm2b4c', 'm2b4d', 'm2b4e', 'm2b4f', 'm2b4g', 'm2b4h', 'm2b5a', 'm2b6', 'm2b9', 'm2b10a', 'm2b11', 'm2b11a', 'm2b11b', 'm2b12', 'm2b14', 'm2b15a7', 'm2b37', 'm2b38', 'm2b38a', 'm2c11', 'm2c24', 'm2c24a', 'm2c24b', 'm2c24c', 'm2c26', 'm2c27', 'm2c34', 'm2c35', 'm2d9b', 'm2e2a1', 'm2e9b', 'm2f3', 'm2f4', 'm2f4a', 'm2f5', 'm2f5a', 'm2f6', 'cm2biok', 'm2h19l', 'm2j1', 'm2j2', 'm2j3', 'm2j3a', 'm2j4', 'm2j4a', 'm2j4b1', 'm2j4b2', 'm2j4b3', 'm2j4b4', 'm2j4b5', 'm2j4b6', 'm2j5', 'm2j5a', 'm2j6', 'm2j7', 'm2j7a', 'm2j8', 'm2j8a', 'm2j9', 'm2j10', 'm2j11', 'm2j12', 'm2j13', 'm2j13a', 'm2j13b', 'm2j13c1', 'm2j13c2', 'm2j13c3', 'm2j14', 'm2j14a', 'm2j14b', 'm2j15a', 'm2j15b', 'm2j15c', 'm2j15cx', 'm2j15c1', 'm2j15d', 'm2j15e', 'm2j15f', 'm2j16', 'm2j16a', 'm2j16b', 'm2j17', 'm2j18a', 'm2j18b', 'm2j18c', 'm2j18d', 'm2j18e', 'm2j19', 'm2j19a', 'm2j20a', 'm2j20b', 'm2j20c', 'm2j20d', 'm2j20e', 'm2j20f', 'm2j20g', 'cm2gad_case', 'cm2md_case_con', 'cm2md_case_lib')
m<-model.matrix(~0+., data=data[,health]) %>% 
  cor(use="pairwise.complete.obs") 
sort(abs(m[,'y']), decreasing = TRUE)

#finance EDA - cm2povca        m2h12       m2h9a3
fin <- c('y','f2b21a12', 'f2b25', 'f2b26a', 'f2b26b', 'f2b26c', 'f2b26d', 'f2b26e', 'f2b26f', 'f2b26g', 'f2b27ap', 'f2b27bp', 'f2b27cp', 'f2b27dp', 'f2b27ep', 'f2b27fp', 'f2b27gp', 'f2b28p', 'f2b29', 'f2b29b', 'f2b29c', 'f2c9', 'f2c9a1', 'f2c10a', 'f2c11p', 'f2c12', 'f2c13', 'f2c13b', 'f2c13c', 'f2c13c2', 'f2c13d', 'f2c14x', 'f2c14a', 'f2c15a', 'f2c15fb1', 'f2c15p', 'f2c16', 'f2c16a1', 'f2c18a', 'f2c18b', 'f2c18c', 'f2c18d', 'f2c18e', 'f2c18f', 'f2c19', 'f2c19a', 'f2c19bp', 'f2c19c1', 'f2c19c2', 'f2c19c3', 'f2c19c4', 'f2c19c5', 'f2c19c6', 'f2c20c', 'f2c21', 'f2c21a', 'f2c21c1', 'f2g7', 'f2g7a1', 'f2g7a2', 'f2g7a3', 'f2g7a4', 'f2g7a5', 'f2g7a6', 'f2g7a7', 'f2g7a8', 'f2g7c', 'f2h5', 'f2h6', 'f2h7a', 'f2h7b', 'f2h7c', 'f2h8a1', 'f2h8a2', 'f2h8a3', 'f2h8b1', 'f2h8b2', 'f2h8b3', 'f2h9', 'f2h9a', 'f2h9b2', 'f2h9c', 'f2h9d1', 'f2h10', 'f2h10a', 'f2h10b', 'f2h11', 'f2h12', 'f2h12a1', 'f2h12a2', 'f2h12a3', 'f2h12a4', 'f2h12a5', 'f2h12a6', 'f2h13', 'f2h14', 'f2h14a', 'f2h14ax', 'f2h14b', 'f2h15', 'f2h16', 'f2h16a', 'f2h16ax', 'f2h16b', 'f2h16c', 'f2h16c1', 'f2h17a', 'f2h17b', 'f2h17c', 'f2h17d', 'f2h17e', 'f2h17f', 'f2h17g', 'f2h17h', 'f2h17i', 'f2h17j', 'f2h17k', 'f2h17l', 'f2h20b17', 'f2h21d17', 'f2j3', 'f2k3h', 'f2k5a8', 'f2k7b8', 'f2k7fp', 'f2k15ap', 'f2k20a', 'f2k22d', 'f2k22e1', 'f2k22f', 'f2k22f1a', 'f2k22f1b', 'f2k22f1c', 'f2k22f1d', 'f2k23d', 'f2k23e1', 'f2k23f', 'f2k23f1a', 'f2k23f1b', 'f2k23f1c', 'f2k23f1d', 'f2k24f', 'f2k24f1a', 'f2k24f1b', 'f2k24f1c', 'f2k24f1d', 'f2k25d', 'f2k25e1', 'f2k25f', 'f2k25f1a', 'f2k25f1b', 'f2k25f1c', 'f2k25f1d', 'f2l1a', 'f2l2', 'f2l4', 'f2l4a', 'f2l5', 'f2l5a', 'f2l6', 'f2l6a', 'f2l7', 'f2l8', 'f2l9', 'f2l10', 'f2l11', 'f2l11a', 'cf2hhimp', 'cf2hhimpb', 'cf2povca', 'cf2povcab', 'm2b25a12', 'm2b29', 'm2b30a', 'm2b30b', 'm2b30c', 'm2b30d', 'm2b30e', 'm2b30f', 'm2b30g', 'm2b31ap', 'm2b31bp', 'm2b31cp', 'm2b31dp', 'm2b31ep', 'm2b31fp', 'm2b31gp', 'm2b32p', 'm2b33', 'm2b33a', 'm2b34', 'm2b34a', 'm2c10', 'm2c10a1', 'm2c11a', 'm2c12p', 'm2c14', 'm2c15', 'm2c15a1', 'm2c16', 'm2c16a1', 'm2c18', 'm2c19a', 'm2c20a', 'm2c20b1', 'm2c20p', 'm2c21', 'm2c21a1', 'm2c23a', 'm2c23b', 'm2c23c', 'm2c23d', 'm2c23e', 'm2c23f', 'm2c25', 'm2c25a1', 'm2c27a', 'm2c27b', 'm2c27cp', 'm2c28', 'm2c29', 'm2c29b', 'm2c36a17', 'm2c38a', 'm2g5', 'm2g5a1', 'm2g5a2', 'm2g5a3', 'm2g5a4', 'm2g5a5', 'm2g5a6', 'm2g5a7', 'm2g5a8', 'm2g5c', 'm2h5', 'm2h7', 'm2h8a', 'm2h8b', 'm2h8c', 'm2h8d', 'm2h8e', 'm2h8f', 'm2h8g', 'm2h9a1', 'm2h9a2', 'm2h9a3', 'm2h9b1', 'm2h9b2', 'm2h9b3', 'm2h10a', 'm2h10b', 'm2h10c2', 'm2h11', 'm2h11a1', 'm2h12', 'm2h12a', 'm2h12b', 'm2h13', 'm2h14', 'm2h14a1', 'm2h14a2', 'm2h14a3', 'm2h14a4', 'm2h14a5', 'm2h14a6', 'm2h14ax', 'm2h15', 'm2h16', 'm2h16a', 'm2h16ax', 'm2h16b', 'm2h17', 'm2h18', 'm2h18a', 'm2h18b', 'm2h18c', 'm2h18c1', 'm2h19a', 'm2h19b', 'm2h19c', 'm2h19d', 'm2h19e', 'm2h19f', 'm2h19g', 'm2h19h', 'm2h19i', 'm2h19j', 'm2h19k', 'm2h19l', 'm2j3', 'm2j3a', 'm2k2h', 'm2k3a8', 'm2k10ap', 'm2k15a', 'm2k17d', 'm2k17e1', 'm2k17f', 'm2k17f1a', 'm2k17f1b', 'm2k17f1c', 'm2k17f1d', 'm2k18d', 'm2k18e1', 'm2k18f', 'm2k18f1a', 'm2k18f1b', 'm2k18f1c', 'm2k18f1d', 'm2k19e1', 'm2k19f', 'm2k19f1a', 'm2k19f1b', 'm2k19f1c', 'm2k19f1d', 'm2k20d', 'm2k20e1', 'm2k20f', 'm2k20f1a', 'm2k20f1b', 'm2k20f1c', 'm2k20f1d', 'm2l1a', 'm2l2', 'm2l4', 'm2l4a', 'm2l5', 'm2l5a', 'm2l6', 'm2l6a', 'm2l8', 'm2l11', 'm2l12', 'm2l13', 'm2l13a', 'cm2hhimp', 'cm2povca')
missing_vars <- setdiff(fin, names(data))
print(missing_vars) #find variable not in df that we got from the metadata so we can exclude it from the fin vector
m<-model.matrix(~0+., data=data[,fin]) %>% 
  cor(use="pairwise.complete.obs") 
sort(abs(m[,'y']), decreasing = TRUE)

#family EDA - cm2gdad
fam <- c('y','cf2gdad', 'cf2gmom', 'f2g1a', 'f2g1b', 'f2g1c', 'f2g1x', 'f2g2', 'f2g2c', 'f2g3', 'f2g3c', 'f2g6c', 'f2g7a1', 'f2g7a3', 'f2g8a', 'f2g8a1', 'f2g8b', 'f2g8c', 'f2g8d', 'f2g8d1', 'f2g9', 'f2g9a', 'f2g10', 'f2g10a', 'f2g11a', 'f2g11b', 'f2g12a', 'f2g12b', 'f2g12c', 'f2g12d', 'f2g13', 'f2g14', 'f2g14a', 'f2g15', 'f2g15a', 'f2g16', 'm2d3a', 'm2d3b1', 'm2d3b2', 'm2d3b3', 'm2d3b4', 'm2d3b5', 'm2d3b6', 'm2d3b7', 'm2d9c', 'm2e9c', 'cm2gdad', 'cm2gmom', 'm2g1a', 'm2g1b', 'm2g1c', 'm2g1x', 'm2g2', 'm2g2c1', 'm2g3', 'm2g3c1', 'm2g4c', 'm2g5a1', 'm2g5a3', 'm2g6a', 'm2g6a1', 'm2g6b', 'm2g6c', 'm2g6d', 'm2g6d1', 'm2g7', 'm2g7a', 'm2g8', 'm2g8a', 'm2g9a', 'm2g9b', 'm2g10a', 'm2g10b', 'm2g10c', 'm2g10d', 'm2g11', 'm2g12', 'm2g12a', 'm2g13', 'm2g13a', 'm2g14')
m<-model.matrix(~0+., data=data[,fam]) %>% 
  cor(use="pairwise.complete.obs") 
sort(abs(m[,'y']), decreasing = TRUE)

#Explore/clean top variables
#features in target
table(data$m2h9c1) 
table(data$cm2povco) 
table(data$cm2hhinc) 
table(data$cm2povca) #cm2povco as categorical variable

#other features
table(data$cm2natsm) #(flag indicating whether in national sample) 
table(data$m2l1) #total household income
data$m2l1 <- ifelse(data$m2l1 >= 0, data$m2l1, NA)
table(data$m2c38) #How much money does father normally earn per year?
data$m2c38 <- ifelse(data$m2c38 >= 0, data$m2c38, NA)
table(data$m2b32) #How much out-of-pocket for all child care you currently use?
data$m2b32 <- ifelse(data$m2b32 >= 0, data$m2b32, NA)
table(data$m2h12) #Do you or child receive SSI?
data$m2h12 <- ifelse(data$m2h12 >= 0, data$m2h12, NA)
table(data$cm2gdad) #Baby's grandfather in the Household
data$cm2gdad <- ifelse(data$cm2gdad >= 0, data$cm2gdad, NA)
table(data$m2h9a3) #In past year, have you received other help?(unemployment/wrkms comp/etc)
data$m2h9a3 <- ifelse(data$m2h9a3 >= 0, data$m2h9a3, NA)

subset_data <- data[, c( 'cm2natsm',
  'm2l1', 'm2c38', 'm2b32', 'm2h12', 
  'cm2gdad', 'm2h9a3','y'
)]
dim(subset_data)

#Check for multicollinearity - none
num_subset <-as.data.frame(lapply(subset_data, as.numeric))
vifstep(num_subset,th=5)

#Ensure categorical variables are factors
subset_data$cm2natsm<-as.factor(subset_data$cm2natsm)
#subset_data$cm2povca<-as.factor(subset_data$cm2povca)
subset_data$m2h12<-as.factor(subset_data$m2h12)
subset_data$cm2gdad<-as.factor(subset_data$cm2gdad)
subset_data$m2h9a3<-as.factor(subset_data$m2h9a3)

#visualize
boxplot(subset_data$y~subset_data$cm2natsm,main='Relative TANF vs. Mother in 1-Year National Sample',ylab='relative TANF', xlab='Is mother in 1-year national sample?',names=c("No","Yes"))
boxplot(subset_data$y~subset_data$m2h12,main='Relative TANF vs. Supplementary Security Income (SSI) Receival',ylab='relative TANF', xlab='Did mother or child receive SSI?',names=c("No","Yes"))
boxplot(subset_data$y~subset_data$cm2gdad,main='Relative TANF vs. Grandfather\'s Presence in Household',ylab='relative TANF', xlab='Is baby\'s grandfather in household?',names=c("No","Yes"))
boxplot(subset_data$y~subset_data$m2h9a3,main='Relative TANF vs. Other Social Welfare Receival',ylab='relative TANF', xlab='Did you receive other help?',names=c('Other','No','Unemployment','Workers comp.','Disability','Medical payment'))

options(scipen=10)
plot(subset_data$y~subset_data$m2l1,main='Relative TANF vs. Total Household Income',ylab='relative TANF', xlab='household income ($)')
plot(subset_data$y~subset_data$m2c38,main='Relative TANF vs. Father\'s Income',ylab='relative TANF', xlab='father\'s income ($)')
plot(subset_data$y~subset_data$m2b32,main='Relative TANF vs. Out-of-Pocket Childcare Expense',ylab='relative TANF', xlab='out-of-pocket childcare expense ($)')

#split
set.seed(1)
num_rows <- nrow(subset_data)
train_indices <- sample(1:num_rows, 0.7 * num_rows) 
train <- subset_data[train_indices, ]
test <- subset_data[-train_indices, ]

par(family = "Times New Roman") 

#glm
mod <- glm(y~.,data=train, family='gaussian')
summary(mod)
par(family = "Times New Roman") 
plot(mod$fitted.values, mod$residuals,xlab='Fitted values',ylab='Residuals',main='Residuals vs. Fitted Values of Relative TANF Generalized Linear Model') #non-constant variance

#tree
tree <- tree(y~., data=train)
summary(tree)

#plot the tree
plot(tree);text(tree, pretty=0)

#cross validation of tree to see where to prune the tree
set.seed(1)
cv_tree <- cv.tree(tree)
plot(cv_tree$size, cv_tree$dev, type='b', main = "Cross-Validated Error Rates", xlab = "Tree Size", ylab = "Cross-Validated Error")

#pruned to 2
prune_data <- prune.tree(tree, best=2)
plot(prune_data, main = "Pruned Decision Tree");text(prune_data, pretty=0)

#used pruned tree to make prediction on the test set
yhat <- predict(prune_data, newdata=test)

#plotting the true versus predicted values
plot(yhat, test$y, main = "Tree Model True vs. Predicted", xlab = "Predicted", ylab = "True"); abline(0,1)

#calculating mean squared error
mean((yhat-test$y)^2)
1 - (sum((test$y-yhat)^2)/sum((test$y-mean(test$y))^2)) #R2
mae(test$y,yhat)

#Random Forest
#removing missing values 
train_clean <- na.omit(train)
test_clean <- na.omit(test)
mtry_values <- c(1:7)

# Create a train control object for cross-validation
set.seed(1)
ctrl <- trainControl(method = "cv",
                     number = 10,
                     search = "grid",
                     summaryFunction = defaultSummary)
rf_mod <- train(y ~ ., data=train_clean, method="rf", trControl=ctrl, tuneGrid=data.frame(mtry = mtry_values), metrics='RMSE')
print(rf_mod)

rf_mod_final <- randomForest(y ~ ., data=train_clean, mtry=2, importance=TRUE)
print(rf_mod_final)

#performance on test set
test_rf_mod <- predict(rf_mod_final, newdata=test_clean)
plot(test_rf_mod, test_clean$y, main = "Random Forest True vs. Predicted", xlab = "Predicted", ylab = "True");abline(0,1)
mean((test_rf_mod - test_clean$y)^2) #MSE
1 - (sum((test_clean$y-test_rf_mod)^2)/sum((test_clean$y-mean(test_clean$y))^2)) #R2
mae(test_clean$y,test_rf_mod) #MAE

#gives the importance of each variable
imp<-varImpPlot(rf_mod_final, main = "Random Forest Variable Importance Plot")
imp_with_title <- imp + ggtitle("Random Forest Variable Importance Plot")

imp <- as.data.frame(imp)
rownames(imp) <- c('In national sample','Total household income','Father\'s income','Out-of-pocket childcare expense','Receives Supplementary Security Income','Baby\'s grandfather in home','Other help')
imp$varnames <- rownames(imp)
rownames(imp) <- NULL  
imp$mse <- imp$`%IncMSE`



ggplot(imp, aes(y=reorder(varnames, mse), weight=mse,fill='pink')) + 
  geom_bar() +
  ylab("Permuted Feature") +
  xlab("Average % Increase in MSE")+
  ggtitle("Average Percent Increase in MSE for each Permuted Feature in Random Forest Regressor")+
  theme(plot.title = element_text(hjust = 0.5))+ 
  guides(fill=FALSE)+
  theme(text = element_text(family = "Times New Roman"),    
        panel.background = element_rect(fill ="white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "grey"))
  