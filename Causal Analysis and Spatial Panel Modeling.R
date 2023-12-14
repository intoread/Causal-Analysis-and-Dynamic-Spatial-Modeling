library(plm)
library(tseries)

panel_dat <- read.csv(file.choose())

#turning into pdata.frame
panel_data<- pdata.frame(panel_dat[,-2], index = c("Provinsi", "Tahun"))

#stationarity testing
purtest(panel_data$PDRB, pmax = 3, exo = "intercept", test = "madwu")
purtest(panel_data$konsumsi, pmax = 3, exo = "intercept", test = "madwu")
purtest(panel_data$Investasi, pmax = 3, exo = "intercept", test = "madwu")
purtest(panel_data$tenaga.kerja, pmax = 3, exo = "intercept", test = "madwu")

#unit-root testing
cipstest(panel_data$PDRB, type = 'trend')

#cross-sectional independence test
pcdtest(PDRB~Investasi+konsumsi+tenaga.kerja, index = c("Provinsi", "Tahun"),test = 'sclm', data = panel_data)
#If data isn't independent then spatial effects are to be added

#pedroni's cointegration test (long-term effects)
library(pco)
mat1 <- matrix(panel_data$PDRB,nrow = 10, byrow = F)
mat2 <- matrix(panel_data$konsumsi,nrow = 10, byrow = F)
mat3 <- matrix(panel_data$Investasi,nrow = 10, byrow = F)
mat4 <- matrix(panel_data$tenaga.kerja,nrow = 10, byrow = F)

cube<-array(c(mat1,mat2,mat3,mat4),dim = c(10,34,4))

pedroni99m(cube)

#  2*(1-pnorm(P-VALUE)) input this for all p-value except first test, switch in first test

#Dumitrescu-Hurlin test for granger causality (short-term effects)
pgrangertest(PDRB~konsumsi, index = c("Provinsi", "Tahun"), data = panel_data)
pgrangertest(PDRB~Investasi, index = c("Provinsi", "Tahun"), data = panel_data)
pgrangertest(PDRB~tenaga.kerja, index = c("Provinsi", "Tahun"), data = panel_data)

#Spatial panel modeling
library(sf)
library(sp)

#Input SHP file
indonesia <-read_sf(file.choose())
shp_data <- st_read(file.choose())

print(str(shp_data))

ID<-c(1:34)
shp_data$ID<-c(1:34)
indonesia_sp$ID<-c(1:34) 

library(spdep)
CoordK <- st_coordinates(shp_data)
plot(shp_data)
summary(shp_data)


#Making spatial object
indonesia_sp <- st_as_sf(indonesia)
indonesia_sp$ID<-c(1:34)

k=5
# Making neighbors list
#Queen contiguity
W <- poly2nb(indonesia_sp, row.names = indonesia_sp$ID, queen = FALSE)
#Rook contiguity
W1 <- poly2nb(indonesia_sp, row.names = indonesia_sp$ID, queen = T)


# Making binary contiguity matrix
WB <- nb2mat(W, style = 'B', zero.policy = TRUE)
WB1 <- nb2mat(W1, style = 'B', zero.policy = TRUE)

#Modeling using SDPD
library(SDPDmod)
data_s_p <- panel_data[,-2]

#Bayesian method to determine the best model
res2<-blmpSDPD(formula = PDRB~Investasi+konsumsi+tenagakerja, data = data_s_p, W = WB1,
               index = c("Provinsi", "Tahun"),
               model = list('ols',"sar","sdm","sem","sdem","slx"),  
               effect = "time",
               dynamic = TRUE,
               prior = "uniform")
res2

#sar is set to be the best model
mod1<-SDPDm(formula = PDRB~Investasi+konsumsi+tenaga.kerja, data = as.data.frame(data_sp), W = WB1,
            index = c("id", "Tahun"),
            model = "sar",
            effect='time',
            dynamic = T,
            tlaginfo = list(ind = NULL, tl = T, stl = T))
summary(mod1)

#Testing assumptions of model
library(lmtest)
#residual stationarity
Box.test(mod1$resuduals,type="Ljung-Box")
#autocorrelation stationarity
Box.test((mod1$resuduals)^2,type="Ljung-Box")

#normality testing
r2=mod1$resuduals
n2=length(r2)
mean2=mean(r2)
sd2=sd(r2)
res2=rnorm(n2,mean2,sd2)
cek.normalitas=ks.test(r2,res2)
cek.normalitas

#long-term and short-term impacts of sar
imp  <- impactsSDPDm(mod1)
summary(imp)