setwd("C:/Users/walte/OneDrive/Documentos/Maestría en Economía Aplicada/Big Data")
db <-readRDS("leverage_dta.Rds")
head(db)
tail(db)
#install.packages("tidyverse")

require("tidyverse")

ggplot(db) +
geom_point(aes(x=x,y=y))


reg1<-lm(y~x,data=db)
summary(reg1)

install.packages("stargazer")
require("stargazer") #muestra las regresiones en formato text, latex y html
stargazer(reg1,type="text")

db<- db %>% mutate(ej=c(rep(0,30),1))
head(db)

reg2<-lm(y~x+ej,db)

stargazer(reg1,reg2,type="text")

#Hago la regresión con los residuales para probar el teorema el FWL
db<-db %>% mutate(res_y_e=lm(y~ej,db)$residuals,
                  res_x_e=lm(x~ej,db)$residuals,
)
reg3<-lm(res_y_e~res_x_e,db)
stargazer(reg1,reg2,reg3,type="text")

db<-db %>% mutate(res_y_x=lm(y~x,db)$residuals,
                  res_e_x=lm(ej~x,db)$residuals,
)
reg4<-lm(res_y_x~res_e_x,db)
stargazer(reg1,reg2,reg3,reg4,type="text")

u<-lm(y~x,data=db)$residual[31]
u

#calcular el h para el alpha que es el peso de la observvación j
h<-lm.influence(reg1)$hat[31]
h

#el alpha es comparativo 
alpha<-u/(1-h)
alpha
