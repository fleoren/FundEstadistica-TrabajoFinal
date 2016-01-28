#library(ggplot2)
#library(dplyr)
#library(tidyr)
#library(gridExtra)
#library(R2jags)
#library(lubridate)
#library(MASS)
#library(hglm)

rm(list = ls())
rutawork = '/Users/fernanda/Dropbox/batmelon/Fundamentos de Estadística/TrabajoProfe'

#	----------------------------------------------------
#		Datos
#	----------------------------------------------------
#	Loading the data...
cdata <- read.csv(paste(rutawork,"/EST4611_AbortoCrimen.csv",sep = ""), header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE)
cdata <- as.data.frame(cdata)
dat<-cdata

################################################################################################

#INCISO A

################################################################################################
#####
####ANALISIS EXPLORATORIO
#analicemos todos los datos como conjunto
#primero la cosa horrible con colores
plot_x <- ggplot(cdata,aes(x = cdata$year+1900, y = cdata$lpc_murd, col = cdata$statenum)) + 
  geom_point() +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(title='Comportamiento de asesinatos para cada estado',
       x='Año',
       y='log(tasa de asesinatos)',
       col='Estado')
plot_x
#otra más bonita
dat%>%
  filter(statenum %in% 1:50) %>%
  ggplot(aes(year+1900, lpc_murd)) +
  geom_line() +
  geom_vline(xintercept=1991, linetype='dashed',colour = "red") +
  facet_wrap(~ statenum, nrow=5)+
  labs(title='Evolución de Asesinatos',
       x='Año',
       y='Tasa de asesinatos')

dat_full <- dat %>%
  filter(!is.na(lpc_murd), !is.na(popul)) %>%
  group_by(year) %>%
  summarise(lpc_murd = log(sum(exp(lpc_murd)*popul)/sum(popul)),
            popul = sum(popul))

#hacemos un promedio ponderado de las variables
plot_y <- ggplot(dat_full, aes(year+1900, lpc_murd)) +
  geom_point() +
  geom_vline(xintercept=1991, linetype='dashed',colour='red') +
  labs(title='Comportamiento de Asesinatos',
       x='Año',
       y='log(tasa de asesinatos)') +
  theme_bw()
plot_y

#REGRESION 1
datos_completos <- dat %>%
  filter(!is.na(lpc_murd), !is.na(popul)) %>%
  group_by(year) %>%
  summarise(lpc_murd = log(sum(exp(lpc_murd)*popul)/sum(popul)),
            popul = sum(popul))

pre_1991<-subset(datos_completos,datos_completos$year<91)
post_1991<-subset(datos_completos,datos_completos$year>=91)

lm_pre_1991 <- lm(lpc_murd ~ year, data=pre_1991)
lm_post_1991 <- lm(lpc_murd ~ year, data=post_1991)

summary(lm_pre_1991)
summary(lm_post_1991)

pre_1991$yhat <- predict(lm_pre_1991, newdata = pre_1991)
post_1991$yhat <- predict(lm_post_1991, newdata = post_1991)
regresion_1 <- rbind(pre_1991,post_1991)

ggplot(regresion_1, aes(year+1900)) +
  geom_point(aes(y=lpc_murd)) +
  geom_line(aes(y=yhat, color=factor(year < 91)))+
  theme_bw() +
  labs(title='Comportamiento Pre-1991 y Post-1991',
       x='Año',
       y='log(tasa de asesinatos)')

#VEAMOS ESTAS REGRESIONES PARA CADA ESTADO
#########################################
#GENEREMOS LAS PREDICCIONES PARA CADA ESTADO PRE Y POST 1991
predicciones<-vector()
predicciones_2000_cambio<-vector()
for(i in 1:50){
pre_1991<-subset(dat[2:6],dat$year<91 & dat$statenum==i)
post_1991<-subset(dat[2:6],dat$year>=91 & dat$statenum==i)
lm_pre_1991 <- lm(lpc_murd ~ year, data=pre_1991)
lm_post_1991 <- lm(lpc_murd ~ year, data=post_1991)
pre_1991$yhat <- predict(lm_pre_1991, newdata = pre_1991)
post_1991$yhat <- predict(lm_post_1991, newdata = post_1991)
#regresion_1 <- rbind(pre_1991,post_1991)
predicciones<-c(predicciones,pre_1991$yhat,post_1991$yhat)
pred_2000_cambio<-lm_post_1991$coefficients[1] + lm_post_1991$coefficients[2]*100
predicciones_2000_cambio<-c(predicciones_2000_cambio,pred_2000_cambio)
}

dat$yhat<-predicciones

dat%>%
  filter(statenum %in% 1:50) %>%
  ggplot(aes(year+1900, lpc_murd)) +
  geom_line() +
  geom_vline(xintercept=1991, linetype='dashed',colour = "red") +
  geom_line(aes(y=yhat, color=factor(year < 91)))+
  facet_wrap(~ statenum, nrow=5)+
  labs(title='Evolución de Asesinatos',
       x='Año',
       y='Tasa de asesinatos')

#########################################
#GENEREMOS LAS PREDICCIONES PARA CADA ESTADO SIN DIFERENCIAR TIEMPO
#QUEREMOS VER PARA EL AÑO 2000
predicciones_sin_dif<-vector()
predicciones_2000<-vector()
for(i in 1:50){
  sin_dif<-subset(dat[2:6],dat$statenum==i)
  lm_sin_dif<- lm(lpc_murd ~ year, data=sin_dif)
  sin_dif$yhat <- predict(lm_sin_dif, newdata = sin_dif)
  pred_2000<-lm_sin_dif$coefficients[1] + lm_sin_dif$coefficients[2]*100
  predicciones_2000<-c(predicciones_2000,pred_2000)
  #regresion_1 <- rbind(pre_1991,post_1991)
  predicciones_sin_dif<-c(predicciones_sin_dif,sin_dif$yhat)
}

dat$yhat_sin_dif<-predicciones_sin_dif

#AGREGAMOS LAS PREDICCIONES AL 2000
for(i in 1:50){
new_row<-c(i,100, NA,NA,NA,NA,NA, NA,NA,NA,NA,NA, NA,NA,NA,NA,NA,NA,predicciones_2000[i])
dat<-rbind(dat,new_row)
}

dat%>%
  filter(statenum %in% 1:50) %>%
  ggplot(aes(year+1900, lpc_murd)) +
  geom_line() +
  #geom_vline(xintercept=1991, linetype='dashed',colour = "red") +
  geom_line(aes(y=yhat_sin_dif),colour="green")+
  facet_wrap(~ statenum, nrow=5)+
  labs(title='Evolución de Asesinatos',
       x='Año',
       y='Tasa de asesinatos')

################################################################################################
#INCISO B

###############################################################################################


rutawork = '/Users/MauricioGS1/Documents/MaestriaDatos/Fundamentos/TrabajoFinal/final/'
dat <- read.table(paste(rutawork,"EST4611_AbortoCrimen.csv",sep = ""), header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE)


# Correlaciones para saber qué variables usar
corrs<-cor(dat[c('lpc_murd','xxprison','xxpolice','xxunemp','xxincome','xxpover')], use = 'pairwise.complete.obs')

plot(dat[c('lpc_murd','xxprison','xxpolice','xxunemp','xxincome','xxpover')])


# xxprison, xxpolice y xxincome
dat_full <- dat %>%
  filter(!is.na(lpc_murd),
         !is.na(popul),
         !is.na(xxprison),
         !is.na(xxpolice),
         !is.na(xxpover)
  ) %>%
  group_by(year) %>%
  summarise(lpc_murd = log(sum(exp(lpc_murd)*popul)/sum(popul)),
            xxprison = sum(xxprison*popul)/sum(popul),
            xxpolice = sum(xxpolice*popul)/sum(popul),
            xxpover = sum(xxpover*popul)/sum(popul),
            popul = sum(popul))


## No tomamos en cuenta el tiempo antes y después

d <- list(
  y = dat_full$lpc_murd,
  x = as.matrix(cbind(1, dat_full[c('xxprison','xxpolice','xxpover')])),
  n = nrow(dat_full)
)
d$p <- ncol(d$x)

inits <- function(){
  list(beta=rep(0,ncol(d$x)), tau=1)
}

params <- c('beta','tau','yf')

jags2a <- jags(d, inits, params, 'mod2.R', n.chains = 10, n.iter=20000, n.burnin = 500)
mod <- jags2a

mod$BUGSoutput$DIC
out <- mod$BUGSoutput$sims.list
out_sum <- mod$BUGSoutput$summary

# Ajuste
yhat <- out_sum[grep('yf', rownames(out_sum)),] %>% as.data.frame
x <- data.frame(year = dat_full$year,
                y = dat_full$lpc_murd,
                yhat = yhat$mean,
                ll = yhat[['2.5%']],
                ul = yhat[['97.5%']]) %>%
  mutate(ind = factor((year < 91) + (year < 97)))

cor(x$y, x$yhat)

plot_yhat <- ggplot(x, aes(year+1900)) +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha = 0.5, fill='red') +
  geom_point(aes(y=y)) +
  geom_line(aes(y=yhat), color='red') +
  geom_vline(xintercept=1991, linetype='dashed') +
  labs(title='Predicción de modelo con covariables xxprison, xxpolice, xxpover',
       x = 'Año', y = 'log(tasa de homicidio)')
plot_yhat

# Resumen 2a
ej2a <- list(jags2a=jags2a,
             out=out,
             out_sum=out_sum,
             x=x,
             plot_yhat=plot_yhat)



##Tomamos en cuenta el antes y después mediante otra variable

d <- list(
  y = dat_full$lpc_murd,
  x = as.matrix(cbind(1,
                      as.numeric(dat_full$year < 91),
                      dat_full[c('xxprison','xxpolice','xxpover')])),
  n = nrow(dat_full)
)
d$p <- ncol(d$x)

inits <- function(){
  list(beta=rep(0,ncol(d$x)), tau=1)
}

params <- c('beta','tau','yf')

jags2b <- jags(d, inits, params, 'mod2.R', n.chains = 10, n.iter=20000, n.burnin = 500)
mod <- jags2b

mod$BUGSoutput$DIC
out <- mod$BUGSoutput$sims.list
out_sum <- mod$BUGSoutput$summary

# Ajuste
yhat <- out_sum[grep('yf', rownames(out_sum)),] %>% as.data.frame
x <- data.frame(year = dat_full$year,
                y = dat_full$lpc_murd,
                yhat = yhat$mean,
                ll = yhat[['2.5%']],
                ul = yhat[['97.5%']]) %>%
  mutate(ind = factor((year < 91) + (year < 97)))

cor(x$y, x$yhat)

plot_yhat <- ggplot(x, aes(year+1900)) +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha = 0.5, fill='red') +
  geom_point(aes(y=y)) +
  geom_line(aes(y=yhat), color='red') +
  geom_vline(xintercept=1991, linetype='dashed') +
  labs(title='Predicción de modelo con covariables xxincome ,xxpolice, xxpover',
       x = 'Año', y = 'log(homicidio)')
plot_yhat


################################################################################################
#INCISO C

################################################################################################


#  	RegresiÃ³n lineal con efectos fijos
#	----------------------------------------------------

crimen <- cdata[complete.cases(cdata),]

Y <- as.matrix(crimen$lpc_murd)

J <- max(unique(crimen[,"statenum"])) -1

rdataf <- crimen

j <-1 

for(j in 1:J){
  f <- 0*crimen[,"year"] #year
  f[which(crimen[,"statenum"]==j)] <- 1
  rdataf <- cbind(rdataf,f)
}

Xf <- as.matrix(cbind(matrix(1,dim(Y)), rdataf$xxprison,rdataf$xxpolice,rdataf$xxpover,
                      rdataf[,c((dim(crimen)[2]+1):(dim(crimen)[2]+J))]))

M <- 1000 
mf_0 <-  solve(t(Xf)%*%Xf)%*%t(Xf)%*%Y
pf <- ncol(Xf)
Sf_0 <- diag(1,pf,pf)
af_0 <- 1
bf_0 <- 1

homicidio.linreg.fixed <- baylinreg(Y,Xf,mf_0,Sf_0,af_0,bf_0)

homicidio_modelo1 <-homicidio.linreg.fixed[[1]]
homicidio_modelo1

#	----------------------------------------------------
#INCISO C
#	----------------------------------------------------
# Generamos los datos
dat_state <- dat %>%
  filter(!is.na(lpc_murd),
         !is.na(xxprison),
         !is.na(xxpolice),
         !is.na(xxpover))

### a) Efectos fijos

a <- dat_state['statenum']
a$statenum <- as.factor(a$statenum)

d <- list(
  y = dat_state$lpc_murd,
  x = as.matrix(cbind(model.matrix(~statenum, a),
                      dat_state[c('xxprison','xxpolice','xxpover')])),
  n = nrow(dat_state)
)
d$p <- ncol(d$x)

inits <- function(){
  list(beta=rep(0,ncol(d$x)), tau=1)
}

params <- c('beta','tau','yf')

# Obs: Usamos el modelo 2 de regresión, pero con X especial
jags3a <- jags(d, inits, params, 'mod2.R', n.chains = 10, n.iter=2000, n.burnin = 500)
mod <- jags3a

mod$BUGSoutput$DIC
out <- mod$BUGSoutput$sims.list
out_sum <- mod$BUGSoutput$summary

# Ajuste
yhat <- out_sum[grep('yf', rownames(out_sum)),] %>% as.data.frame
x <- data.frame(statenum = dat_state$statenum,
                year = dat_state$year,
                y = dat_state$lpc_murd,
                yhat = yhat$mean,
                ll = yhat[['2.5%']],
                ul = yhat[['97.5%']]) %>%
  mutate(ind = factor((year < 91) + (year < 97)))

cor(x$y, x$yhat)

plot_yhat <- x %>%
  filter(statenum %in% c(4,27)) %>%
  ggplot(aes(year+1900)) +
  geom_ribbon(aes(ymin=ll, ymax=ul), fill='blue',alpha = 0.5) +
  geom_point(aes(y=y)) +
  theme_bw() +
  geom_line(aes(y=yhat), color='orange') +
  facet_wrap(~ statenum, nrow=5)+
  geom_vline(xintercept=1991, linetype='dashed') +
  labs(title='Predicción por Estado',
       x = 'Año', y = 'Tasa de Asesinatos')
plot_yhat

# Resumen de 3a
ej3a <- list(dat_state=dat_state,
             #jags3a=jags3a, # Demasiado pesado
             dic=mod$BUGSoutput$DIC,
             out=out,
             out_sum=out_sum,
             yhat=yhat,
             x=x,
             plot_yhat=plot_yhat)

### b) Modelo jerárquico (sin covariables)

# Generamos los datos
dat_state <- dat %>%
  filter(!is.na(lpc_murd))

dat_state <- dat_state[,c("statenum","year","lpc_murd")]

aux.y <- dat_state %>%
  spread(statenum, lpc_murd) #%>%

d <- list(
  y = aux.y,
  n = nrow(aux.y),
  npred = 0,
  nstates = ncol(aux.y)
)

inits <- function(){
  list(mu.b=rep(10,2),
       tau.b=rep(10,2),
       a.t=1,
       b.t=1,
       beta=matrix(0,2,d$nstates),
       tau=rep(1, d$nstates))
}

params <- c('mu.b','tau.b','a.t','b.t','beta','tau','yf')

# Obs: Usamos el modelo 2 de regresión, pero con X especial
jags3b <- jags(d, inits, params, 'mod3.R', n.chains = 10, n.iter=2000, n.burnin = 500)
mod <- jags3b

mod$BUGSoutput$DIC
out <- mod$BUGSoutput$sims.list
out_sum <- mod$BUGSoutput$summary

# Ajuste
yhat <- out_sum[grep('yf', rownames(out_sum)),] %>%
  as.data.frame %>%
  .[!is.na(as.numeric(as.matrix(aux.y))),]
x <- data.frame(statenum = dat_state$statenum,
                year = dat_state$year,
                y = dat_state$lpc_murd,
                yhat = yhat$mean,
                ll = yhat[['2.5%']],
                ul = yhat[['97.5%']]) %>%
  mutate(ind = factor((year < 91) + (year < 97)))

cor(x$y, x$yhat)

plot_yhat <- x %>%
  filter(statenum %in% c(4,27)) %>%
  ggplot(aes(year+1900)) +
  geom_ribbon(aes(ymin=ll, ymax=ul), fill='blue', alpha = 0.5) +
  geom_point(aes(y=y)) +
  geom_line(aes(y=yhat), color='green') +
  geom_vline(xintercept=1991, linetype='dashed') +
  facet_wrap(~statenum, nrow=5) +
  labs(title='Predicción por Estado - Modelo Jerárquico',
       x = 'Año', y = 'Tasa de Asesinatos')
plot_yhat

# Resumen de 3b
ej3b <- list(dat_state=dat_state,
             # jags3b=jags3b, # Demasiado pesado
             dic=mod$BUGSoutput$DIC,
             out=out,
             out_sum=out_sum,
             yhat=yhat,
             x=x,
             plot_yhat=plot_yhat)
