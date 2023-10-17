# ========= BANCO DE DADOS DE NORMAIS ==========
# ====== TESE MESTRADO INGRID FERNANDES ========
# ===== modificado no RStudio do Macbook Air ===

library(readxl)
library(tidyverse)
library(ggpubr)
library(table1)
library(quantreg)
library(patchwork)
library(ggsci)

# macbook
setwd("~/Dropbox/))) FOT PUCRS/z z Mestrado Ingrid - 2019")

# PC windows 10
setwd("E:/Dropbox/))) FOT PUCRS/z z Mestrado Ingrid - 2019")


# PC PUCRS
setwd("C:/Users/mjones/Dropbox/))) FOT PUCRS/z z Mestrado Ingrid - 2019")


# Load database

ref_0 <- read_excel("valores_referencia_dec2019.xlsx")
View(ref_0)

# remove outliers

ref <- ref_0 %>% filter(reprebd_6<8.2, imprebd_6>(-3), prebd_med_xmeane>(-2.5), prebd_c<0.08, prebd_med_xee<1)
View(ref)

ref %>% select(1:5, reprebd_6) %>% arrange(desc(reprebd_6))

#==== paleta de cores
p2 <- c("#4DBBD5AA", "#F39B7FAA")
p3 <- c("#4DBBD5AA", "#F39B7FAA", "#DC0000AA")
p2a <- c("#D8A499", "#7294D4")


mycols <- c(
  red    = "#E27D60",
  blue   = "#085DCB",
  orange = "#E8A87C",
  purple = "#C38D9E",
  green  = "#41B3A3"
)



# recodificacao variaveis

ref$prebd_e <- 1/ref$prebd_c

# sexo
ref$sex[ref$sexo==0] <- "Male"
ref$sex[ref$sexo==1] <- "Female"
ref$sex <- factor(ref$sex, ordered=T, levels=c("Male", "Female"))

# Color
ref$etnia[ref$etnia==0] <- "White"
ref$etnia[ref$etnia==1] <- "Black"
ref$etnia[ref$etnia==2] <- "Yellow"
ref$etnia[ref$etnia==3] <- "Mixed"
ref$etnia[ref$etnia==4] <- "Native"
ref$etnia <- factor(ref$etnia)

# Gestational Age
ref$ig[ref$ig==0] <- "NA"
ref$ig[ref$ig==1] <- "Preterm"
ref$ig[ref$ig==2] <- "Full Term"
ref$ig[ref$ig==3] <- "Post Term"
ref$ig <- factor(ref$ig, ordered=T, levels=c("Preterm", "Full Term", "Post Term", "NA"))

table(ref$ig)

ref$bw[ref$peso_nascimento==0] <- "NA"
ref$bw[ref$peso_nascimento==1] <- ">2500g"
ref$bw[ref$peso_nascimento==2] <- "1500g to 2500g"
ref$bw[ref$peso_nascimento==3] <- "<1500g"
ref$bw <- factor(ref$bw, ordered=T, levels=c("<1500g","1500g to 2500g",">2500g", "NA"))


# labels
label(ref$h) <- "Height (m)"
label(ref$w) <- "Weight(kg)"
label(ref$age) <- "Age (year)"
label(ref$sexo) <- "Sex"
label(ref$imc) <- "Body Mass Index"

label(ref$reprebd_6) <- "R6 (hPa.s.L-1)"
label(ref$reprebd_8) <- "R8 (hPa.s.L-1)"
label(ref$reprebd_10) <- "R10 (hPa.s.L-1)"
label(ref$prebd_r) <- "Rrs (hPa.s.L-1)"

label(ref$imprebd_6) <- "X6 (hPa.s.L-1)"
label(ref$imprebd_8) <- "X8 (hPa.s.L-1)"
label(ref$imprebd_10) <- "X10 (hPa.s.L-1)"
label(ref$prebd_c) <- "Crs (mL.hPa-1)"


table(ref$bw)

# histogramas
c1 <- ggplot(ref, aes(round(age, digits=0)))+geom_histogram(fill="deepskyblue4", col="black", bins=15)+xlab("Age (years)")+theme_classic2()
c2 <- ggplot(ref, aes(h))+geom_histogram(fill="deepskyblue4", col="black", bins=15)+xlab("Height (m)")+theme_classic2()
c1+c2


c1 <- ggplot(ref, aes(round(age, digits=0)))+geom_histogram(fill="deepskyblue4", col="black", bins=15)+scale_x_continuous(name="Age (years)", breaks = seq(5,75,10))+theme_classic2()
c2 <- ggplot(ref, aes(h))+geom_histogram(fill="deepskyblue4", col="black", binwidth = 0.1)+scale_x_continuous(name="Height (m)", breaks = seq(1,2,0.1))+theme_classic2()
c1+c2

# demographics
table1(data=ref, ~age+h+w | sex, overall = F)

# plots

# plot

ggplot(ref, aes(h, reprebd_6, col=sex))+geom_point()+geom_smooth(method="lm")+ scale_color_manual(values=p2)+scale_x_continuous(name="Altura (m)", limits=c(1,2), breaks=seq(1,2,0.2))+scale_y_continuous(name="R6 (hPa.s.L-1)", limits = c(0,8), breaks = seq(0,8,1)) +theme_classic2()
ggplot(ref, aes(h, reprebd_6, col=sexo))+geom_point()+geom_smooth(method="lm")
ggplot(ref, aes(h, reprebd_8, col=sexo))+geom_point()+geom_smooth(method="lm")
ggplot(ref, aes(h, reprebd_10, col=factor(sexo)))+geom_point()+geom_smooth(method="lm")

# ============= M O D E L S =================================
# ============ linear models ================================

modr6 <- lm(ref$reprebd_6~ref$h)
modr8 <- lm(ref$reprebd_8~ref$h)
modr10 <- lm(ref$reprebd_10~ref$h)
modx6 <- lm(ref$imprebd_6~ref$h)
modx8 <- lm(ref$imprebd_8~ref$h)
modx10 <- lm(ref$imprebd_10~ref$h)

modr <- lm(ref$prebd_r~ref$h)
modc <- lm(ref$prebd_c~ref$h)
modl <- lm(ref$prebd_l~ref$h)
modfres <- lm(ref$prebd_fres~ref$h)

# summaries
summary(modr6)
summary(modr8)
summary(modr10)
summary(modx6)
summary(modx8)
summary(modx10)

summary(modr)
summary(modc)
summary(modl)
summary(modfres)

# ============= M O D E L S =================================
# ========== quantilic models ===============================

# quantile regression
# spectral
sink("rq_spectral.txt")
rqr6 <- rq(ref$reprebd_6~ref$h, tau = c(0.5, 0.95))
rqr8 <- rq(ref$reprebd_8~ref$h, tau = c(0.5, 0.95))
rqr10 <- rq(ref$reprebd_10~ref$h, tau = c(0.5, 0.95))
rqx6 <- rq(ref$imprebd_6~ref$h, tau = c(0.5, 0.05))
rqx8 <- rq(ref$imprebd_8~ref$h, tau = c(0.5, 0.05))
rqx10 <- rq(ref$imprebd_10~ref$h, tau = c(0.5, 0.05))

rqr <- rq(ref$prebd_r~ref$h+ref$age, tau = c(0.5, 0.95))
rqc <- rq(ref$prebd_c~ref$h+ref$w, tau = c(0.5, 0.05))
rql <- rq(ref$prebd_l~ref$h+ref$w, tau = c(0.5, 0.05))
rqfres <- rq(ref$prebd_fres~ref$h+ref$w, tau = c(0.5, 0.95))

summary(rqr6)
summary(rqr8)
summary(rqr10)

summary(rqx6)
summary(rqx8)
summary(rqx10)

summary(rqr)
summary(rqc)
summary(rql)
summary(rqfres)

sink()



sink("rq_intrabreath.txt")
# intra-breath
rqree <- rq(ref$prebd_med_ree~ref$h, tau = c(0.5, 0.95))
rqrei <- rq(ref$prebd_med_rei~ref$h, tau = c(0.5, 0.95))
rqreerei <- rq(ref$prebd_med_reerei~ref$h, tau = c(0.5, 0.95))
rqrmeane <- rq(ref$prebd_med_rmeane~ref$h, tau = c(0.5, 0.95))
rqrmeani <- rq(ref$prebd_med_rmeani~ref$h, tau = c(0.5, 0.95))

rqxee <- rq(ref$prebd_med_xee~ref$h, tau = c(0.5, 0.05))
rqxei <- rq(ref$prebd_med_xei~ref$h, tau = c(0.5, 0.05))
rqxeexei <- rq(ref$prebd_med_xeexei~ref$h, tau = c(0.5, 0.05))
rqxmeane <- rq(ref$prebd_med_xmeane~ref$h, tau = c(0.5, 0.05))
rqxmeani <- rq(ref$prebd_med_xmeani~ref$h, tau = c(0.5, 0.05))

summary(rqree)
summary(rqrei)
summary(rqreerei)
summary(rqrmeane)
summary(rqrmeani)

summary(rqxee)
summary(rqxei)
summary(rqxeexei)
summary(rqxmeane)
summary(rqxmeani)

sink()



#==================================================================
#============================GRAFICOS==============================
#==================================================================

#==============  S P E C T R A L    F O T  =========================
# R6

a1 <- ggplot(ref, aes(h, reprebd_6))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="R6 (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.95), color="black")
a1b <- ggplot(ref, aes(h, reprebd_6))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="R6 (hPa.s.L-1)")+geom_smooth()

# R8
a2 <- ggplot(ref, aes(h, reprebd_8))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="R8 (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.95), color="black")
a2b <- ggplot(ref, aes(h, reprebd_8))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="R8 (hPa.s.L-1)")+geom_smooth()

# R10
a3 <- ggplot(ref, aes(h, reprebd_10))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="R10 (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.95), color="black")
a3b <- ggplot(ref, aes(h, reprebd_10))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="R10 (hPa.s.L-1)")+geom_smooth()


# X6
a4 <- ggplot(ref, aes(h, imprebd_6))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="X6 (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.05), color="black")
a4b <- ggplot(ref, aes(h, imprebd_6))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="X6 (hPa.s.L-1)")+geom_smooth()

# X8
a5 <- ggplot(ref, aes(h, imprebd_8))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="X8 (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.05), color="black")
a5b <- ggplot(ref, aes(h, imprebd_8))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="X8 (hPa.s.L-1)")+geom_smooth()

# X10
a6 <- ggplot(ref, aes(h, imprebd_10))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="X10 (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.05), color="black")
a6b <- ggplot(ref, aes(h, imprebd_10))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="X10 (hPa.s.L-1)")+geom_smooth()


# R
a7 <- ggplot(ref, aes(h, prebd_r))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="R (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.95), color="black")
a7b <- ggplot(ref, aes(h, prebd_r))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="R (hPa.s.L-1)")+geom_smooth()

# C
a8 <- ggplot(ref, aes(h, prebd_c))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="Crs (mL.hPa-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.05), color="black")
a8b <- ggplot(ref, aes(h, prebd_c))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="Crs (mL.hPa-1)")+geom_smooth()

# Fres
a9 <- ggplot(ref, aes(h, prebd_fres))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="Fres (Hz)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.95), color="black")
a9b <- ggplot(ref, aes(h, prebd_fres))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="Fres (Hz)")+geom_smooth()


# plots patchwork spectral
(a1+a2)/(a3+a7)
(a4+a5)/(a6+a8)

(a1b+a2b)/(a3b+a7b)
(a4b+a5b)/(a6b+a8b)





#==============  I N T R A - B R E A T H    F O T  =========================

#ReE
b1 <- ggplot(ref, aes(h, prebd_med_ree))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="ReE (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.95), color="black")
b1b <- ggplot(ref, aes(h, prebd_med_ree))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="ReE (hPa.s.L-1)")+geom_smooth()

#ReI
b2 <- ggplot(ref, aes(h, prebd_med_rei))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="ReI (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.95), color="black")
b2b <- ggplot(ref, aes(h, prebd_med_rei))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="ReI (hPa.s.L-1)")+geom_smooth()

#ReE-ReI
b3 <- ggplot(ref, aes(h, prebd_med_reerei))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="\u394R (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.95), color="black")
b3b <- ggplot(ref, aes(h, prebd_med_reerei))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="\u394R (hPa.s.L-1)")+geom_smooth()

#Rmeane
b4 <- ggplot(ref, aes(h, prebd_med_rmeane))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="RE (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.95), color="black")
b4b <- ggplot(ref, aes(h, prebd_med_rmeane))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="RE (hPa.s.L-1)")+geom_smooth()

#Rmeani
b5 <- ggplot(ref, aes(h, prebd_med_rmeani))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="RI (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.95), color="black")
b5b <- ggplot(ref, aes(h, prebd_med_rmeani))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="RI (hPa.s.L-1)")+geom_smooth()

(b1+b2)/(b4+b5)
(b1b+b2b)/(b4b+b5b)


#XeE
b6 <- ggplot(ref, aes(h, prebd_med_xee))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="XeE (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.05), color="black")
b6b <- ggplot(ref, aes(h, prebd_med_xee))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="XeE (hPa.s.L-1)")+geom_smooth()

#XeI
b7 <- ggplot(ref, aes(h, prebd_med_xei))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="XeI (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.05), color="black")
b7b <- ggplot(ref, aes(h, prebd_med_xei))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="XeI (hPa.s.L-1)")+geom_smooth()

#XeE-XeI
b8 <- ggplot(ref, aes(h, prebd_med_xeexei))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="\u394X (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.05), color="black")
b8b <- ggplot(ref, aes(h, prebd_med_xeexei))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="\u394X (hPa.s.L-1)")+geom_smooth()

#Xmeane
b9 <- ggplot(ref, aes(h, prebd_med_xmeane))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="XE (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.05), color="black")
b9b <- ggplot(ref, aes(h, prebd_med_xmeane))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="XE (hPa.s.L-1)")+geom_smooth()

#Xmeani
b10 <- ggplot(ref, aes(h, prebd_med_xmeani))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="XI (hPa.s.L-1)")+ geom_quantile(quantiles = c(0.5), color="black", size=1)+geom_quantile(quantiles = c(0.05), color="black")
b10b <- ggplot(ref, aes(h, prebd_med_xmeani))+geom_point(alpha=0.5)+scale_color_d3()+theme_bw()+scale_x_continuous(name="Height (m)", breaks = seq(0,2,0.25), limits = c(1,2))+scale_y_continuous(name="XI (hPa.s.L-1)")+geom_smooth()

# plots patchwork intrabreath

(b6+b7)/(b9+b10)
(b6b+b7b)/(b9b+b10b)

b3+b8
b3b+b8b


# ========= Thu Jan 23 12:18:15 2020 ------------------------------
# ====== z scores adjusted ======

library(standardize)
ref$reprebd_6_z <- scale_by(reprebd_6 ~ h, ref)
ref$reprebd_8_z <- scale_by(reprebd_8 ~ h, ref)
ref$reprebd_10_z <- scale_by(reprebd_10 ~ h, ref)

ref$imprebd_6_z <- scale_by(imprebd_6 ~ h, ref)
ref$imprebd_8_z <- scale_by(imprebd_8 ~ h, ref)
ref$imprebd_10_z <- scale_by(imprebd_10 ~ h, ref)

ggplot(ref, aes(age, reprebd_6_z))+geom_point()
cor(ref$h, ref$reprebd_6,use="complete.obs")

# Sat Jan 25 21:51:08 2020 ------------------------------
# calculando escore Z

modr6 <- lm(ref$reprebd_6~ref$h) # cria modelo
s <- summary(modr6)$sigma # extraindo standart error
ggplot(ref, aes(h, (reprebd_6-modr6$fitted.values)/s))+geom_point() # plot z score conra altura
ref$reprebd_6_z <- (ref$reprebd_6-modr6$fitted.values)/s # cria variavel z score
ggplot(ref, aes(h, reprebd_6))+geom_point()+geom_smooth(method = "lm") # plot r6 score 
ggplot(ref, aes(h, reprebd_6_z))+geom_point()+geom_smooth(method = "lm") # plot z score r6 com linha de tendencia



#
#
# ==========================
# ==========================
# ==========================
# ==========================
# ===== controls 2017 ======
# ==========================
# ==========================
# ==========================
# ==========================

controls2017 <- read_excel("controls2017.xlsx")
View(controls2017)
names(controls2017)<-tolower(names(controls2017))



