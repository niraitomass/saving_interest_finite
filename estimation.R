#################################################################################################
#  This is the estimation code for "Interest rate effects and finite horizons", bachelor thesis 
#  by Nirai Alexander Tomass. This code is adapted from that of the publicly available code from 
#  Chen et al. (2019). 

#################################################################################################

library(data.table)
library(readxl)
library(tidyverse)
library(foreign)
library(xtable)
library(plm)
library(gmm)

code_path = "C:/Users/nirai/Desktop/Bachelor's thesis/Code"
data_path = "C:/Users/nirai/Desktop/Bachelor's thesis/Data"
output_path = "C:/Users/nirai/Desktop/Bachelor's thesis/Output"

###--------------------------------------------------------------------------------------------------------
##Load in 

data = readRDS(file.path(data_path, "data_aizenman.rds"))
data = pdata.frame(data, index = c("ccode", "year"))


attach(data)

###--------------------------------------------------------------------------------------------------------
##Descriptive statistics 

table1 = matrix(NA, nrow = 12, ncol = 2, dimnames = list(c("Private saving (% of GDP)", "Public saving (% of GDP)",  "Real interest rate", "Old dependency", "Young dependency", "Health expenditure (% of GDP)", "Financial openness", "Output volatility", "Income per capita growth", "Log income per capita (PPP)", "Financial development", "Credit growth"), c("Mean", "SD")))
table1[1, ] = c(mean(p_saving, na.rm = T), sd(p_saving, na.rm = T))
table1[2, ] = c(mean(gbb, na.rm = T), sd(gbb, na.rm = T))
table1[3, ] = c(mean(real, na.rm = T), sd(real, na.rm = T))
table1[4, ] = c(mean(depend_old, na.rm = T), sd(depend_old, na.rm = T))
table1[5, ] = c(mean(depend_young, na.rm = T), sd(depend_young, na.rm = T))
table1[6, ] = c(mean(healthcare, na.rm = T), sd(healthcare, na.rm = T))
table1[7, ] = c(mean(ka_open, na.rm = T), sd(ka_open, na.rm = T))
table1[8, ] = c(mean(rypc_g_un_sd5, na.rm = T), sd(rypc_g_un_sd5, na.rm = T))
table1[9, ] = c(mean(rypc_g_un, na.rm = T), sd(rypc_g_un, na.rm = T))
table1[10, ] = c(mean(lrypc_ppp, na.rm = T), sd(lrypc_ppp, na.rm = T))
table1[11, ] = c(mean(hp_pcgdp, na.rm = T), sd(hp_pcgdp, na.rm = T))
table1[12, ] = c(mean(pcgdp_g, na.rm = T), sd(pcgdp_g, na.rm = T))
xtable(table1, digits=2)

###--------------------------------------------------------------------------------------------------------
##Reproduce Table 2 of Aizenman et al. (2019)

s = summary
lag_iv = 15

form.ab = p_saving ~ lag(p_saving) + real*depend_old + gbb + pcgdp_g + hp_pcgdp + lrypc_ppp + rypc_g_un + depend_young + healthcare + ka_open + rypc_g_un_sd5 | lag(p_saving, 2:lag_iv) + lag(gbb, 1:lag_iv) + lag(pcgdp_g, 1:lag_iv) + lag(hp_pcgdp, 1:lag_iv) + lag(lrypc_ppp, 1:lag_iv) + lag(rypc_g_un, 1:lag_iv) + lag(real, 1:lag_iv) 
ab.fit = pgmm(form.ab, na.omit(data), model = "twosteps", effect = "twoways", 
              transformation = "ld", index = c("ccode", "year"), collapse = TRUE)
coefs.ab = coef(ab.fit)
HCV.coefs = vcovHC(ab.fit, cluster = "group")
cse.ab = sqrt(diag(HCV.coefs))
s(ab.fit)
mtest(ab.fit, order = 2, vcov = vcovHC)

options(digits = 3)
table = matrix(NA, nrow = 26, ncol = 1, dimnames = list(c("Old x Real rate", "CSE", "Old", "CSE", "Real rate", "CSE", "lag(Savings)", "CSE", 
                                                           "Public savings", "CSE", "FDI", "CSE", "Financial openness", "CSE",
                                                          "Credit growth", "CSE", "Health", "CSE", "Young", "CSE", "Income", "CSE", 
                                                          "Income growth", "CSE", "Income volatility", "CSE"),
                                                        c("GMM")))
table[seq(1, 26, by = 2), 1] = coefs.ab[c(13, 3, 2, 1, 4, 6, 11, 5, 10, 9, 7, 8, 12)]  
table[seq(2, 26, by = 2), 1] = cse.ab[c(13, 3, 2, 1, 4, 6, 11, 5, 10, 9, 7, 8, 12)]  
xtable(table, digits = 3)

###--------------------------------------------------------------------------------------------------------
##Reduce instrument count and remove insignificant variables

lag_iv = 14

form.ab = p_saving ~ lag(p_saving) + real*depend_old + gbb + pcgdp_g + hp_pcgdp + lrypc_ppp + rypc_g_un + depend_young + healthcare | lag(p_saving, 2:lag_iv) + lag(gbb, 1:lag_iv) + lag(pcgdp_g, 1:lag_iv) + lag(hp_pcgdp, 1:lag_iv) + lag(lrypc_ppp, 1:lag_iv) + lag(rypc_g_un, 1:lag_iv) + lag(real, 1:lag_iv) 
ab.fit = pgmm(form.ab, na.omit(data), model = "twosteps", effect = "twoways", 
              transformation = "ld", index = c("ccode", "year"), collapse = TRUE)
coefs.ab14 = coef(ab.fit)
HCV.coefs14 = vcovHC(ab.fit, cluster = "group")
cse.ab14 = sqrt(diag(HCV.coefs14))
s(ab.fit)
mtest(ab.fit, order = 2, vcov = vcovHC)

lag_iv = 10

form.ab = p_saving ~ lag(p_saving) + real*depend_old + gbb + pcgdp_g + hp_pcgdp + lrypc_ppp + rypc_g_un + depend_young + healthcare | lag(p_saving, 2:lag_iv) + lag(gbb, 1:lag_iv) + lag(pcgdp_g, 1:lag_iv) + lag(hp_pcgdp, 1:lag_iv) + lag(lrypc_ppp, 1:lag_iv) + lag(rypc_g_un, 1:lag_iv) + lag(real, 1:lag_iv) 
ab.fit = pgmm(form.ab, na.omit(data), model = "twosteps", effect = "twoways", 
              transformation = "ld", index = c("ccode", "year"), collapse = TRUE)
coefs.ab10 = coef(ab.fit)
HCV.coefs10 = vcovHC(ab.fit, cluster = "group")
cse.ab10 = sqrt(diag(HCV.coefs10))
s(ab.fit)
mtest(ab.fit, order = 2, vcov = vcovHC)

lag_iv = 5

form.ab = p_saving ~ lag(p_saving) + real*depend_old + gbb + pcgdp_g + hp_pcgdp + lrypc_ppp + rypc_g_un + depend_young + healthcare | lag(p_saving, 2:lag_iv) + lag(gbb, 1:lag_iv) + lag(pcgdp_g, 1:lag_iv) + lag(hp_pcgdp, 1:lag_iv) + lag(lrypc_ppp, 1:lag_iv) + lag(rypc_g_un, 1:lag_iv) + lag(real, 1:lag_iv) 
ab.fit = pgmm(form.ab, na.omit(data), model = "twosteps", effect = "twoways", 
              transformation = "ld", index = c("ccode", "year"), collapse = TRUE)
coefs.ab5 = coef(ab.fit)
HCV.coefs5 = vcovHC(ab.fit, cluster = "group")
cse.ab5 = sqrt(diag(HCV.coefs5))
s(ab.fit)
mtest(ab.fit, order = 2, vcov = vcovHC)


options(digits = 3)
table = matrix(NA, nrow = 22, ncol = 3, dimnames = list(c("Old x Real rate", "CSE", "Old", "CSE", "Real rate", "CSE", "lag(Savings)", "CSE", 
                                                          "Public savings", "CSE", "FDI", "CSE",
                                                          "Credit growth", "CSE", "Health", "CSE", "Young", "CSE", "Income", "CSE", 
                                                          "Income growth", "CSE"),
                                                        c("System GMM - 14 Lags", "System GMM - 10 Lags", "System GMM - 5 Lags")))
table[seq(1, 22, by = 2), 1] = coefs.ab14[c(11, 3, 2, 1, 4, 6, 5, 10, 9, 7, 8)]  
table[seq(2, 22, by = 2), 1] = cse.ab14[c(11, 3, 2, 1, 4, 6, 5, 10, 9, 7, 8)]  
table[seq(1, 22, by = 2), 2] = coefs.ab10[c(11, 3, 2, 1, 4, 6, 5, 10, 9, 7, 8)]  
table[seq(2, 22, by = 2), 2] = cse.ab10[c(11, 3, 2, 1, 4, 6, 5, 10, 9, 7, 8)]  
table[seq(1, 22, by = 2), 3] = coefs.ab5[c(11, 3, 2, 1, 4, 6, 5, 10, 9, 7, 8)]  
table[seq(2, 22, by = 2), 3] = cse.ab5[c(11, 3, 2, 1, 4, 6, 5, 10, 9, 7, 8)]  
xtable(table, digits = 3)
###--------------------------------------------------------------------------------------------------------
##Fixed effects estimation

form.fe = p_saving ~ lag(p_saving) + real*depend_old + gbb + pcgdp_g + hp_pcgdp + lrypc_ppp + rypc_g_un + depend_young + healthcare 
fe.fit = plm(form.fe, na.omit(data), model = "within", effect = "twoways", index = c("ccode", "year"))
coefs.fe = coef(fe.fit)
HCV.coefs = vcovHC(fe.fit, cluster = "group")
cse.fe = sqrt(diag(HCV.coefs))
s(fe.fit)

##Split panel jacknife bias correction (See equation (3.3) in Dhaene and Jochmans (2015), or Chen et al. (2019))
fe.fit1 = plm(form.fe, na.omit(data), subset = (as.double(year) <= 11), model = "within", effect = "twoways", index = c("ccode", "year")) 
fe.fit2 = plm(form.fe, na.omit(data), subset = (as.double(year) > 11), model = "within", effect = "twoways", index = c("ccode", "year"))
coefs.jbc = 2*coef(fe.fit) - (1/2)*(coef(fe.fit1) + coef(fe.fit2))

##Bootstrap standard errors 
data.rg = function(data, mle)
{
  N                  = length(unique(data$ccode))
  T                  = length(unique(data$year))
  ids                = kronecker(sample.int(N, N, replace = TRUE), rep(1,T))
  data.b             = data[(ids-1)*T + rep(c(1:T),N), ]
  data.b$ccode       = kronecker(c(1:N), rep(1,T))
  data.b$year        = rep(c(1995:2014),N) 
  data.b             = data.frame(data.b)
  data.b             = pdata.frame(data.b, index = c("ccode","year"))         # reset indexes of the panel 
  return(data.b)
}

boot.SE.fe = function(data, form.fe, form.abc){
  
  # Fixed Effects
  fe.fit    = plm(form.fe, na.omit(data), model = "within", effect = "twoways", index = c("id","year"))
  coefs.fe  = coef(fe.fit)
  
  # Split-sample bias correction
  fe.fit1   = plm(form.fe, na.omit(data), subset = (as.double(year) <= 11), model = "within", effect = "twoways", index = c("ccode","year"))
  fe.fit2   = plm(form.fe, na.omit(data), subset = (as.double(year) > 11), model = "within", effect = "twoways", index = c("ccode","year"))
  coefs.jbc = 2*coef(fe.fit) - (1/2)*(coef(fe.fit1) + coef(fe.fit2))
  
  
  return(c(coefs.fe, coefs.jbc))
}

library(boot) 

set.seed(888)
result.boot.SE.fe = boot(data = na.omit(data), statistic=boot.SE.fe, sim = "parametric", ran.gen = data.rg, mle = 0, form.fe = form.fe, 
                         parallel="multicore", ncpus = 1, R=500);


rsd = function(x) {return((quantile(x,.75,na.rm=TRUE)-quantile(x,.25,na.rm=TRUE))/(qnorm(.75) - qnorm(.25)))} # robust estimator of std deviation based on IQR (see Benson, 1949)

result      = structure(vapply(result.boot.SE.fe$t, as.double, numeric(1)), dim=dim(result.boot.SE.fe$t)) # transforms "Error in La.svd(x, nu, nv) : error code 1 from Lapack routine 'dgesdd'\n" to NA
bse.fe      = apply(result[,1:11], 2, rsd)
bse.jbc     = apply(result[,12:22], 2, rsd)

options(digits = 3)
table = matrix(NA, nrow = 33, ncol = 2, dimnames = list(c("Old x Real rate", "CSE", "BSE", "Old", "CSE", "BSE", 
                                                          "Real rate", "CSE", "BSE", "lag(Savings)", "CSE", "BSE",
                                                          "Public savings", "CSE", "BSE", "FDI", "CSE", "BSE",
                                                          "Credit growth", "CSE", "BSE", "Health", "CSE", "BSE", 
                                                          "Young", "CSE", "BSE","Income", "CSE", "BSE", 
                                                          "Income growth", "CSE", "BSE"),
                                                        c("FE", "FE - Jackknife")))
table[seq(1, 33, by = 3), 1] = coefs.fe[c(11, 3, 2, 1, 4, 6, 5, 10, 9, 7, 8)] 
table[seq(2, 33, by = 3), 1] = cse.fe[c(11, 3, 2, 1, 4, 6, 5, 10, 9, 7, 8)] 
table[seq(3, 33, by = 3), 1] = bse.fe[c(11, 3, 2, 1, 4, 6, 5, 10, 9, 7, 8)] 
table[seq(1, 33, by = 3), 2] = coefs.jbc[c(11, 3, 2, 1, 4, 6, 5, 10, 9, 7, 8)] 
table[seq(3, 33, by = 3), 2] = bse.jbc[c(11, 3, 2, 1, 4, 6, 5, 10, 9, 7, 8)] 
xtable(table, digits = 3)

###--------------------------------------------------------------------------------------------------------
##Fixed effects estimation - LDC subsample

form.fe = p_saving ~ lag(p_saving) + real*depend_old + gbb + pcgdp_g + hp_pcgdp + lrypc_ppp + rypc_g_un + depend_young + healthcare 
fe.fit = plm(form.fe, na.omit(data[ldc == 1,]), model = "within", effect = "twoways", index = c("ccode", "year"))
coefs.fe = coef(fe.fit)
HCV.coefs = vcovHC(fe.fit, cluster = "group")
cse.fe = sqrt(diag(HCV.coefs))
s(fe.fit)

##Split panel jacknife bias correction (See equation (3.3) in Dhaene and Jochmans (2015), or Chen et al. (2019))
fe.fit1 = plm(form.fe, na.omit(data[ldc == 1,]), subset = (as.double(year) <= 11), model = "within", effect = "twoways", index = c("ccode", "year")) 
fe.fit2 = plm(form.fe, na.omit(data[ldc == 1,]), subset = (as.double(year) > 11), model = "within", effect = "twoways", index = c("ccode", "year"))
coefs.jbc = 2*coef(fe.fit) - (1/2)*(coef(fe.fit1) + coef(fe.fit2))

##Bootstrap standard errors 
data.rg = function(data, mle)
{
  N                  = length(unique(data$ccode))
  T                  = length(unique(data$year))
  ids                = kronecker(sample.int(N, N, replace = TRUE), rep(1,T))
  data.b             = data[(ids-1)*T + rep(c(1:T),N), ]
  data.b$ccode       = kronecker(c(1:N), rep(1,T))
  data.b$year        = rep(c(1995:2014),N) 
  data.b             = data.frame(data.b)
  data.b             = pdata.frame(data.b, index = c("ccode","year"))         # reset indexes of the panel 
  return(data.b)
}

boot.SE.fe = function(data, form.fe, form.abc){
  
  # Fixed Effects
  fe.fit    = plm(form.fe, na.omit(data), model = "within", effect = "twoways", index = c("id","year"))
  coefs.fe  = coef(fe.fit)
  
  # Split-sample bias correction
  fe.fit1   = plm(form.fe, na.omit(data), subset = (as.double(year) <= 11), model = "within", effect = "twoways", index = c("ccode","year"))
  fe.fit2   = plm(form.fe, na.omit(data), subset = (as.double(year) > 11), model = "within", effect = "twoways", index = c("ccode","year"))
  coefs.jbc = 2*coef(fe.fit) - (1/2)*(coef(fe.fit1) + coef(fe.fit2))
  
  
  return(c(coefs.fe, coefs.jbc))
}

library(boot) 

set.seed(888)
result.boot.SE.fe = boot(data = na.omit(data[ldc == 1,]), statistic=boot.SE.fe, sim = "parametric", ran.gen = data.rg, mle = 0, form.fe = form.fe, 
                         parallel="multicore", ncpus = 1, R=500);


rsd = function(x) {return((quantile(x,.75,na.rm=TRUE)-quantile(x,.25,na.rm=TRUE))/(qnorm(.75) - qnorm(.25)))} # robust estimator of std deviation based on IQR (see Benson, 1949)

result      = structure(vapply(result.boot.SE.fe$t, as.double, numeric(1)), dim=dim(result.boot.SE.fe$t)) # transforms "Error in La.svd(x, nu, nv) : error code 1 from Lapack routine 'dgesdd'\n" to NA
bse.fe      = apply(result[,1:11], 2, rsd)
bse.jbc     = apply(result[,12:22], 2, rsd)

