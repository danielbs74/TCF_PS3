library(stats)
library(tidyverse)
library(data.table)
library(lattice)

rm(list = ls())
set.seed(1)
n = 10000
A = 1
B = 1
qu = 0.75
ql = 0.10
z = 1
e = exp(1)
zetaL = qu*(e^z-B/(qu - ql)) #Zeta for the Lenders. Maximizes Pledgeable Income
zetaC = qu*e^z #Zeta for the CEO. Maximizes CEO Utility (NPV)


#the integrand inside E(zeta)
mean_exp = function(x){
  return(x*dexp(x, Z))
}

#The function to be minimized in order to find zeta*
zeta_optimizer = function(zeta){
  return(-(1 + integrate(mean_exp, 0, zeta)$value/(pexp(zeta, Z))))
}

rand = c(runif(n, 1, 3)) #Random rates for the exponential distribution
rand = cbind(rand, unlist(lapply(rand, rexp, n = 1)), runif(n)) #Random values for the exponential distr. (liq. shocks) and random values for project success

res = c()
for(i in 1:10000){
  Z = rand[i, 1]
  shock = rand[i, 2]
  
  zeta_optim = optim((zetaL + zetaC)/2, 
                     zeta_optimizer, 
                     lower = 0.9*zetaL, 
                     upper = 1.1*zetaC,
                     method = "L-BFGS-B")$par #Finding zeta*. The function is optimized in an interval just larger than the feasible one in order to avoid boundary issues
  
  if(zeta_optim >= zetaC){zeta_optim = zetaC} else if(zeta_optim < zetaL){
    I = 0
    pi_c = 0
    p1 = 0
    status = 'Not invested'
    res = rbind(res, c(I, pi_c, zeta_optim, shock, pi1, status))
    next}
  
  
  
  mean_shock = integrate(mean_exp, 0, zeta_optim)$value #E(zeta*)
  
  I = A/(1 - pexp(zeta_optim, Z)*qu*(e^z - B/(qu - ql)) + mean_shock) #Maximum I*
  
  pi_c =  e^z*I + (A - (1 + mean_shock))*I/(pexp(zeta_optim, Z)) #CEO's Income
  
  project = rand[i, 3]
  
  if(shock > zeta_optim){pi_c = 0
  status = "Liquidated"
  pi1 = 0
  res = rbind(res, c(I, pi_c, zeta_optim, shock, pi1, status))
  next}
  
  
  if(project > qu){pi_c = 0
  status = "Failed"
  pi1 = 0
  res = rbind(res, c(I, pi_c, zeta_optim, shock, pi1, status))
  next}
  
  pi1 = e^z*I  
  status = "Successful"
  res = rbind(res, c(I, pi_c, zeta_optim, shock, pi1, status))
  
  print(i)
}

res = data.frame(res, stringsAsFactors = F)

names(res) = c('Investment', 'CEO Income', 'Cutoff', 'Effective Shock', 'Project Yield', 'Project Status')

res = res %>% mutate(Investment = as.numeric(Investment),
                     `CEO Income` = as.numeric(`CEO Income`),
                     Cutoff = as.numeric(Cutoff),
                     `Effective Shock` = as.numeric(`Effective Shock`),
                     `Project Yield` = as.numeric(`Project Yield`),
                     `Project Status` = as.factor(`Project Status`),
                     `Lender Income` = `Project Yield` - `CEO Income`,
                     `Lender Profit` = `Lender Income` - Investment + A,
                     Withdrawn = Investment * `Effective Shock`)

res$Withdrawn = ifelse(res$`Project Status` == "Liquidated", 0, res$Withdrawn)
res$`Lender Profit` = ifelse(res$`Project Status` != "Liquidated",
                             res$`Lender Profit` - res$Withdrawn,
                             res$`Lender Profit`)

res$`Project Proceeds` = res$`Project Yield` - (res$Investment - A) - res$Withdrawn

histogram(res$`Project Status`, col = 'white', xlab = 'Project Status')

colMeans(res[, -c(6, 9)])

mean(res$Withdrawn[res$`Project Status` != "Liquidated"])
