#########5.3########
I = 1
qu = 0.75
ql = 0.25
z = 2
e = exp(1)

const = function(x){
  return(ql*log(x) + (1 - ql)*log((qu/(1-qu))*((e^z - 1/qu)*I - x)) - log(ql*e^z*I - I))
}

x = seq(0.0001, (e^z - 1/qu)*I - 0.0001, 0.0001)
y = unlist(lapply(x, const))

res = cbind(x,y)
res[,2] = round(res[,2], 4)

#Separating incomes
pi_up = res[res[,2] == 0,1] 
pi_down = (qu/(1-qu))*((e^z - 1/qu)*I - pi_up)
exp_u = qu*log(pi_up) + ql*log(pi_down) #expected utility of separating equilibrium

#Pooling income

f = seq(0, 1, 1e-4)
p = f*qu + (1-f)*ql
pi = p*e^z*I - I
u = log(pi) #utility in the pooling equilibrium
r = cbind(f, u)

r[which.min(abs(r[,2] - exp_u)),]



#########5.4########

z = 1.5

x = seq(0.0001, (e^z - 1/qu)*I - 0.0001, 0.0001)
y = unlist(lapply(x, const))

res = cbind(x,y)
res[,2] = round(res[,2], 4)

#Separating incomes
pi_up = res[which.min(abs(res[,2])),1] 
pi_down = (qu/(1-qu))*((e^z - 1/qu)*I - pi_up)
exp_u = qu*log(pi_up) + ql*log(pi_down) #expected utility of separating equilibrium

#Pooling income

f = seq(0, 1, 1e-4)
p = f*qu + (1-f)*ql
pi = p*e^z*I - I
u = log(pi) #utility in the pooling equilibrium
r = cbind(f, u)

r[which.min(abs(r[,2] - exp_u)),]
