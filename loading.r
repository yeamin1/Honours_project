library(rjags)
library(tfer)
setwd('C:/Users/yeamin/Desktop/bk/project')
source('source.fun.r')
N = 10000
data = transfer(N = 10000, d = 0.01, 
                deffect = 1, lambda = 120, 
                Q = 0.05, 
                l0 = 0.9, u0 = 0.9, 
                lstar0 = 0.1, ustar0 = 0.15, 
                lj = 0.9, uj = 0.9, 
                lstarj = 0.05, ustarj = 0.1, 
                lR = 0.5, uR = 0.7, 
                t = 1.5, r = 0.5)@Y

data = transfer(d = 0.01)@Y


hist(data,breaks = max(data))
x = 0:max(data)

j = 1
BIC.store = 0


