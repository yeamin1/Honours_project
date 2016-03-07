mix = 10
out = output.jags(data,100,zip.x,mix = mix)

## information extraction
samples = out$s
result = out$r
result.1 = result$statistic
par = result.1[2:(dim(result.1)[1] - 1)]
x0 = data[data == 0]
x1 = data[data != 0]
pi = result.1[((dim(result.1)[1]/2)+1):dim(result.1)[1]]
lambda = result.1[1:(dim(result.1)[1]/2)][-1]

## zip plot
zip.plot(result.1,pi = pi, lambda = lambda)

## qqplot
zip.qqplot()

## likelihood plot
lik.plot()

BIC = 2 * zip.10.lik(par) + dim(result.1)[1] * log(N)

## frequency MLE
#optim(par,zip.lik,method = 'BFGS')