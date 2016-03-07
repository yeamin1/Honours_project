
## model
zip.x = "
model 
{
  
  for(i in 1:N)
  {
  C[i] ~ dcat(pi[1:mix])
  x[i] ~ dpois(mu[C[i]])
  }
  
  pi[1:mix] ~ ddirch(xi[1:mix])
  for(j in 1:mix)
  {
  xi[j] <- 1
  }
  for(k in 2:mix)
  {
  mu[k] ~ dgamma(0.001, 0.001)
  }
  mu[1] <- 0.000001
}
"


output.jags = function(data, step = 5000, model,mix)
{
    tmpf=tempfile()
    tmps=file(tmpf,"w")
    cat(model,file=tmps)
    close(tmps)
    ###jags needs
    bugsFile = tmpf
    parameters = c("mu", "pi")
    bugsData = list(x = data, N = length(data),
                    mix = mix)
    sim = jags.model(bugsFile, bugsData)
    update(sim, step)
    samples = coda.samples(sim, parameters, n.iter = step)
    result = summary(samples)
    result
    list(s = samples, r = result)
  
}

zip.plot = function(result,pi,lambda)
{

    x1 = 1:max(data)

    p0 = pi[1] + sum(pi[-1] * dpois(0,lambda))
    a = rep(pi[-1],each = length(x1)) *(dpois(rep(x1,length(lambda)),rep(lambda,each = length(x1))))
    b = rep(1:max(data), length(lambda))
    p1 = tapply(a,b,sum)

    main = paste('zip mixture of', mix)
    hist(data,breaks = 200,main = main)
    lines(x = 0:max(data),y = c(p0,p1) * 10000,lwd = 2,col = 'red')
}


zip.lik = function(par)
{
    pi.index = (1+length(par)/2):(length(par))
    lambda.index = 1:(length(par)/2)
    x0.index = rep(1:length(lambda.index),each = length(x0))
    x1.index = rep(1:length(lambda.index),each = length(x1))

    lambda.mul.0 = rep(par[lambda.index],each = length(x0))
    pi.mul.0 = rep(par[pi.index],each = length(x0))
    pi.mul.1 = rep(par[pi.index],each = length(x1))
    lambda.mul.1 = rep(par[lambda.index],each = length(x1))

    pi.0 = pi.mul.0[!(x0.index == min(x0.index))]
    lambda.0 = lambda.mul.0[!(x0.index == max(x0.index))]
    lambda.1 = lambda.mul.0[x0.index == max(x0.index)]

    pi.1 = pi.mul.1[!(x1.index == min(x1.index))]
    lambda.2 = lambda.mul.1[!(x1.index == max(x1.index))]
    lambda.3 = lambda.mul.1[x1.index == max(x1.index)]

    pi.last = 1 - sum(par[pi.index])

    p0 = c(pi.0 * dpois(x0,lambda.0),pi.last * dpois(x0,lambda.1))
    p0 = as.numeric(tapply(p0,rep(1:length(x0),length(pi.index)),sum)) + 
                  rep(par[pi.index[1]],length(x0))

    p1 = c(pi.1 * dpois(x1,lambda.2), 
         pi.last * dpois(x1,lambda.3))
    p1 = as.numeric(tapply(p1,rep(1:length(x1),length(pi.index)),sum))
    -sum(log(c(p0,p1)))
}

zip.qqplot = function(){
    x = 0:max(data)
    p = 0
    for(i in x)
    {
        p[i+1] = zip.cdf(x[i+1])
    }
    Qx = approxfun(p,x)
    n = max(data)
    Eqx = quantile(data,ppoints(n),na.rm = TRUE,type = 2)
    Tqx = Qx(ppoints(n))
    main = paste('Quantile-Quantile plot of', mix)
    plot(Tqx,Eqx,type = 'p',main = main)
    abline(1,1)
}

lik.plot = function()
{
    index = rep(1:dim(result.1)[1],each = 1000)
    d = !(index %in% range(index))
    all.par = samples[[1]]
    aa = all.par[,c(-1,-dim(all.par)[2])]
    ll.dis = apply(aa,1,zip.lik)

    main = paste('Likelihood distribution of zip mix ', mix)
    hist(ll.dis,main = main)
}

zip.cdf = function(x)
{
  x1 = 1:x
  p0 = pi[1] + sum(pi[-1] * dpois(0,lambda))
  a = rep(pi[-1],each = length(x1)) *(dpois(rep(x1,length(lambda)),rep(lambda,each = length(x1))))
  b = rep(1:length(x1), length(lambda))
  p1 = tapply(a,b,sum)
  ifelse(x!=0,p0 + sum(p1),p0)
}




###example:
### pdf.fun(result = result,0:10,method = m)

pdf.fun = function(result,x,method)
{
	result.1 = result[[2]]$statistic
	if(method == 'zip')
	{	
		##use the definition for the pdf of zip to do the calculation
		params = c(result.1[2],result.1[1])
		p0 = params[1] + (1 - params[1]) * dpois(0,params[2])
		p1 = (1 - params[1]) * dpois(x,params[2])

	}
	if(method == 'zinb')
	{
		##use the definition for the pdf of zinb to do the calculation
		params = c(result.1[3], result.1[2],result.1[1])
		p0 = params[1] + (1 - params[1]) * dnbinom(0,params[3],params[2])
		p1 = (1 - params[1]) * dnbinom(x,params[3],params[2])

	}
	ifelse(x == 0,p0,p1)
}