require(nleqslv)

fnWeibull <-function(start,mean,variance)
{
    rate <- start[2] *gamma(1+1/start[1]) - mean
    shape <- start[2]^2 *  (gamma(1 + 2/start[1]) - gamma(1+1/start[1])^2) - variance
    return (c(rate,shape))  
}

fnBeta <- function(start,mean,variance)
{
    rate <- start[1]/(start[1] + start[2]) - mean
    shape <- start[1]*start[2]/((start[1]+start[2])^2 (start[1]+start[2]+1)) - variance
    return(c(rate,shape))
}

fnLogNorm <- function(start,mean,variance)
{
    rate <- exp(start[1] + 1/2*start[2]^2) - mean
    shape <-  exp(2*start[1] + start[2]^2)*(exp(start[2]^2) - 1) - variance
    return(c(rate,shape))
}


fnGamma <- function(start,mean,variance)
{
    rate <- start[1]*start[2]- mean
    shape <-   start[1]*start[2]^2 - variance
    return(c(rate,shape))
}


dWeibullMeanVar <- function(x, mean, variance) 
{
  
    parametros <- nleqslv(c(1,1),fnWeibull,jac=NULL,mean,variance)$x
    return (dweibull(x,parametros[1],parametros[2]))
}

dBetaMeanVar <- function(x, mean, variance) 
{
    parametros <- nleqslv(c(1,1),fnBeta,jac=NULL,mean,variance)$x
    return (dweibull(x,parametros[1],parametros[2]))
  
}

dLognormMeanVar <- function(x, mean, variance) 
{
    parametros <- nleqslv(c(1,1),fnLogNorm,jac=NULL,log(mean),log(variance))$x #conferir se o uso do log está correto
    return (dweibull(x,parametros[1],parametros[2]))
  
}

dGammaMeanVar <- function(x, mean, variance) 
{
    parametros <- nleqslv(c(1,1),fnGamma,jac=NULL,mean,variance)$x #conferir se o uso do log está correto
    return (dweibull(x,parametros[1],parametros[2]))
  
}

#não encontrei a função na biblioteca padrão do R. Encontrei alguns pacotes, mas alguns se referem 
dHyperbolicMeanVar <- function(x, mean, variance) 
{
  
}
