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
    shape <- (start[1]*start[2]) /( (start[1]+start[2])^2 *(start[1]+start[2]+1) ) - variance
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
#dist pode ser 'weibull', 'Beta', 'Lnorm', 'Gamma', 'Hyperbolic'(ainda não implementada)
fn <-function(start,mean,variance, dist='Weibull')
{
   return (switch(
        dist,
        "Weibull" = fnWeibull(start,mean,variance),
        "Beta" = fnBeta(start,mean,variance),
        "Lnorm" = fnLogNorm(start,mean,variance),
        "Gamma" = fnGamma(start,mean,variance)
    ))
}


dReparmetrized <- function(x,mean,variance, dist="Weibull")
{
    parametros <- nleqslv(c(1,1),fn,jac=NULL,mean,variance,dist)$x
    return (switch(
        dist,
        "Weibull" = dweibull(x,parametros[1],parametros[2]),
        "Beta" = dbeta(x,parametros[1],parametros[2]),
        "Lnorm" = dlnorm(x,parametros[1],parametros[2]),
        "Gamma" = dgamma(x,parametros[1],parametros[2])
    ))
}

#alguns testes para as funções implementadas
mean = 0.5
variance = 0.1

#weibull

parametros = nleqslv(c(1,1),fn,jac=NULL,mean,variance,"Weibull")$x
dweibull(0.56,parametros[1],parametros[2]) - dReparmetrized(0.56,mean,variance,"Weibull")

#Beta
parametros = nleqslv(c(1,1),fn,jac=NULL,mean,variance,"Beta")$x
dbeta(0.56,parametros[1],parametros[2]) - dReparmetrized(0.56,mean,variance,"Beta")

#Lnorm
parametros = nleqslv(c(1,1),fn,jac=NULL,mean,variance,"Lnorm")$x
dlnorm(0.56,parametros[1],parametros[2]) - dReparmetrized(0.56,mean,variance,"Lnorm")

#Gamma
parametros = nleqslv(c(1,1),fn,jac=NULL,mean,variance,"Gamma")$x
dgamma(0.56,parametros[1],parametros[2]) - dReparmetrized(0.56,mean,variance,"Gamma")

