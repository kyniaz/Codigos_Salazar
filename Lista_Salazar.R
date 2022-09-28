cria_intervalo_predicao = function(X, Y, x, p) {
  alfa = 1 - p
  ###coef
  modelo = lm(Y ~ X)
  coeficientes = coef(modelo)
  
  n = length(X)
  ypred = as.numeric(coeficientes%*%t(cbind(1, x)))
  sigmax = sqrt(sum((modelo$residuals)^2)/(n-2))*sqrt((n + 1)/n + ((x - mean(X))^2)/((n-1)*var(X)))
  
  L = ypred - qt(1-alfa/2, n - 2)*sigmax
  U = ypred + qt(1-alfa/2, n - 2)*sigmax
  
  out = data.frame(ypred, L, U)
  colnames(out) = c("Ypred", paste0(100*alfa/2, '%'), paste0(100*(1 - alfa/2), '%'))
  return(out)
}

####### REGIAO
cria_intervalo_tolerancia = function(X, Y, x, p, gama){
  n_sim = 1000
  n = length(X)
  alfa = 1 - p
  Z = rnorm(n_sim)
  U = rchisq(n_sim, n - 2)
  k = vector(mode = "numeric", length(x))
  
  for(i in 1:length(x)){
    xx = ((x[i] - mean(X))^2)/sum((X - mean(X))^2)
    
    cobertura = function(k) {
      SUPERIOR = pnorm(Z*sqrt(1 + 1/n + xx) + k*sqrt(U))
      INFERIOR = pnorm(Z*sqrt(1 + 1/n + xx) - k*sqrt(U))
      
      media = mean((SUPERIOR - INFERIOR) > p) - gama
      return(media)
    }
    
    k[i] = uniroot(cobertura, interval = c(0,100))$root 
  }

  
  modelo = lm(Y ~ X)
  coeficientes = coef(modelo)
  
  ypred = as.numeric(coeficientes%*%t(cbind(1, x)))
  sigmax = sqrt(sum((modelo$residuals)^2))
  
  L = ypred - k*sigmax
  U = ypred +  k*sigmax
  
  out = data.frame(ypred, L, U)
  colnames(out) = c("Ypred", "L", "U")
  
  return(out)
}

n = 1000
X = rgamma(n, 1, 1)
b0 = 1
b1 = 3
#erro = rnorm(n, sd = abs(X))
erro = rt(n, df = 4)

Y = b0 + b1*X + erro

novox = rgamma(10, 1, 1)
p = 0.95

cria_intervalo_predicao(X, Y, novox, 0.95)
modelo = lm(Y~X, data.frame(X, Y))
predict(modelo, newdata = data.frame(X = novox), interval = "prediction")

cria_intervalo_tolerancia(X, Y, novox, 0.95, 0.8)
