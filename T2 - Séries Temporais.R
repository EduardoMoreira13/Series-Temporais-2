# TRABALHO 2 - SÉRIES TEMPORAIS - Grupo 2


# PACOTES UTILIZADOS E SÉRIE TEMPORAL ESCOLHIDA ----

library(Mcomp)
library(forecast)
library(tidyverse)

data(M3)
id=2539 # id da série escolhida

horizonte <- M3[[id]]$h
serie_temp <- M3[[id]]$x
out_sample <- M3[[id]]$xx

M3[[id]]$description # descrição da série

M3[[id]]$type
M3[[id]]$period
M3[[id]]$n


# Gráfico da Série
serie_temp %>% autoplot() + 
  geom_line(color = "#1f0866", linewidth = 0.8) +
  xlab("Anos") +
  ylab("Depósitos Totais (bilhões de dólares)") +
  scale_x_continuous(breaks = seq(from = 1983,to = 1993,by = 1), 
                     limits = c(1983,1993)) +
  scale_y_continuous(breaks = seq(from = 4000,to = 10000,by = 1000), 
                     limits = c(4000,10000)) +
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme_minimal()

serie_temp %>% plot()  # opção mais simples



# DECOMPOSIÇÃO DA SÉRIE TEMPORAL VIA MSTL ----

mstl(serie_temp, s.window = "periodic", lambda = 0) %>% plot()

mstl(serie_temp, s.window = "periodic") %>% autoplot() +
  geom_line(color = c("#1f0866"), linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 1983,to = 1993,by = 1), 
                     limits = c(1983,1993)) +
  theme_minimal()



# MODELO ARIMA - SEM TRANSFORMAÇÃO DE BOX-COX ----

# verificando estacionariedade
tseries::kpss.test(serie_temp)

# Operador de diferença (tendencia e sazonalidade)
ndiffs(serie_temp) # 1 diferença   d = 1
diff(serie_temp) %>% nsdiffs() # 0 diferença  D = 0


# SARIMA(p,1,q)x(P,0,Q)
x <- diff(serie_temp) # Série estacionária
par(mfrow=c(1,3))
plot(x);acf(x,lag.max = 12*5);pacf(x, lag.max = 12*5)

tseries::kpss.test(x)

par(mfrow=c(1,1))

# VERIFICANDO MODELOS ADEQUADOS VIA CRITÉRIO AIC CORRIGIDO
melhor_AICc = Inf
for(P in 0:2){
  for(Q in 0:2){
    for(p in 0:3){
      for(q in 0:3){
        fit = Arima(serie_temp, order = c(p,1,q), seasonal = c(P,0,Q))
        if(fit$aicc < melhor_AICc){
          melhor_AICc = fit$aicc
          cat("p =",p,",q =",q,"P =",P,",Q =",Q,
              ",AICc =",fit$aicc, "\n")
        }
      }
    }
  }
  }


# ANÁLISE DE RESÍDUOS DO MODELO 1 ARIMA

# Modelo SARIMA(1,1,3)x(1,0,1)
fit = Arima(serie_temp, order=c(1,1,3), seasonal=c(1,0,1))
fit

# Resíduos
par(mfrow=c(1,2))
E_1 <- fit$residuals
plot(E_1)
E <- fit$residuals %>% window(start=c(1984,2))
plot(E)

# gráficos
par(mfrow=c(1,3))
plot(E)
qqnorm(E); qqline(E)
acf(E, lag.max = 12*5)
hist(E)

# verificando estacionariedade
tseries::kpss.test(E)

# verificando independência
Box.test(E,lag = 15, type = "Ljung-Box")
Box.test(E,lag = 20, type = "Ljung-Box")

# Normalidade dos resíduos
shapiro.test(E)

# Horizonte de previsão
par(mfrow=c(1,1))
prev = forecast(fit, horizonte, level = 95, bootstrap = T)
prev

library(zoo)
d1 = out_sample %>% as.numeric()
d2 = as.yearmon(time(out_sample))

prev %>% autoplot() +
  geom_line(linewidth = 0.8) +
  xlab("Anos") +
  ylab("Depósitos Totais (bilhões de dólares)") +
  scale_x_continuous(breaks = seq(from = 1983,to = 1995,by = 1), 
                     limits = c(1983,1995)) +
  scale_y_continuous(breaks = seq(from = 4000,to = 13000,by = 1000), 
                     limits = c(4000,13000)) +
  theme(axis.text.x=element_text(angle=0, hjust=1)) + 
  geom_line(aes(y = d1,x = d2), 
            linewidth = 0.4, color = "#bf0d1c") +
  theme_minimal()


MAE <- mean(abs(out_sample - prev$mean))
MAE


# MODELO ARIMA - TRANSFORMAÇÃO DE BOX-COX ----

lambda_auto = serie_temp %>% BoxCox.lambda() 
lambda_auto

serie_temp_boxcox = serie_temp %>% BoxCox(lambda_auto)
serie_temp_boxcox

serie_temp_boxcox %>% plot()
serie_temp %>% plot()

tseries::kpss.test(serie_temp_boxcox)
mstl(serie_temp_boxcox, s.window = "periodic") %>% plot()

# Gráfico da Série
serie_temp_boxcox %>% autoplot() + 
  geom_line(color = "#1f0866", linewidth = 0.8) +
  xlab("Anos") +
  ylab("Depósitos Totais (bilhões de dólares)") +
  scale_x_continuous(breaks = seq(from = 1983,to = 1993,by = 1), 
                     limits = c(1983,1993)) +
  scale_y_continuous(breaks = seq(from = 3.6,to = 3.71,by = 0.01), 
                     limits = c(3.6,3.71)) +
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme_minimal()

# Operador de diferença (tendencia e sazonalidade)
ndiffs(serie_temp_boxcox) # 1 diferença   d = 1
diff(serie_temp_boxcox) %>% nsdiffs() # 0 diferença  D = 0


# SARIMA(p,1,q)x(P,0,Q)
x <- diff(serie_temp_boxcox) # Série estacionária
par(mfrow=c(1,3))
plot(x);acf(x,lag.max = 12*5);pacf(x, lag.max = 12*5)
tseries::kpss.test(x)

# VERIFICANDO MODELOS ADEQUADOS VIA CRITÉRIO AIC CORRIGIDO
melhor_AICc = Inf
for(P in 0:2){
  for(Q in 0:2){
    for(p in 0:3){
      for(q in 0:3){
        fit = Arima(serie_temp, order = c(p,1,q), seasonal = c(P,0,Q),
                    lambda = "auto")
        if(fit$aicc < melhor_AICc){
          melhor_AICc = fit$aicc
          cat("p =",p,",q =",q,"P =",P,",Q =",Q,
              ",AICc =",fit$aicc, "\n")
        }
      }
    }
  }
}


# ANÁLISE DE RESÍDUOS DO MODELO 2 ARIMA

# Modelo SARIMA(1,1,2)x(0,0,2)
fit = Arima(serie_temp, order=c(1,1,2), seasonal=c(0,0,2),
            lambda = "auto")
fit

# Resíduos
par(mfrow=c(1,2))
E_1 <- fit$residuals
plot(E_1)
E <- fit$residuals %>% window(start=c(1983,2))
plot(E)

# gráficos
par(mfrow=c(1,3))
plot(E)
qqnorm(E); qqline(E)
acf(E, lag.max = 12*5)
hist(E)

# verificando estacionariedade
tseries::kpss.test(E)

# verificando independência
Box.test(E,lag = 15, type = "Ljung-Box")
Box.test(E,lag = 20, type = "Ljung-Box")

# Normalidade dos resíduos
shapiro.test(E)

# Horizonte de previsão

par(mfrow=c(1,1))
prev = forecast(fit, horizonte, level = 95)
prev

library(zoo)
d1 = out_sample %>% as.numeric()
d2 = as.yearmon(time(out_sample))

prev %>% autoplot() +
  geom_line(linewidth = 0.8) +
  xlab("Anos") +
  ylab("Depósitos Totais (bilhões de dólares)") +
  scale_x_continuous(breaks = seq(from = 1983,to = 1995,by = 1), 
                     limits = c(1983,1995)) +
  scale_y_continuous(breaks = seq(from = 4000,to = 13000,by = 1000), 
                     limits = c(4000,13000)) +
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  geom_line(aes(y = d1,x = d2), 
            linewidth = 0.4, color = "#bf0d1c") +
  theme_minimal()

MAE <- mean(abs(out_sample - prev$mean))
MAE


# Eduardo Moreira
data(M3)
id=2539 # id da série escolhida

horizonte <- M3[[id]]$h
serie_temp <- M3[[id]]$x
out_sample <- M3[[id]]$xx

lambda_auto = serie_temp %>% BoxCox.lambda() 
lambda_auto

serie_temp_boxcox = serie_temp %>% BoxCox(lambda_auto)
serie_temp_boxcox

fit = ets(serie_temp,model = "MAM", damped = T)
fit

fit = ets(serie_temp,model = "AAA", damped = T, lamda = "auto")
fit

par(mfrow=c(1,1))
prev = forecast(fit, horizonte, level = 95, bootstrap = T)
prev

library(zoo)
d1 = out_sample %>% as.numeric()
d2 = as.yearmon(time(out_sample))

prev %>% autoplot() +
  geom_line(linewidth = 0.8) +
  xlab("Anos") +
  ylab("Depósitos Totais (bilhões de dólares)") +
  scale_x_continuous(breaks = seq(from = 1983,to = 1995,by = 1), 
                     limits = c(1983,1995)) +
  scale_y_continuous(breaks = seq(from = 4000,to = 15000,by = 1000), 
                     limits = c(4000,15000)) +
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  geom_line(aes(y = d1,x = d2), 
            linewidth = 0.4, color = "#bf0d1c") +
  theme_minimal()

MAE <- mean(abs(out_sample - prev$mean))
MAE























