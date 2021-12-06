#importar data
liquid <- read.table(file = "D:/Cursos-capacitacion/2021-02/Clase-R-06-12-2021/liquidity.csv", 
                     header = TRUE, sep = ",")


#adjuntar la base de datos
attach(liquid)

#matriz de diseño
X <- liquid[ , c("AVGT","NTRAN","PRICE","SHARE","VALUE","DEBEQ")]

#matriz de correlación variables independientes
round(cor(X), 2)

View(liquid)

#ajuste Modelo (modelo final con intercepto)
Modelo <- lm(VOLUME ~ NTRAN + SHARE + I(SHARE^2), data = liquid)

summary(Modelo)


#ajuste Model_1 (Modelo sin el intercepto)
Model_1 <- lm(VOLUME ~ -1 + NTRAN + SHARE + I(SHARE^2), data = liquid)

summary(Model_1)


#diagnósticos
windows()
layout(matrix(c(1,2,3,4), 2, 2)) # optional 4 graphs/page 
plot(Model_1)

#las observaciones 37, 79, 85 son (posiblemente) ouliers
#la observación 122 es (posiblemente) influyente
#los outliers parecen desviar el supuesto de normalidad

#ajuste Model_1_sin_outliers (sin outliers de Model_1)
Model_1_sin_outliers <- lm(VOLUME ~ -1 + NTRAN + SHARE + I(SHARE^2), data = liquid[-c(37, 79, 85), ])

summary(Model_1_sin_outliers)


#diagnósticos
windows()
layout(matrix(c(1,2,3,4), 2, 2)) # optional 4 graphs/page 
plot(Model_1_sin_outliers)

#prueba de normalidad
shapiro.test(rstandard(Model_1_sin_outliers))
shapiro.test(rstandard(Model_1))

#parece haber desvíos de la distribución normal 
#(la colas de la distribución son pesadas)
#opciones:

#transformación (logaritmo, raiz cuadrada, Box-Cox)
#regresión robusta
#regresión rígida
#modelo lineal generalizado
#modelo no lineal



#ajuste Model_2 (transformación)
#En M5 ya no utilizamos outliers. 
M5 <- lm(sqrt(VOLUME) ~ NTRAN + SHARE + I(SHARE^2), data = liquid[-c(65, 79, 122), ])

summary(M5)

#Adjusted R-squared:      0.8479
#Residual standard error: 0.4914 
# sqrt(VOLUME) = 1.037 + 0.000396*NTRAN + 0.00794*SHARE 
#                - 9.59*10^-9*NTRAN^2-0.000053*SHARE^2
#diagnostico
windows()
layout(matrix(c(1,2,3,4), 2, 2)) # optional 4 graphs/page 
plot(M5)

#prueba de normales
shapiro.test(rstandard(M5))

#-----------------------------------------------


X2 <- liquid[ , c("AVGT","NTRAN","PRICE","SHARE","VALUE","DEBEQ", "VOLUME")]
round(cor(X2), 2)


M6 <- lm(VOLUME ~ NTRAN + VALUE, data = liquid)

summary(M6)

M7 <- lm(VOLUME ~ -1 + NTRAN + VALUE, data = liquid)

summary(M7)


M8 <- lm(VOLUME ~ -1 + NTRAN + I(VALUE^2), data = liquid)

summary(M8)


windows()
layout(matrix(c(1,2,3,4), 2, 2)) # optional 4 graphs/page 
plot(M8)


M8_sin_outliers <- lm(VOLUME ~ -1 + NTRAN + I(VALUE^2), data = liquid[-c(37, 79, 85), ])

summary(M8_sin_outliers)


windows()
layout(matrix(c(1,2,3,4), 2, 2)) # optional 4 graphs/page 
plot(M8_sin_outliers)
