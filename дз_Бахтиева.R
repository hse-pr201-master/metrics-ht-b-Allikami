"
Hometask B
Бахтиева Камилла БЭК172
"
library(fable)
library(tidyverse)
library(feasts)
library(fasster)
library(forecast)
library(lubridate) 
library(tsibble) 
library(rio)
library(corrplot)
library(texreg)
library(car)
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
set.seed(1000)
data = read.csv('country_profile_variables.csv')
data
"
Сформулируйте исследовательский вопрос. В соответствии с ним выберите непрерывную зависимую переменную. 
Заметим, что в серьёзных научных работах выбор следует объяснять ссылкой
на литературу.
"

"
В датасете данные типа cross-section с экономическими, социальными и общими 
характеристиками разных стран. Фокус своего исследования
я сделаю на экономических характеристиках, а именно: как влияет структура
экономики страны на её экономическое развитие. 
Этот вопрос интересует многих экономистов, а одним из самых ярких примеров
таких исследований можно назвать, я считаю, статьи о ресурсном проклятии (низких темпах роста экономики 
у стран, специализирующихся на добыче (не переработке) ресурсов).
Однако я добавлю и некоторые контрольные переменные - другие характеристики помимо темпов роста ВВП
и долей отраслей в добавленной стоимости.
"

"Выберите и/или создайте объясняющие переменные. Итоговая матрица
регрессоров должна включать:
Не менее одной непрерывной переменной
Не менее одной бинарной переменной
Не менее одной нелинейной переменной
"
"
Я собираюсь использовать по большей части экономические индикаторы. 

"
data_primary = data[1:22]
#большая часть переменных будет непрерывной (темп роста ВВП, 
#ВВП на душу населения, доли отраслей, участие в рабочей силе)

colnames(data_primary)

#Бинарные переменные: специализация в какой-то отрасли (доля выше 50%)
data_primary['industrys'] = (data_primary['Economy..Industry....of.GVA.'] >= 50) * 1
data_primary['services'] = (data_primary['Economy..Services.and.other.activity....of.GVA.'] >= 50) * 1

h1 <- hist(data_primary$industrys, main = 'Гистограмма стран,
           специализирующихся в промышленности', sub = '1 - Специализируются в промышленности' )
h1
h2 <- hist(data_primary$services , main = 'Гистограмма стран,
           специализирующихся в услугах',
           sub = '1 - Специализируются в услугах')

#Нелинейные переменные
data_primary$gdpgrowr = as.numeric(data_primary$gdpgrowr)
hist(data_primary$gdpgrowr, main = 'Гистограмма темпов роста реального ВВП')
summary(data_primary$gdpgrowr)
typeof(data_primary$gdpgrowr)
data_primary$gdpgrowr[163]
typeof(data['GDP.growth.rate..annual....const..2005.prices.'])

data$GDP.growth.rate..annual....const..2005.prices.[163]
data_pr = filter(data_primary, data_primary$gdpgrowr != -~0.0)
data_pr <- data[-c(163), ]
data_pr$GDP.growth.rate..annual....const..2005.prices.
as.numeric(data_pr$GDP.growth.rate..annual....const..2005.prices.)
dr = apply(data, c(1,2), as.numeric)
dr
data
df = data[3:22]
colnames(df)[colnames(df) == 'Population.in.thousands..2017.'] <- 'populth'
colnames(df)[colnames(df) == 'GDP..Gross.domestic.product..million.current.US..'] <- 'gdp'
colnames(df)[colnames(df) == 'GDP.growth.rate..annual....const..2005.prices.'] <- 'gdpgrowr'
colnames(df)[colnames(df) == 'Economy..Industry....of.GVA.'] <- 'indfr'
colnames(df)[colnames(df) == 'Economy..Agriculture....of.GVA.'] <- 'agricfr'
colnames(df)[colnames(df) == 'Economy..Services.and.other.activity....of.GVA.'] <- 'servfr'
colnames(df)[colnames(df) == 'Employment..Agriculture....of.employed.'] <- 'empagr'
colnames(df)[colnames(df) == 'Employment..Industry....of.employed.'] <- 'empind'
colnames(df)[colnames(df) == 'Employment..Services....of.employed.'] <- 'empser'
colnames(df)[colnames(df) == 'International.trade..Exports..million.US..'] <- 'export'
colnames(df)[colnames(df) == 'International.trade..Imports..million.US..'] <- 'import'
colnames(df)
df$gdpgrowr = apply(as.matrix(df$gdpgrowr), 2, as.numeric)
hist(df$gdp, main = 'Гистограмма ВВП в миллионах $', breaks = 200)
# Распределение очень несбалансированное - попробуем преобразовать через логарифмирование
df['lgdp'] = log(df$gdp)

hist(df$lgdp, main = 'Гистограмма логарифма ВВП в миллионах $', , breaks = 200)
# данные более сбалансированны (в силу логарифмирования автоматически отсеялись наблюдения с нулевым 
#показателем gdp)

hist(df$gdpgrowr, main = 'Гистограмма темпов роста ВВП', breaks = 200)
#есть много выбросов с видимо пропущенным значением, замененным на -99, которые лучше удалить при анализе.

#Здесь пало моё рвение к этой части работы

#####################

"
Табалуга и река времени
"
# Сгенерируйте временные ряды, задающиеся следующими уравнениями
n <- 120
#y_t = 0.8*y_t-1 + e_t
y_1 <- arima.sim(n = n, list(ar = c(0.8))) #AR(1)
graph1 <- ts.plot(y_1, col = 'blue') + title(main = 'График процесса AR(1)')
#Стационарность:
# (1-0.8L)y_t = e_t, 1- 0.8z = 0, z = 10/8 = 1.25 > 1 - есть не заглядывающее в будущее стационарное решение


#y_t = 0.1*y_t-1 + 0.2*y_t-2 + 0.3*y_t-3 + e_t
y_2 <- arima.sim(n = n, list(ar = c(0.1, 0.2, 0.3))) #AR(3)
graph2 <- ts.plot(y_2, col = 'red') + title(main = 'График процесса AR(3)')
#Стационарность:
#(1 - 0.1L - 0.2L^2 - 0.3L^3)y_t = e_t
#z_1 = 1.238, z_2 = -0.952 + 1.336i, z_3 = -0.952 - 1.336i
#нет равных 1 корней, значит стационарное решение (заглядывающее в будущее или прошлое) существует

#y_t = e_t + 1.2e_t-1 + 2e_t-2
y_3 <- arima.sim(n = n, list(ma = c(1.2, 2))) #MA(2)
graph3 <- ts.plot(y_3, col = 'orange') + title(main = 'График процесса MA(2)')
#MA процессы стационарны (линейная комбинация стационарных процессов)


"
На основе предыдущего пункта, сгенерируйте временные ряды, уравнения которых
специфицируется как ARIMA(0, 1, 2), ARIMA(0, 0, 0), ARIMA(3, 0, 0). 
Постройте графики и прокомментируйте,
имеют ли соответствующие уравнения стационарные решения.
"
#ARIMA(0, 1, 2)
arima_one <- arima.sim(n = 120, model=list(ma = c(0.1, 0.3), order= c(0, 1, 2)))
graph_one <- ts.plot(arima_one, col = 'blue') + title(main = 'График процесса ARIMA(0, 1, 2)')
#Сам процесс нельзя судя по графику назвать стационарным (виден тренд)
#судя по коэффициентам модели можно говорить о стационарности процесса в первых разностях.

#ARIMA(0, 0, 0)
arima_two <- arima.sim(n = 120, model=list(order=c(0, 0, 0)))
graph_two <- ts.plot(arima_two, col = 'red') + title(main = 'График процесса ARIMA(0, 0, 0)')
#Судя по графику процесс стационарен, судя по коэффициентам  тоже - просто белый шум

#ARIMA(3, 0, 0)
arima_three <- arima.sim(n = 120, model=list(ar = c(-0.1, 0.16, 0.52), order=c(3, 0, 0)))
graph_three <- ts.plot(arima_three, col = 'orange') + title(main = 'График процесса ARIMA(3, 0, 0)')
#Судя по графику процесс стационарен (как и по коэффициентам) - процесс должен иметь порядок
#интегрированности, равный 0.

"
Вспомните уравнение случайного блуждания. Сгенерируйте соответствующий временной ряд
и постройте его график. Имеет ли это уравнение стационарные решения?
"
#https://campus.datacamp.com/courses/time-series-analysis-in-r/predicting-the-future?ex=11
rand_walk <- arima.sim(n = 120, model = list(order=c(0, 1, 0)))
graph_rw <- ts.plot(rand_walk, col = 'red') + title(main = 'График случайного блуждания')                        
#В первых разностях это стационарный процесс ARMA(0, 0)
#уравнение случайного блуждания не имеет стационарного решения (есть единичный корень)

"
Из созданных выше рядов выберите ряд, задаваемый моделью AR(1), уравнение которого име-
ет стационарные решения. Постройте автокорреляционную и частную автокорреляционную функции
для этого ряда. Сравните их с ACF и PACF случайного блуждания. Прокомментируйте результаты.
"
#Это ряд y_1 - стационарный AR(1)

acf_1 <- acf(y_1, plot = FALSE, type = c('correlation'))
plot(acf_1, main = 'ACF для стационарного AR(1) процесса')
#мы видим, что автокорреляционная функция убывает

acf_2 <- acf(rand_walk, plot = FALSE)
plot(acf_2, main = 'ACF для процесса случайного блуждания')
#ACF также убывает, но со значительно меньшей скоростью, чем у  стационарного AR(1)

pacf_1 <- pacf(y_1, plot = FALSE)
plot(pacf_1, main = 'PACF для стационарного AR(1) процесса')
#все значения PACF, кроме первого, неотличимы от 0 (справедливо для стационарного AR(1))

pacf_2 <- pacf(rand_walk, plot = FALSE)
plot(pacf_2, main = 'PACF для процесса случайного блуждания')
#как и для стационарного AR(1), для процесса случайного блуждания PACF не отличим от нуля для порядка 2 и выше,
#но первое её значение намного больше, чем для стационарного AR(1) (неудивительно, ведь они соответствуют
# коэффициентам в уравнениях - 0,8 и 1)

"
Сгенерируйте ряд из 120 наблюдений, задаваемый моделью ARIMA(2, 0, 3).
"
arima203 <- arima.sim(n = 120, model = list(ar=c(-0.1, 0.2), ma=c(0.3, 0.4, 0.15)))

"
Разделите ряд на обучающую выборку из 100 наблюдений и тестовую выборку из 20 наблюде-
ний.
"
train = arima203[1:100]
test = arima203[101:120]

"
Оцените модель ARIMA(2, 0, 3) на обучающей выборке.
"
model_arima = arima(train, order=c(2, 0, 3))
fcst = predict(model_arima, 20)
upper <- fcst$pred + 1.96 * fcst$se
lower <- fcst$pred - 1.96 * fcst$se
tspl <-ts.plot(lower, test, upper, fcst$pred, gpars=list(xlab = 'time', 
                                              ylab = 'value', lty = c(1:4))) + title(
                                  main = 'Прогнозный интервал и 
                                  реальные значения временного ряда')
#Качество прогноза не очень хорошее - 
