# Imagine functions Testing - 

source("imagine_functions.R")

# DATA
c = clinics(randomize = "clinic", n_clinics = 10)
c %>% group_by(district, intervention) %>% summarise( n = n())

# View(c)

## data for randomize ####
d = study_data( design = 'randomized')

d %>% group_by( period, intervention) %>% summarise( 
   n = n(),
   pop = sum(population), 
   total.cases = sum(cases),
   mean.cases = mean(cases)
) %>%
   mutate(
      dif = percent( ( lag(mean.cases, n = 1) - mean.cases ) / lag(mean.cases, n = 1) )
   )
   
g = ggplot( data = d ) +
   geom_boxplot( aes( x = factor(period), y = cases, group = period)) +
   labs( x = 'Period', y = 'Cases') +
   theme_tufte() +
   theme( axis.ticks = element_blank() )
g

# glm model
linmod = glm( cases ~ trt, family = poisson, data = d)
post <- extract.samples( linmod )
e = (1 - exp(post$trt)) * 100
plotPost( e)

e = effect(design = 'randomized', d = d )
plotPost( e)

   # MAP model
   m1 = alist(
      cases ~ dbinom( population, p ),
      logit(p) <- a + bt * trt ,
      a ~ dnorm(0, 10),
      bt ~ dnorm(0, 10)
   )

   data = d %>% as.data.frame() %>%
       select(
         population, cases, trt
      )

   eff =   map( m1,
                start=list( a= 0, bt=0),
                data = data
   )

   # extract posterior; calculate effect
   post <- extract.samples( eff )
   effect = (1 - exp(post$bt)) * 100
   plotPost( effect )

# imagine_function
e = effect( d = d , .analysis = 'randomized')
plotPost( e )





## data for pre-post design ####
d = bind_rows( disease(c, .period = 'pre', effectiveness = .25),
               disease(c, .period = 'post', effectiveness = .25) ) %>%
   mutate(
               time = c(0,1)[ factor( period, levels = c("pre", "post") )]
            )

d = study_data( design = 'pre-post')

d %>% group_by( intervention, period) %>% summarise( 
   n = n(),
   pop = sum(population), 
   total.cases = sum(cases),
   mean.cases = mean(cases)
   ) %>%
   arrange( intervention, desc(period) ) %>%
   mutate(
      dif = percent( ( lag(mean.cases, n = 1) - mean.cases ) / lag(mean.cases, n = 1) )
   )

# glm model
linmod = glm( cases ~ trt + time + trt*time, family = poisson, data = d)
summary(linmod)
post <- extract.samples( linmod )
e = (1 - exp(post$`trt:time`)) * 100
plotPost( e )

# imagine_function
e = effect( d = d , .analysis = 'pre-post')
plotPost( e )




## interrupted time-series ####

c = clinics(randomize = "clinic", n_clinics = 16)
d = study_data( design = "interrupted time-series", case.specificity = .5,
                effectiveness = .2 , season = 2)
# View(d)
d %>% group_by( intervention, period) %>% summarise( 
   n = n(),
   pop = sum(population), 
   total.cases = sum(cases),
   mean.cases = mean(cases)
   ) %>%
   arrange( intervention, desc(period) ) %>%
   mutate(
      dif = percent( ( lag(mean.cases, n = 1) - mean.cases ) / lag(mean.cases, n = 1) )
   )

ggplot( data = d ) +
   geom_boxplot( aes( x = period, y = cases, group = period)) +
   facet_grid( trt ~ .) +
   theme_tufte()

# aggregate and split into trt groups
dd = d %>% group_by(period, trt) %>% summarise( cases = mean(cases))
dc = dd %>% as.data.frame() %>% filter(trt == 0) %>% select(cases)
dt = dd %>% as.data.frame() %>% filter(trt == 1) %>% select(cases)

dts = ts(dt, start=c(0,-12), end=c(0,12), frequency = 12)
plot(dts)

# install.packages("devtools")
devtools::install_github("robjhyndman/forecast")
library(forecast)
auto.arima(dts)
acf(dts)
pacf(dts)
mod1 = arima(dts)
acf(mod1$residuals)
pacf(mod1$residuals)


fit <- auto.arima(dts, lambda=0, d=0, D=1)
plot(forecast(fit))

dcs = ts(dc, start=c(0,-12), end=c(0,12), frequency = 12)
plot(dcs)

# Seasonal decomposition
fit.t <- stl(dts, s.window="period")
plot(fit.t)
trend = as.data.frame(fit.t$time.series)
trend$id = as.integer(rownames(trend))
m =  lm( trend ~ id, trend)
post <- extract.samples( m )
effect = 1 - (post$Intercept +  24 * post$id )/ post$Intercept


effect = effect( d, design = "interrupted time-series")
plotPost( effect )


fit.c <- stl(dcs, s.window="period")
plot(fit.c)












d.sum = d %>% group_by(trt, period ) %>% arrange(trt, desc(period) ) %>%
  summarise(
     `no.clinics` = n(),
     `population` = mean(population),
     `bkrnd incidence` = mean(incidence),
     `sensitivity`= mean(sensitivity),
     `specificity`= mean(specificity),
     `cases`= round( mean(cases), 1 ) )
View(d.sum)

plot( cases ~ factor(trt), data = d,
      main = "Cases by treatment group", theme.o = 3, point.o = .7, hdi.o = .5,
      bar.o = 0 )

library(lattice)
d$trt.f = factor(d$trt, levels = 0:1, labels = c('control', 'treatment'))
d$period.f = factor(d$period, levels = c("pre", "post"))
bwplot( cases ~ period.f|trt.f, data = d,
        main = "Cases by treatment group",
        layout=(c(1,2)) )

e = effect( d = d , .analysis = 'randomized')
plotPost( e )

e = effect( d = d , .analysis = 'pre-post')
plotPost( e )


# summarise clinics
d %>% summarise( n_clinics = n() )
d %>% group_by(trt) %>% summarise( cases = mean(cases))


# pirate plots
# install.packages("devtools") # Only if you don't have the devtools library already installed
# library("devtools")
# install_github("ndphillips/yarrr")