
# pbo_power

library(lubridate)

# load clinics function from pbo.R

# test values
i=3
.population = 2000
.num_districts = 2
.n_clinics = 10
.effectiveness = 0
.bkrd_mean_incidence = .1
.reporting_sensitivity = .4
.reporting_specificity = .4
.percent.fever.not.malaria = .7                              
                               
iter = 5
trials = data_frame()[0,]

for ( .randomize in c("cluster", "district")){
for ( .num_districts in seq(2:2) ){
for ( .population in seq(1000,  5000, 4000)){
for ( .n_clinics in seq(10, 30, 20)){
   
for ( .bkrd_mean_incidence in seq(.1, .6, .5) ){   
for ( .effectiveness in seq(0, .5, .5) ){
   for ( .reporting_sensitivity in seq( .4, .8, .4)) {
      for ( .reporting_specificity in seq( .4, .8, .4)) {
         for ( .percent.fever.not.malaria in seq( .4, .8, .4)) {
            
         cat( 'r =',.randomize, 
              ' n_d =', .num_districts, 
              ' pop =', .population, 
              ' n_c =', .n_clinics, 
              ' incid =', .bkrd_mean_incidence, 
              ' eff =', .effectiveness , 
              ' sens =', .reporting_sensitivity, 
              ' spec =', .reporting_specificity,
              ' not_mal =', .percent.fever.not.malaria, "\n", sep = ";")  
            
         for ( i in 1:iter ){
   
   cat( i )  
   # progbar(i, 1, iter, update.interval = 10, starttime = now())
   
   # DATA
   c = clinics(
               randomize = .randomize ,
               num_districts = .num_districts ,
               n_clinics = .n_clinics  , # mean number of clinics.
               mean_population = .population 
            )
   c$i = i
   
   for ( .period in c("pre", "post") ){
   d = disease(c,
               
               bkrd_mean_incidence = .bkrd_mean_incidence  , # yearly total
               reporting_sensitivity = .reporting_sensitivity ,
               reporting_specificity = .reporting_specificity ,
               percent.fever.not.malaria = .percent.fever.not.malaria, 
               effectiveness = .effectiveness,
               period = .period 
       ) 
   d$i = i

   
   df = data_frame(
      i = i, 
      randomize = .randomize,
      period = .period , 
      n_clinics = .n_clinics ,  
      bkrd_mean_incidence = .bkrd_mean_incidence,
      reporting_sensitivity = .reporting_sensitivity , #.6,
      reporting_specificity = .reporting_specificity , #.6,
      percent.fever.not.malaria = .percent.fever.not.malaria ,
      effectiveness = .effectiveness
   ) %>%  inner_join(d, by = c("i"="i", "period"="period"))
   
   if ( nrow(trials) == 0 ){ trials = df
   } else{
      trials = bind_rows( trials , df )
   }
   
   }
         
         }
         }}}}}}}}}

save(trials, file = "trials.rda")


for (i in 1:row_number(trials)){
   
   d = trials[i,] %>% 
      as.data.frame() %>%
       select(
          population, cases, id, trt 
       )
   
   m1.list =
   alist( 
      cases ~ dbinom( population, p ),
      logit(p) <- a + bt * trt ,
      a ~ dnorm( 0, 20 ),
      bt ~ dnorm( 0 , 1 )
      )
   # m1 = map( m1.list, data = d) 
   m1 = try( map( m1.list, data = d) )
   
   if ( class(m1) == "try-error" ) {
      print("try-error" )
      next
   }
   
   # precis(m1)
   
   post <- extract.samples( m1 )
   eff = (1 - exp(post$bt)) * 100
   options(digits = 3)
   q  = quantile(eff, probs = c(0, .05, .1 , .5, .9, .95, 1))
   
   power = trials[i, ] %>% 
      bind_cols(
         as.data.frame( as.list(q) )
      )
}
   
save(power, file = "power.rda")
   
  # CHARTs
   
   load("power.rda")
   
   View( power %>%  count(n_clinics, reporting_sensitivity, reporting_specificity, effectiveness) )
   
   library(ggplot2)
   
   # effectiveness by sensitivity, averaged over all specificity, n_clinics
   
   power.sens = power %>% 
      group_by( randomize, effectiveness, reporting_sensitivity, period ) %>%
         summarise(
            significant_pos = sum( X2.5. > 0 ) / n() ,
            notsig = sum( X2.5. < 0 & X97.5. > 0) / n() ,
            significant_neg = sum( X97.5. < 0 ) / n() ,
            median = median( X50. ),
            n = n()
         )  
   
      ggplot( power.sens, aes( x = effectiveness, y = median, group = reporting_sensitivity, color =  reporting_sensitivity)) +
         geom_line() +
         geom_point(  )
      
      power.sens %>% gather( test, percent , -period, -effectiveness, - median, -reporting_sensitivity, -n) %>%
         ggplot( aes( x = effectiveness, y = percent , 
                      group = reporting_sensitivity, color =  reporting_sensitivity)) +
         geom_line() +
         geom_point( ) +
         facet_grid( test ~ period + randomize)
      
   # effectiveness by specificity, averaged over all sensitivity, n_clinics
      power.spec = power %>% 
         group_by( effectiveness, reporting_specificity, period ) %>%
         summarise(
            significant_pos = sum( X2.5. > 0 ) / n() ,
            notsig = sum( X2.5. < 0 & X97.5. > 0) / n() ,
            significant_neg = sum( X97.5. < 0 ) / n() ,
            median = median( X50. ),
            n = n()
         ) 
      
      ggplot( power.spec, aes( x = effectiveness, y = median, group = reporting_specificity, color =  reporting_specificity)) +
         geom_line() +
         geom_point( )
      
    power.spec %>% gather( test, percent , -period, -effectiveness, - median, -reporting_specificity, -n) %>%
         ggplot( aes( x = effectiveness, y = percent , 
                      group = reporting_specificity, color =  reporting_specificity)) +
         geom_line() +
         geom_point( ) +
         facet_grid( test ~ period)
    
    # effectiveness by bkrd_mean_incidence
      power.incid = power %>% 
         group_by( effectiveness, bkrd_mean_incidence ) %>%
         summarise(
            significant_pos = sum( X2.5. > 0 ) / n() ,
            notsig = sum( X2.5. < 0 & X97.5. > 0) / n() ,
            significant_neg = sum( X97.5. < 0 ) / n() ,
            median = median( X50. ),
            n = n()
         ) 
      
    power.incid %>% gather( test, percent , -effectiveness, - median, 
                           -bkrd_mean_incidence, -n) %>%
         ggplot( aes( x = effectiveness, y = percent , 
                      group = bkrd_mean_incidence, color =  bkrd_mean_incidence)) +
         geom_line() +
         geom_point( ) +
         facet_grid( test ~ .)
    
      
   # effectiveness by n_clinics, averaged over all sensitivity and specificity
      power.n = power %>% 
         group_by( effectiveness, n_clinics ) %>%
         summarise(
            significant_pos = sum( X2.5. > 0 ) / n() ,
            notsig = sum( X2.5. < 0 & X97.5. > 0) / n() ,
            significant_neg = sum( X97.5. < 0 ) / n() ,
            median = median( X50. ),
            n = n()
         ) 
      
      ggplot( power.n, aes( x = effectiveness, y = significant_pos, group = n_clinics, color =  n_clinics)) +
         geom_line() +
         geom_point( )
      
      ggplot( power.n, aes( x = effectiveness, y = notsig, group = n_clinics, color =  n_clinics)) +
         geom_line() +
         geom_point( )
      
      ggplot( power.n, aes( x = effectiveness, y = significant_neg, group = n_clinics, color =  n_clinics)) +
         geom_line() +
         geom_point( )
      
      ggplot( power.n, aes( x = effectiveness, y = significant_pos / significant_neg, 
                            group = n_clinics, color =  n_clinics)) +
         geom_line() +
         geom_point( )
      
      ggplot( power.n, aes( x = effectiveness, y = median, group = n_clinics, color =  n_clinics)) +
         geom_line() +
         geom_point( )
      
      power.n %>% gather( test, percent , -effectiveness, - median, -n_clinics, -n) %>%
         ggplot( aes( x = effectiveness, y = percent , group = n_clinics, color =  n_clinics)) +
         geom_line() +
         geom_point( ) +
         facet_grid( test ~ .)
   
 #   m2 = 
 #   alist( 
 #      cases ~ dbinom( population, p ),
 #      logit(p) <- a[id] + bt * trt ,
 #      a[id] ~ dnorm( 0, 10 ),
 #      bt ~ dnorm( 0 , 10)
 #      
 #      )
 # 
 #   m2map = try( map( m2, data = d) )
 #   
 #   if ( class(m2map) == "try-error" ) next
 #   
 # # precis(m2map)
 #   
 #   post <- extract.samples( m2map )
 #   eff = (1 - exp(post$bt)) * 100
 #   options(digits = 3)
 #   q  = quantile(eff, probs = c(0, .025, .1 , .5, .9, .975, 1))
 #   
 #   
 #   if (i==1){ power2 = data.frame( as.list(q) )
 #   } else{
 #      power2 = bind_rows( power2 , data.frame( as.list(q) ) )
 #   }
 

hist(power$X2.5.)
hist(power$X50.)

hist(power2$X2.5.)
hist(power2$X50.)
