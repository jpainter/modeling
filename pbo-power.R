
# pbo_power
library(rethinking)
library(dplyr)
library(tidyr)
library(lubridate)

# load clinics function from pbo.R

# test values
iter = 100L
seq.randomize = c("cluster", "district")
seq.num_districts = seq(2L, 20L, 18L) 
seq.population = seq(1000L,  5000L, 4000L) 
seq.n_clinics = seq(10L, 30L, 20L)
seq.bkrd_mean_incidence = seq(.1, .6, .5)
seq.effectiveness = seq(0.1, .5, .4)
seq.case.sensitivity = seq( .4, .8, .4)
seq.case.specificity = seq( .4, .8, .4)
seq.test.positive.rate = seq( .4, .8, .4)       
seq.reporting.fidelity = seq(.4, .9, .5)

sequences = c( "seq.randomize", "seq.num_districts", "seq.population", "seq.n_clinics", "seq.bkrd_mean_incidence", 
               "seq.effectiveness", "seq.case.sensitivity", "seq.case.specificity", 
               "seq.test.positive.rate", "seq.reporting.fidelity"  )

df.rows = iter * prod( sapply( sequences, function(x) length(get(x)) ) )
                               

trials = data_frame()[0,] # initialize data frame
iter = 100L
trial = 0L # initialize counter
for ( .randomize in seq.randomize ){
for ( .num_districts in seq.num_districts ){
for ( .population in seq.population ){
for ( .n_clinics in seq.n_clinics ){
   
for ( .bkrd_mean_incidence in seq.bkrd_mean_incidence ){   
for ( .effectiveness in seq.effectiveness ){
   for ( .case.sensitivity in seq.case.sensitivity ) {
      for ( .case.specificity in seq.case.specificity) {
         for ( .test.positive.rate in seq.test.positive.rate ) {
            for( .reporting.fidelity in seq.reporting.fidelity){
            
 
         trial  = trial  + 1
         
         cat( 'trial = ' = trial ,
            'r =',.randomize, 
              ' n_d =', .num_districts, 
              ' pop =', .population, 
              ' n_c =', .n_clinics, 
              ' incid =', .bkrd_mean_incidence, 
              ' eff =', .effectiveness , 
              ' sens =', .case.sensitivity, 
              ' spec =', .case.specificity,
              ' tpr =', .test.positive.rate, "\n", sep = "")  
            
         for ( i in 1L:iter ){
   
   # cat( i )  
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
               test.positive.rate = .test.positive.rate, 
               effectiveness = .effectiveness,
               period = .period 
       ) 
   d$i = i

   
   df = data_frame(
      trial = trial,
      i = i, 
      randomize = .randomize,
      period = .period , 
      num_districts = .num_districts,
      n_clinics = .n_clinics ,  
      bkrd_mean_incidence = .bkrd_mean_incidence,
      reporting_sensitivity = .reporting_sensitivity , #.6,
      reporting_specificity = .reporting_specificity , #.6,
      test.positive.rate = .test.positive.rate ,
      effectiveness = .effectiveness
   ) %>%  inner_join(d, by = c("i"="i", "period"="period"))
   
   if ( nrow(trials) == 0 ){ trials = df
   } else{
      trials = bind_rows( trials , df )
   }
   
   }
         
         }
         }}}}}}}}}}

save(trials, file = "trials.rda")


load("trials.rda")
power = data_frame()[0,]
p <- progress_estimated( max( trials$i ), min_time = 3)
# for (trial in 1:max( trials$trial )){
   for (.trial in 1:10){
for (.i in 1:max( trials$i )){
   
   p$pause(0.5)$tick()$print()
   
   d = trials %>% 
      filter( trial == .trial, i == .i, period == 'post') %>%
       select(
          population, cases, id, trt 
       ) %>%
      as.data.frame() 
   
   m1.list =
   alist( 
      cases ~ dbinom( population, p ),
      logit(p) <- a + bt * trt ,
      a ~ dnorm( 0, 10 ),
      bt ~ dnorm( 0 , 10 )
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
   
   df = as.data.frame( as.list(q) )
   df$trial = i
   
   if ( nrow(power) == 0 ){ power = df
   } else{
      power = bind_rows( power , df )
   }
}
}
save(power, file = "power_map.rda")
   
  # CHARTs
   
   load("power.rda")
   
   p = trials %>% inner_join( power )
   View( p %>%  count(n_clinics, reporting_sensitivity, reporting_specificity, effectiveness) )
   
   library(ggplot2)
   
   # effectiveness by sensitivity, averaged over all specificity, n_clinics
   
   power.sens = p %>% 
      filter( period %in% "post") %>%
      group_by( randomize, effectiveness, reporting_sensitivity, period ) %>%
         summarise(
            significant_pos = sum( X5. > 0 ) / n() ,
            notsig = sum( X5. < 0 & X95. > 0) / n() ,
            significant_neg = sum( X95. < 0 ) / n() ,
            median = median( X50. ),
            n = n()
         )  
   
      ggplot( power.sens, aes( x = effectiveness, y = median, group = reporting_sensitivity, color =  reporting_sensitivity)) +
         geom_line() +
         geom_point(  )
      
      power.sens %>% 
         gather( test, percent , 
                             -randomize, -period, -effectiveness, - median, -reporting_sensitivity, -n) %>%
         ggplot( aes( x = effectiveness, y = percent , 
                      group = reporting_sensitivity, color =  reporting_sensitivity)) +
         geom_line() +
         geom_point( ) +
         facet_grid( test ~ randomize )
      
   # effectiveness by specificity, averaged over all sensitivity, n_clinics
      power.spec = p %>% 
         group_by( effectiveness, reporting_specificity, period ) %>%
         summarise(
            significant_pos = sum( X5. > 0 ) / n() ,
            notsig = sum( X5. < 0 & X95. > 0) / n() ,
            significant_neg = sum( X95. < 0 ) / n() ,
            median = median( X50. ),
            n = n()
         ) 
      
      ggplot( power.spec, aes( x = effectiveness, y = median, group = reporting_specificity, color =  reporting_specificity)) +
         geom_line() +
         geom_point( )
      
    power.spec %>% 
       gather( test, percent , 
                             -randomize, -period, -effectiveness, - median, -reporting_sensitivity, -n) %>%
         ggplot( aes( x = effectiveness, y = percent , 
                      group = reporting_specificity, color =  reporting_specificity)) +
         geom_line() +
         geom_point( ) +
         facet_grid( test ~ period)
    
    # effectiveness by bkrd_mean_incidence
      power.incid = p %>% 
         group_by( effectiveness, bkrd_mean_incidence ) %>%
         summarise(
            significant_pos = sum( X5. > 0 ) / n() ,
            notsig = sum( X5. < 0 & X95. > 0) / n() ,
            significant_neg = sum( X95. < 0 ) / n() ,
            median = median( X50. ),
            n = n()
         ) 
      
    power.incid %>% 
       gather( test, percent , 
                             -randomize, -period, -effectiveness, - median, -reporting_sensitivity, -n) %>%
         ggplot( aes( x = effectiveness, y = percent , 
                      group = bkrd_mean_incidence, color =  bkrd_mean_incidence)) +
         geom_line() +
         geom_point( ) +
         facet_grid( test ~ .)
    
      
   # effectiveness by n_clinics, averaged over all sensitivity and specificity
      power.n = p %>% 
         group_by( effectiveness, n_clinics ) %>%
         summarise(
            significant_pos = sum( X5. > 0 ) / n() ,
            notsig = sum( X5. < 0 & X95. > 0) / n() ,
            significant_neg = sum( X95. < 0 ) / n() ,
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
      
      power.n %>% 
         gather( test, percent , 
                             -randomize, -period, -effectiveness, - median, -reporting_sensitivity, -n) %>%
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
 

hist(power$X5.)
hist(power$X50.)

hist(power2$X5.)
hist(power2$X50.)
