
# functions for the Imagine shiny app

library(ggthemes)
library(ggplot2)
library(scales)
library(rethinking)
library(dplyr)
library(tidyr)
library(BEST) # for posterior plot

intervention = function(
   .clinics = clinics, 
   .randomize = randomize) {
   
   clinics = .clinics$clinic
   districts = .clinics$district
   
   if ( .randomize == "district") {
         
         n = floor( length( unique(districts) ) / 2)
         r = sample(districts, n)
         intervention = ifelse( grepl( paste0(r, collapse = "|")  , districts) , 1L, 0L )

   } else if ( .randomize == "clinic") {
   
         n = floor( length( unique(clinics)) / 2)
         r = sample( unique(clinics), n, replace = FALSE)
         intervention = ifelse( clinics %in% r , 1L, 0L )
   
   } else NA
   
   return(intervention)
}
      
clinics = function(
   randomize = "clinic" ,
   num_districts = 2  ,
   n_clinics = 30  , # mean number of clinics.
   mean_population = 2500L ,
   var_clinics = FALSE,
   var_pop = FALSE
   ){
   
   # trt = function(district) district %in% district_treatment
   
   districts = LETTERS[1:num_districts]
   
   atLeast = function(x, min){
      ifelse( x < min, min, x)
   } 
   
   district_list = data_frame( 
         district = unname(unlist(
            lapply( districts, FUN = function(x){
            rep( x, 
                 ifelse( var_clinics, 
                         atLeast( as.integer( rnorm(1, n_clinics, n_clinics/3) ),
                                  2),
                 
                 n_clinics
               )
               )
            } )
            ))
         )
   
   clinics_list = district_list  %>% 
      group_by( district ) %>%
      mutate(
         clinic =  as.character( row_number() ),
         district_clinic = paste0( district, clinic) ,
         population = ifelse( var_pop, 
                              round( rnorm(n(), mean_population, mean_population/6) ),
                              mean_population
         )
      ) 
   
   clinics_list$intervention = intervention(.clinics = clinics_list, .randomize = randomize)
      
   
   clinics_list$id = as.numeric(rownames(clinics_list))

return(clinics_list)
}

# calculate beta shape parameters based on mean (m) and number of observations (n)
   betaShapeA = function(m, n=100){  # formulas for shape parameters from Kruschke, ch. 5, 'doing basyesian data analysis'
      m*n  # m = mean (e.g. .5), n = number observations--strength of data
      # m * ( (m*(1-m)/(s^2)) - 1 ) # alternative formula with s = std dev (must be < .288)
   }

   betaShapeB = function(m, n=100){
      (1-m)*n
      # ( 1- m) * ( (m*(1-m)/(s^2)) - 1 )
   }
   
disease = function(
   clinic_list = c,
   bkrd_mean_incidence = .1  , # yearly total
   case.sensitivity = 1,
   case.specificity = 1,
   test.positive.rate = 1,
   reporting.fidelity = 1,
   effectiveness = 0,
   var_incidence = FALSE, 
   .period = NA
){

   disease = clinic_list  %>% as.data.frame() %>% # as.data.frame removes prior grouping
      mutate(
         district_id = coerce_index(district),
         clinic_id = coerce_index(clinic),
         period = .period

         ) %>%

      group_by( district ) %>%

      mutate(

         trt =  intervention,
         
         
         incidence = ifelse( var_incidence, 
                             rbeta( n(),
                                    betaShapeA( bkrd_mean_incidence, 100),
                                    betaShapeB( bkrd_mean_incidence,  100)
                                    ),
                             bkrd_mean_incidence ),

         mean_cases =  incidence  * population ,

        sensitivity = rbeta( n(),
                              betaShapeA( case.sensitivity),
                              betaShapeB( case.sensitivity)
                              ),

         specificity = rbeta( n(),
                              betaShapeA( case.specificity),
                              betaShapeB( case.specificity)
                              ) ,

         false_positive =
            rpois( n(),
                   (1 - case.specificity) * mean_cases * ( (1/test.positive.rate) - 1 ) ) ,

         cases = as.integer(
            ifelse( trt == 1 & ( is.na(period) || 
                                    period %in% "post" || 
                                    period %in% 1:12 
                                 ) ,
                    rpois(n(),
                         reporting.fidelity * case.sensitivity *
                                            ( mean_cases * (1-(effectiveness))  + false_positive ) ),
                         rpois(n(),
                               reporting.fidelity * case.sensitivity * (mean_cases  + false_positive) )
         ))
      )

   disease$group = factor( disease$trt , levels = 0:1, labels = c('Control', 'Treatment') )
   disease$trt.f = factor( disease$trt, levels = 0:1, labels = c('control', 'treatment') )

   
   return(disease)
}

study_data = function( design = "randomized", seasonal = 1, ...
                       ){
   if ( design %in% 'randomized'){
      
      d = disease(...)
      
   } else if ( design %in% "pre-post"){
      
      d = bind_rows( disease(.period = 'pre', ...),
               disease(.period = 'post', ...) 
               ) 
      
      d$period = factor(d$period, levels = c("pre", "post"))
      # %>%
      #    mutate(
      #          time = c(0,1)[ factor( period, levels = c("pre", "post") )]
      #       )
      
   } else if ( design %in% "interrupted time-series"){
      
      s = c(0,0,0.25,0.5, 0.75,1,1,0.75,0.5,0.25,0,0)
      d = NULL
      
      for (p in c(-12:-1, 1:12) ){
         
         if ( is.null(d)){ d = disease( .period = p, ... ) } 
         else{
            
            d = bind_rows( d, disease( .period = p, ...  )  )
         }
         
      }
      
      # divide annual casese into 12 months and apply seasonal factor
      
      d$cases = d$cases * (seasonal^s[ abs(d$period) ] )/12
      
      # d = d %>%
      #    mutate(
      #          time = c(0,1)[ factor( period, levels = c("pre", "post") )]
      #       )
   }
   
   return(d)
}
   
effect = function( 
   d, 
   design = 'randomized' ){
   
       # randomize
      if ( design %in% "randomized"){
         
         data = d %>% as.data.frame() %>%
            select(
               population, cases, trt
            ) 
         
         # bayes
         # m1 = alist(
         #    cases ~ dbinom( population, p ),
         #    logit(p) <- a + bt * trt ,
         #    a ~ dnorm(0, 1),
         #    bt ~ dnorm(0, 1)
         # )
         
         # run bayes models
         # eff =   map( m1,
         #              start=list( a= 0, bt=0),
         #              data = data
         # )
         #
         # effect = (1 - exp(post$binter)) * 100
         
         # effect = (1 - exp(post$bt)) * 100
         
         
         linmod = glm( cases ~ trt, family = poisson, data = data)
         post <- extract.samples( linmod )
         effect = (1 - exp(post$trt)) * 100
         return(effect)
         
      } else if ( design %in% "pre-post"){
         
         data = d %>% as.data.frame() %>%
            select(
               population, cases, trt, period
            ) %>% 
            mutate(
               time = c(0,1)[ factor( period, levels = c("pre", "post") )]
            ) %>% 
            select(-period)
         
         
         # bayes
         # m1 = alist(
         #    cases ~ dbinom( population, p ),
         #    logit(p) <- a + (bt * trt) + (bperiod * time) + (binter * trt * time) ,
         #    a ~ dnorm(0, 3),
         #    bt ~ dnorm(0, 1),
         #    bperiod ~ dnorm(0, 1),
         #    binter ~ dnorm(0, 1)
         # )
         
         # run bayes models
         # eff =   map( m1,
         #              start=list( a= 0, bt=0),
         #              data = data
         # )
         #
         # effect = (1 - exp(post$binter)) * 100

         
         linmod = glm( cases ~ trt + time + trt*time, family = poisson, data = data)
         post <- extract.samples( linmod )
         effect = (1 - exp(post$`trt:time`)) 
         return(effect)
   
         
      } else if ( design %in% "interrupted time-series"){
         

         # aggregate and split into trt groups
         dd = d %>% as.data.frame() %>% group_by(period, trt) %>% summarise( cases = mean(cases))
         dt = dd %>% as.data.frame() %>% filter(trt == 1) %>% select(cases)
         dts = ts(dt, start=c(0,-12), end=c(0,12), frequency = 12)

         # Seasonal decomposition         
         fit <- stl(dts, s.window="period")
         trend = as.data.frame(fit$time.series)
         trend$id = as.integer(rownames(trend))
         m =  lm( trend ~ id, trend)
         post <- extract.samples( m )
         effect =  ( 1 - (post$Intercept +  24 * post$id )/ post$Intercept ) * 100
         return(effect)
         
      }


}




