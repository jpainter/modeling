
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
   mean_population = 2500L 
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
                 n_clinics
                 # atLeast(
                 #    as.integer( rnorm(1, n_clinics, n_clinics/3) ),
                 #    2)
               )
            } )
            ))
         )
   
   clinics_list = district_list  %>% 
      group_by( district ) %>%
      mutate(
         clinic =  as.character( row_number() ),
         district_clinic = paste0( district, clinic) ,
         population = mean_population
            # round( rnorm(n(), mean_population, mean_population/6) )
         
      ) 
   
   clinics_list$intervention = intervention(.clinics = clinics_list, .randomize = randomize)
      
   
   clinics_list$id = as.numeric(rownames(clinics_list))

return(clinics_list)
}

disease = function(
   clinic_list = c,
   bkrd_mean_incidence = .1  , # yearly total
   case.sensitivity = 1,
   case.specificity = 1,
   test.positive.rate = 1,
   reporting.fidelity = 1,
   effectiveness = 0,
   .period = "post" # or "post" referring to intervention period

){

# calculate beta shape parameters based on mean (m) and number of observations (n)
   betaShapeA = function(m, n=100){  # formulas for shape parameters from Kruschke, ch. 5, 'doing basyesian data analysis'
      m*n  # m = mean (e.g. .5), n = number observations--strength of data
      # m * ( (m*(1-m)/(s^2)) - 1 ) # alternative formula with s = std dev (must be < .288)
   }

   betaShapeB = function(m, n=100){
      (1-m)*n
      # ( 1- m) * ( (m*(1-m)/(s^2)) - 1 )
   }

   disease = clinic_list  %>% as.data.frame() %>% # as.data.frame removes prior grouping
      mutate(
         district_id = coerce_index(district),
         clinic_id = coerce_index(clinic),
         period = .period

         ) %>%

      group_by( district ) %>%

      mutate(

         trt =  intervention,

         incidence = bkrd_mean_incidence ,
         #    rbeta( n(),
         #                    betaShapeA( bkrd_mean_incidence, 100),
         #                    betaShapeB( bkrd_mean_incidence,  100)
         # ),

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
            ifelse( trt == 1 & (period %in% "post") ,
                    rpois(n(),
                         reporting.fidelity * case.sensitivity *
                                            ( mean_cases * (1-(effectiveness))  + false_positive ) ),
                         rpois(n(),
                               reporting.fidelity * case.sensitivity * (mean_cases  + false_positive) )
         ))
      )

   disease$group = factor( disease$trt , levels = 0:1, labels = c('Control', 'Treatment') )
   disease$trt.f = factor( disease$trt, levels = 0:1, labels = c('control', 'treatment') )
   disease$period.f = factor(disease$period, levels = c("pre", "post"))
   
   return(disease)
}

effect = function( 
   d, 
   .analysis = 'randomized' ){
      
   # if (.analysis %in% 'glm'){
   #    
   #    linmod = glm( cases ~ trt, family = poisson, data = d)
   #    post <- extract.samples( linmod )
   #    effect = (1 - exp(post$trt)) * 100
   #    return(effect)
   # }
   
       # randomize
      if ( .analysis %in% "randomized"){
         
         m1 = alist(
            cases ~ dbinom( population, p ),
            logit(p) <- a + bt * trt ,
            a ~ dnorm(0, 1),
            bt ~ dnorm(0, 1)
         )
         
         data = d %>% as.data.frame() %>%
            filter( period %in% "post") %>%
            select(
               population, cases, trt
            ) 
         
         linmod = glm( cases ~ trt, family = poisson, data = data)
         post <- extract.samples( linmod )
         effect = (1 - exp(post$trt)) * 100
         return(effect)
         
      } else if ( .analysis %in% "pre-post"){
         
         # pre-post
         m1 = alist(
            cases ~ dbinom( population, p ),
            logit(p) <- a + (bt * trt) + (bperiod * time) + (binter * trt * time) ,
            a ~ dnorm(0, 3),
            bt ~ dnorm(0, 1),
            bperiod ~ dnorm(0, 1),
            binter ~ dnorm(0, 1)
         )
         
         data = d %>% as.data.frame() %>%
            select(
               population, cases, trt, period
            ) %>% 
            mutate(
               time = c(0,1)[ factor( period, levels = c("pre", "post") )]
            ) %>% 
            select(-period)
         
         linmod = glm( cases ~ trt + time + trt*time, family = poisson, data = data)
         post <- extract.samples( linmod )
         effect = (1 - exp(post$`trt:time`)) * 100
         return(effect)
      
      }
      
   # run models
   eff =   map( m1,
                start=list( a= 0, bt=0),
                data = data
   )
   
   # extract posterior; calculate effect
   post <- extract.samples( eff )
   
   
   if ( .analysis %in% "randomized"){ 
      # randomize 
      effect = (1 - exp(post$bt)) * 100
      
      } else if ( .analysis %in% "pre-post"){
         # pre-post
         effect = (1 - exp(post$binter)) * 100
         }
   
   return(effect)
}

