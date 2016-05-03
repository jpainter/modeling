
# functions for the Imagine shiny app

# library(rethinking)
# library(dplyr)
# library(tidyr)
# library(BEST) # for posterior plot

intervention = function(.clinics = clinics, .randomize = randomize) {
   
   clinics = .clinics$clinic
   districts = .clinics$district
   
   if ( .randomize == "district") {
         
         n = floor( length( unique(districts) ) / 2)
         r = sample(districts, n)
         intervention = ifelse( grepl( paste0(r, collapse = "|")  , districts) , 1L, 0L )

   } else if ( .randomize == "clinic") {
   
         n = floor( length(clinics) / 2)
         r = sample(clinics, n)
         intervention = ifelse( grepl( paste0(r, collapse = "|") , clinics) , 1L, 0L )
   
   } else NA
   
   return(intervention)
}
      
clinics = function(
   randomize = "district" ,
   num_districts = 2L  ,
   n_clinics = 10L  , # mean number of clinics.
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
                 atLeast(
                    as.integer( rnorm(1, n_clinics, n_clinics/3) ),
                    2)
               )
            } )
            ))
         )
   
   clinics_list = district_list  %>% 
      group_by( district ) %>%
      mutate(
         clinic =  letters[row_number()] ,
         clinic = paste0( district, clinic) ,
         population = round( rnorm(n(), mean_population, mean_population/6) )
         
      ) 
   
   clinics_list$intervention = intervention(.clinics = clinics_list, .randomize = randomize)
      
   
   clinics_list$id = as.numeric(rownames(clinics_list))

return(clinics_list)
}

disease = function(
   clinic_list = c,
   bkrd_mean_incidence = .1  , # yearly total
   case.sensitivity = .6,
   case.specificity = .6,
   test.positive.rate = .7,
   reporting.fidelity = .8,
   effectiveness = 0,
   period = "post" # or "post" referring to intervention period
   
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
         clinic_id = coerce_index(clinic)) %>%

      group_by( district ) %>%

      mutate(

         trt =  (period %in% "post") * intervention,

         incidence = rbeta( n(),
                            betaShapeA( bkrd_mean_incidence, 100),
                            betaShapeB( bkrd_mean_incidence,  100)
         ),

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
   
   return(disease)
}

effect = function( d, randomize = 'district' ){
   
   # fixed effects
   # models
   if (randomize %in% 'district'){ # random district
      
      m1 = alist(
         cases ~ dbinom( population, p ),
         logit(p) <- a[clinic_id] + bt * trt ,
         a[clinic_id] ~ dnorm( 0, 10 ),
         bt ~ dnorm( 0 , 10)
      )  
      
      data = d %>% as.data.frame() %>%
         select(
            population, cases, trt, clinic_id
         ) 
      
   } else if (randomize %in% 'clinic') {  # random cluster
      
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
      
   }
   
   # run models
   eff =   map( m1,
                start=list( a= 0, bt=0),
                data = data
   )
   
   # extract posterior; calculate effect
   post <- extract.samples( eff )
   effect = (1 - exp(post$bt)) * 100
   
   return(effect)
}

# Testing - 

# DATA
# c = clinics(randomize = "clinic" )
# d = disease(c, period = 'post', effectiveness = 0)
# View(d)

# e = effect( d = d , randomize = "clinic")
# plotPost( e )

 
# # summarise clinics
# d %>% summarise( n_clinics = n() )
# d %>% group_by(trt) %>% summarise( cases = mean(cases))


# pirate plots
# install.packages("devtools") # Only if you don't have the devtools library already installed
# library("devtools")
# install_github("ndphillips/yarrr")