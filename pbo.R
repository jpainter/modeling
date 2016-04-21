
# PBO
library(rethinking)
library(dplyr)
library(tidyr)

randomize = "district" # versus "cluster"
num_districts = 2  
n_clinics = 20 # mean number of clinics.
mean_population = 25000
bkrd_mean_incidence = .1 # yearly total
bkrd_std_incidence = bkrd_mean_incidence / 6
effectiveness = .1

# set.seed(067)

intervention = function(.clinics = clinics, .randomize = randomize) {
   
   clinics = .clinics$clinic
   districts = .clinics$district
   
   if ( .randomize == "district") {
         
         n = floor( length( unique(districts) ) / 2)
         r = sample(districts, n)
         intervention = ifelse( grepl( paste0(r, collapse = "|")  , districts) , 1, 0 )

   } else if ( .randomize == "cluster") {
   
         n = floor( length(clinics) / 2)
         r = sample(clinics, n)
         intervention = ifelse( grepl( paste0(r, collapse = "|") , clinics) , 1, 0 )
   
   } else NA
   
   return(intervention)
}

   
      
clinics = function(
   randomize = "district" ,
   num_districts = 2  ,
   n_clinics = 20  , # mean number of clinics.
   mean_population = 2500 
   ){
   
   # trt = function(district) district %in% district_treatment
   
   districts = LETTERS[1:num_districts]
   
   district_list = data_frame( 
         district = unname(unlist(
            lapply( districts, FUN = function(x){
            rep( x, 
                as.integer( rnorm(1, n_clinics, n_clinics/6) ) 
               )
            } )
            ))
         )
   
   clinics_list = district_list  %>% 
      group_by( district ) %>%
      mutate(
         clinic =  letters[row_number()] ,
         clinic = paste0( district, clinic) ,
         population = rpois(n(), mean_population)
         
      ) 
   
   clinics_list$intervention = intervention(.clinics = clinics_list, .randomize = randomize)
      
   
   clinics_list$id = as.numeric(rownames(clinics_list))

return(clinics_list)
}

disease = function(
   clinic_list = c,
   bkrd_mean_incidence = .1  , # yearly total
   reporting_sensitivity = .5,
   reporting_specificity = .6,
   percent.fever.not.malaria = .7, 
   effectiveness = 0.1,
   period = "pre" # or "post" referring to intervention period
   
){
   

   betaShapeA = function(m, s){  # formulas for shape parameters from Kruschke, ch. 5, 'doing basyesian data analysis' 
      m * ( (m*(1-m)/s^2) - 1 )
   }
   
   betaShapeB = function(m, s){
      ( 1- m) * ( (m*(1-m)/s^2) - 1 )
   }
   
   disease = clinic_list  %>% 
      group_by( district ) %>%
      mutate(
         
         period = period,
         
         trt = (period %in% "post") * intervention,

         incidence = rbeta( n(), 
                            betaShapeA( bkrd_mean_incidence, bkrd_mean_incidence/3),
                            betaShapeB( bkrd_mean_incidence, bkrd_mean_incidence/3)
         ),
         
         mean_cases =  incidence  * population , 
         
         sensitivity = rbeta( n(), 
                              betaShapeA( reporting_sensitivity, reporting_sensitivity/3),
                              betaShapeB( reporting_sensitivity, reporting_sensitivity/3)
                              ),
         
         specificity = rbeta( n(), 
                              betaShapeA( reporting_sensitivity, reporting_specificity/3),
                              betaShapeB( reporting_sensitivity, reporting_specificity/3)
                              ) ,
         
         false_positive = 
            rpois( n(), 
                   (1 - specificity) * (mean_cases * percent.fever.not.malaria) /
                                    (1 - percent.fever.not.malaria) ) ,
         
         cases = as.integer(
            ifelse( trt == 1 & (period %in% "post") ,  
                    rpois(n(), 
                          reporting_sensitivity * 
                                            ( mean_cases * (1-effectiveness)  + false_positive ) ),
                         rpois(n(), 
                               reporting_sensitivity * (mean_cases  + false_positive) )
         ))
      ) 
   
   return(disease)
}



# DATA
c = clinics(randomize = "cluster")
d = disease(c, period = 'post')

# summarise clinics
d %>% summarise( n_clinics = n() )
d %>% group_by(trt) %>% summarise( cases = mean(cases))

 for (o in 1:20){
   d = disease(c, period = 'post')
   print(
      d %>% group_by(trt) %>% summarise( cases = mean(cases))
   )
}

# pirate plots
# install.packages("devtools") # Only if you don't have the devtools library already installed
   # library("devtools")
   # install_github("ndphillips/yarrr")


plot( population ~ factor(trt), data = d, main = "Population by treatment group", theme.o = 3, point.o = .7, hdi.o = .6, bar.o = 0)
plot( cases ~ factor(trt), data = d, main = "Cases by treatment group", theme.o = 3, point.o = .7, hdi.o = .5, bar.o = 0 )

library(yarrr)
pirateplot( cases ~ trt , data = d, main = "Cases by treatment group", theme.o = 3, point.o = .7, hdi.o = .5, bar.o = 0 )

# lM
 mlm = glm(cases ~ trt, family = poisson, data = d)
 summary(mlm)
 post <- extract.samples( mlm )
 eff = (1 - exp(post$trt)) * 100
 quantile(eff)

# MAP 

d = d %>% as.data.frame() %>%
    select(
       population, cases, id, trt
    ) 


# fixed effects
m1 = map(
   alist( 
      cases ~ dbinom( population, p ),
      logit(p) <- a + bt * trt ,
      a ~ dnorm( 0, 10 ),
      bt ~ dnorm( 0 , 10)
      
      ), data = d
)
precis(m1)

post <- extract.samples( m1 )
eff = (1 - exp(post$bt)) * 100
options(digits = 3)
q  = quantile(eff)
q

hist(eff)

library(BEST)
plotPost(eff)

# MAP2STAN
 mm1 = map2stan( m1
    , data = d, iter = 4000, chains = 4, WAIC = FALSE, cores = 4
 )
 precis(mm1)
 post <- extract.samples( mm1 )
 eff = (1 - exp(post$bt)) * 100
 quantile(eff)

# random effects (individual intercept)

m2 = 
   alist( 
      cases ~ dbinom( population, p ),
      logit(p) <- a[id] + bt * trt ,
      a[id] ~ dnorm( 0, 10 ),
      bt ~ dnorm( 0 , 10)
      
      )

 m2map = map( m2, data = d) # much faster
   post <- extract.samples( m2map)
   eff = (1 - exp(post$bt)) * 100
   quantile(eff)


 mm2 = map2stan( m2
    , data = d, iter = 4000, chains = 4, WAIC = FALSE
 )
 
   precis(mm2, depth = 2)
   post <- extract.samples( mm2 )
   eff = (1 - exp(post$bt)) * 100
   quantile(eff)

# fancier version...
 m2m = alist(
    
      cases ~ dbinom( population, p),
      logit(p) <- a_clinic[id] + bt * trt ,
      a_clinic[id] ~ dnorm( a, sigma ),
      a ~ dnorm(0,1),
      sigma ~ dcauchy(0,1),
      bt ~ dnorm( 0 , 10)

      )
 
 mm2m = map2stan( m2m
    , data = d, iter = 4000, chains = 4, WAIC = FALSE
 )

   post <- extract.samples( mm2m )
   eff = (1 - exp(post$bt)) * 100
   quantile(eff)


