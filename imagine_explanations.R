
imagine_explanations = 
   "
This document summarizes simulations of the natural experiment, a comparison of bednets for example, where the study design does not come first, but rather follows the natural distribution of bednets.  In some settings the distribution could pay by district or by health facility or by village and in this document we allow one to simulate each of those scenarios and examine the relative ability to to make inferences about the bednets efficacy.  In some cases, the trial may not be entirely natural.  In some cases distribution of bednets would normally be across several regions but because of the interest in comparing two nets the distribution is done otherwise, perhaps noon and district A and the old net and district B. 

The simulations assume that the district's where the study is done are roughly compare a ball and that they have similar seasonality two rainfall and other factors that contribute to malaria transmission. If single background rate for malaria incidence is applied to each health-facility in the trial, as are random sample from a distribution with that background rate.  Hence, two health facilities and the same district will have different malaria incidence.  The mean of cross all health facilities will be approximately equal to the expected malaria rate. 

(Note: consider adding another variable that takes the expected malaria rate across the whole area and randomizes it to each district, and then from that takes back to trade in each district randomizes it to each clinic.) 

Similarly, the simulation assumes that there's a mean number of clinics within each district; the simulation assigns a number of clinics to each district from a distribution rosé mean is that expected number of clinics. 

The same is done for the new case detection sensitivity, the case detection specificity, expected test positivity rate, and the reporting fidelity.  Details on each of these is given below. 

The effectiveness is treated as a constant factor and is used to adjust the expected rate at each clinic.  Thus, if the background rate at a clinic were 10% of all children the catchment area having malaria, and the effectiveness was 20%, the resulting in expected rate at the clinic would be 8% (10% - 20% of 10%). 

The average incidence is calculated as the total number of malaria episodes divided by the the population.  Multiple episodes are all counted.

The primary unit of analysis is the health facility catchment area.  The total number of malaria cases reported by health facility are compared with a number of malaria cases reported by other facilities.

The case detection sensitivity represents the percentage of true malaria cases that are confirmed by laboratory testing. 

The **case detection specificity** and that **test positivity rate** to estimate the number of false positive cases reported by each health-facility. The case detection specificity refers to the proportion of patients without malaria that are in not reported as malaria.  An ideal surveillance system would have 100% specificity.  Where this is less than 100%, we assume that the primary reason is that children with fever due to causes other than malaria are misdiagnosed as malaria.  For example, in child presents with fever and laboratory testing for malaria is not done; nonetheless the child is diagnosed as having malaria.  Some of these children may in fact have malaria.  The proportion that do not are based on the test positivity rate.  The lower the test positivity rate them or false positive cases will be reported.

When a simulation is run, there are two primary functions that create the dataset. The first function  is called clinics.  This function creates a list of clinics within each district, assigns the population size, and designs the intervention (treatment or control).  
"