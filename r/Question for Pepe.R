####BASIC ODE USE:
library(deSolve)
library(ggplot2)
library(reshape2)


## first set up the parameters and intiial conditions
time <- seq(0, 4, by = 0.05) # step 1: set the time interval you want to run the model over
parameters <- c(kot = 25, kow = 0.12, kwt = 0.12) # step 2: set up the rate constant parameters
benzene_oil_initial = 1465 # step 3 set up the initial conditions
benzene_water_initial = 0 # no BTEX starts in the water



# define the initials
initial <- c(O = benzene_oil_initial, W = benzene_water_initial)



# R function to calculate the value of the derivatives at each time value
# Use the names of the variables as defined in the vectors above
oil_water_function <- function(t, initial, parameters){
  with(as.list(c(initial, parameters)), {
    dO = -kot*O
    dW = kow*O-kwt*W
    return(list(c(dO, dW)))
  })
}


## Integration with 'ode'
out <- ode(y = initial, times = time, func = oil_water_function, parms = parameters)

out.df = as.data.frame(out) # required by ggplot: data object must be a data frame
out.m = melt(out.df, id.vars='time') # this makes plotting easier by puting all variables in a single column

dat<-out.m%>%
  mutate(conc = case_when(variable == "O"~value/(1.5*1000),
                          variable == "W"~value/100000)) ## convert to masses

plot<-dat%>%
  ggplot(aes(x = time, y = conc))+
  facet_wrap(~variable, scale = "free")+
  # geom_point() + 
  geom_line()


plot




####### QUESTION:

# This is just an example for one compound in one treatment, and I will need to run this model for all 5 BTEX compounds in the 7 treatments

# I was wondering if there would be a way for me to create a dataframe with all the rate constants specified for each compound and treatment, and then
# run the function through this dataframe and produce like a nested data frame for each treatment and cmpound where the results would be stored
# sort of like how we can nest plot functions and apply it to each Limno/compound



