library(deSolve)
library(tidyverse)

SIR = function(t, state, parameters) {
  with(as.list(c(state, parameters)),{ 
    # rate of change
    dS = -beta*S*I/N;
    dI= beta*S*I/N - gamma*I;
    
    # cumulative number of infection
    dcumI = beta*S*I/N
    
    # return the rate of change
    list(c(dS, dI, dcumI))
  }) # ending with(as.list...)
} # ending of the entire function

# Step 2: specify initial conditions/parameters:
N = 1e5; # population size
I0 = 10; # initial No. of Infectious people
S0 = N - I0;  # initial No. of Susceptible people
state = c(S = S0, I = I0, cumI = I0);  # store the initial conditions I & S together 
parameters = c(beta = .5, gamma = .3);  # store the model parameters, unit: per day

times = seq(1, 100, by = 1);  # set simulation time steps: 1 to 100 days here

# Step 3: call the ode function to generate the simulation
sim = ode(y = state, times = times, func = SIR, parms = parameters);
