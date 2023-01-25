## Lab 2: SIR MODEL
## SCRIPT TO TEST THE SIR MODEL

#############################################################################
## !!! NOTE: Please focus on the questions in the lab slides (i.e. pdf file)  
##              -those are the core questions you need to answer. 
## Any additional questions in this R script are meant as guidelines.
#############################################################################

## if you haven't had it installed yet, type in the following first:
## install.packages('deSolve')
library(deSolve)

## PRE-LAB: TRY THE ODE SOLVER
## SIMPLE ODE: dy/dx=x^2

# Step 1: code the ODE model (or function here)
myfunc=function(x,y,parms){
  dy=x^2
  list(dy)
}

# Step 2: specify initial conditions/parameters
xs=seq(0,10,by=.2)  # the 'time' step to integrate upon
state=c(y=0); 

# Step 3: call the ode function to generate the simulation
out=ode(y=state,times=xs,func=myfunc,parms=NULL)

# the analytic solution for this problem (i.e. true solution here)
y.true=1/3*xs^3

# Step 4: Check model outputs

View(out)  # to see the whole output - the out data frame here

# Print column names using the colnames function, so you can refer to them by name
colnames(out)

# Print part of the model output to see data structure
head(out, 3)  # this prints the first 3 lines of the model output
tail(out, 6) # this prints the last 6 lines of the model output


# to get the first column of the output
out[,1]  # use column indexing 
out[,'time']  # use the column name



# Similarly to get the 2nd column of the output
out[,2]  # use column indexing 
out[,'y']  # use the column name


# Now try checking the rows
# e.g., print 3rd row of the output
out[3,]

# Now try specific row & column
out[3, 2]
out[3, 'time']
out[3, 'y']
out[out[,'time']==3, ] # get a specific value
out[out[,'time'] >=3 & out[,'time'] < 4, ] 
out[out[,'y'] > 10 & out[,'y'] < 11, ]  # get value within a specific range

## Now plot to check if the ode is working
par(mar=c(3,3,1,1),mgp=c(1.8,.5,0),cex=1.2)
plot(x = out[,1], y = out[,2],xlab='x',ylab='y',type='l')
points(x = xs, y = y.true,col='red',pch=20)
legend('topleft',legend=c('Analytic solution: y.ture','Numerical solution: ODE'),
       col=c('red','black'),lty=c(NA,1),pch=c(20,NA),bty='n')


# alternatively 
plot(x = out[,'time'], y = out[,'y'], xlab='x',ylab='y',type='l')


# Put the two plots side by side for comparison
par(mar=c(3,3,1,1),mgp=c(1.8,.5,0),cex=1.2,
    mfrow = c(2,1)) # allow 2 rows in the plot
plot(x = out[,1], y = out[,2],xlab='x',ylab='y',type='l', 
     main = "Using column indices")  # add title
points(x = xs, y = y.true,col='red',pch=20)
legend('topleft',legend=c('Analytic solution: y.ture','Numerical solution: ODE'),
       col=c('red','black'),lty=c(NA,1),pch=c(20,NA),bty='n')
# row 2 using alternative method
plot(x = out[,'time'], y = out[,'y'],xlab='x',ylab='y',type='l', 
     main = "Using column names")  # add title
points(x = xs, y = y.true,col='red',pch=20)
legend('topleft',legend=c('Analytic solution: y.ture','Numerical solution: ODE'),
       col=c('red','black'),lty=c(NA,1),pch=c(20,NA),bty='n')


par(mar=c(3,3,1,1),mgp=c(1.8,.5,0),cex=1.2,
    mfrow = c(1,2)) # allow 2 columns in the plot
plot(x = out[,1], y = out[,2],xlab='x',ylab='y',type='l', 
     main = "Using column indices")  # add title
points(x = xs, y = y.true,col='red',pch=20)
legend('topleft',legend=c('Analytic solution: y.ture','Numerical solution: ODE'),
       col=c('red','black'),lty=c(NA,1),pch=c(20,NA),bty='n')
# row 2 using alternative method
plot(x = out[,'time'], y = out[,'y'],xlab='x',ylab='y',type='l', 
     main = "Using column names")  # add title
points(x = xs, y = y.true,col='red',pch=20)
legend('topleft',legend=c('Analytic solution: y.ture','Numerical solution: ODE'),
       col=c('red','black'),lty=c(NA,1),pch=c(20,NA),bty='n')




################################################################################
## SIMULAITON USING THE SIR MODEL
################################################################################
# Step 1: code the SIR model (or function here)
SIR=function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    # rate of change
    dS = -beta*S*I/N;
    dI= beta*S*I/N - gamma*I;
    
    # Optional: also compute the cumulative number of infection
    dcumI = beta*S*I/N
    
    # return the rate of change
    list(c(dS, dI, dcumI))
  }) # end with(as.list...)
}

# Step 2: specify initial conditions/parameters
N=1e5; # population size
I0=10; # initial No. of Infectious people
S0=N-I0;  # initial No. of Susceptible people
state=c(S=S0, I=I0, cumI = I0);  # store the inital conditions I & S together 
parameters=c(beta=.5,gamma=.3);  # store the model parameters, unit: per day

times=seq(1,100,by=1);  # set simulation time steps: 1 to 100 days here

# Step 3: call the ode function to generate the simulation
sim=ode(y=state,times=times,func=SIR,parms=parameters);


# Step 4: check model outputs
View(sim)  # to see what's in the model output 'sim'

# Print part of the model simput to see data structure
head(sim, 3)  # this prints the first 3 lines of the model output
tail(sim, 1) # this prints the last line of the model output

# Print column names using the colnames function, so you can refer to them by name
colnames(sim)

# to get the column for simulation: 
sim[,1]  # use column indexing 
sim[,'time']  # use the column name

# get output on specific day using column name
sim[sim[,'time']==10, ]

# CODE IT YOURSELF - now get the column for simulated number of Susceptibles
# CODE IT YOURSELF - now get the column for simulated number of Infectious:



# Visualize model outputs:
# plot S over time
# plot I over time
# plot to compare S and I in one graph
# plot to compare S and cumI - think about the trend and why






# [Q1] CODE IT YOURSELF - what is the number of susceptibles on Day 50? 


# [Q1] CODE IT YOURSELF - what is the number of susceptibles at the end of the simulation? 


# [Q2] CODE IT YOURSELF - what is the number of Infectious on Day 50? 


# [Q2] CODE IT YOURSELF - what is the number of Infectious at the end of the simulation? 


# [LQ3] Based on the simulation, how many people have been infected by Day 50? 
# [LQ3]   How many people have been infected by the end of simulation? 
  

# [Q4] CODE IT YOURSELF - Plot model simulated number of Susceptibles for each day during the simulation period



# [Q4] CODE IT YOURSELF - Plot model simulated number of Infectious for each day over time during the simulation period


################################################################################
## PROCESSING AND VISULIZING THE MODEL OUTPUTS
################################################################################

# Output Processing 1: 
# Convert numbers to fractions relative to the population size - for easier comparison across populations of different sizes
s=sim[,'S']/N * 100  # %S
i=sim[,'I']/N * 100 # %I
cumi = sim[, 'cumI']/N * 100 # %cumulative infections


# Plot fraction susceptible over time
par(mar=c(3,3,1,1),mgp=c(1.8,.5,0),cex=1.2)
# show as percentage: 
plot(x = times, y = s, xlab='Time (days)',ylab='Fraction Susceptible (%)',type='l')

# CODE IT YOURSELF:

# [LQ5] Based on the simulation, What is the population susceptibility (i.e. %S) at the end of simulation? What percentage of the population are infectious at the end of simulation?  What percentage of the population have been infected by the end of simulation?


# [LQ6] Plot the outputs %I and %S for each day during the simulated time period.



# Output Processing 2: 
# Convert the daily outputs to weekly outputs
numWk = ceiling(length(times) / 7); 
tmstep=7; # weekly 
out.weekly = matrix(0, # place holder for the actual numbers
                    numWk, # number of rows is the number of weeks
                    ncol(sim)) # number of columns is the same as the simulation output ('sim')
# set column names, so it would be more readable & easier to access to the results
colnames(out.weekly) = c('Week', 'Number of Susceptibles', 'Number of Infectious', 'Number of New Infections') 
out.weekly[,'Week'] = 0: (numWk - 1)  # Week column is just the number of week
# Get the number of Susceptibles for 4th day of each week (i.e. middle of the week) - this is sort of a snapshot of the epidemic
out.weekly[,'Number of Susceptibles'] = c(S0, # use the initial susceptibility for the 1st entry
                                          sim[seq(4, length.out = numWk - 1, by  = tmstep),  # indices of the first day of each week
                                            'S']) 

# Get the number of Infectious for 4th day of each week (i.e. middle of the week) - this is the prevalence
out.weekly[,'Number of Infectious'] = c(I0, # use the initial susceptibility for the 1st entry
                                        sim[seq(4, length.out = numWk - 1, by  = tmstep),  # indices of the first day of each week
                                            'I']) 

# Get the Number of New Infections - these are cumulative numbers so need to get the increment (i.e., new infection) for each week
out.weekly[,'Number of New Infections'] = c(0, # add a 0 for the start of the 1st week
                                            (sim[seq(tmstep+1, length.out = numWk - 1, by  = tmstep), 'cumI'] - # cumulative infections on the start of week 2, 3, ... 
                                            sim[seq(1, length.out = numWk - 1, by  = tmstep), 'cumI']) # cumulative infections on the start of week 1, 2, ...
                                            )  

# CODE IT YOURSELF -
# [Q7] How many people are infected on Week 7? (0.5pt)

# [Q8] Plot number of Susceptibles mid-week over time (0.5pt)

# [Q9] Plot number of Infectious mid-week over time (0.5pt)

# [Q10] Plot number of New Infections each week over time (0.5pt)














################################################################################
## CODE FOR THE DEMO: 
## YOU CAN USE THE FOLLOWING CODE TO SEE THE CHANGES IN S AND I STEP BY STEP
################################################################################
## CHANGE IN NUMBER OF INFECTIOUS
numWk=15; tmstep=7; # weekly increase
wklyS=wklyI=rep(0,numWk+1);
wklyS[1]=S0; wklyI[1]=I0;
wk=1; # first week
times=seq(tmstep*(wk-1),tmstep*wk,by=1);
state=c(S=wklyS[wk],I=wklyI[wk], cumI=wklyI[wk])
sim=ode(y=state,times=times,func=SIR,parms=parameters);
wklyS[wk+1]=tail(sim[,'S'],1); # save new state
wklyI[wk+1]=tail(sim[,'I'],1);

par(mfrow = c(1,1), mar=c(3,3,1,1),mgp=c(1.8,.5,0),cex=1.2)
plot(sim[,'time'],sim[,'I'],ylim=c(0,N*.12),xlim=c(0,tmstep*numWk),
     ylab='I',xlab='Time', pch=20)
for (wk in 2:numWk){
  times=seq(tmstep*(wk-1),tmstep*wk,by=1);
  state=c(S=wklyS[wk],I=wklyI[wk], cumI=wklyI[wk])
  sim=ode(y=state,times=times,func=SIR,parms=parameters);
  wklyS[wk+1]=tail(sim[,'S'],1); # save new state
  wklyI[wk+1]=tail(sim[,'I'],1);
  points(tail(sim[,'time'],tmstep),tail(sim[,'I'],tmstep),pch=20)
  readline("Press Run to continue")  ## PRESS RUN TO PROCEED
}

## CHANGE IN THE NUMBER OF SUSCEPTIBLES
wklyS=wklyI=rep(0,numWk+1);
wklyS[1]=S0; wklyI[1]=I0;
wk=1; # first week
times=seq(tmstep*(wk-1),tmstep*wk,by=1);
state=c(S=wklyS[wk],I=wklyI[wk], cumI=wklyI[wk])
sim=ode(y=state,times=times,func=SIR,parms=parameters);
wklyS[wk+1]=tail(sim[,'S'],1); # save new state
wklyI[wk+1]=tail(sim[,'I'],1);
par(mfrow = c(1,1), mar=c(3,3,1,1),mgp=c(1.8,.5,0),cex=1.2)
plot(sim[,'time'],sim[,'S']/N*100,ylim=c(0,100),xlim=c(0,tmstep*numWk),
     ylab='% Susceptible',xlab='Time', pch=20)
for (wk in 2:numWk){
  times=seq(tmstep*(wk-1),tmstep*wk,by=1);
  state=c(S=wklyS[wk],I=wklyI[wk], cumI=wklyI[wk])
  sim=ode(y=state,times=times,func=SIR,parms=parameters);
  wklyS[wk+1]=tail(sim[,'S'],1); # save new state
  wklyI[wk+1]=tail(sim[,'I'],1);
  points(tail(sim[,'time'],tmstep),tail(sim[,'S']/N,tmstep)*100,pch=20)
  readline("Press Run to continue") ## PRESS RUN TO PROCEED
}


