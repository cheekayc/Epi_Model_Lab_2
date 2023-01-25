library(deSolve)

# Try out SIMPLE ODE first:

## Step 1: Start with writing a function:
simple_func = function(x, y, parms) {
  dy = x^2   # We don't need to write dx because by default it's already there.
  list(dy)   # This is telling R which one is the dependent variable, which in this case is the dy.
}

## Step 2: Specify initial conditions:
xs = seq(0, 10, by = 0.1)
state = c(y = 0);

## Step 3: Call the ODE function to generate the simulation
out = ode(y = state, times = xs, func = simple_func, parms = NULL)

### the analytic solution for this problem (i.e. true solution here)
y.true = 1/3 * xs^3

## Step 4: Check model outputs
view(out)

### Print column names using `colnames` function, so we can refer to them by name:
colnames(out)

### Print part of the model output to see data structure:
head(out, 3)
tail(out, 6)





