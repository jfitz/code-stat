NProd <- 4
NResources <- 5

ProdNames <- lapply(list(rep("Prod",NProd)),paste0,1:NProd)

# Product names: Prod1, Prod2, ...

ResNames<- lapply(list(rep("Res",NResources)),paste0,1:NResources)

# Resource names: Res1, Res2, ...

Profit <- matrix(c(20, 14, 3, 16),

ncol=NProd,dimnames=c("Profit",ProdNames))

Resources <- matrix(c( 6, 8, 6, 7 , 40, 2, 6, 4, 10, 25, 1, 1, 1, 2, 5, 4, 8, 25, 12, 16), ncol=NProd,dimnames=c(ResNames,ProdNames))

Available <- matrix(c(1440, 1440, 2000, 1000, 9600), ncol=1,dimnames=c(ResNames,"Available"))

library(pander)
pander(Profit, caption="Profit per Product")

library (magrittr, quietly = TRUE) #Used for pipes/dplyr
library (dplyr, quietly = TRUE)
##
## Attaching package: dplyr
## The following objects are masked from package:stats:
##
## filter, lag
##
## The following objects are masked from package:base:
##
## intersect, setdiff, setequal, union
##

library (ROI, quietly = TRUE)

## ROI.plugin.glpk: R Optimization Infrastructure
## Registered solver plugins: nlminb, glpk.


## Default solver: auto.
library (ROI.plugin.glpk, quietly = TRUE)
library (ompr, quietly = TRUE)
library (ompr.roi, quietly = TRUE)

prodmodel <- MIPModel() %>%
  add_variable (x[i], i=1:NProd, type="continuous", lb=0) %>%
  set_objective (sum_expr(Profit[i] * x[i] , i=1:NProd ), "max") %>%
  add_constraint (sum_expr(Resources[j,i]*x[i], i=1:NProd) # Left hand side of constraint
		<= Available[j], # Inequality and Right side of constraint
		j=1:NResources) %>% # Repeat for each resource, j.

  solve_model(with_ROI(solver = "glpk"))

prodmodel

## Status: optimal
## Objective value: 2857.143

results.products <- matrix (rep(-1.0,NProd), nrow = NProd, ncol=1, dimnames=c(ProdNames,c("x")))

temp <- get_solution (prodmodel, x[i]) # Extracts optimal values of variables
results.products <- t(temp [,3] )      # Extracts third column

# Resizes and renames
results.products <- matrix (results.products, nrow = 1, ncol=NProd, dimnames=c(c("x"),ProdNames))

pander (results.products, caption = "Optimal Production Plan")

Results.Resources <- Resources[]%*%t(results.products)

# Multiply matrix of resources by amount of products produced

colnames(Results.Resources)<-c("Used")

# Change column name to reflect amount of resources used.

pander(Results.Resources, caption="Resources Used")

Resources[4,1] <- 20 # Set value of the 4th row, 1st column to 20

# In our example, this is the paint resource used by making a chair

prodmodel <- MIPModel() %>%
  add_variable (x[i], i=1:NProd, type="continuous", lb=0) %>%
  set_objective (sum_expr(Profit[i] * x[i] , i=1:NProd ), "max") %>%
  add_constraint (sum_expr(Resources[j,i]*x[i], i=1:NProd) # Left hand side of constraint
		<= Available[j],
		j=1:NResources) %>% # Repeat for each resource, j.

# Inequality and Right side of constraint

  solve_model(with_ROI(solver = "glpk"))

prodmodel

## Status: optimal
## Objective value: 1500

results.products <- matrix (rep(-1.0,NProd), nrow = NProd, ncol=1, dimnames=c(ProdNames,c("x")))

temp <- get_solution (prodmodel, x[i]) # Extracts optimal values of variables
results.products <- t(temp [,3] )

# Extracts third column

results.products <- matrix (results.products, nrow = 1, ncol=NProd, dimnames=c(c("x"),ProdNames))

# Resizes and renames

pander (results.products, caption = "Revised Optimal Production Plan")


