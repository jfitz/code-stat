library(pander, quietly=TRUE)

x <- matrix(c(10, 20, 30, 50), ncol=1, dimnames=list(LETTERS[1:4], "x"))

y <- matrix(c(75, 100, 300, 400), ncol=1, dimnames=list(LETTERS[1:4], "y"))

pander(cbind(x,y), caption="First Dataset for DEA")

library(Benchmarking, quietly=TRUE)

dea.plot(x, y, RTS="crs", ORIENTATION="in-out", txt=LETTERS[1:length(x)],
add=FALSE, wx=NULL, wy=NULL, TRANSPOSE=FALSE, fex=1, GRID=TRUE,
RANGE=FALSE, param=NULL)

ND <- nrow(x); NX <- ncol(x); NY <- ncol(y); # Define data size

xdata <- x[1:ND,]
dim(xdata) <- c(ND,NX)
ydata <- y[1:ND,]
dim(ydata) <- c(ND,NY)

# Now we will create lists of names

DMUnames <- list(c(LETTERS[1:ND]))
Xnames <- lapply(list(rep("X",NX)),paste0,1:NX)
Ynames <- lapply(list(rep("Y",NY)),paste0,1:NY)
Vnames <- lapply(list(rep("v",NX)),paste0,1:NX)
Unames <- lapply(list(rep("u",NY)),paste0,1:NY)

# DMU names: A, B, ...
# Input names: x1, ...
# Output names: y1, ...
# Input weight names: v1, ...
# Output weight names: u1, ...

SXnames <- lapply(list(rep("sx", NX)), paste0, 1:NX) # Input slack names: sx1, ...
SYnames <- lapply(list(rep("sy", NY)), paste0, 1:NY) # Output slack names: sy1, ...
Lambdanames <- lapply(list(rep("L_", ND)), paste0, LETTERS[1:ND])

results.efficiency <- matrix(rep(-1.0, ND), nrow=ND, ncol=1)
dimnames(results.efficiency) <- c(DMUnames, "CCR-IO")

# Attach names

results.lambda <- matrix(rep(-1.0, ND^2), nrow=ND, ncol=ND)
dimnames(results.lambda) <- c(DMUnames, Lambdanames)

results.xslack <- matrix(rep(-1.0, ND*NX), nrow=ND, ncol=NX)
dimnames(results.xslack) <- c(DMUnames, SXnames)

results.yslack <- matrix(rep(-1.0, ND*NY), nrow=ND, ncol=NY)
dimnames(results.yslack) <- c(DMUnames, SYnames)

library(dplyr, quietly=TRUE)
library(ROI, quietly=TRUE)
library(ROI.plugin.glpk, quietly=TRUE) # Connection to glpk as solver
# Optimization Modeling using R
library(ompr, quietly=TRUE)
library(ompr.roi, quietly=TRUE)

# DMU to analyze.
k<-2

result <- MIPModel() %>%
  add_variable(vlambda[j], j = 1:ND, type = "continuous", lb = 0) %>%
  add_variable(vtheta, type = "continuous") %>%

  set_objective(vtheta, "min") %>%

  add_constraint(sum_expr(vlambda[j] * xdata[j,1], j = 1:ND) <= vtheta * xdata[k,1]) %>%
  add_constraint(sum_expr(vlambda[j] * ydata[j,1], j = 1:ND) >= ydata[k,1]) %>%

  solve_model(with_ROI(solver = "glpk"))

omprtheta <- get_solution(result, vtheta)
omprlambda <- get_solution(result, vlambda[j])

ND <- 4 # Four Decision Making Units or DMUs
NX <- 1 # One input
NY <- 1 # One output

# Only doing analysis for one unit at a time to start

results.efficiency <- matrix(rep(-1.0, 1), nrow=1, ncol=1)
results.lambda <- matrix(rep(-1.0, ND), nrow=1,ncol=ND)

results.efficiency <- t(omprtheta)
colnames(results.efficiency) <- c("CCR-IO")
results.lambda <- t(omprlambda[3])

# Takes the third column from the results and transposes results
# to be structured correctly for later viewing

colnames(results.lambda) <- c("L_A", "L_B", "L_C", "L_D")

pander(cbind(results.efficiency, results.lambda),

caption="Input-Oriented Envelopment Analysis for DMU B (CCR-IO)")
