library(pander)
library(magrittr)
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

BaseModelCDT <- MIPModel() %>%
  add_variable(Chairs, type = "continuous", lb = 0) %>%
  add_variable(Desks, type = "continuous", lb = 0) %>%
  add_variable(Tables, type = "continuous", lb = 0) %>%

  set_objective(20*Chairs + 14*Desks + 16*Tables, "max") %>%

  add_constraint(6*Chairs + 2*Desks + 4*Tables <= 2000) %>% #fabrication
  add_constraint(8*Chairs + 6*Desks + 4*Tables <= 2000) %>% #assembly
  add_constraint(6*Chairs + 4*Desks + 8*Tables <= 1441) %>% #machining
  add_constraint(40*Chairs + 25*Desks + 25*Tables <= 9600) %>% #wood
  add_constraint(Tables <= 200) %>% #

  solve_model(with_ROI(solver = "glpk"))

obj_val <- objective_value(BaseModelCDT)
xchairs <- get_solution (BaseModelCDT, Chairs)
xdesks <- get_solution (BaseModelCDT, Desks)
xtables <- get_solution (BaseModelCDT, Tables)

inc_mc_res <- cbind(xchairs, xdesks, xtables, obj_val)
rownames(inc_mc_res) <- ""
pander(inc_mc_res, caption="Production Plan with Continuous variables")

IntModelCDT <- MIPModel() %>%
  add_variable(Chairs, type = "integer", lb = 0) %>%
  add_variable(Desks, type = "integer", lb = 0) %>%
  add_variable(Tables, type = "integer", lb = 0) %>%

  set_objective(20*Chairs + 14*Desks + 16*Tables, "max") %>%

  add_constraint(6*Chairs + 2*Desks + 4*Tables <= 2000) %>% #fabrication
  add_constraint(8*Chairs + 6*Desks + 4*Tables <= 2000) %>% #assembly
  add_constraint(6*Chairs + 4*Desks + 8*Tables <= 1441) %>% #machining
  add_constraint(40*Chairs + 25*Desks + 25*Tables <= 9600) %>% #wood
  add_constraint(Tables <= 200) %>% #

  solve_model(with_ROI(solver = "glpk"))

obj_val <- objective_value(IntModelCDT)
xchairs <- get_solution (IntModelCDT, Chairs)
xdesks <- get_solution (IntModelCDT, Desks)
xtables <- get_solution (IntModelCDT, Tables)

IntModelCDTres <- cbind(xchairs, xdesks, xtables, obj_val)
rownames(IntModelCDTres) <- ""
pander(IntModelCDTres, caption="Production Plan with Integer Variables")

LPRelax <- MIPModel() %>%
  add_variable(xx1, type = "continuous", lb = 0) %>%
  add_variable(xx2, type = "continuous", lb = 0) %>%

  set_objective(2*xx1 + 3*xx2, "max") %>%

  add_constraint(1.0*xx1 + 3.0*xx2 <= 8.25) %>% #Surfactant
  add_constraint(2.5*xx1 + 1.0*xx2 <= 8.75) %>% #Steel

  solve_model(with_ROI(solver = "glpk"))


obj_val <- objective_value(LPRelax)

x1 <- get_solution (LPRelax, xx1)
x2 <- get_solution (LPRelax, xx2)

inc_mc_res <- cbind(x1,x2,obj_val)
rownames(inc_mc_res) <- ""
pander(inc_mc_res, caption="Acme's Production Plan based on the LP Relaxation")

LPSubI <- MIPModel() %>%
  add_variable(xx1, type = "continuous", lb = 0) %>%
  add_variable(xx2, type = "continuous",lb = 0) %>%

  set_objective(2*xx1 + 3*xx2, "max") %>%

  add_constraint(1.0*xx1 + 3.0*xx2 <= 8.25) %>% #Surfactant
  add_constraint(2.5*xx1 + 1.0*xx2 <= 8.75) %>% #Steel
  add_constraint(xx1 <= 2.0) %>% #Bound for Subproblem 1

  solve_model(with_ROI(solver = "glpk"))

obj_val <- objective_value(LPSubI)
x1 <- get_solution (LPSubI, xx1)
x2 <- get_solution (LPSubI, xx2)

LPSubI_res <- cbind(x1,x2,obj_val)
rownames(LPSubI_res) <- ""
pander(LPSubI_res, caption="Acme's Production Plan based on the LP Subproblem I")

LPSubIII <- MIPModel() %>%
  add_variable(xx1, type = "continuous", lb = 0, ub = 2) %>%
  add_variable(xx2, type = "continuous", lb = 0, ub = 2) %>%

  set_objective(2*xx1 + 3*xx2, "max") %>%

  add_constraint(1.0*xx1 + 3.0*xx2 <= 8.25) %>% #Surfactant
  add_constraint(2.5*xx1 + 1.0*xx2 <= 8.75) %>% #Steel

  solve_model(with_ROI(solver = "glpk"))

obj_val <- objective_value(LPSubIII )
x1 <- get_solution (LPSubIII, xx1)
x2 <- get_solution (LPSubIII, xx2)

LPSubIII_res <- cbind(x1,x2,obj_val)
rownames(LPSubIII_res) <- ""
pander(LPSubIII_res, caption="Acme's Production Plan based on the LP Subproblem III")

LPSubIV <- MIPModel() %>%
  add_variable(xx1, type = "continuous", lb = 0, ub = 2) %>%
  add_variable(xx2, type = "continuous",lb = 3) %>%

  set_objective(2*xx1 + 3*xx2, "max") %>%

  add_constraint(1.0*xx1 + 3.0*xx2 <= 8.25) %>% #Surfactant
  add_constraint(2.5*xx1 + 1.0*xx2 <= 8.75) %>% #Steel

  solve_model(with_ROI(solver = "glpk"))

obj_val <- objective_value(LPSubIV )
x1 <- get_solution (LPSubIV , xx1)
x2 <- get_solution (LPSubIV , xx2)

LPSubIV_res <- cbind(x1,x2,obj_val)
rownames(LPSubIV_res) <- ""
pander(LPSubIV_res, caption="Acme's Production Plan based on the LP Subproblem IV")

LPSubII <- MIPModel() %>%
  add_variable(xx1, type = "continuous", lb = 3) %>%
  add_variable(xx2, type = "continuous",lb = 0) %>%

  set_objective(2*xx1 + 3*xx2, "max") %>%

  add_constraint(1.0*xx1 + 3.0*xx2 <= 8.25) %>% #Surfactant
  add_constraint(2.5*xx1 + 1.0*xx2 <= 8.75) %>% #Steel

  solve_model(with_ROI(solver = "glpk"))

obj_val <- objective_value(LPSubII )
x1 <- get_solution (LPSubII , xx1)
x2 <- get_solution (LPSubII , xx2)

LPSubII_res <- cbind(x1, x2, obj_val)

cat("Status of Subproblem II:", LPSubII$status)
## Status of Subproblem II: optimal
rownames(LPSubII_res) <- ""
pander(LPSubII_res, caption="Acme's Production Plan based on the LP Subproblem II")
