library(pander)
library(magrittr)
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

result0 <- MIPModel() %>%
  add_variable(Chairs, type = "continuous", lb = 0) %>%
  add_variable(Desks, type = "continuous", lb = 0) %>%
  set_objective(20*Chairs + 14*Desks, "max") %>%
  add_constraint(6*Chairs + 2*Desks <= 2000) %>%
  add_constraint(8*Chairs + 6*Desks <= 2000) %>%
  add_constraint(6*Chairs + 4*Desks <= 1440) %>%
  add_constraint(40*Chairs + 25*Desks <= 9600) %>%
  solve_model(with_ROI(solver = "glpk"))

print(solver_status(result0))
print(objective_value(result0))
print(get_solution(result0, Chairs))
print(get_solution(result0, Desks))

