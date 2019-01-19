library(pander)
library(magrittr)
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

Base3VarModel <- MIPModel() %>%
  add_variable(Chairs, type = "continuous", lb = 0) %>%
  add_variable(Desks, type = "continuous", lb = 0) %>%
  add_variable(Tables, type = "continuous", lb = 0) %>%

  set_objective(20*Chairs + 14*Desks + 16*Tables, "max") %>%

  add_constraint(6*Chairs + 2*Desks + 4*Tables <= 2000) %>% #fabrication
  add_constraint(8*Chairs + 6*Desks + 4*Tables <= 2000) %>% #assembly
  add_constraint(6*Chairs + 4*Desks + 8*Tables <= 1440) %>% #machining
  add_constraint(40*Chairs + 25*Desks + 25*Tables <= 9600) %>% #wood
  add_constraint(Tables <= 200) %>% #

  solve_model(with_ROI(solver = "glpk"))

obj_val <- objective_value(Base3VarModel)
xchairs <- get_solution(Base3VarModel, Chairs)
xdesks <- get_solution(Base3VarModel, Desks)
xtables <- get_solution(Base3VarModel, Tables)


base_case_res <- cbind(xchairs, xdesks, xtables, obj_val)
rownames(base_case_res) <- ""
pander(base_case_res, caption="Production Plan for Base Case")

#rduals1 <- as.matrix(get_row_duals(result1), ncol=5)
rduals1 <- as.matrix(get_row_duals(Base3VarModel))
dimnames(rduals1) <- list(c("fabrication", "assembly", "machining", "wood", "demand"), c("Row Duals"))
pander(rduals1, caption="Shadow Prices of Constrained Resources")

IncMachiningHrs <- MIPModel() %>%
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

obj_val <- objective_value(IncMachiningHrs)
xchairs <- get_solution (IncMachiningHrs, Chairs)
xdesks <- get_solution (IncMachiningHrs, Desks)
xtables <- get_solution (IncMachiningHrs, Tables)

inc_mc_res <- cbind(xchairs, xdesks, xtables, obj_val)
rownames(inc_mc_res) <- ""
pander(inc_mc_res, caption="Production Plan with One Additional Machining Hour")

IncWood <- MIPModel() %>%

  add_variable(Chairs, type = "continuous", lb = 0) %>%
  add_variable(Desks, type = "continuous", lb = 0) %>%
  add_variable(Tables, type = "continuous", lb = 0) %>%

  set_objective(20*Chairs + 14*Desks + 16*Tables, "max") %>%

  add_constraint(6*Chairs + 2*Desks + 4*Tables <= 2000) %>% #fabrication
  add_constraint(8*Chairs + 6*Desks + 4*Tables <= 2000) %>% #assembly
  add_constraint(6*Chairs + 4*Desks + 8*Tables <= 1440) %>% #machining
  add_constraint(40*Chairs + 25*Desks + 25*Tables <= 19600) %>% #wood
  add_constraint(Tables <= 200) %>% #

  solve_model(with_ROI(solver = "glpk"))

obj_val <- objective_value(IncWood)
xchairs <- get_solution (IncWood, Chairs)
xdesks <- get_solution (IncWood, Desks)
xtables <- get_solution (IncWood, Tables)

inc_wd_res <- cbind(xchairs, xdesks, xtables, obj_val)
rownames(inc_wd_res) <- ""
pander(inc_wd_res, caption="Production Plan with 10,000 Additional Square Feet of Wood")

#cduals1 <- as.matrix(get_column_duals(result1), ncol=1 )

cduals1 <- as.matrix(get_column_duals(Base3VarModel) )

dimnames(cduals1) <- list(c("Chairs", "Desks", "Tables"), c("Column Duals"))
pander(cduals1, caption="Incorrect Reduced Costs of Variables")

# Value of Resources
chair_res_used <- cbind(rduals1, c(2, 8, 6, 40, 0))
colnames(chair_res_used) <- c("Row Duals", "Resources Used")
pander(chair_res_used, caption="Resources Used by a Chair and their Shadow Prices")

desk_res_used <- cbind(rduals1, c(2, 6, 4, 25, 0))
colnames(desk_res_used) <- c("Row Duals", "Resources Used")
pander(desk_res_used, caption="Resources Used by a Desk and their Shadow Prices")

Table1Model <- MIPModel() %>%
  add_variable(Chairs, type = "continuous", lb = 0) %>%
  add_variable(Desks, type = "continuous", lb = 0) %>%
  add_variable(Tables, type = "continuous", lb = 1) %>%

  set_objective(20*Chairs + 14*Desks + 16*Tables, "max") %>%

  add_constraint(6*Chairs + 2*Desks + 4*Tables <= 2000) %>% #fabrication
  add_constraint(8*Chairs + 6*Desks + 4*Tables <= 2000) %>% #assembly
  add_constraint(6*Chairs + 4*Desks + 8*Tables <= 1440) %>% #machining
  add_constraint(40*Chairs + 25*Desks + 25*Tables <= 9600) %>% #wood
  add_constraint(Tables <= 200) %>% #

  solve_model(with_ROI(solver = "glpk"))

obj_val <- objective_value(Table1Model)
xchairs <- get_solution (Table1Model, Chairs)
xdesks <- get_solution (Table1Model, Desks)
xtables <- get_solution (Table1Model, Tables)

Table1_case_res <- cbind(xchairs, xdesks, xtables, obj_val)
rownames(Table1_case_res) <- ""
#rownames(Table1_case_res) <- "Amount to Produce"
pander(Table1_case_res, caption="Production Plan with Table Set to One")

rownames(base_case_res) <- "Base Case"
Table1_case_res <- cbind(xchairs, xdesks, xtables, obj_val)

rownames(Table1_case_res) <- "with forced change to Tables"
pander(rbind(base_case_res, Table1_case_res), caption="Impact of a Forced Change in Tables")

bookcase_res_used <- cbind(rduals1, c(6, 12, 16, 80, 0))
colnames(bookcase_res_used) <- c("Row Duals", "Resources Used")
pander(bookcase_res_used, caption="Resources Used by a Bookcase and their Shadow Prices")
