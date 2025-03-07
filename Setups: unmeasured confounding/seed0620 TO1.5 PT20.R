# PT20 IO0.5-------------------
# Specific
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "Specific"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.005,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO0.5_S.RData")

rm(list = ls())
gc()

# System Clustered
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "System Clustered"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.005,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO0.5_SC.RData")

rm(list = ls())
gc()

# Center
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "Center"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.005,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO0.5_C.RData")

rm(list = ls())
gc()

# Broad
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "Broad"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.005,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO0.5_B.RData")

rm(list = ls())
gc()

# UShape
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "UShape"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.005,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO0.5_US.RData")

rm(list = ls())
gc()

# PT20 IO5-----------------------------
# Specific
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "Specific"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.05,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO5_S.RData")

rm(list = ls())
gc()

# System Clustered
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "System Clustered"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.05,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO5_SC.RData")

rm(list = ls())
gc()

# Center
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "Center"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.05,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO5_C.RData")

rm(list = ls())
gc()

# Broad
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "Broad"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.05,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO5_B.RData")

rm(list = ls())
gc()

# UShape
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "UShape"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.05,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO5_US.RData")

rm(list = ls())
gc()

# PT20 IO10----------------------------
# Specific
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "Specific"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.1,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO10_S.RData")

rm(list = ls())
gc()

# System Clustered
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "System Clustered"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.1,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO10_SC.RData")

rm(list = ls())
gc()

# Center
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "Center"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.1,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO10_C.RData")

rm(list = ls())
gc()

# Broad
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "Broad"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.1,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO10_B.RData")

rm(list = ls())
gc()

# UShape
source("D:/JJWu/Unmeasured_code/Function.R")
set.seed(0620)
D_input <- "UShape"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.2
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.1,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input_un, UCO = UCO_input_un)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
})
save.image("D:/JJWu/Result_500_unmeasured/seed0620 TO1.5 PT20IO10_US.RData")

rm(list = ls())
gc()
