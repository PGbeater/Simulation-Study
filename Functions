# needed pack
library("dplyr")
library("tidyr")
library("ggplot2")
library("patchwork")
#------------------------------------------------------Parameter description-------------------------------------------------------
# D: distribution of the association between the covariates and the outcomes                                                      
# Y_num: the number of outcomes; X_num: the number of covariates; sample_num: the number of sample size ; sim_num: simulation times (iteration)
# CO_effect: a vector, the effects of covariates on outcomes
# TO: a vector, the treatment effect on the outcomes
# Tpre: the prevalence of treatment
# Oinci: the incidence of outcomes
# UCT: a vector, the effects of unmeasured covariates on the treatment
# UCO: a matrix, the effects of unmeasured covariates on the outcomes

# Propensity score models
PS_Model_name <- c("Full model","Treatment specific model",
                   "Related to 100% outcomes model",
                   "Related to at least 90% outcomes model",
                   "Related to at least 75% outcomes model",
                   "Related to at least 50% outcomes model",
                   "Related to at least 25% outcomes model",
                   "Related to at least 10% outcomes model",
                   "Generic related to at least one outcome model",
                   "Y specific model")
# Function 1: Association Pattern & Propensity Score Model Generating Function
ASSO_PSM_Function <- function(Y_num,X_num,D){
  # name covariates, X1 to Xn
  Covariate_X <- paste0("X",1:X_num)
  
  ## Pattern
  # create empty binary numbers for the association between covariates and outcomes
  binary_num_X <- rep(paste0(rep(0,Y_num),collapse = ""),X_num) 
  if (D == "Specific"){
    # Specific Adverse Event Association Pattern
    # use beta distribution
    u_data <- rbeta(X_num,0.2,1)
    count_relatedY <- ceiling(u_data*Y_num)
    for (i_XS in 1:X_num) {
      times <- count_relatedY[i_XS]
      # total number of outcomes that this covariate is related to
      vector_bi <- c(rep(1,times),rep(0,(Y_num-times)))
      # randoml position for "1" 
      binary_num <- sample(vector_bi) 
      binary_num <- paste0(binary_num, collapse = "") 
      binary_num_X[i_XS] <- binary_num
    }
    
  } else if (D == "Broad"){
    # Broad Adverse Event Association Pattern
    u_data <- rbeta(X_num,1,0.2)
    count_relatedY <- ceiling(u_data*Y_num)
    for (i_XB in 1:X_num) {
      times <- count_relatedY[i_XB]
      # total number of outcomes that this covariate is related to
      vector_bi <- c(rep(1,times),rep(0,(Y_num-times)))
      # randomly position for "1"
      binary_num <- sample(vector_bi) 
      binary_num <- paste0(binary_num, collapse = "") 
      binary_num_X[i_XB] <- binary_num
    }
  } else if (D == "Center"){
    # Center Adverse Event Association Pattern
    count_relatedY <- rpois(X_num,lambda = Y_num/2)
    count_relatedY <- ifelse(count_relatedY>Y_num,Y_num,count_relatedY)
    for (i_XC in 1:X_num) {
      times <- count_relatedY[i_XC]
      # total number of outcomes that this covariate is related to
      vector_bi <- c(rep(1,times),rep(0,(Y_num-times)))
      # randomly position for "1"
      binary_num <- sample(vector_bi) 
      binary_num <- paste0(binary_num, collapse = "") 
      binary_num_X[i_XC] <- binary_num
    }
  } else if (D == "UShape"){
    # Broad Adverse Event Association Pattern
    u_data <- rbeta(X_num,0.2,0.2)
    count_relatedY <- ceiling(u_data*Y_num)
    for (i_XU in 1:X_num) {
      times <- count_relatedY[i_XU]
      # total number of outcomes that this covariate is related to
      vector_bi <- c(rep(1,times),rep(0,(Y_num-times)))
      # randomly position for "1"
      binary_num <- sample(vector_bi) 
      binary_num <- paste0(binary_num, collapse = "") 
      binary_num_X[i_XU] <- binary_num
    }
  } else if (D == "System Clustered"){
    # System Clustered Adverse Event Association Pattern
    # generate the association between 11/10 covariates with each 5 outcomes that are systematic (cardiovascular, neurological, allergic...) diseases
    # Here we assumed 4 systems
    bnx1 <- rep(NA,11)
    for (i_bnx1 in 1:11) {
      num <- sample(c(0,1),5,replace = TRUE,prob = c(0.3, 0.7))
      bnx <- paste0(c(num,rep(0,Y_num-5)),collapse = "")
      bnx1[i_bnx1] <- bnx
    }
    binary_num_X[9:19] <- bnx1
    
    bnx2 <- rep(NA,11)
    for (i_bnx2 in 1:11) {
      num <- sample(c(0,1),5,replace = TRUE,prob = c(0.3, 0.7))
      bnx <- paste0(c(rep(0,5),num,rep(0,Y_num-10)),collapse = "")
      bnx2[i_bnx2] <- bnx
    }
    binary_num_X[20:30] <- bnx2
    
    bnx3 <- rep(NA,10)
    for (i_bnx3 in 1:10) {
      num <- sample(c(0,1),5,replace = TRUE,prob = c(0.3, 0.7))
      bnx <- paste0(c(rep(0,10),num,rep(0,Y_num-15)),collapse = "")
      bnx3[i_bnx3] <- bnx
    }
    binary_num_X[31:40] <- bnx3
    
    
    bnx4 <- rep(NA,10)
    for (i_bnx4 in 1:10) {
      num <- sample(c(0,1),5,replace = TRUE,prob = c(0.3, 0.7))
      bnx <- paste0(c(rep(0,15),num),collapse = "")
      bnx4[i_bnx4] <- bnx
    }
    binary_num_X[41:50] <- bnx4
    binary_num_X[9:50] <- sample(binary_num_X[9:50])
    count_relatedY <- unname(sapply(binary_num_X, function(x) sum(as.numeric(unlist(strsplit(x, ""))))))
  } else {
    stop("Invalid distribution type. Choose from 'Specific', 'Broad','Center','UShape','System Clustered'.")
  }
  
  # 20% prob of being risk factor (not related to treatment), last 20% of 50 covariates are not related to treatment
  Treatment_related <- c(rep(1,floor(0.8*X_num)),rep(0,ceiling(0.2*X_num)))
  # Create a data form
  check_X_form <- data.frame(Covariate_X,Treatment_related,binary_num_X,count_relatedY)
  
  # Fixed some associations betweeen covariates and the outcomes
  # make sure at least 1 Instrumental variable X1
  check_X_form[1,] <- c("X1",1,paste(rep(0,Y_num),collapse = ""),0) 
  # related to one outcome
  check_X_form[2,] <- c("X2",1,paste(c(1,rep(0,Y_num-1)),collapse = ""),1)
  # related to 10% outcomes
  check_X_form[3,] <- c("X3",1,paste(c(rep(1,ceiling(0.1*Y_num)),rep(0,Y_num-ceiling(0.1*Y_num))),collapse = ""),ceiling(0.1*Y_num))
  # related to 25% outcomes
  check_X_form[4,] <- c("X4",1,paste(c(rep(1,ceiling(0.25*Y_num)),rep(0,Y_num-ceiling(0.25*Y_num))),collapse = ""),ceiling(0.25*Y_num))
  # related to 50% outcomes
  check_X_form[5,] <- c("X5",1,paste(c(rep(1,ceiling(0.5*Y_num)),rep(0,Y_num-ceiling(0.5*Y_num))),collapse = ""),ceiling(0.5*Y_num))
  # related to 75% outcomes
  check_X_form[6,] <- c("X6",1,paste(c(rep(1,ceiling(0.75*Y_num)),rep(0,Y_num-ceiling(0.75*Y_num))),collapse = ""),ceiling(0.75*Y_num))
  # related to 90% outcomes
  check_X_form[7,] <- c("X7",1,paste(c(rep(1,ceiling(0.9*Y_num)),rep(0,Y_num-ceiling(0.9*Y_num))),collapse = ""),ceiling(0.9*Y_num))
  # related to 100% outcomes, for related to 100% outcome model
  check_X_form[8,] <- c("X8",1,paste(c(rep(1,Y_num)),collapse = ""),Y_num)
  
  
  # numeric 
  check_X_form$count_relatedY <- as.numeric(check_X_form$count_relatedY)
  colnames(check_X_form) <- c("Covariate","Related to Treatment","Binary number_X","Related to how many Outcomes")
  
  # seperate binary numbers
  # create association table for the treatment, covariates and outcomes
  Binary_1 <- check_X_form$`Binary number_X`
  TotalX_Y_list <- strsplit(Binary_1,split = "")
  TotalX_Y_table <- do.call(rbind,TotalX_Y_list)
  TotalX_Y_table <- cbind(check_X_form$Covariate,TotalX_Y_table)
  colnames(TotalX_Y_table) <- c("Covariate",paste0("Y",1:Y_num,"_related"))
  TotalX_Y_table <- as.data.frame(TotalX_Y_table)
  
  # Propensity score model----------------------------------------------------------------------------------------------------------------------------------------
  ## Full model
  Full_model <- paste0("X",1:X_num,collapse = "+")
  Full_model <- paste0("Treat~",Full_model)
  
  ## Treatment model- select covariates related to treatment
  Treatment_model <- paste0(check_X_form$Covariate[check_X_form$`Related to Treatment`== 1], collapse = "+")
  Treatment_model <- paste0("Treat~",Treatment_model)
  
  
  ## 100% outcome related model
  Related_100perc_num <- Y_num*1
  Related_100perc_Covariate <- check_X_form$Covariate[check_X_form$`Related to how many Outcomes` >= Related_100perc_num]
  Related_100perc_model <- paste0(Related_100perc_Covariate, collapse = "+") 
  Related_100perc_model <- paste0("Treat~",Related_100perc_model)
  
  ## 90% outcome related model(at least)
  Related_90perc_num <- Y_num*0.9
  Related_90perc_Covariate <- check_X_form$Covariate[check_X_form$`Related to how many Outcomes` >= Related_90perc_num]
  Related_90perc_model <- paste0(Related_90perc_Covariate, collapse = "+") 
  Related_90perc_model <- paste0("Treat~",Related_90perc_model)
  
  ## 75% outcome related model(at least)
  Related_75perc_num <- Y_num*0.75
  Related_75perc_Covariate <- check_X_form$Covariate[check_X_form$`Related to how many Outcomes` >= Related_75perc_num]
  Related_75perc_model <- paste0(Related_75perc_Covariate, collapse = "+") 
  Related_75perc_model <- paste0("Treat~",Related_75perc_model)
  
  
  ## 50% outcome related model(at least)
  Related_50perc_num <- Y_num*0.50
  Related_50perc_Covariate <- check_X_form$Covariate[check_X_form$`Related to how many Outcomes` >= Related_50perc_num]
  Related_50perc_model <- paste0(Related_50perc_Covariate, collapse = "+") 
  Related_50perc_model <- paste0("Treat~",Related_50perc_model)
  
  ## 25% outcome related model(at least)
  Related_25perc_num <- Y_num*0.25
  Related_25perc_Covariate <- check_X_form$Covariate[check_X_form$`Related to how many Outcomes` >= Related_25perc_num]
  Related_25perc_model <- paste0(Related_25perc_Covariate, collapse = "+") 
  Related_25perc_model <- paste0("Treat~",Related_25perc_model)
  
  ## 10% outcome related model(at least)
  Related_10perc_num <- Y_num*0.1
  Related_10perc_Covariate <- check_X_form$Covariate[check_X_form$`Related to how many Outcomes` >= Related_10perc_num]
  Related_10perc_model <- paste0(Related_10perc_Covariate, collapse = "+") 
  Related_10perc_model <- paste0("Treat~",Related_10perc_model)
  
  ## Generic model - select covariates related to at least one outcome
  Generic_model <- paste0(check_X_form$Covariate[check_X_form$`Related to how many Outcomes` > 0], collapse = "+")
  Generic_model <- paste0("Treat~",Generic_model)
  
  ## Y specific model for each Y
  Y_specific_Covariate <- vector("list",length = Y_num)
  names(Y_specific_Covariate) <- paste0("Y",1:Y_num,"_Specific")
  for (i_YSpeX in 1:Y_num) {
    # start from the 2nd column, because the first column is the covariates' name
    # check from X1 to Xn, whether the covariate is related to a specific Y
    index <- TotalX_Y_table[,(1+i_YSpeX)] == "1"
    # extract the covriate
    Covariate_Spe <- TotalX_Y_table$Covariate[index]
    Y_specific_Covariate[[i_YSpeX]] <- Covariate_Spe
  }
  # Build the Y-specific model for every Y
  Y_specific_model <- sapply(1:Y_num, function(i_ysm)paste0(Y_specific_Covariate[[i_ysm]], collapse = "+"))
  Y_specific_model <- paste0("Treat~",Y_specific_model)
  
  PS_model <- c(Full_model,Treatment_model,Related_100perc_model,Related_90perc_model,
                Related_75perc_model,Related_50perc_model,Related_25perc_model,
                Related_10perc_model,Generic_model )
  names(PS_model) <- c("Full_model","Treatment_model",
                       "Related_100perc_model",
                       "Related_90perc_model",
                       "Related_75perc_model",
                       "Related_50perc_model",
                       "Related_25perc_model",
                       "Related_10perc_model",
                       "Generic_model")
  return(list(check_X_form = check_X_form,
              TotalX_Y_table = TotalX_Y_table,
              PS_model = PS_model,
              Y_specific_model = Y_specific_model))
}

# Function 2: Simulation Function
Simulation_Function <- function(D,Y_num,X_num,sample_num,sim_num,CO_effect,
                                TO, Tpre, Oinci,UCT, UCO){
  # Choose which association pattern is going to be used
  if (D == "Specific"){
    Association_and_PSmodel <- Association_and_PSmodel_S
  } else if (D == "System Clustered"){
    Association_and_PSmodel <- Association_and_PSmodel_SC
  } else if (D == "Broad"){
    Association_and_PSmodel <- Association_and_PSmodel_B
  } else if (D == "Center"){
    Association_and_PSmodel <- Association_and_PSmodel_C
  } else if (D == "UShape"){
    Association_and_PSmodel <- Association_and_PSmodel_US
  } else {
    stop("Invalid distribution type. Choose from 'Specific', 'Broad','Center','UShape','System Clustered'.")
  }
  check_X_form <- Association_and_PSmodel[["check_X_form"]]
  TotalX_Y_table <- Association_and_PSmodel[["TotalX_Y_table"]]
  PS_model <- Association_and_PSmodel[["PS_model"]]
  Y_specific_model <- Association_and_PSmodel[["Y_specific_model"]]
  
  # empty lists in final result: 
  # 1. estimated betas. 
  # 2. 95% confidence interval of each estimated beta. 
  # 3. Associations & PS models of each iteration
  empty_list <- vector("list",length = Y_num)
  names(empty_list) <- paste0("Y",1:Y_num)
  empty_df <- as.data.frame(matrix(nrow = sim_num,ncol = 2))
  for (i_EL in 1:Y_num) {
    empty_list[[i_EL]] <- vector("list", length = length(PS_Model_name))
  }
  Yn_Betas_list <- empty_list
  Yn_CIs_list <- empty_list
  for (i_EM in 1:Y_num) {
    for (j_EM in 1:length(PS_Model_name)) {
      Yn_CIs_list[[i_EM]][[j_EM]] <- empty_df
    }
  }
  fitmodel_list <- vector("list",length = sim_num)
  fitmodelYspecific_list <- vector("list",length = sim_num)
  Association_and_PSmodel_list <- vector("list",length = sim_num)
  for (i_sim in 1:sim_num) {
    # set parameters
    # always assume each Y has an unmeasured confounder
    U_num <- Y_num 
    # CT , if covariates are related to treatment, effects of these covariates on treatment is 1.2
    CT <- c(rep(1,X_num))
    index_CT <- which(check_X_form$`Related to Treatment`== 1)
    CT[index_CT] <- 1.2
    
    ### CO 
    # firstly, create empty data frame for CO
    # create all RR = 1/ no effects (log(1) = 0)
    CO <- data.frame(matrix(ncol = 0, nrow = X_num))
    for (i_co in 1:Y_num) {
      CO[i_co] <- rep(1, X_num)
    }
    # secondly, if the covariate is only related to 1 outcome, the effect of this covariate on the outcome is 1.2. 
    # If the covariate is only related to more than 1 outcome, the effect of this covariate on those outcomes is 1.3 (CO_effect = CO_effect_input, is a vector of 1.3).
    for (i_Yrelated in 1:Y_num) {
      index_change <- TotalX_Y_table[,1+i_Yrelated]=="1"
      CO[,i_Yrelated][index_change] <- 1.2
      index_shared <- check_X_form$`Related to how many Outcomes`>= 2 & TotalX_Y_table[,1+i_Yrelated]=="1"
      CO[,i_Yrelated][index_shared] <- CO_effect[i_Yrelated]
    }
    
    # generate data
    seqn <- 1:sample_num
    beta_Tpre <- log(Tpre/(1-Tpre))
    beta_TO <- log(TO)
    beta_CT <- log(CT)
    beta_UCT <- log(UCT)
    beta_Oinci <- log(Oinci)
    beta_CO <- log(CO)
    beta_UCO <- log(UCO)
    
    X_list <- lapply(1:X_num, function(x_i)rnorm(sample_num,0,1))
    names(X_list) <- paste0("X",1:X_num)
    U_list <- lapply(1:U_num, function(u_i)rnorm(sample_num,0,1))
    names(U_list) <- paste0("U",1:U_num)
    X_df <- as.data.frame(X_list)
    U_df <- as.data.frame(U_list)
    df_XU <- cbind(X_df,U_df)
    
    # Creat Treat
    # Vector * matrix/data.frame by column
    beta_CT_UCT <- c(beta_CT,beta_UCT)
    # column * the beta_CT_UCT
    BCT_X <- sweep(df_XU, 2, beta_CT_UCT, FUN = "*")
    # Prevalence of treatment
    BCT_X$B0 <- beta_Tpre 
    # Take Sum 
    BCT_X$SUM <- rowSums(BCT_X)
    # formula 1/(1+1/exp()) or 1/(1+exp(-))
    BCT_X$Prob_Treat <- with(BCT_X,1/(1+1/exp(SUM)))
    BCT_X$Treat <- rbinom(sample_num,1,prob = BCT_X$Prob_Treat)
    
    ### new dataframe for X,U and treatment
    df_XUT <- cbind(df_XU,Treat = BCT_X$Treat)
    
    ## creat outcomes follow the poission distribution
    ### for loop , similar as upper process
    Y_list <- vector("list",length = Y_num)
    for (Y_i in 1:Y_num) {
      beta_CO_UCO <- c(beta_CO[,Y_i],beta_UCO[,Y_i])
      BCO_X <- sweep(df_XU, 2, beta_CO_UCO, FUN = "*")
      # Take Sum
      BCO_X$SUM <- rowSums(BCO_X)
      # calculated the lambda of poission distribution
      lambda <- exp(beta_Oinci[Y_i]+beta_TO[Y_i]*df_XUT$Treat+BCO_X$SUM)
      Y_list[[Y_i]] <- rpois(sample_num,lambda = lambda)
    }
    names(Y_list) <- paste0("Y",1:Y_num)
    df_Y <- as.data.frame(Y_list)
    df_XUTY <- cbind(seqn,df_XUT,df_Y)
    
    # Calculation of inverse probability weighting
    # Extract PS models from PS_model (a vector)
    input_PSmodels = c(PS_model["Full_model"],PS_model["Treatment_model"],
                       PS_model["Related_100perc_model"],PS_model["Related_90perc_model"],
                       PS_model["Related_75perc_model"],PS_model["Related_50perc_model"],
                       PS_model["Related_25perc_model"],PS_model["Related_10perc_model"],
                       PS_model["Generic_model"])
    ipws_fit <-  lapply(input_PSmodels, function(TXformula){
      fit1 <- glm(as.formula(TXformula),family = "binomial",data = df_XUTY)
      p1 <- predict(fit1, type = "response")
      # ATT, the treatment effect among the treated group
      ipw <- ifelse(df_XUTY$Treat == 1, 1, p1/(1-p1)) 
      list(ipws = ipw,fit_model=fit1)
    })
    ipws <- lapply(ipws_fit, function(x)x$ipws)
    names(ipws) <- paste0("ipw",1:length(input_PSmodels))
    fit_model <- lapply(ipws_fit, function(x)x$fit_model$coefficients)
    
    ## Y_specific model ipw_Yspecific
    ipwfit_Yspecific <- lapply(Y_specific_model, function(TXformula1){
      fit1 <- glm(as.formula(TXformula1),family = "binomial",data = df_XUTY)
      p1 <- predict(fit1, type = "response")
      ipw <- ifelse(df_XUTY$Treat == 1, 1, p1/(1-p1))
      list(ipw_Yspecific = ipw,fit_model_Yspecific=fit1)
    })
    ipw_Yspecific <- lapply(ipwfit_Yspecific, function(x)x$ipw_Yspecific)
    names(ipw_Yspecific) <- paste0("ipw_Y",1:Y_num,"_Specific")
    fit_model_Yspecific <- lapply(ipwfit_Yspecific, function(x)x$fit_model_Yspecific$coefficients)
    
    # Outcome regression for all Ys using different propensity score models
    betas_list <- list()  
    cis_list <- list()
    outcome_model <- paste0("Y",1:Y_num,"~Treat")
    for (i_Betas_in_Y in 1:Y_num) {
      betasCI <- lapply(ipws, function(weight){
        model <- glm(as.formula(outcome_model[i_Betas_in_Y]),family = "poisson",
                     data = df_XUTY,weights = weight)
        conf_intervals <- confint.default(model, level = 0.95)[2, ]
        betas <- coef(model)[2]
        list(betas = betas, cis = list(lower_bound = conf_intervals[1],upper_bound = conf_intervals[2]))
      })
      model_Yspe <- glm(as.formula(outcome_model[i_Betas_in_Y]),family = "poisson",
                        data = df_XUTY,weights = ipw_Yspecific[[i_Betas_in_Y]])
      conf_intervals_Yspe <- confint.default(model_Yspe, level = 0.95)[2, ]
      betas_Yspe <- coef(model_Yspe)[2]
      betasCI[[length(PS_Model_name)]] <- list(betas = betas_Yspe, cis = list(lower_bound = conf_intervals_Yspe[1],upper_bound = conf_intervals_Yspe[2]))
      betas_list[[i_Betas_in_Y]] <- sapply(betasCI, function(x) x$betas)
      cis_list[[i_Betas_in_Y]] <- sapply(betasCI, function(x) x$cis)
    }
    names(betas_list) <- paste0("Y",1:Y_num)
    for (i_nameBeta in 1:Y_num) {
      names(betas_list[[i_nameBeta]]) <- paste0("IPW",1:length(PS_Model_name))
    }
    names(cis_list) <- paste0("Y",1:Y_num)
    
    # Store Betas
    for (i_YnBetas in 1:Y_num) {
      for (i_YnIPW in 1:length(PS_Model_name)) {
        Yn_Betas_list[[i_YnBetas]][[i_YnIPW]][i_sim] <- betas_list[[i_YnBetas]][[i_YnIPW]] 
      }
    }
    # Store CIs
    cis_df_list <- list()
    for (i_CIdf in 1:Y_num) {
      df <- data.frame(cis_list[[i_CIdf]])
      df <- data.frame(t(df))
      rownames(df) <- paste0("IPW",1:length(PS_Model_name))
      colnames(df) <- c("lower_bound","upper_bound")
      cis_df_list[[i_CIdf]] <- df
    }
    names(cis_df_list) <- paste0("Y",1:Y_num) 
    for (i_YnCIs in 1:Y_num) {
      for (i_YnCIIPW in 1:length(PS_Model_name)) {
        Yn_CIs_list[[i_YnCIs]][[i_YnCIIPW]][i_sim,] <- cis_df_list[[i_YnCIs]][i_YnCIIPW,]
      }
    }
    # Store Association and PS models
    fitmodel_list[[i_sim]] <- fit_model
    fitmodelYspecific_list[[i_sim]] <- fit_model_Yspecific
    Association_and_PSmodel_list[[i_sim]] <- Association_and_PSmodel
  }
  return(list(Yn_Betas_list = Yn_Betas_list,Yn_CIs_list = Yn_CIs_list,
              Association_and_PSmodel_list = Association_and_PSmodel_list,
              fitmodel_list = fitmodel_list,
              fitmodelYspecific_list = fitmodelYspecific_list))
}

# Function 3: Coverage Table Function
Coverage_Table_Function <- function(Result_list){
  Coverage_table <- as.data.frame(matrix(nrow = Y_num_input,ncol = length(PS_Model_name)))
  rownames(Coverage_table) <- paste0("Y",1:Y_num_input)
  colnames(Coverage_table) <- PS_Model_name
  for (i_YnCover in 1:Y_num_input) {
    for (i_IPWCover in 1:length(PS_Model_name)) {
      log_TO <- log(TO_input[i_YnCover])
      df_covered <- Result_list[["Yn_CIs_list"]][[i_YnCover]][[i_IPWCover]]
      df_covered$Covered <- ifelse(df_covered$V1<log_TO & df_covered$V2>log_TO,1,0)
      Coverage <- sum(df_covered$Covered)/sim_num_input
      Coverage_table[i_YnCover,i_IPWCover] <- Coverage
    }
  }
  return(Coverage_table)
}


# Function 4: Calculate absolute bias, variance, and MSE of each propensity score models 
Result_Form_function_allY<- function(Result_list,Coverage_table){
  Result_Form_list <- vector("list",length = Y_num_input)
  #for each Y
  for (i_Yn in 1:Y_num_input) { 
    # Effect of treatment on outcomes(Ys)
    TO_log <- log(TO_input) 
    # Empty vectors
    ABS_BIAS <- c(rep(NA,length(PS_Model_name)))
    VARIANCE <- c(rep(NA,length(PS_Model_name)))
    MSE <- c(rep(NA,length(PS_Model_name))) 
    # estimates using different ps models
    for (i_BetasinYn in 1:length(PS_Model_name)) { 
      # Result_list[[1]] is the log(RR) = betas of all iterations
      Result_Betalist <- Result_list[[1]]
      beta_values <-  Result_Betalist[[i_Yn]][[i_BetasinYn]]
      ABS_BIAS[i_BetasinYn] <- abs(mean(beta_values-TO_log[i_Yn]))
      VARIANCE[i_BetasinYn] <- sd(beta_values)^2
      MSE[i_BetasinYn] <- ABS_BIAS[i_BetasinYn]^2 + VARIANCE[i_BetasinYn]
    }
    METHOD <-PS_Model_name
    COVERAGE <- as.numeric(Coverage_table[i_Yn,])
    Result_Form <- data.frame(METHOD,ABS_BIAS,VARIANCE,MSE,COVERAGE)
    Result_Form_list[[i_Yn]] <- Result_Form
  }
  names(Result_Form_list) <- paste0("Y",1:Y_num_input)
  return(Result_Form_list)
}

# Function 5: average abs(bias), variance, and MSE of each model, across different outcomes.
Avery_form_function <- function(Result_Form_list){
  newdf <- do.call(rbind,Result_Form_list)
  Avery_abs_bias <- c(rep(NA,length(PS_Model_name)))
  Avery_variance <- c(rep(NA,length(PS_Model_name)))
  Avery_mse <- c(rep(NA,length(PS_Model_name)))
  Avery_coverage <- c(rep(NA,length(PS_Model_name)))
  for (i_method in 1:length(PS_Model_name)) {
    Avery_abs_bias[i_method] <- mean(newdf$ABS_BIAS[newdf$METHOD == newdf$METHOD[i_method]])
    Avery_variance[i_method] <- mean(newdf$VARIANCE[newdf$METHOD == newdf$METHOD[i_method]])
    Avery_mse[i_method] <- mean(newdf$MSE[newdf$METHOD == newdf$METHOD[i_method]])
    Avery_coverage[i_method] <- mean(newdf$COVERAGE[newdf$METHOD == newdf$METHOD[i_method]])
  }
  METHOD <- PS_Model_name
  Avery_form <- data.frame(METHOD,Avery_abs_bias,Avery_variance,Avery_mse,Avery_coverage)
  return(Avery_form)
}
