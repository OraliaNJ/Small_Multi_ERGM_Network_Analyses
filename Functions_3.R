### Modeling Analysis ####

modeling_edges <- function(n)
{  
  n
  n1 <- n~edges
  fit_n1 <-ergm(n1)
  vcov(fit_n1)#extracts the variance-covariance matrix of parameter estimates.
  print(fit_n1)#Print the call, the estimate, and the method used to obtain it.
  nobs(fit_n1) #Return the number of informative dyads of a model fit.
  summary(fit_n1)
  return (fit_n1)
}

modeling_covariate <- function(n)
{
  #Covariates
  fit_nc <- ergm(n~edges+ nodecov(".NetworkName"))
  fit_nc
  summary(fit_nc)
  return(fit_nc)
}

modeling_names <- function(n)
{
  fit_n1_names <- ergm(n~edges+ nodefactor("vertex.names"))
  fit_n1_names
  summary(fit_n1_names)
  return(fit_n1_names)
}

mod_goodness <- function(fit_n1)
{
  #Goodness-of-fit for in-degree 
  gof_n <-gof(fit_n1)
  gof_n
  
  #Close the device using 
  graphics.off()
  par(mfrow=c(2,2))
  plot(gof_n)
  plot(gof_n)
  plot(gof_n)
  plot(gof_n)
  
  return (gof_n)
}

modeling_residuals <- function(fit_n1)
{
  gof_n1 <-gofN(fit_n1, GOF = ~edges) #Pearson residuals 
  gof_n1
  summary(gof_n1)
  
  #plot(gof_n1)
  #autoplot(gof_n1)
  return (gof_n1)
}

comparative_mod<- function(n_mod_Imp, n_mod_Tar, sel_p)
{
  #Close the device using 
  graphics.off()
  
  n_mod_Imp
  n_mod_Tar
  
  par(mfrow=c(2,2))
  #t_main <-paste("Model Importation by",sel_p, "main Partners")
  plot(n_mod_Imp)
  
  #t_main <-paste("Model Tariffs by",sel_p, "main Partners")
  plot(n_mod_Tar)
  
}

Compare_Dist_Degrees_sel_p <- function(net_r_Imp, net_r_Tar, sel_p, path2)
{
  file_name <- "_Comp_Distribution_"
  #Close the device using 
  graphics.off()
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name, sel_p)
  image_ff <- paste(image_f, "png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  
  #saving file png 
  png(temp_ff)
  
  #Node_Degrees
  par(mfrow=c(1,2))
  

  t_main_i <- paste(sel_p, "Partners-Import")
  deg <- degree(net_r_Imp, mode="all")
  deg
  hist(deg, breaks=1:vcount(net_r_Imp)-1, main=t_main_i)
  
  t_main_t <- paste(sel_p, "Partners-Tariffs")
  deg <- degree(net_r_Tar, mode="all")
  deg
  hist(deg, breaks=1:vcount(net_r_Tar)-1, main=t_main_t)

}

modeling_log_odds<- function()
{
  #sel_p=10
  #Interpretability of log-odds and homogeneous probability in Imports
  I_10_p <-exp(-2.0794)/(1+exp(-2.0794))
  I_10_p  #0.1111152
  #Interpretability of log-odds and homogeneous probability in Tariffs
  T_10_p <-exp(-2.0900)/(1+exp(-2.0900))
  T_10_p#0.1100726
  #sel_p=20
  #Imports and Tariffs the same edges, the maximum likelihood, see Table B.1
  I_20_p <- exp(-2.89037)/(1+exp(-2.89037))
  I_20_p #0.05263167
  #sel_p=50
  #Imports and Tariffs the same edges, the maximum likelihood, see Table B.1
  I_50_p <- exp(-3.87120)/(1+exp(-3.87120))
  I_50_p #0.02040818
}
