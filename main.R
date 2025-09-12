
country_extractions <- function()
{
  #Multinetworks
  library(ergm.multi)
  library(data.table)
  #Base: Introduction to R and network analysis
  library(igraph)
  
  
  library(usethis)
  library(devtools) #packages usethis
  # CRAN
  
  #1)To load the frame from file
  library(stringr)
  #PGN
  library("grDevices")
  
  #Modeling
  library(ggplot2)
  sel_p <- 20 #main partners number
  
  ### Regions ####
  #Regions_Importations
  {
    path2= "/home/oralian/Documents/Multi_ERGM_2025/Fig-Imp-Reg"
    n_r_Imp <- row_regions_diff_Imp(path2, sel_p)
    file_name <- "(A)Importations"
    #Modeling analysis
    n <-n_r_Imp
    fit_n1 <-modeling_edges(n_r_Imp)
    n_mod_Imp_r_cov <-modeling_covariate(n_r_Imp)
    n_mod_Imp_r_names <-modeling_names(n_r_Imp)
    n_mod_Imp_r_e <-modeling_residuals(fit_n1)
    n_mod_Imp_r_g <-mod_goodness(fit_n1)
    
    
    #net_igraph
    main="(A)Tariffs by Degrees"
    net_r_Imp = graph_analysis_n_Imp(n_r_Imp, path2, main)
    #plot(n_r_Imp, displaylabels=F)
    net <- net_r_Imp#object_net_igraph
    file_name <- "(A)Importations"
    graph_analysis_2(net_r_Imp, file_name, path2, sel_p) #_1.png & _2.png
    graph_analysis_3(net_r_Imp, file_name, path2) #_3_1 & 3_2.png
    graph_analysis_4(net_r_Imp, file_name, path2) #_4.png
    graph_analysis_5(net_r_Imp, file_name, path2) #_5_1, _5_2 & 5_3.png
    graph_analysis_6_0(net_r_Imp, file_name, path2) #_7_1.png
    graph_analysis_6_1(net_r_Imp, file_name, path2) #_7_2, _8_1, _8_2, _9-_12.png
  }
  
  #Regions_Tariffs
  {
    path2= "/home/oralian/Documents/Multi_ERGM_2025/Fig-Tar-Reg"
    n_r_Tar = row_regions_Tar(sel_p)
    file_name <- "(B)Tariffs"
    #Modeling analysis
    n <-n_r_Tar
    fit_n1 <-modeling_edges(n_r_Tar)
    n_mod_Tar_r_cov <-modeling_covariate(n_r_Tar)
    n_mod_Tar_r_names <-modeling_names(n_r_Tar)
    n_mod_Tar_r_e <-modeling_residuals(fit_n1)
    n_mod_Tar_r_g <-mod_goodness(fit_n1)
    
    #net_igraph
    main="(B)Tariffs by Degrees"
    net_r_Tar = graph_analysis_n_Tar(n_r_Tar, path2, main)
    net <- net_r_Tar #object_net_igraph
    file_name <- "(B)Tariffs"
    graph_analysis_2(net_r_Tar, file_name, path2, sel_p)
    graph_analysis_3(net_r_Tar, file_name, path2)
    graph_analysis_4(net_r_Tar, file_name, path2)
    graph_analysis_6_0(net_r_Tar, file_name, path2)
    graph_analysis_6_1(net_r_Tar, file_name, path2)
  }
  
  #Comparative Modeling Edges-analysis
  comparative_mod(n_mod_Imp_r_e, n_mod_Tar_r_e, sel_p)
  #Distribution Degree comparative
  Compare_Dist_Degrees_sel_p(net_r_Imp, net_r_Tar, sel_p, path2)
  
  ### Countries ####
  #Countries_Importations
  {
    path2= "/home/oralian/Documents/Multi_ERGM_2025/Fig-Imp-Cont"
    n_c_Imp <- row_countries_diff_Imp(path2, sel_p)
    #Modeling analysis
    file_name <- "C_Importations"
    n <- n_c_Imp
    n_mod_imp_c_e <- modeling_edges(n_c_Imp)
    
    #net_igraph
    net_c_Imp = graph_analysis_c_Imp(n_c_Imp, path2)
    net <- net_c_Imp#object_net_igraph
    graph_analysis_2_c(net_c_Imp, file_name, path2)#_1.png & _2.png
    graph_analysis_3(net_c_Imp, file_name, path2)
  }
  
  #Countries_Tariffs
  {
    path2= "/home/oralian/Documents/Multi_ERGM_2025/Fig-Tar-Cont"
    n_c_Tar = row_countries_Tar(sel_p)
    #Modeling analysis
    file_name <- "C_Tariffs"
    n <- n_c_Tar
    n_mod_tar_c_e <- modeling_edges(n_c_Tar)
    
    #net_igraph
    net_c_Tar = graph_analysis_c_Tar(n_c_Tar, path2)
    net <- net_c_Tar#object_net_igraph
    graph_analysis_2_c(net_c_Tar, file_name, path2)
  }
  
  #Comparative Modeling analysis
  comparative_mod(n_mod_imp_c_e, n_mod_tar_c_e, sel_p)

}
