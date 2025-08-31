### Countries Analysis ####

#df = test_upload_data(file_name, path)
test_upload_data1 <- function(file_name, path)
{
  library(usethis)
  library(devtools) #packages usethis
  # CRAN
  
  #1)To load the frame from file
  library(stringr)
  tmp = paste(path,file_name, sep = "/")# packae stringr
  table1<- read.csv(tmp)
  row.names(table1) <-t(table1$Partner.Name)
  row.names(table1)
  
  return(table1)
}

# Tables with the maximum 10 Importations by countries
max_imp_countries <- function(t_temp)
{
  library(data.table)
  t_temp <- data.table(t_temp)
  t_temp <- t_temp[order(t_temp$Import..US..Thousand., decreasing = TRUE)]
  return(t_temp)
}

max_tariff_countries <- function(t_temp)
{
  t_temp <- data.table(t_temp)
  order_tariff <- t_temp[order(t_temp$AHS.Total.Tariff.Lines,decreasing = TRUE)]
  View(order_tariff)
  return(order_tariff)
}

window_countries_frame <-function(t_e)
{
  tt_window <- data.frame( ID = t_e$Partner.Name, 
                                  Country = t_e$Reporter.Name,
                                  Import = t_e$Import..US..Thousand. )
  t_window <- window_extract_reg(tt_window)[2:10,]
  View(t_window)
  plot(t_window, title="Principal partners Importations of All the Products")
  return(t_window)
}

window_countries_Imp <-function(t_e, sel_p)
{
  tt_window <- data.frame( ID = t_e$Partner.Name, 
                          Country = t_e$Reporter.Name,
                          Import = t_e$Import..US..Thousand. )
  t_window <- window_extract_reg(tt_window)[2:sel_p,]
  n_window <- network(t_window, loop=T)
  return(n_window)
}

window_countries_tariff <-function(t_e, sel_p)
{
  tt_window <- data.frame( ID = t_e$Partner.Name, 
                          Country = t_e$Reporter.Name,
                          Import = t_e$AHS.Total.Tariff.Lines )
  t_window <- window_extract_reg(tt_window)[2:sel_p,]
  n_window <- network(t_window, loop=T)
  return(n_window)
}

#Extract regions
window_extract_reg <- function(t_window)
{
  #7 Regions
  t_tem <- t_window[t_window$ID != "North America",]
  t_tem <- t_tem[t_tem != "East Asia & Pacific",]
  t_tem <- t_tem[t_tem != "South Asia",]
  t_tem <- t_tem[t_tem != "Europe & Central Asia",]
  t_tem <- t_tem[t_tem != "Sub-Saharan Africa",]
  t_tem <- t_tem[t_tem != "Latin America & Caribbean",]
  t_tem <- t_tem[t_tem !=  "Middle East & North Africa",]
  t_tem <- t_tem[t_tem !=  "Unspecified",]
  
  return(t_tem)
}

#Extract country network
#n_Country_n <- extract_country_n(file_name, path, sel_p)
extract_country_n <- function(file_name, path, sel_p)
{
  t_temp <- test_upload_data1(file_name, path)
  # Tables with the maximum sel_p Importations by countries
  t_e <- max_imp_countries(t_temp)
  n_Country <- window_countries_Imp(t_e, sel_p)
  
  return(n_Country)
}

#Extract country which has the tariff values
#t_Country <- extract_country_t(file_name, path, sel_p)
extract_country_t <- function(file_name, path, sel_p)
{
  t_temp <- test_upload_data1(file_name, path)
  # Tables with the maximum sel_p tariffs Importations by countries
  tariff <-max_tariff_countries(t_temp)
  t_Country <-window_countries_tariff(tariff, sel_p)
  
  return(t_Country)
}

#Extract diff Importations partner countries
#diff_Country <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
extract_country_diff_Imp <- function(file_name, path1, path2, sel_p)
{
  # Tables with the maximum 10 Importations by countries
  # display the countries that are in both vectors
  #c <- intersect(t_e$Partner.Name[2:12], tariff$Partner.Name[2:12])
  # display the countries that appears in t_e but not in tariff
  #cc <- setdiff(t_e$Partner.Name[2:12], tariff$Partner.Name[2:12])
  
  t_Country <- extract_country_t(file_name, path1, sel_p)
  n_Country <- extract_country_n(file_name, path1, sel_p)
  #graph mode
  tt_Country <- graph_from_data_frame(extract_country_t(file_name, path1, sel_p))
  nn_Country <- graph_from_data_frame(extract_country_n(file_name, path1, sel_p))
  #Close the device using 
  graphics.off()
  
  #To load the frame from file
  #library(stringr)
  string_f <- file_name
  image_f <- substr(string_f, start = 1, stop = (nchar(string_f)-4) ) 
  file_name <- paste(image_f,"png", sep = ".")
  path_name <- paste(path2,  file_name, sep = "/")
  
  #saving file png 
  reso <- 1200
  png(path_name, reso)
  #Plot
  par(mfrow=c(1,2))
  l <- layout_in_circle(nn_Country)
  plot(nn_Country, layout=l, displaylabels=TRUE, edge.arrow.size=.6, 
       vertex.color="lightblue", vertex.label.cex=0.7, vertex.label.dist=2, edge.curved=0.2)
  title(main="Country-Partners by Importations", line=-4)
  
  l <- layout_in_circle(tt_Country)
  plot(tt_Country, layout=l, displaylabels=TRUE, edge.arrow.size=.6, 
       vertex.color="pink", vertex.label.cex=0.7, vertex.label.dist=2, edge.curved=0.2)
  title(main="by Tariffs", line=-4)
 
  #Close the device using
  dev.off()
  graphics.off()
 
  return(n_Country)
}

#Extract Tariffs partner countries
#diff_Country <- extract_country_tar(file_name, path, sel_p)
extract_country_tar <- function(file_name, path1, sel_p)
{

  t_Country <- extract_country_t(file_name, path1, sel_p)
  
  return(t_Country)
}

# Analysis of size and colors
#Igraph
# net_c_Imp = graph_analysis_c_Imp(n_c_Imp, path2)
graph_analysis_c_Imp <- function(n_c_Imp, path2)
{
  #Base: Introduction to R and network analysis
  #Working over net, because it is the igraph class
  library(igraph)
  net <- graph_from_data_frame(d=n_c_Imp)
  net <- simplify(net, remove.multiple = F, remove.loops = F)
  class(net)
  #as_edgelist(net, names=T)
  #head(net)
  
  #Sized Vertices
  networklist <- as_adj_edge_list(net)
  vsize <- degree(net, mode="all")
  
  #Plot 1
  v <- V(net)
  v
  colrs <- c("green","purple")
  
  #length 158
  V(net)$color<- colrs[1+(vsize>26)]
  V(net)$size <- vsize*0.22
  V(net)$label.cex <- 0.5
  V(net)$label.dist <- 0.8
  #Close the device using 
  graphics.off()
  
  #To load the frame from file
  #library(stringr)
  image_f <- "graph_analysis_c_Imp"
  file_name <- paste(image_f,"png", sep = ".")
  path_name <- paste(path2,  file_name, sep = "/")
  
  #saving file png 
  png(path_name)
  
  #Pag 37 differents layours
  #graph_attr(net, "layout") <-layout_with_fr
  graph_attr(net, "layout") <-layout_as_star
  plot(net, main="Network Country-Partners by Importations")
  legend(x=0.7, y=1.3, c("Less 27/1269 edges Shared-Partners","27/1269 edges & More Shared-Partners"), pch=21,
         col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
  
  #Close the device using
  dev.off()
  graphics.off()
  
  return(net)
}

# Analysis of size and colors
#Igraph
# net_c_Tar = graph_analysis_c_Tar(n_c_Tar, path2)
graph_analysis_c_Tar <- function(n_c_Tar, path2)
{
  #Base: Introduction to R and network analysis
  #Working over net, because it is the igraph class
  library(igraph)
  net <- graph_from_data_frame(d=n_c_Tar)
  net <- simplify(net, remove.multiple = F, remove.loops = F)
  class(net)
  #as_edgelist(net, names=T)
  #head(net)
  
  #Sized Vertices
  networklist <- as_adj_edge_list(net)
  vsize <- degree(net, mode="all")
  
  #Plot 1
  v <- V(net)
  v
  colrs <- c("green","purple")
  
  #length 1269 edges
  V(net)$color<- colrs[1+(vsize>24)]
  V(net)$size <- vsize*0.22
  V(net)$label.cex <- 0.5
  V(net)$label.dist <- 0.8
  #Close the device using 
  graphics.off()
  
  #To load the frame from file
  #library(stringr)
  image_f <- "graph_analysis_c_Tar"
  file_name <- paste(image_f,"png", sep = ".")
  path_name <- paste(path2,  file_name, sep = "/")
  
  #saving file png 
  png(path_name)
  #Pag 37 differents layours
  #graph_attr(net, "layout") <-layout_with_fr
  graph_attr(net, "layout") <-layout_as_star
  plot(net, main="Network Country-Partners by Tariffs")
  legend(x=0.7, y=1.3, c("Less 25/1269 edges Shared-Partners","25/1269 edges & More Shared-Partners"), pch=21,
         col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
  #Close the device using
  dev.off()
  graphics.off()
  
  return(net)
}

# Countries-Network Analysis
# Analysis of density, Reciprocity, Transitivity, Diameters, Node degrees, Degree distribution,
# Centrality & centralization
graph_analysis_2_c <- function(net, file_name, path2)
{
  
  extra = 0.3
  #Density
  #Method 1: Calculating density
  #vertices count
  vcount(net)
  #edges count
  ecount(net)
  edge_density(net, loops=F)
  #Method 2: Calculating density
  ecount(net)/(vcount(net)*vcount(net)-1)
  
  #Reciprocity
  # Method1: Calculating reciprocity
  reciprocity(net)
  # Mutual, asymmetric, and null node pairs
  dyad_census(net) 
  # Method2: Calculating reciprocity
  2*dyad_census(net)$mut/ecount(net)
  
  #Transitivity
  transitivity(net, type="global") # net is treated as an undirected network
  transitivity(as_undirected(net, mode="collapse")) # same as above
  transitivity(net, type="local")
  #length 16
  triad_census(net) # for directed networks
  
  #Diameters
  diameter(net, directed=F, weights=NA)
  diameter(net, directed=F)
  diam <- get_diameter(net, directed=T)
  diam
  as.vector(diam)
  
  
  #Close the device using 
  graphics.off()
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_1")
  image_ff <- paste(image_f,"png", sep = ".")
  path_name <- paste(path2,  image_ff, sep = "/")
  
  #saving file png 
  png(path_name)
  
  net_r <- simplify(net, remove.multiple = F, remove.loops = F)
  class(net_r)
  vsize <- degree(net_r, mode="all")
  V(net_r)$size <- vsize*extra
  
  vcol <- rep("gray40", vcount(net))
  vcol[diam] <- "gold"
  ecol <- rep("gray80", ecount(net))
  ecol[E(net, path=diam)] <- "orange"
  # E(net, path=diam) finds edges along a path, here 'diam'
  plot(net_r, main="Diameter/Edges Analysis", vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)
  #Close the device using
  dev.off()
  graphics.off()
  
  #Node_Degrees
  #Close the device using 
  graphics.off()
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_1_1")
  image_ff <- paste(image_f,"png", sep = ".")
  path_name <- paste(path2,  image_ff, sep = "/")
  
  #saving file png 
  png(path_name)
  deg <- degree(net, mode="all")
  deg
  plot(net, vertex.size=deg* extra)
  #Close the device using
  dev.off()
  graphics.off()
  
  #Close the device using 
  graphics.off()
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_1_2")
  image_ff <- paste(image_f,"png", sep = ".")
  path_name <- paste(path2,  image_ff, sep = "/")
  
  #saving file png 
  png(path_name)
  deg <- degree(net, mode="all")
  deg
  hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")
  #Close the device using
  dev.off()
  graphics.off()
  
  #Degree_Distribution
  #Close the device using 
  graphics.off()
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_2")
  image_ff <- paste(image_f,"png", sep = ".")
  path_name <- paste(path2,  image_ff, sep = "/")
  
  #saving file png 
  png(path_name)
  deg.dist <- degree_distribution(net, cumulative=T, mode="all")
  plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",
        main= "Degree_Distribution", xlab="Degree", ylab="Cumulative Frequency")
  #Close the device using
  dev.off()
  graphics.off()
  
  
  #Degree (number of ties)
  degree(net, mode="in")
  d_in <-centr_degree(net, mode="in", normalized=T)
  d_in
  
  # Closeness (centrality based on distance to others in the graph)
  #Inverse of the node’s average geodesic distance to others in the network.
  closeness(net, mode="all", weights=NA)
  centr_clo(net, mode="all", normalized=T)
  
  #Eigenvector (centrality proportional to the sum of connection centralities)
  #Values of the first eigenvector of the graph matrix.
  eigen_centrality(net, directed=T, weights=NA)
  centr_eigen(net, directed=T, normalized=T)
  
  #Betweenness (centrality based on a broker position connecting others)
  #Number of geodesics that pass through the node or the edge.
  betweenness(net, directed=T, weights=NA)
  edge_betweenness(net, directed=T, weights=NA)
  centr_betw(net, directed=T, normalized=T)
}

