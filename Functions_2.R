### Regions Analysis ####

#Extract regions network Importations
#n_reg_n <- extract_reg_n(file_name, path)
#path <- path1
extract_reg_n <- function(file_name, path, sel_p)
{
  t_temp <- test_upload_data1(file_name, path)
  # Tables with the maximum sel_p Importations by regions
  t_e <- max_imp_regions(t_temp)
  n_reg <- window_regions_Imp(t_e, sel_p)
  
  return(n_reg)
}

#Extract reg which has the tariff values
#t_reg <- extract_reg_t(file_name, path)
extract_reg_t <- function(file_name, path, sel_p)
{
  t_temp <- test_upload_data1(file_name, path)
  # Tables with the maximum sel_p tariffs Importations by regions
  tariff <-max_tariff_regions(t_temp)
  t_reg <-window_regions_tariff(tariff, sel_p)
  
  return(t_reg)
}

#Extract Unspecified
window_extract_Unspecified <- function(t_window)
{
  t_tem <- t_window[t_window$ID != "Unspecified",]
  return(t_tem)
}

miss_regions_Imp <-function(t_window)
{
  names(t_window)<-NULL
  library(missMDA)
  nb <- estim_ncpPCA(t_window[3], method.cv = "Kfold", verbose = FALSE) 
  nb$ncp #2
  #plot(0:5, nb$criterion, xlab = "nb dim", ylab = "MSEP")
  res.comp <- imputePCA(t_window[3], ncp = nb$ncp) # iterativePCA algorithm
  res.comp$completeObs
  t_window[3]<-res.comp$completeObs
  colnames(t_window)<-c("ID","Country","Import")
  
  return(t_window)
}

# Tables with the maximum 10 Importations by regions
max_imp_regions <- function(t_temp)
{
  library(data.table)
  t_temp <- data.table(t_temp)
  t_temp <- t_temp[order(t_temp$Import..US..Thousand., decreasing = TRUE)]
  return(t_temp)
}

max_tariff_regions <- function(t_temp)
{
  t_temp <- data.table(t_temp)
  order_tariff <- t_temp[order(t_temp$AHS.Total.Tariff.Lines,decreasing = TRUE)]
  View(order_tariff)
  return(order_tariff)
}

window_regions_Imp <-function(t_e, sel_p)
{
  tt_window <- data.frame( ID = t_e$Partner.Name, 
                           Country = t_e$Reporter.Name,
                           Import = t_e$Import..US..Thousand. )
  t_window <- window_extract_Unspecified(tt_window)[2:sel_p,]
  #tem_window <- miss_regions_Imp(t_window)
  n_window <- network(t_window, loop=T)
  return(n_window)
}


window_regions_tariff <-function(t_e, sel_p)
{
  tt_window <- data.frame( ID = t_e$Partner.Name, 
                           Country = t_e$Reporter.Name,
                           Import = t_e$AHS.Total.Tariff.Lines )
  t_window <- window_extract_Unspecified(tt_window)[2:sel_p,]
  #tem_window <- miss_regions_Imp(t_window)
  n_window <- network(t_window, loop=T)
  return(n_window)
}

#Extract diff Importations and partner regions
#diff_reg_Imp <- extract_regions_diff_Imp(file_name, path1, path2, sel_p)
extract_regions_diff_Imp <- function(file_name, path1, path2, sel_p)
{
  nn_reg <- extract_reg_n(file_name, path1, sel_p)
  #graph mode
  t_reg <- graph_from_data_frame(extract_reg_t(file_name, path1, sel_p))
  n_reg <- graph_from_data_frame(extract_reg_n(file_name, path1, sel_p))
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
  l <- layout_in_circle(n_reg)
  plot(n_reg, layout=l, displaylabels=TRUE, edge.arrow.size=.6, 
       vertex.color="lightblue", vertex.label.cex=0.7, vertex.label.dist=2, edge.curved=0.2)
  #title(main="Region-Partners by Importations", line=-4)
  
  l <- layout_in_circle(t_reg)
  plot(t_reg, layout=l, displaylabels=TRUE, edge.arrow.size=.6, 
       vertex.color="pink", vertex.label.cex=0.7, vertex.label.dist=2, edge.curved=0.2)
  #title(main="by Tariffs", line=-4)
  
  

  #Close the device using
  dev.off()
  graphics.off()
  
  return(nn_reg)
}

#Extract Tar and partner regions
#reg_Tar <- extract_regions_Tar(file_name, path)
extract_regions_Tar <- function(file_name, path1, sel_p)
{
  tt_reg <- extract_reg_t(file_name, path1, sel_p)
  
  return(tt_reg)
}

# Analysis of size and colors
#Igraph
# net_r_Imp = graph_analysis_n_Imp(n_r_Imp, path2)
graph_analysis_n_Imp <- function(n_r_Imp, path2)
{
  #Base: Introduction to R and network analysis
  #Working over net, because it is the igraph class
  library(igraph)
  net_r <- graph_from_data_frame(d=n_r_Imp)
  net_r <- simplify(net_r, remove.multiple = F, remove.loops = F)
  class(net_r)
  #as_edgelist(net_r, names=T)
  #head(net_r)
  
  #Number of elements/size vertexes
  networklist <- as_adj_edge_list(net_r)

  vsize <- degree(net_r, mode="all")
  
  #Plot 1
  v <- V(net_r)
  v
  
  #16-East Asia & Pacific[1]
  #16-Europe & Central Asia Middle[3]
  #15-North America[5]
  colrs <- c("purple","green")
  
  #length 15
  V(net_r)$color<- colrs[1+(vsize==vsize[1])]
  V(net_r)$size <- vsize*3
  text_p <- paste((vsize[1]-1),"& less shared-partners")
  text_g <- paste(vsize[1],"shared-partners")
  main="A)Importations by Degrees"
  
  #Close the device using 
  graphics.off()
  
  #To load the frame from file
  #library(stringr)
  image_f <- "graph_analysis_n_Imp"
  file_name <- paste(image_f,"png", sep = ".")
  path_name <- paste(path2, file_name, sep = "/")
  
  #saving file png 
  reso <- 1200
  png(path_name, reso)
  
  
  #Pag 37 differents layours
  #graph_attr(net_r, "layout") <- layout_with_fr
  #l <-layout_components(net_r)
  #plot(net_r, layout=l, main=main)
  graph_attr(net_r, "layout") <- layout_as_star
  plot(net_r, main=main)
  legend(x=1.1, y=1.3, c(text_p, text_g), pch=21,
         col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
  
  #Close the device using
  dev.off()
  graphics.off()
  
  return(net_r)
}

# Analysis of size by Tariffs
#Igraph
# net_r_Tar = graph_analysis_n_Tar(n_r_Tar,path2)
graph_analysis_n_Tar <- function(n_r_Tar, path2)
{
  #Base: Introduction to R and network analysis
  #Working over net, because it is the igraph class
  library(igraph)
  net_r <- graph_from_data_frame(d=n_r_Tar)
  net_r <- simplify(net_r, remove.multiple = F, remove.loops = F)
  class(net_r)
  #as_edgelist(net_r, names=T)
  #head(net_r)
  
  #Number of elements/size vertexes
  networklist <- as_adj_edge_list(net_r)
  #vsize<-0
  
  #for (i in 1:length(as_adj_edge_list(net_r)) )
  #{
  #vsize[i] <- length(unlist(networklist[i]))
  #}
  #the colors are configured with automatically in base of vsize (vertex size) umbrals:
  vsize <- degree(net_r, mode="all")
  
  #Plot 1
  v <- V(net_r)
  v
  colrs <- c("purple","green")
  text_p <- paste((vsize[1]-1),"& less Degrees")
  text_g <- paste(vsize[1],"Degrees")
 
  #Close the device using 
  graphics.off()
  
  #To load the frame from file
  #library(stringr)
  image_f <- "graph_analysis_n_Tar"
  file_name <- paste(image_f,"png", sep = ".")
  path_name <- paste(path2, file_name, sep = "/")
  main="B)Tariffs by Degrees"
  #saving file png 
  png(path_name)
  
  #length 15
  #V(net_r)$color<- colrs[1+(v == v[1])+(v == v[5])+(v == v[7])]
  V(net_r)$color<- colrs[1+(vsize==16)+(vsize==15)]
  V(net_r)$size <- vsize*3
  #Pag 37 differents layours
  #graph_attr(net_r, "layout") <-layout_with_fr
  graph_attr(net_r, "layout") <-layout_as_star
  plot(net_r, main=main)
  #legend(x=1.1, y=1.3, c(text_p, text_g), pch=21,
        #col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
  
  #Close the device using
  dev.off()
  graphics.off()
  
  return(net_r)
}

# Regions-Network Analysis
# Analysis of density, Reciprocity, Transitivity, Diameters, Node degrees, Degree distribution,
# Centrality & centralization
graph_analysis_2 <- function(net, file_name, path2, sel_p)
{
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
  t <-transitivity(net, type="local")
  #t[!is.na(t)]
  #which.max(t)
  
  
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
  image_f <- paste(file_name, "_1")
  image_ff <- paste(image_f, "png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste(file_name, "by Diameter/Edges")

  #saving file png 
  png(temp_ff)
  
  vcol <- rep("gray40", vcount(net))
  vcol[diam] <- "gold"
  ecol <- rep("gray80", ecount(net))
  ecol[E(net, path=diam)] <- "orange"
  text_p <- paste((max(diam)-1),"& less weight")
  text_g <- paste(max(diam),"Degrees weight")
  # E(net, path=diam) finds edges along a path, here 'diam'
  plot(net, main=t_main, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)
  #legend(x=1.1, y=1.3, c(text_g, text_p), pch=21,
  #col="#777777", pt.bg=vcol, pt.cex=2, cex=.8, bty="n", ncol=1)
  
  #Close the device using
  dev.off()
  graphics.off()

  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name, "_1_1")
  image_ff <- paste(image_f, "png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  tt_main <- paste("by", sel_p, "main Partners" )
  t_main <- paste(file_name, tt_main)
  
  #saving file png 
  png(temp_ff)
  
  #Node_Degrees
  deg <- degree(net, mode="all")
  deg
  plot(net, vertex.size=deg*3)
  hist(deg, breaks=1:vcount(net)-1, main=t_main)
  
  #Degree_Distribution
  #Close the device using 
  graphics.off()
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_2")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste(file_name, "by Degree_Distribution")
  
  #saving file png 
  png(temp_ff)
  deg.dist <- degree_distribution(net, cumulative=T, mode="all")
  plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",
        main=t_main , xlab="Degree", ylab="Cumulative Frequency")
  #Close the device using
  dev.off()
  graphics.off()
  
  
  #Degree (number of ties)
  # which.max(d)
  # which(d==1)
  d <- degree(net, mode="in")
  d_in <-centr_degree(net, mode="in", normalized=T)
  d_in
  
  # Closeness (centrality based on distance to others in the graph)
  #Inverse of the node’s average geodesic distance to others in the network.
  c <- closeness(net, mode="all", weights=NA)
  c_clo <- centr_clo(net, mode="all", normalized=T)
  
  #Eigenvector (centrality proportional to the sum of connection centralities)
  #Values of the first eigenvector of the graph matrix.
  c_e <- eigen_centrality(net, directed=T, weights=NA)
  centr_eigen(net, directed=T, normalized=T)
  
  #Betweenness (centrality based on a broker position connecting others)
  #Number of geodesics that pass through the node or the edge.
  b <- betweenness(net, directed=T, weights=NA)
  edge_betweenness(net, directed=T, weights=NA)
  centr_betw(net, directed=T, normalized=T)
  # which.max(b)
  # which(b==39.0)
}

# Exploring metwork data
# Hubs and authorities, Distances and paths, Cliques, Community detection
graph_analysis_3 <- function(net, file_name, path2)
{
  #Close the device using 
  graphics.off()
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_3_1")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <-  paste(file_name, "by Hubs")
  

  #saving file png 
  png(temp_ff)
  #The hubs and authorities algorithm developed by Jon Kleinberg was initially used to examine
  #web pages. Hubs were expected to contain catalogs with a large number of outgoing links; while
  #authorities would get many incoming links from hubs, presumably because of their high-quality
  #relevant information.
  # which.max(hs)
  # which(hs==39.0)
  hs <- hub_score(net, weights=NA)$vector
  hs
  # which.max(as)
  # which(as==39.0)
  as <- authority_score(net, weights=NA)$vector
  as
  #plot(net, vertex.size=hs*50, main=t_main)
  plot(net, vertex.size=hs*50, main=t_main)
  #Close the device using
  dev.off()
  graphics.off()
  
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_3_2")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste(file_name, "by Authorities")
  #saving file png 
  png(temp_ff)
  plot(net, vertex.size=as*30, main=t_main)
  #Close the device using
  dev.off()
  graphics.off()
}

#Average path length: the mean of the shortest distance between each pair of nodes in the network
#(in both directions for directed graphs).
graph_analysis_4 <- function(net, file_name, path2)
{
  #Close the device using 
  graphics.off()
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_4")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste(file_name, "& Latin America Distances")

  #saving file png 
  png(temp_ff)
  #We can also find the length of all shortest paths in the graph
  mean_distance(net, directed=F)
  mean_distance(net, directed=T)
  # with edge weights
  distances(net)
  # ignore weights
  distances(net, weights=NA) 
  
  #Dynamic name vertice
  v<-V(net)$name == "Latin America & Caribbean"
  LA <-which(v)
  dist.from.LA <- distances(net, v=V(net)[LA], to=V(net), weights=NA)
  LA
  dist.from.LA
  # which.max(dist.from.LA)
  # which(dist.from.LA==1)
  # Set colors to plot the distances:
  oranges <- colorRampPalette(c("dark blue", "gold"))
  col <- oranges(max(dist.from.LA)+1)
  col <- col[dist.from.LA+1]
  
  plot(net, main=t_main, vertex.color=col, vertex.label=dist.from.LA, edge.arrow.size=.6,
       vertex.label.color="white")
  #Close the device using
  dev.off()
  graphics.off()
}

#We can also find the shortest path between specific nodes
graph_analysis_5 <- function(net, file_name, path2)
{
  #Close the device using 
  graphics.off()
  
  #Dynamic name vertice
  v_LA<-V(net)$name == "Latin America & Caribbean"
  LA <-which(v_LA)
  v_LA
  LA
  v_MNA<-V(net)$name == "Middle East & North Africa"
  MNA <-which(v_MNA)
  v_MNA
  MNA
  
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_5_1")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- file_name
  #t_main <- paste("Shortest_paths Analysis by", file_name)
  #saving file png 
  png(temp_ff)
  # both path nodes and edges
  news.path <- shortest_paths(net,
                              from = V(net)[LA],
                              to = V(net)[MNA],
                              output = "both") 
  news.path
  # Generate edge color variable to plot the path:
  ecol <- rep("gray80", ecount(net))
  ecol[unlist(news.path$epath)] <- "orange"
  # Generate edge width variable to plot the path:
  ew <- rep(2, ecount(net))
  ew[unlist(news.path$epath)] <- 4
  # Generate node color variable to plot the path:
  vcol <- rep("gray40", vcount(net))
  vcol[unlist(news.path$vpath)] <- "gold"
  plot(net, main=t_main, vertex.color=vcol, edge.color=ecol,
       edge.width=ew, edge.arrow.mode=0)
  #Close the device using
  dev.off()
  graphics.off()
  
  
  #Identify the edges going into or out of a vertex, for instance the WSJ. For a single node, use
  #incident(), for multiple nodes use incident_edges()
  #To load the frame from file
  #library(stringr)
  #OUTPUT
  image_f <- paste(file_name,"_5_2")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- file_name
  #t_main <- paste("Output incident_edges Analysis by", file_name)
  #saving file png 
  png(temp_ff)
  inc.edges <- incident(net,
                        V(net)[LA], mode="out")
  # Set colors to plot the selected edges.
  ecol <- rep("gray80", ecount(net))
  ecol[inc.edges] <- "orange"
  vcol <- rep("light blue", vcount(net))
  vcol[V(net)$media=="Multi-ERGM"] <- "gold"
  plot(net, main=t_main, vertex.color=vcol, edge.color=ecol)
  #Close the device using
  dev.off()
  graphics.off()
  
  #INPUT
  image_f <- paste(file_name,"_5_3")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- file_name
  #t_main <- paste("Input incident_edges Analysis by", file_name)
  #saving file png 
  png(temp_ff)
  inc.edges <- incident(net,
                        V(net)[LA], mode="in")
  # Set colors to plot the selected edges.
  ecol <- rep("gray80", ecount(net))
  ecol[inc.edges] <- "orange"
  vcol <- rep("light blue", vcount(net))
  vcol[V(net)$media=="Multi-ERGM"] <- "gold"
  plot(net, main=t_main, vertex.color=vcol, edge.color=ecol)
  #Close the device using
  dev.off()
  graphics.off()
}

graph_analysis_5_1 <- function(net, file_name, path2)
{
  #Close the device using
  dev.off()
  graphics.off()
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_7")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- file_name
  #t_main <- paste("adjacent_vertices/neighbors by", file_name)
  #saving file png 
  png(temp_ff)
  #We can also easily identify the immediate neighbors of a vertex, say WSJ. The neighbors function
  #finds all nodes one step out from the focal actor.To find the neighbors for multiple nodes, use
  #adjacent_vertices() instead of neighbors(). To find node neighborhoods going more than one
  #step out, use function ego() with parameter order set to the number of steps out to go from the
  #focal node(s).
  neigh.nodes <- neighbors(net, V(net)[LA], mode="in")
  # Set colors to plot the neighbors:
  vcol[neigh.nodes] <- "#ff9d00"
  plot(net, main=t_main, vertex.color=vcol)
  #Close the device using
  dev.off()
  graphics.off()
}

# by Mutual(cliques&Cocitation), k-core, clustering
graph_analysis_6_1 <- function(net, file_name, path2)
{
  
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_8_1")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste(file_name, "by Mutual(cliques&Cocitation)")
  
  length <- 10
  #saving file png 
  png(temp_ff)
  
  #Co-citation Matrix, which.max(co)
  co <- cocitation(net)
  length(co)
  rownames(co)
  #as.undirected mutual, collapse, each
  net.sym <- as.undirected(net, mode= "mutual",
                           edge.attr.comb=list(weight="sum", "ignore"))
  #list, length(cli)
  cliques(net.sym) # list of cliques
  cli <- sapply(cliques(net.sym), length) # clique sizes
  #max(unlist(cli))
  #names(unlist(cli))
  largest_cliques(net.sym) # cliques with max number of nodes
  
  vcol <- rep("gold", vcount(net.sym))
  vcol[unlist(largest_cliques(net.sym))] <- "lightblue"
  plot(as.undirected(net.sym), main=t_main, vertex.label=V(net.sym)$name, vertex.color=vcol)
  #Close the device using
  dev.off()
  graphics.off()
  
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_8_2")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste(file_name, "by Collapse(cliques&Cocitation)")
 
  #saving file png 
  png(temp_ff)
  
  #The co-cited matrix is the same for mutual and collapse analysis
  #Matrix, which.max(co)
  #Co-citation Matrix, which.max(co)
  #co <- cocitation(net)
  #length(co)
  #rownames(co)
  #as.undirected mutual, collapse, each
  net.sym <- as.undirected(net, mode= "collapse",
                           edge.attr.comb=list(weight="sum", "ignore"))
  #list, length(cli)
  cliques(net.sym) # list of cliques
  cli <- sapply(cliques(net.sym), length) # clique sizes
  #max(unlist(cli))
  #names(unlist(cli))
  largest_cliques(net.sym) # cliques with max number of nodes
  
  vcol <- rep("gold", vcount(net.sym))
  vcol[unlist(largest_cliques(net.sym))] <- "lightblue"
  plot(as.undirected(net.sym), main=t_main, vertex.label=V(net.sym)$name, vertex.color=vcol)
  #Close the device using
  dev.off()
  graphics.off()
  
  #Dengrogram
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_9")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste("hclust betweenness by", file_name)
  #saving file png 
  png(temp_ff)
  ceb <- cluster_edge_betweenness(net)
  dendPlot(ceb, main=t_main, mode="hclust")
  #Close the device using
  dev.off()
  graphics.off()
  
  
  #Plotting communities
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_10")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste("cluster_edge_betweenness by", file_name)
  #saving file png 
  png(temp_ff)
  plot(ceb, main=t_main, net)
  
  length(ceb)# number of communities
  membership(ceb) # community membership for each node
  #Close the device using
  dev.off()
  graphics.off()
  
  #Community detection based on based on propagating labels
  #Assigns node labels, randomizes, than replaces each vertex’s label with the label that appears most
  #frequently among neighbors. Those steps are repeated until each vertex has the most common label
  #of its neighbors.

  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_11")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste("cluster_label_prop by", file_name)
  #saving file png 
  png(temp_ff)
  
  clp <- cluster_label_prop(net)
  plot(clp, main=t_main, net)
  #Close the device using
  dev.off()
  graphics.off()
  
  #Clustering
  cfg <- cluster_fast_greedy(as.undirected(net))
  
  # We can also plot the communities without relying on their built-in plot:
  V(net)$community <- cfg$membership
  colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
  
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_12_1")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste("Clustering by", file_name)
  #saving file png 
  png(temp_ff)
  
  plot(net, main=t_main, vertex.color=colrs[V(net)$community])
  
  #Close the device using
  dev.off()
  graphics.off()
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_12_2")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste (file_name, "by Clustering-fast-greedy")

  #saving file png 
  png(temp_ff)
  
  #cfg[1]
  membership(cfg)
  plot(cfg, main=t_main, as.undirected(net))
  #Close the device using
  dev.off()
  graphics.off()
  
  
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_12_3")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste (file_name, "by Dendogram-fast-greedy")
  
  #saving file png 
  png(temp_ff)
  
  #cfg[1]
  membership(cfg)
  dendPlot(cfg,main=t_main, mode="auto")
  #Close the device using
  dev.off()
  graphics.off()
}

graph_analysis_6_0 <-function(net, file_name, path2)
{
  #K-core decomposition
  #Assortativity and homophily
  colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
  kc <- coreness(net, mode="all")
  
  #To load the frame from file
  #library(stringr)
  image_f <- paste(file_name,"_7_1")
  image_ff <- paste(image_f,"png", sep = ".")
  temp_ff <- paste(path2, image_ff, sep = "/")
  t_main <- paste(file_name,"by K-core")

  #saving file png 
  png(temp_ff)
  
  plot(net, main=t_main, vertex.size=kc*6, vertex.label=kc, vertex.color=colrs[kc])
  dev.off()
  graphics.off()
}

