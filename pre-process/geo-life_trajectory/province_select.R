prov_lat_lon <- read.csv("/home/spaul/Desktop/new/province_lat_long.csv")
prov_lat_lon_round <- round(prov_lat_lon[,2:3],digits = 2)

folder_list <- list.dirs(path ="/home/spaul/Desktop/new/Geolife Trajectories 1.3/Data/",full.names = FALSE)
f_list <- as.data.frame(folder_list)
f_list_f <- data.frame(filename = rep(0,182))
for(i in 0:182){
  f_list_f[i,1] <- f_list[2*i,1]
}

for(j in 1:182){
  path1 <- paste0("/home/spaul/Desktop/new/Geolife Trajectories 1.3/Data/", f_list_f[j,1],"/Trajectory/sum.csv")
  data_user <- read.csv(path1,sep = ",")
  lat_long <- round(data_user[,2:3], digits = 2)
  place <- data.frame(province = rep(0,nrow(lat_long)))
  dist <- data.frame(dist = rep(0, nrow(prov_lat_lon_round)))
  library(geosphere)
  for(l in 1:nrow(lat_long)){
    for(k in 1:nrow(prov_lat_lon_round)){
      dist[k,1] <- distGeo(c(prov_lat_lon_round[k,2],prov_lat_lon_round[k,1]),c(lat_long[l,2],lat_long[l,1]))
    }
    place[l,1] <- prov_lat_lon[which.min(dist$dist),1]
  }
  place_f <- unique(place$province)
  place_f <- as.data.frame(place_f)
  path2 <- paste0("/home/spaul/Desktop/new/unq_dist/",f_list_f[j,1],"_unq.csv")
  write.csv(place_f,path2, sep = ",")
}

setwd("/home/spaul/Desktop/new/unq_dist/")
user_data <- read.csv(paste0(f_list_f[1,1],"_unq.csv"), sep = ",")
user_data <- user_data %>% mutate (user_id = f_list_f[1,1])
for(j in 2:182){
  user_data1 <- read.csv(paste0(f_list_f[j,1],"_unq.csv"), sep = ",")
  user_data1 <- user_data1 %>% mutate (user_id = f_list_f[j,1])
  user_data <- rbind(user_data, user_data1)
}
user_data_f <- user_data[,2:3]
write.csv(user_data_f, "/home/spaul/Desktop/new/sorted_place.csv", sep = ",")

#########
library(dplyr)
col0 <- data.frame(col = prov_lat_lon[,1])
col1 <- data.frame(col = as.numeric(f_list_f[,1]))
col0 <- col0 %>% mutate(Type = "Province")
col1 <- col1 %>% mutate(Type = "User_id")
col <- rbind(col0,col1)
write.csv(col, "/home/spaul/Desktop/new/color.csv", sep = ",")
library(igraph)
user_data_f1 <- read.csv("/home/spaul/Desktop/new/sorted_place.csv", sep = ",")
user_data_f <- user_data_f1[,2:3]
user_f_graph <- graph.data.frame(user_data_f, vertices = col, directed = FALSE)
user_f_graph <- graph.data.frame(user_data_f, directed = FALSE)
plot(user_f_graph1, edge.arrow.size = 0.2)
g <- simplify(user_f_graph)
V(g)$color <- ifelse(V(g)$Type == "Province", "lightblue", "orange")
V(g)$size <-  ifelse(V(g)$Type == "Province", 10, 2)
V(g)$label <- ifelse(V(g)$Type == "User_id",NA,V(g)$name)
plot(g, edge.arrow.size = 0.2)
start_time <- Sys.time()
p <- cluster_louvain(g)
plot(p,g)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
p1 <- cluster_leading_eigen(g)
plot(p1,g)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
p2 <- cluster_fast_greedy(g)
plot(p2,g)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
p3 <- cluster_edge_betweenness(g)
plot(p3,g)
end_time <- Sys.time()
end_time - start_time
##########