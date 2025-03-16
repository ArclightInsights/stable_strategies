library(tidyverse)
library(leaflet)

data<-read.csv("Major_city_driving_distance.csv")

trip_list<-data%>%
  filter(dist_mi <= 300)


node_list<-NULL
edge_df<-data.frame(matrix(ncol = ncol(trip_list), nrow =nrow(trip_list) ))
colnames(edge_df)<-colnames(trip_list)
start<-trip_list$C1[which(trip_list$score1 == min(trip_list$score1))][1]

node_list<-append(node_list, start)

necessary_edges<-(length(unique(trip_list$C1)) + length(unique(trip_list$C2)))-1

for (i in 1:necessary_edges){   
  
options<-trip_list%>%
  subset(C1 %in% node_list)%>%
  subset(!(C2 %in% node_list))

if(nrow(options)== 0){
  
  new_start<-trip_list%>%
    subset(!(C1 %in% node_list))%>%
    subset(!(C2 %in% node_list))
  if(nrow(new_start) ==0){
    break
  }
  
  start2<-new_start$C1[which(new_start$score1 == min(new_start$score1))][1]
  node_list<-append(node_list, start2)
  next
  }


edge_df[i,]<-options[which(options$score2 == min(options$score2))[1],]
node_list<-append(node_list, options$C2[which(options$score2 == min(options$score2))][1])
}

edge_df<-drop_na(edge_df)

map<-leaflet()%>%
  addTiles()%>%
  addMarkers(data= edge_df, lat=edge_df$lat1, lng=edge_df$lng1)%>%
  addMarkers(data=edge_df, lat=edge_df$lat2, lng=edge_df$lng2)

for (i in 1:nrow(edge_df)){
  map<-addPolylines(map,
                    lat = as.numeric(edge_df[i, c(3,4)]),
                    lng = as.numeric(edge_df[i, c(5,6)])
  )
}  
map

#add in red trips (between 300 and 600 miles)
#add in cities in the list that are more than 600 miles from anywhere


