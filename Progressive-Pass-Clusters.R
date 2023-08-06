library(tidyverse)
library(caret)
library(FNN)

# passing dataset
passes <- data.frame(start_x=c(49.1,71.2), 
                     start_y=c(10.5,44.7), 
                     end_x=c(77.8,84.9), 
                     end_y=c(24.8,39), 
                     pass_angle=c(17.9,-15.1), 
                     pass_distance=c(33.7,15.6), 
                     percent_of_remaining_distance_to_goal=c(0.54,0.46),
                     head_pass=c(0,0),
                     set_play=c(0,0),
                     cross=c(0,0))


# create arc going through edge of box to corner flag
arc_x = 100 - (17 * cos(seq(-0.5*pi, 0.5*pi, length.out = 1001)))
y = seq(0, 100, length.out = 1001)


xy <- data.frame(arc_x,y)

passes$y <- as.character(passes$end_y)
xy$y <- as.character(xy$y)

passes <- inner_join(passes, xy, by=c('y'='y'))

passes <- passes %>%
            filter(start_x<arc_x)

# create modified y axis metrics
passes$start_mod_y <- ifelse(passes$start_y>50, -50+passes$start_y, 50-passes$start_y)
passes$end_mod_y <- ifelse(passes$end_y>50, -50+passes$end_y, 50-passes$end_y)


# filter progressive passes of interes
progressive_passes <- passes %>% filter(percent_of_remaining_distance_to_goal>=0.2 & head_pass==0 & set_play==0 & cross==0)

# example penalty box progressive pass
pb_passes <- progressive_passes %>% filter(end_x>=83 & end_y>=21.1)

# add row numbers as column
pb_passes$row <- rownames(pb_passes)

# example progressive pass ending outside penalty box
out_pb_passes <- progressive_passes %>% filter(end_x<83 | end_y<21.1)

# add row numbers as column
out_pb_passes$row <- rownames(out_pb_passes)

# create min-max scaling to 0-1
# min-max penalty box progressive passes
pb_passes_min_max <- data.frame(min_max=c('min','max'), start_x=c(2, 94.6), start_mod_y=c(0, 49.9), pass_angle=c(-55.69489, 74.9764), pass_distance=c(5.06727, 98.03031))

pb_passes_standardised_process <- preProcess(as.data.frame(pb_passes_min_max), method=c("range"))


# min-max outside penalty box progressive passes
out_pb_passes_min_max <- data.frame(min_max=c('min','max'), start_x=c(0.3, 93.9), start_mod_y=c(0, 49.9), end_x=c(21.6, 100), end_mod_y=c(0, 50), pass_angle=c(-58.07921, 74.99796), pass_distance=c(5.17065, 115.80738))

out_pb_passes_standardised_process <- preProcess(as.data.frame(out_pb_passes_min_max), method=c("range"))




# create penalty box centroids dataframes
cluster <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
start_x <- c(0.66246, 0.76661, 0.79558, 0.41016, 0.74480, 0.80898, 0.71216, 0.37975, 0.62762, 0.81779)
start_mod_y <- c(0.83496, 0.16041, 0.43503, 0.74150, 0.14691, 0.43904, 0.44457, 0.23017, 0.48839, 0.76657)
pass_angle <- c(0.71461, 0.26205, 0.44935, 0.56394, 0.67563, 0.78352, 0.79447, 0.47334, 0.51076, 0.74003)
pass_distance <- c(0.44350,0.17215,0.12128,0.59350,0.24066,0.20344,0.46365,0.57974,0.32155,0.20110)
cluster_name <- c("2. Mid Wide To PB Wide 2nd 6 Yd Across", "3. Final Central To PB Wide 1st 6 Yd",
                   "3. Final Half Space To PB Wide 2nd 6 Yd", "1. Def Wide To PB Wide 1st 6 Yd", 
                   "3. Final Central To PB Central 1st 6 Yd", "3. Final Half Space To PB Central 1st 6 Yd",
                   "2. Mid Half Space To PB Wide 2nd 6 Yd Across", "1. Def Central To PB Wide 1st 6 Yd",      
                   "2. Mid Half Space To PB Wide 2nd 6 Yd", "3. Final Wide To PB Wide 2nd 6 Yd")

pb_centroids <- data.frame(cluster,cluster_name,start_x,start_mod_y,pass_angle,pass_distance)


# penalty box centroids
cluster <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
start_x <- c(0.57797, 0.51844, 0.40743, 0.70403, 0.63576, 0.49335, 0.16813, 0.53447, 0.32960, 0.16891, 0.16849, 0.20738, 0.23869, 0.60454)
start_mod_y <- c(0.15054, 0.34265, 0.81301, 0.80299, 0.80789, 0.47217, 0.17404, 0.35979, 0.76231, 0.23995, 0.78095, 0.23877, 0.15103, 0.33535)
end_x <- c(0.60864, 0.69895, 0.55025, 0.81866, 0.63665, 0.55184, 0.53320, 0.73185, 0.46745, 0.37090, 0.27337, 0.28437, 0.42302, 0.62777)
end_mod_y <- c(0.26960, 0.79450, 0.81747, 0.75897, 0.46089, 0.47645, 0.30146, 0.75338, 0.30413, 0.79627, 0.72544, 0.22746, 0.75765, 0.18003)
pass_angle <- c(0.34232, 0.21452, 0.43487, 0.48005, 0.77636, 0.43569, 0.45502, 0.82779, 0.69764, 0.22056, 0.46981, 0.50825, 0.76473, 0.73020)
pass_distance <- c(0.11702, 0.30088, 0.21406, 0.15088, 0.16545, 0.13598, 0.42927, 0.55520, 0.33536, 0.38453, 0.22373, 0.21360, 0.48988, 0.17372)
cluster_name <- c("2b. Mid Central To Mid Half Space", "2a. Mid Half Space To Final Wide",            
                  "2a. Mid Wide To Mid Wide", "2b. Mid Wide To Final Wide",
                  "2b. Mid Wide To Final Half Space", "2a. Mid Half Space To Mid Half Space",
                  "1a. GK Central To Mid Half Space", "2b. Mid Half Space To Final Wide Diagonal",
                  "1b. Def Wide To Mid Half Space", "1a. GK Central To Mid Wide",
                  "1b. Def Wide To Mid Wide", "1b. Def Central To Mid Central",
                  "1b. Def Central To Mid Wide", "2b. Mid Half Space To Final Central" )

out_pb_centroids <- data.frame(cluster,cluster_name,start_x,start_mod_y,end_x,end_mod_y,pass_angle,pass_distance)


# standardise pass data features to 0-1
pb_passes_standardised <- predict(pb_passes_standardised_process, as.data.frame(pb_passes))

# data frame to add pb passes distance to clusters
pb_dist <- data.frame()

# progress bar for loop
pb = txtProgressBar(min = 0, max = length(pb_passes_standardised$start_x), style = 3)

# loop through each penalty box pass
for (row in 1:nrow(pb_passes_standardised)) {
  
  # create progress bar
  setTxtProgressBar(pb, row)
  
  # loop through clusters 1 to 10
  for (i in 1:10) {
    
    # filter cluster centroids
    centroids <- pb_centroids %>%
                  filter(cluster==i) %>%
                  select(start_x,start_mod_y,pass_angle,pass_distance)
    
    # filter pass by row_no
    pass <- pb_passes_standardised[row, c("start_x","start_mod_y","pass_angle","pass_distance")]
    
    # calculate eucilidan distance
    pb_dist_temp <- data.frame(dist = eucDist(pass, centroids))
    
    # cluster no
    pb_dist_temp$cluster <- i
    
    # original row no
    pb_dist_temp$row <- row
    
    # union data to temp df
    pb_dist <- rbind(pb_dist_temp, pb_dist)
    
  }
  
}


# filter out top distance measure for each pass - group by row
pb_dist_nn <- pb_dist %>%
              group_by(row) %>%
              filter(dist==min(dist)) %>%
              group_by(cluster) %>%
              arrange(desc(dist)) %>%
              mutate(nn_desc=row_number()) %>%
              arrange(dist) %>%
              mutate(nn=row_number()) %>%
              arrange(row) %>%
              ungroup()

# join onto original pass data
pb_passes <- merge(pb_passes, pb_dist_nn, by = c("row"="row"))

# add pb cluster names
pb_centroids <- pb_centroids %>% distinct(cluster,cluster_name)

# join cluster name
pb_passes <- inner_join(pb_passes, pb_centroids, by=c("cluster"="cluster"))





# standardise pass data features to 0-1
out_pb_passes_standardised <- predict(out_pb_passes_standardised_process, as.data.frame(out_pb_passes))

# data frame to add pb passes distance to clusters
out_pb_dist <- data.frame()


# loop through each penalty box pass
for (row in 1:nrow(out_pb_passes_standardised)) {
  
  # create progress bar
  setTxtProgressBar(pb, row)
  
  # loop through clusters 1 to 14
  for (i in 1:14) {
    
    # filter cluster centroids
    centroids <- out_pb_centroids %>%
                 filter(cluster==i) %>%
                 select(start_x,start_mod_y,end_x,end_mod_y,pass_angle,pass_distance)
                
    # filter pass by row_no
    pass <- out_pb_passes_standardised[row, c("start_x","start_mod_y","end_x","end_mod_y","pass_angle","pass_distance")]
    
    # calculate eucilidan distance
    out_pb_dist_temp <- data.frame(dist = eucDist(pass, centroids))
    
    # cluster no
    out_pb_dist_temp$cluster <- i
    
    # original row no
    out_pb_dist_temp$row <- row
    
    # union data to temp df
    out_pb_dist <- rbind(out_pb_dist_temp, out_pb_dist)
    
  }
  
}


# filter out top distance measure for each pass - group by row
out_pb_dist_nn <- out_pb_dist %>%
                  group_by(row) %>%
                  filter(dist==min(dist)) %>%
                  group_by(cluster) %>%
                  arrange(desc(dist)) %>%
                  mutate(nn_desc=row_number()) %>%
                  arrange(dist) %>%
                  mutate(nn=row_number()) %>%
                  arrange(row) %>%
                  ungroup()


# join onto original pass data
out_pb_passes <- merge(out_pb_passes, out_pb_dist_nn, by = c("row"="row"))

# add pb cluster names
out_pb_centroids <- out_pb_centroids %>% distinct(cluster,cluster_name)

# join cluster name
out_pb_passes <- inner_join(out_pb_passes, out_pb_centroids, by=c("cluster"="cluster"))
 
