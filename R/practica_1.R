stations <- read.csv("/Users/cristofer/Downloads/bi_r_dm_ca/Ch5_bike_station_locations.csv")
summary(stations)
hist(stations$latitude, col='gray')
hist(stations$longitude, ylim=c(0, 60), col='gray')
plot(stations$longitude, stations$latitude, asp=1) # asp=1 aspect ratio of 1:1 on the Y and X axes

# k-means algorithm can randomly place initial centers in the data space for you
set.seed(123)
two <- kmeans(stations, 2) # object with two clusters
three <- kmeans(stations, 3) # object with three clusters

two$centers # coordinates for the clusters
two$size # size of each cluster

# combine the existing stations data frame along with two new variables, clus2 and clus3
clus <- cbind(stations, clus2=two$cluster, clus3=three$cluster) # column-binding
head(clus)

# developing a business case
# the plots show all the station locations, a colored shape indicating its cluster memership,
# and numbered diamonds representing the center of the cluster.
plot(clus$longitude, clus$latitude, col = two$cluster, asp = 1,
     pch = two$cluster, main = "Sites for two kiosks",
     xlab = "Longitude",  ylab = "Latitude")
points(two$centers[ ,2], two$centers[ ,1], pch = 23,
       col = 'maroon', bg = 'lightblue', cex = 3)
text(two$centers[ ,2], two$centers[ ,1], cex = 1.1,
     col = 'black', attributes(two$centers)$dimnames[[1]])

plot(clus$longitude, clus$latitude, col = three$cluster, asp = 1,
     pch = three$cluster, main = "Sites for three kiosks",
     xlab = "Longitude",  ylab = "Latitude")
points(three$centers[ ,2], three$centers[ ,1],
       pch = 23, col = 'maroon', bg = 'lightblue', cex = 3)
text(three$centers[ ,2], three$centers[ ,1], cex = 1.1,
     col = 'black', attributes(three$centers)$dimnames[[1]])

# The marketing wants to know how a kiosk's location affects the distance from each
# station to the nearest kiosk.

# Creating a new hybrid data frame, creating the shapes to highlight cluster assignment
# differences to support the business case analysis:
# 1. Create a new hybrid_shape variable and fill it with zeros.
# 2. Loop through each of the observations.
# 3. Compare the cluster number that is assigned under both models.
# 4. Assign the hybrid shape based on the comparison results.
hybrid <- cbind(clus, hybrid_shape = rep(0, dim(clus)[1]))
for (e in 1:dim(hybrid[1])[1]) {
  if (hybrid[e, 3] == hybrid[e, 4]) {
    hybrid[e, 5] <- hybrid[e, 3]
  }
  if (hybrid[e, 3] != hybrid[e, 4]) {
    hybrid[e, 5] <- hybrid[e ,3] + 15
  } }
plot(hybrid$longitude, hybrid$latitude, col = two$cluster,
     main = "Hybrid: Sites for two kiosks in three cluster
        locations", pch = hybrid$hybrid_shape, cex = 1.1,
     xlab = "Longitude",  ylab = "Latitude", asp = 1)
points(three$centers[1:2, 2], three$centers[1:2, 1],
       pch = 23, col = 'maroon', bg = 'lightblue', cex = 3)
text(three$centers[1:2, 2], three$centers[1:2, 1], cex = 1.1,
     col = 'black', attributes(two$centers)$dimnames[[1]])

# Clustering using hierarchical techniques
# we don't work a with predetermined number of centers and iterating to find membership
#, hierarchical techniques continually pair or split data into clusters based on similarity (distance). 
# There are two different approaches:
# Divisive clustering: This begins with all the data in a single cluster and then splits it 
# and all subsequent clusters until each data point is its own individual cluster
# Agglomerative clustering: This begins with each individual data point and pairs them together 
# in a hierarchy until there is just one cluster
market <- read.csv("/Users/cristofer/Downloads/bi_r_dm_ca/Ch5_age_income_data.csv")
str(market)
summary(market)
boxplot(market$age ~ market$bin, main = "Explore Age") # binned ages
boxplot(market$income ~ market$bin, main = "Explore Income") # shows a relationship between age and income
cor.test(market$age, market$income)

market$age_scale <- as.numeric(scale(market$age))
market$inc_scale <- as.numeric(scale(market$income))

set.seed(456)
hc_mod <- hclust(dist(market[ ,4:5]), method = "ward.D2")
# dist() is required because hclust() works using a distance matrix. A distance matrix is a square matrix 
# that compares every data point with its distance from every other data point. 
# In our case, this matrix will be 8105 x 8105, with zeros down the diagonal.
# [ ,4:5] is a subset of the market data frame. Specifically, the two columns 
# containing the normalized (centered and scaled) ages and incomes.

# ward.D2 is the clustering algorithm used. There are a few algorithms to choose from. 
# Ward D is often used due to its speed and practicality. It is the same as using the 
# agglomerative nesting (AGNES) method. Divisive analysis (DIANA) is used for divisive clustering.
dend <- as.dendrogram(hc_mod)
install.packages("dendextend")
library(dendextend)
dend_six_color <- color_branches(dend, k = 6) 
plot(dend_six_color, leaflab = "none", horiz = TRUE,
     main = "Age and Income Dendrogram", xlab = "Height")
abline(v = 37.5, lty = 'dashed', col = 'blue')

str(cut(dend, h = 37.5)$upper)




