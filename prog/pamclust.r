library(cluster)

d  <- daisy(q2009)

fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

#partition around mediods 5 clusters
fit1  <- pam(d, 5)

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric  MDS", col = fit1$clustering)






