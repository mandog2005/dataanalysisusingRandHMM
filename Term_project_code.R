View(df)
df$Global_active_power <- as.numeric(df$Global_active_power)
pca <- prcomp(na.omit(df1[,3:9]),scale=TRUE)
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

pca

plot(pca, type = "l")

pca.var <- pca$sdev^2


biplot(pca, scale = 0)
