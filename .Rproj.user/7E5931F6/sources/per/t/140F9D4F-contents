source("R/kde.R")

# load whalemigration.txt dataset
whale <- read.table(file = "data/whalemigration.dat")

head(whale)
range(whale)

kde(data = whale$V1, y = 1005, h = 10)

y_val <- seq(1000, 1400, length.out = 201)

kde_whale <- numeric(length(y_val))

for(i in 1:length(y_val)){
  kde_whale[i] <- kde(data = whale$V1, y = y_val[i], h = 10)
}

plot(x = y_val, y = kde_whale, type = "l")
rug(x = whale$V1)


hist(x = whale$V1, probability = TRUE, ylim = c(0, 0.015),
     breaks = 18)
lines(x = y_val, y = kde_whale, col = "blue")

kde_whale_2 <- kde_vect(data = whale$V1, y = y_val, h = 10)

plot(x = y_val, y = kde_whale, type = "l")
lines(x = y_val, y = kde_whale_2, col = "blue")
rug(x = whale$V1)

all.equal(kde_whale, kde_whale_2)

## only for Gaussian kernel!
bw.nrd(x = whale$V1)
bw.SJ(x = whale$V1, method = "ste")
bw.SJ(x = whale$V1, method = "dpi")
bw.ucv(x = whale$V1)
bw.bcv(x = whale$V1)

