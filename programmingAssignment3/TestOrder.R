###test order

##create 2 vectors
v <- sample(1:26, replace= TRUE)
c <- sample(letters, replace= TRUE)

##make list, data.frame 
l <- cbind(v,c)
colnames(l) <- c("number", "letter") ## give column names

## clear some values
s <- sample(1:26, 3)
s2 <- sample(1:26, 2)
l[s,1] <- NA
l[s2,2] <- NA

df <- as.data.frame(l)

ndf <- df[order(df$letter),]
ndf <- df[order(df$letter, df$number),]

