x <- rnorm(100, mean=5, sd =2)
x
y <-(x *5) + 2 + (rnorm(100, 0:0.1 ))
y
plot(x, y)
abline(lm(y~x), col='red')
coef(lm(y~x))

z <- c()
x <- rnorm(100, mean=5, sd=2)
for (i in 1:100) {
	z[i] <- runif(1)
	y <- (x * z[i]) + 2 + (rnorm(100, 0:0.1))
	l <- coef(lm(z[1:100]~y))
}
l
plot(z[1:100],y)
abline(lm(y~z[1:100]))
plot(c(z, -0.029))

install.packages('meme')
library('meme')