
settings <- list(par.main.text = list(font = 1, just = "left", x = grid::unit(5, "mm"))
                 , par.sub.text = list(font = 2, just = "left", x = grid::unit(5, "mm")))

barchart(sort(table(dsFacturacion$sector))
         , col = "#0080ff"
         , border = "transparent"
         , xlim = c(0, 27000)
         , xlab = "Number of shots"
         , main = "Number of shots by shot type"
         , par.settings = settings
         , panel = function(...){
           panel.abline(v = seq(0, 26000, 1000), col = "gray90")
           args <- list(...)
           panel.text(
             args$x, args$y, paste0(args$x, " (", round(prop.table(args$x), 3)*100, "%)")
             , pos = 4)
           panel.barchart(...)})

glimpse(dsFacturacion)

head(dsFacturacion, n=10)


sapply(dsFacturacion$facturacion,mean,na.rm=TRUE)
fivenum(dsFacturacion$facturacion)


library(Hmisc)
describe(mydata) 


library(pastecs)

library(psych)

v1 <- c(1,2,3)
v2 <- c(4,5,6)
c(v1,v2)

v1 <- c(1,2,3)
v3 <- c("A","B","C")
c(v1,v3)

mode(3.1415)
mode("foo")

x <- c(0,1,1,2,3,5,8,13,21,34)
y <- log(x+1)
mean(x)
median(x)
sd(x) # standard deviation
var(x) 
cor(x, y)
cov(x, y)


x <- c(0,1,1,2,3,NA)
mean(x)
sd(x)
mean(x, na.rm=TRUE)
sd(x, na.rm=TRUE)

1:5
seq(from=1, to=5, by=2)
rep(1, times=5)


x[1:4]
years <- c(1960, 1964, 1976, 1994)
names(years) <- c("Kennedy", "Johnson", "Carter", "Clinton")
years

cv <- function(x) sd(x)/mean(x)
cv(1:10)

gcd <- function(a,b) {
  if (b == 0) return(a)
  else return(gcd(b, a %% b))
}


getwd()

save.image()
history(50)
.Last.value
search()
library(MASS)
help(lda)

head(pressure)
help("pressure")
data()
data(Cars93, package="MASS")



scores <- c(61, 66, 90, 88, 100)
scores <- data.frame() # Create empty data frame
scores <- edit(score) # Invoke editor, overwrite with edited data

points <- data.frame(
  label=c("Low", "Mid", "High"),
  lbound=c( 0, 0.67, 1.64),
  ubound=c(0.674, 1.64, 2.33)
)

print(pi, digits=2)
print(100*pi, digits=4)
cat(pi, "\n")
cat(format(pi,digits=4), "\n") 

pnorm(-3:3)
print(pnorm(-3:3), digits=3)

q <- seq(from=0,to=3,by=0.5)
tbl <- data.frame(Quant=q, Lower=pnorm(-q), Upper=pnorm(q))
tbl
print(tbl,digits=2)

list.files()
list.files(all.files=TRUE)

records <- read.fwf("filename", widths=c(w1, w2, ..., wn))


plot(dsFacturacion)

v <- c(10, 20, 30)
names(v) <- c("Moe", "Larry", "Curly")
print(v)
v["Larry"]

mode(3.1415)
mode(c(2.7182, 3.1415))
mode("Moe")
mode(list("Moe","Larry","Curly"))

d <- as.Date("2010-03-15") 
mode(d)
length(d)
class(d)

A <- 1:6
dim(A)
print(A)

dim(A) <- c(2,3)
print(A)


attach(dsFacturacion)
sector


