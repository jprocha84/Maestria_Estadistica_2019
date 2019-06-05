# http://www.rpubs.com/venkatritch/333327
f=expression(x^2+3*x)
f=expression(1/(n*(x^2)))
D(f,'x')
D(D(f,'x'),'x')

f=expression(-n*x)
D(f,'x')
D(D(f,'x'),'x')


# http://homepages.math.uic.edu/~jyang06/stat522/handouts/handout8.pdf
## define the integrated function
integrand <- function(x) {1/((x+1)*sqrt(x))}
## integrate the function from 0 to infinity
integrate(integrand, lower = 0, upper = Inf)


#https://cran.r-project.org/web/packages/PolynomF/PolynomF.pdf
#https://www.rdocumentation.org/packages/PolynomF/versions/1.0-2
f<-function(x) 2*x
integrate(f,0,2)

f=expression(1/(n*x))
integral(2*x, limits = NULL)



# NOT RUN {
p <- poly.from.zeros(-2:5)
ip <- integral(p)
ipv <- integral(p, limits = c(-2, 5))

plot(polylist(p, deriv(p)))

x <- polynom()
H <- polylist(1, x)Y
for(n in 2:10)
  H[[n+1]] <- x * H[[n]] - (n-1)*H[[n-1]]

solve(deriv(H))

# }
library(PolynomF)
