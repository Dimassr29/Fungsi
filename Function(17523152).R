#Dimas Setyawan Ramadhansyah (17523152)

#Exercise
#No.1.1
f1 <- function (x){
  result <- x^3+x^2-6
  return(result)
}
f1(2)

#No.1.2
g1 <- function (a,b){
  result <- a*b *(b-a)
  return(result)
}
g1(2,4)

#No. 1.3
h1 <- function(m,n){
  result <- (sqrt(m)/n) + m - 2*n
  return(result)
}
h1(16,2)

#No 2.1
a <- matrix(c(1:4),2,2,T)
b <- matrix(c(5:8),2,2,T)

f2 <- function(a,b){
  result <- (a+b)%*%a%*%b
  return(result)
}
f2(a,b)

#No. 2.2
m <- matrix(c(1:4),2,2,T)
n <- matrix(c(5:8),2,2,T)

h2 <- function(m,n){
  result <- det(m)*n-m%*%n
  return(result)
}
h2(m,n)

#No.2.3
x <- matrix(c(1:4),2,2,T)
g <- function(x){
  result <- solve(x)%*%x-2%*%x
  return(result)
}
g(2)

#GRAPH FUNGSI KONSTAN
c1 <- function(x){
  fx <- 5
  return (fx)
}

input <- 0:15
plot (input ,
      sapply (input,c1), 
      type = "l",xlab = "x", 
      ylab = "f(x)")

#GRAPH FUNGSI LINIER

l1 <- function(x){
  fx <- 2*x+5
  return(fx)
}

input <- 0:15
plot(input,
     sapply(input,l1),
     type="l",
     xlab="x",
     ylab="f(x)")

#GRAPH FUNGSI KUADRAT

q1 <- function(x){
  fx<- 2*x^2+1*x+2
  return(fx)
}

input <- -10:10
plot (input,
      sapply(input , q1),
      type="l" ,
      xlab = "x",
      ylab = "f(x)")

# GRAPH FUNGSI POLINOMIAL

p1 <- function(x){
  fx <- 2*x^3 + 1*x^2 + 2*x 
  return (fx)
}

input <- seq(-5,6,0.1)
plot (input,
      sapply(input,p1),
      type="l",
      xlab="x",
      ylab="f(x)")


# GRAPH FUNGSI RATIONAL

r1 <- function(x){
  fx <- 2/x
  return(fx)
}

input <- seq(3,27,0.1)
plot(input,
     sapply(input, r1),
     type="l",
     xlab="x",
     ylab="f(x)")