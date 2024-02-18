# Weronika Duda

# zad 1
set.seed(11)
n <- sample(seq(5, 29, by = 2), 1) 
B <- matrix(0, n, n)
diag(B) <- 1:n
diag(B[ , n:1]) <- 1:n

print(B)

# zad 2
set.seed(41)

C <- matrix(runif(5*10, min = 2, max = 5), 5, 10)
D <- matrix(runif(5*10, min = 2, max = 5), 5, 10)

# a
a2 <- t(C) %*% D
print(a2)

# b
print(crossprod(t(C), t(D)))

# zad 3
set.seed(40)
x <- c(as.vector(C), as.vector(D))

x[sample(length(x), 20)] <- NA

# a
print(mean(x, na.rm = TRUE))

# b
print(sd(x, na.rm = TRUE))

# zad 4
#a
m <- 10
print(outer(1:m-1, 1:m-1, "+") %% m)

#b
n <- 9
print(outer(1:n-1, n:1, "+") %% n)


# zad 5
set.seed(31)  
G <- matrix(sample(-20:20, 200, replace = TRUE), nrow = 20, ncol = 10)
print(G)
print(t(apply(G, 1, sort))[,9:10])

# zad 6
set.seed(31) 
E <- matrix(sample(1:10, 60, replace = TRUE), nrow = 6, ncol = 10) 

# a
sumy_kol <- which(outer(colSums(E), colSums(E), '+') > 75, arr.ind = TRUE)
print(sumy_kol)

# b
pary <- sumy_kol[sumy_kol[,1] < sumy_kol[,2], ]
print(pary)

# zad 7
set.seed(27)

x <- sample(20:27, 200, replace = TRUE)
print(x)

y <- character(length(x))
y[x >= 20 & x <= 22] <- sample(c("lic", "inż."), sum(x >= 20 & x <= 22), 
                               replace = TRUE, prob = c(0.4, 0.6))
y[x > 22] <- sample(c("mgr", "mgr inż."), sum(x > 22), 
                    replace = TRUE, prob = c(0.3, 0.7))
print(y)

z <- sample(c("Kraków", "Warszawa", "Katowice", "Rzeszów", "Częstochowa"), 200, replace = TRUE)
print(z)


dane.stud <- data.frame(wiek = x, wykształcenie = y, adres = z)
print(dane.stud)


# zad 8
# a
print(nrow(na.omit(dane.stud)))

# b
print(sum(!duplicated(dane.stud)))

# c
print(table(dane.stud$wiek))

# zad 9
# a
print(subset(dane.stud, wiek > 20 & (adres == "Kraków" | adres == "Warszawa"), select = c("wiek", "adres")))

# b
print(subset(dane.stud, wiek > 20 & (adres == "Kraków" | adres == "Warszawa")))

# zad 10
library(reshape2)
library(gridExtra)

# sposob 1
avg_age <- aggregate(wiek ~ adres + wykształcenie, data = dane.stud, FUN = function(x) round(mean(x), 2))
avg_age_flipped <- dcast(avg_age, wykształcenie ~ adres, value.var = "wiek")
#tabela1 <- grid.table(avg_age_flipped)

# sposob 2
wyniki_2 <- round(tapply(dane.stud$wiek, list(dane.stud$wykształcenie, dane.stud$adres), mean), 2)
wyniki_dataframe <- as.data.frame(wyniki_2)
tabela2 <- tableGrob(wyniki_dataframe)
#grid.arrange(tabela2)

# zad 11
set.seed(23)
lista1 <- lapply(1:6, runif, min = 2, max = 8)
print(lista1)

# zad 12
lista2 <- lapply(1:6, runif, min = 2, max = 8)
print(lista2)

wynik_sumowania <- mapply(FUN = function(x, y) x + y, lista1, lista2, SIMPLIFY = FALSE)
print(wynik_sumowania)






