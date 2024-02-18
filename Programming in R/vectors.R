# Weronika Duda

# Zadanie_1
#a
wektor_1a <- c(1:20, 19:2, 1)
print(wektor_1a)

#b
wektor_1b <- rep(c(4, 6, 3), length.out = 50)
print(wektor_1b)

#c
wektor_1c <- c(rep(4, 10), rep(6, 20), rep(3,30))
print(wektor_1c)

#d
wektor_1d <- seq(100, 4, by = -8)
print(wektor_1d)

#e
wektor_1e <- c(0.1^seq(3, 36, by = 3)) * (0.2^seq(1, 34, by = 3))
print(wektor_1e)

# Zadanie_2
zad_2 <- c(paste0(c("A_", "X_"), 1:30, c(".B", ".D", ".F")))
print(zad_2)


# Zadanie_3
set.seed(50)

#a
wektor3a <- sample(5:15, 100, replace = T)
print(wektor3a)

#b
wektor3b <- sample(c(LETTERS, letters), 100, replace = TRUE)
print(wektor3b)

# Zadanie 4
set.seed(30)
x <- sample(0:999, 250)
y <- sample(0:999, 250)

a4 <- x[-(249:250)] + 2 * x[2:249] - y[3:250]
print(a4)

#b
b4 <- sum(exp(-x[2:250])/(x[1:249] + 10))
print(b4)

#c
c4 <- sum(c(1:20)^4*sum(1/(3 + c(1:5))))
print(c4)


# Zadanie 5
set.seed(50)
wektor_5 <- sample(0:999, 500)

podz_przez_2 <- sum(wektor_5 %% 2 == 0)
print(podz_przez_2)

podz_przez_2_i_3 <- sum(wektor_5 %% 2 == 0 & wektor_5 %% 3 == 0)
print(podz_przez_2_i_3)

ile_30__70 <- sum(wektor_5 < 30 | wektor_5 > 70)
print(ile_30__70)

# Zadanie 6
napis <- c("Katedra", "Informatyki", "Biznesowej", "i", "Inżynierii", "Zarządzania", "WZ", "AGH", "2022")

ile_roznych_znakow <- table(unlist(strsplit(napis, "")))
print(ile_roznych_znakow)

znak <- names(which.max(table(unlist(strsplit(napis, "")))))
print(znak)
                                                          