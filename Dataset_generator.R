

# Dataset Generator -------------------------------------------------------
## This R scrip will Generate a number of data sets


# Setting up the Betas ----------------------------------------------------


n = 1000
beta1 = rnorm(n) * rnorm(n) #This will simulate a coefficient and some data
beta2 = rbinom(n, 1, .2) * rnorm(n) # a Dummy Variable
beta3 = rnorm(n) * rnorm(n)
beta4 = rbinom(n, 1, .5) * rnorm(n)
beta5 = rnorm(n) * rnorm(n)
alpha = rnorm(n)

X <- cbind(beta1, beta2, beta3, beta4, beta5)




# Calculate z1
z1 = alpha + sin(beta1 * pi) * log(beta2 + exp(beta3)) + cos(beta4) ^ (exp(beta5)) + rnorm(n)
pr1 = 1 / (1 + exp(-z1))
y1 = (rbinom(n,1,pr1))

# Calculate z2
z2 = alpha + tan(beta1 + pi) * log(beta2 ^ (exp(beta3))) * sin(beta4 + beta5)+ rnorm(n)
pr2 = 1 / (1 + exp(-z2))
y2 = (rbinom(n,1,pr2))

# Calculate z3
z3 = alpha + cos(beta1 * pi) ^ (log(beta2) * exp(beta3)) * log(beta4 + exp(beta5)) ^ (sin(beta1 + beta5) / pi)+ rnorm(n)
pr3 = 1 / (1 + exp(-z3))
y3 = (rbinom(n,1,pr3))

# Calculate z4
z4 = alpha + cos(beta1) * log(beta2 + exp(beta3)) ^ (sin(beta4) * exp(beta5))+ rnorm(n)
pr4 = 1 / (1 + exp(-z4))
y4 = (rbinom(n,1,pr4))

# Calculate z5
z5 = alpha + sin(beta1 + pi) * log(beta2 ^ (exp(beta3))) + cos(beta4) * exp(beta5)+ rnorm(n)
pr5 = 1 / (1 + exp(-z5))
y5 = (rbinom(n,1,pr5))

# Calculate z6
z6 = alpha + cos(beta1) * log(beta2 + exp(beta3)) + sin(beta4) ^ (exp(beta5))+ rnorm(n)
pr6 = 1 / (1 + exp(-z6))
y6 = (rbinom(n,1,pr6))

# Calculate z7
z7 = alpha + sin(beta1) * log(beta2 + exp(beta3)) + cos(beta4) * exp(beta5)+ rnorm(n)
pr7 = 1 / (1 + exp(-z7))
y7 = (rbinom(n,1,pr7))

# Calculate z8
z8 = alpha + tan(beta1) * log(beta2 ^ (exp(beta3))) * cos(beta4 + beta5)+ rnorm(n)
pr8 = 1 / (1 + exp(-z8))
y8 = (rbinom(n,1,pr8))

z9 = alpha + cos(beta1 * pi) ^ (log(beta2) * exp(beta3)) * sin(beta4 + beta5) ^ (log(beta1) + exp(beta5))+ rnorm(n)
pr9 = 1 / (1 + exp(-z9))
y9 = (rbinom(n,1,pr9))

# Calculate z10
z10 = alpha + sin(beta1) * log(beta2 + exp(beta3)) + cos(beta4) ^ (exp(beta5))+ rnorm(n)
pr10 = 1 / (1 + exp(-z10))
y10 = (rbinom(n,1,pr10))

# Calculate z11
z11 = alpha + log(beta1 + exp(beta2)) * sin(beta3) + cos(beta4) ^ (log(beta5))+ rnorm(n)
pr11 = 1 / (1 + exp(-z11))
y11 = (rbinom(n,1,pr11))

# Calculate z12
z12 = alpha + cos(beta1) ^ (exp(beta2)) * log(beta3 + exp(beta4)) + sin(beta5)+ rnorm(n)
pr12 = 1 / (1 + exp(-z12))
y12 = (rbinom(n,1,pr12))

# Calculate z13
z13 = alpha + sin(beta1) * exp(beta2) * log(beta3) + cos(beta4) ^ (log(beta5))+ rnorm(n)
pr13 = 1 / (1 + exp(-z13))
y13 = (rbinom(n,1,pr13))

# Calculate z14
z14 = alpha + log(beta1) * exp(beta2) + sin(beta3) * cos(beta4) ^ (log(beta5))+ rnorm(n)
pr14 = 1 / (1 + exp(-z14))
y14 = (rbinom(n,1,pr14))

# Calculate z15
z15 = alpha + sin(beta1) * log(beta2) * cos(beta3) ^ (exp(beta4)) + log(beta5)+ rnorm(n)
pr15 = 1 / (1 + exp(-z15))
y15 = (rbinom(n,1,pr15))

# Calculate z16
z16 = alpha + cos(beta1) * log(beta2 + exp(beta3)) * sin(beta4) + exp(beta5)+ rnorm(n)
pr16 = 1 / (1 + exp(-z16))
y16 = (rbinom(n,1,pr16))

# Calculate z17
z17 = alpha + cos(beta1) ^ (exp(beta2)) * sin(beta3) * log(beta4 + exp(beta5))+ rnorm(n)
pr17 = 1 / (1 + exp(-z17))
y17 = (rbinom(n,1,pr17))

# Calculate z18
z18 = alpha + log(beta1) * sin(beta2) * cos(beta3) + exp(beta4) ^ (log(beta5))+ rnorm(n)
pr18 = 1 / (1 + exp(-z18))
y18 = (rbinom(n,1,pr18))

# Calculate z19
z19 = alpha + cos(beta1) * exp(beta2) + log(beta3) * sin(beta4) ^ (log(beta5))+ rnorm(n)
pr19 = 1 / (1 + exp(-z19))
y19 = (rbinom(n,1,pr19))

# Calculate z20
z20 = alpha + sin(beta1) * exp(beta2) * cos(beta3) + log(beta4) ^ (exp(beta5))+ rnorm(n)
pr20 = 1 / (1 + exp(-z20))
y20 = (rbinom(n,1,pr20))

# Calculate z21
z21 = alpha + cos(beta1) ^ (log(beta2)) * log(beta3 + exp(beta4)) + sin(beta5)+ rnorm(n)
pr21 = 1 / (1 + exp(-z21))
y21 = (rbinom(n,1,pr21))

# Calculate z22
z22 = alpha + log(beta1) * cos(beta2) * sin(beta3) ^ (exp(beta4)) + log(beta5)+ rnorm(n)
pr22 = 1 / (1 + exp(-z22))
y22 = (rbinom(n,1,pr22))

# Calculate z23
z23 = alpha + log(beta1) * sin(beta2) * cos(beta3) ^ (log(beta4)) + exp(beta5)+ rnorm(n)
pr23 = 1 / (1 + exp(-z23))
y23 = (rbinom(n,1,pr23))

# Calculate z24
z24 = alpha + sin(beta1) * log(beta2 + exp(beta3)) * cos(beta4) + exp(beta5)+ rnorm(n)
pr24 = 1 / (1 + exp(-z24))
y24 = (rbinom(n,1,pr24))

# Calculate z25
z25 = alpha + cos(beta1) * sin(beta2) * log(beta3) + exp(beta4) ^ (log(beta5))+ rnorm(n)
pr25 = 1 / (1 + exp(-z25))
y25 = (rbinom(n,1,pr25))

# Calculate z26
z26 = alpha + log(beta1 + exp(beta2)) * sin(beta3) * cos(beta4) ^ (log(beta5))+ rnorm(n)
pr26 = 1 / (1 + exp(-z26))
y26 = (rbinom(n,1,pr26))

# Calculate z27
z27 = alpha + cos(beta1) * exp(beta2) * sin(beta3) + log(beta4) ^ (exp(beta5))+ rnorm(n)
pr27 = 1 / (1 + exp(-z27))
y27 = (rbinom(n,1,pr27))

# Calculate z28
z28 = alpha + sin(beta1) * log(beta2) * cos(beta3) ^ (exp(beta4)) + log(beta5)+ rnorm(n)
pr28 = 1 / (1 + exp(-z28))
y28 = (rbinom(n,1,pr28))

# Calculate z29
z29 = alpha + log(beta1) * cos(beta2) * sin(beta3) ^ (log(beta4)) + exp(beta5)+ rnorm(n)
pr29 = 1 / (1 + exp(-z29))
y29 = (rbinom(n,1,pr29))

# Calculate z30
z30 = alpha + sin(beta1) * cos(beta2) * log(beta3) + exp(beta4) ^ (log(beta5))+ rnorm(n)
pr30 = 1 / (1 + exp(-z30))
y30 = (rbinom(n,1,pr30))

# Calculate z31
z31 = alpha + log(beta1 + sin(beta2)) * cos(beta3) * exp(beta4) ^ (log(beta5))+ rnorm(n)
pr31 = 1 / (1 + exp(-z31))
y31 = (rbinom(n,1,pr31))

# Calculate z32
z32 = alpha + cos(beta1) * log(beta2) * sin(beta3) + exp(beta4) ^ (log(beta5))+ rnorm(n)
pr32 = 1 / (1 + exp(-z32))
y32 = (rbinom(n,1,pr32))

# Calculate z33
z33 = alpha + sin(beta1) * cos(beta2) * exp(beta3) + log(beta4) ^ (exp(beta5))+ rnorm(n)
pr33 = 1 / (1 + exp(-z33))
y33 = (rbinom(n,1,pr33))

# Calculate z34
z34 = alpha + log(beta1) * sin(beta2) * cos(beta3) ^ (exp(beta4)) + log(beta5)+ rnorm(n)
pr34 = 1 / (1 + exp(-z34))
y34 = (rbinom(n,1,pr34))

# Calculate z35
z35 = alpha + cos(beta1) * log(beta2) * sin(beta3) + exp(beta4) ^ (log(beta5))+ rnorm(n)
pr35 = 1 / (1 + exp(-z35))
y35 = (rbinom(n,1,pr35))

# Calculate z36
z36 = alpha + sin(beta1) * cos(beta2) * log(beta3) + exp(beta4) ^ (log(beta5))+ rnorm(n)
pr36 = 1 / (1 + exp(-z36))
y36 = (rbinom(n,1,pr36))

# Calculate z37
z37 = alpha + log(beta1 + sin(beta2)) * cos(beta3) * exp(beta4) ^ (log(beta5))+ rnorm(n)
pr37 = 1 / (1 + exp(-z37))
y37 = (rbinom(n,1,pr37))

# Calculate z38
z38 = alpha + cos(beta1) * log(beta2) * sin(beta3) + exp(beta4) ^ (log(beta5))+ rnorm(n)
pr38 = 1 / (1 + exp(-z38))
y38 = (rbinom(n,1,pr38))

# Calculate z39
z39 = alpha + sin(beta1) * cos(beta2) * exp(beta3) + log(beta4) ^ (exp(beta5))+ rnorm(n)
pr39 = 1 / (1 + exp(-z39))
y39 = (rbinom(n,1,pr39))

# Calculate z40
z40 = alpha + log(beta1) * sin(beta2) * cos(beta3) ^ (exp(beta4)) + log(beta5)+ rnorm(n)
pr40 = 1 / (1 + exp(-z40))
y40 = (rbinom(n,1,pr40))

z41 = alpha + cos(beta1 + beta2) * sin(beta3) * log(beta4) + exp(beta5)+ rnorm(n)
pr41 = 1 / (1 + exp(-z41))
y41 = (rbinom(n,1,pr41))

z42 = alpha + log(beta1) * cos(beta2) * sin(beta3) + exp(beta4 + beta5)+ rnorm(n)
pr42 = 1 / (1 + exp(-z42))
y42 = (rbinom(n,1,pr42))

z43 = alpha + sin(beta1) * log(beta2) * cos(beta3) + exp(beta4 * beta5)+ rnorm(n)
pr43 = 1 / (1 + exp(-z43))
y43 = (rbinom(n,1,pr43))

z44 = alpha + cos(beta1) * sin(beta2) * log(beta3) + exp(beta4 / beta5)+ rnorm(n)
pr44 = 1 / (1 + exp(-z44))
y44 = (rbinom(n,1,pr44))

z45 = alpha + log(beta1 + beta2) * cos(beta3) * sin(beta4) + exp(beta5)+ rnorm(n)
pr45 = 1 / (1 + exp(-z45))
y45 = (rbinom(n,1,pr45))

z46 = alpha + sin(beta1) * log(beta2) * cos(beta3) + exp(beta4 - beta5)+ rnorm(n)
pr46 = 1 / (1 + exp(-z46))
y46 = (rbinom(n,1,pr46))

z47 = alpha + cos(beta1) * sin(beta2) * log(beta3) + exp(beta4 + beta5)+ rnorm(n)
pr47 = 1 / (1 + exp(-z47))
y47 = (rbinom(n,1,pr47))

z48 = alpha + log(beta1) * cos(beta2) * sin(beta3) + exp(beta4 - beta5)+ rnorm(n)
pr48 = 1 / (1 + exp(-z48))
y48 = (rbinom(n,1,pr48))

z49 = alpha + sin(beta1) * log(beta2) * cos(beta3) + exp(beta4 * beta5)+ rnorm(n)
pr49 = 1 / (1 + exp(-z49))
y49 = (rbinom(n,1,pr49))

z50 = alpha + cos(beta1) * sin(beta2) * log(beta3) + exp(beta4 / beta5) + rnorm(n)
pr50 = 1 / (1 + exp(-z50))
y50 = (rbinom(n,1,pr50))
