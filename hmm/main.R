setwd("/home/marco/Documents/PSNC/2019/Hidden Markov Model")
source("forward.R")
source("viterbi.R")

Q = c("CALM", "WIND", "HURRICANE")
obs = c("COLD", "WARM")

a = c(0.8, 0.2, 0.0, 
      0.4, 0.5, 0.1,
      0.1, 0.7, 0.2)
A = t(matrix (a, nrow = 3, ncol = 3))
rownames(A) = Q
colnames(A) = Q

b = c(0.3, 0.7,
      0.6, 0.4,
      0.8, 0.2)
B = t(matrix (b, nrow = 2, ncol = 3))
rownames(B) = Q
colnames(B) = obs

Pi = c(0.6, 0.4, 0.0)
names(Pi) = Q

O = c("COLD", "COLD", "COLD", "COLD", "COLD")

forward(O, Q, A, B, Pi)
viterbi(O, Q, A, B, Pi)
