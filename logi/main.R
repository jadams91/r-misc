
setwd("/home/sebastian/Documents/2019/Logi")

source("logi.R")
source("lcnf.R")
source("lsat.R")

s1 <- lquote(((A & B) %<=>% (C | -D) | -(-A & D %xor% E)) %=>% -E)
s2 <- lquote(-A & B | C | -D | E | -(A %<=>% -C))
s3 <- s1 %xor% s2


t1 <- as_lcnf(standardize(s3))
t2 <- as_lcnf(standardize(-s1))

lsolve(s3, truth_table(literals(s3)))
lsolve(t1, truth_table(literals(t1)))
lsolve(t2, truth_table(literals(t2)))

dpll_sat(t2)
walk_sat(t2, p = 0.7, max_flips = 10)

t3 <- t1 %<=>% t2
t4 <- t1 & t2 & t3

dpll_sat(t4)
walk_sat(t4, p = 0.7, max_flips = 10)

