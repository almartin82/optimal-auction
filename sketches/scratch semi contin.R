
library(lpSolve)
const_mat_so<-matrix(c(
c(0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,1,0,0,1,0,1)
,c(0,  0,  0,  0,  0,  0,  0,  1,  0,  1,  0,1,1,0,0,1,1)
,c(0,  0,  0,  0,  0,  1,  1,  0,  0,  0,  1,0,1,0,1,0,0)
,c(1,  1,  0,  1,  1,  0,  0,  0,  1,  0,  0,0,0,0,0,0,0)
,c(8800,   8500,   7600,   8600,   8400,   7500, 7000, 8500,   8800,   7700,   6700,5500,1200,6700,9500,8700,6500)
,c(0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,0,0,0,0,0,0)
,c(0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,0,0,0,0,0,0)
,c(0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,0,0,0,0,0,0)
,c(0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  1,0,0,1,0,1,0)
,c(0,  0,  1,  0,  0,  0,  0,  0,  1,  0,  0,1,1,0,0,0,0)
,c(0,  0,  1,  0,  0,  0,  0,  1,  0,  0,  0,0,0,0,0,0,0)
,c(0,  0,  0,  1,  0,  0,  0,  0,  0,  1,  0,1,1,1,0,1,0)
,c(0,  1,  0,  0,  0,  0,  0,  1,  0,  0,  0,0,0,0,0,1,0)
,c(0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,0,0,0,1,0,0)
,c(0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  1,0,0,0,0,0,0)
),nrow=15,byrow = TRUE)

const_dir_so<-c("=","=","=","=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=")

max_cost_so = 25000

objective_so = c(21.0, 19.3, 19.2, 18.8, 18.5, 16.6, 16.4, 16.4, 16.0, 16.0, 14.9, 14.6, 14.0, 13.9,12.0,5.5,24.6)

const_rhs_so<-c(1,1,1,1,25000,3,3,3,2,2,2,2,2,2,2)

x = lp("max", objective_so, const_mat_so, const_dir_so, const_rhs_so,     all.bin=TRUE, all.int=TRUE 
)

const_mat_so2 <- cbind(const_mat_so, rbind(matrix(0, nrow = 8, ncol = 7), diag(-1, 7)))

## [EDITED] make a model and set a constraint matrix and objective coefs
model <- make.lp(nrow(const_mat_so2), 0)

for(i in 1:ncol(const_mat_so2)) add.column(model, const_mat_so2[,i])
set.constr.type(model, c(const_dir_so[-c(9:15)], rep("=", 7)))
set.rhs(model, c(const_rhs_so[-c(9:15)], rep(0, 7)))   # each original output - 18-24th = 0

set.objfn(model, c(objective_so, rep(0, 7)))           # 18-24th are 0

## define semi-continuous and bounds.
set.semicont(model, col = 18:24)
set.bounds(model, lower = rep(1.9, 7), col = 18:24)   # default upper is Inf.

## define other things
set.type(model, col = 1:17, type = "binary")     # original variable
set.type(model, col = 18:24, type = "integer")   # outputs of original constraint formulas
lp.control(model, sense = "max")                 # do maximize

write.lp(model, 'ex.lp', 'lp')
