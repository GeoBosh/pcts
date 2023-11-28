test_that("pcarma_XXX work correctly",
{
    ## for now, use the examples from pcarma_solve.Rd
    
## periodic acf of Lambert-Lacroix
data(ex1f)
(pc3 <- slMatrix(period = 2, maxlag = 5, f = ex1f, type = "tt"))
## find the parameters
s3 <- pcarma_param_system(pc3, NULL, NULL, 2, 0, 2)
coef3 <- solve(s3$A, s3$b)
pcarma_unvec(list(p = 2, q = 0, period = 2, param = coef3))

## actually, the model is PAR(1,2):
s3a <- pcarma_param_system(pc3, NULL, NULL, c(1, 2), 0, 2)
coef3a <- solve(s3a$A, s3a$b)
coef3a_more <- pcarma_unvec(list(p = c(1,2), q = 0, period = 2, param = coef3a))
coef3a_vec <- pcarma_tovec(coef3a_more)
    
## prepare test parameters for a PAR(2) model with period=2.
##   (rounded to 6 digits from the above example.
m1 <- rbind(c(1, 0.81, 0), c(1, 0.4972376, 0.4972376) )
m2 <- rbind(c(1, 0, 0), c(1, 0, 0) )
testphi <- slMatrix(init = m1)
testtheta <- slMatrix(init = m2)
si2 <- PeriodicVector(c(0.3439000, 0.1049724)) #     # or si2 <- c(1,1)

## acf from parameters
myf <- pcarma_acvf_lazy(testphi, testtheta, si2, 2, 0, 2, maxlag = 110)
myf(1,4)        # compute a value
a1 <- myf(1:2,0:9)    # get a matrix of values

## h from parameters
h <- pcarma_h_lazy(testphi, testtheta, 2, 2, 2)
h(3, 2)           # a scalar
h1 <- h(1:2, 1:4) # a matrix

## compute acvf from parameters
( acfsys <- pcarma_acvf_system(testphi, testtheta, si2, 2, 0, 2) )
acfvec <- solve(acfsys$A, acfsys$b)
acf1 <- slMatrix(acfvec, period = 2)

## TODO: examples wirh q != 0    



    ## examples from pc.acf2model.Rd
    
data(ex1f)
pc3 <- slMatrix(period=2,maxlag=5,f=ex1f,type="tt")
# pcarma_param_system(pc3, NULL, NULL, 2, 0, 2)
parsys <- pcarma_param_system(pc3, NULL, NULL, c(2,2), 0, 2)
param <- solve(parsys$A,parsys$b)

# res <- pcarma_acvf2model(pc3, list(p=c(1,2),q=0,period=2))
# res <- pcarma_acvf2model(pc3, list(p=c(1,2),q=0))
# res <- pcarma_acvf2model(pc3, list(p=c(1,2),period=2))
res <- pcarma_acvf2model(pc3, list(p=c(1,2)))

expect_output(print(param))
expect_output(print(res))

    ## examples from pcAr2acf.Rd
    
m <- rbind( c(0.81,0), c(0.4972376, 0.4972376) )
si2 <- PeriodicVector(c(0.3439000,0.1049724))

pcAR2acf(m)
pcAR2acf(m, si2)
pcAR2acf(m, si2, 2)
pcAR2acf(m, si2, 2, maxlag = 10)

# same using pcarma_acvf_lazy directly
m1 <- rbind( c(1, 0.81, 0), c(1, 0.4972376, 0.4972376) )

testphi <- slMatrix(init = m1)
myf <- pcarma_acvf_lazy(testphi, testtheta, si2, 2, 0, 2, maxlag = 10)
myf(1:2, 0:9)    # get a matrix of values

all(myf(1:2, 0:9) == pcAR2acf(m, si2, 2, maxlag = 9)) # TRUE




    ## examples from pcalg1util.Rd
r1 <- rbind(c(1,0.81,0.729),c(1,0.90,0.900))

# example of Lambert-Lacroix
data(ex1f)
pc3 <- slMatrix(period=2,maxlag=5,f=ex1f,type="tt")
res0p2 <- alg1(pc3[],c(0,2))
res1p2 <- alg1(pc3[],c(1,2))
res3p3 <- alg1(pc3[],c(3,3))

## acfsys1.R in package 'pctsArma    '
m1a <- rbind(c(1, 0, 0), c(1, 0.4972376, 0.4972376))
m2 <- rbind(c(1, 0, 0), c(1, 0, 0))

testphia <- slMatrix(init = m1a)
testtheta <- slMatrix(init = m2)

si2a <- PeriodicVector(c(1, 0.1049724)) 

acfsys <- pcarma_acvf_system(testphia, testtheta, si2a, 2, 0, 2)
solve(acfsys$A, acfsys$b)

expect_identical(acfsys, readRDS("acfsys_old_RData.rds"))

    ## h from parameters
h <- pcarma_h_lazy(testphia, testtheta, 2, 2, 2)
h(3, 2)           # a scalar
h1 <- h(1:2, 1:4) # a matrix
})
