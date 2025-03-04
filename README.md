# macro-labour
Projects and exercises in labour economics.

1. multi-way fixed effects regression

Wage equations ran in Stata 14 that provide two ways of estimating three-way fixed effects models. The first method is based on the Gauss-Seidel algorithm and was introduced by Guimaraes and Portugal (2010), "A simple feasible procedure to fit models with high-dimensional fixed effects". The second method uses the command stata command reghdfe (https://scorreia.com/software/reghdfe/) which is an implementation of the model by Abowd, Kramarz and Margolis (1999). In a comment, I added the requirements for inference using the AKM framework.

2. WangBellemare2019

I wrote an R script that uses the specification of Wang and Bellemare (2019) to estimate the OLS and IV bias of specifications with lagged instruments.

