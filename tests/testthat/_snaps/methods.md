# print() works

    Code
      print(m)
    Output
      
      Call:
      lmls(location = y ~ x1 + x3, scale = ~x2 + x3, light = FALSE)
      
      Location coefficients (identity link):
      (Intercept)           x1           x3  
         0.002884     0.963462     1.009119  
      
      Scale coefficients (log link):
      (Intercept)           x2           x3  
          -2.9832       0.9708       1.1063  
      

# summary() works

    Code
      summary(m)
    Output
      
      Call:
      lmls(location = y ~ x1 + x3, scale = ~x2 + x3, light = FALSE)
      
      Deviance residuals:
          Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
      -2.33600 -0.65790 -0.04923  0.02135  0.67570  2.94200 
      
      Location coefficients (identity link):
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept) 0.002884   0.026342   0.109    0.913    
      x1          0.963462   0.039923  24.133   <2e-16 ***
      x3          1.009119   0.049615  20.339   <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Scale coefficients (log link):
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)  -2.9832     0.1862 -16.020  < 2e-16 ***
      x2            0.9708     0.2516   3.859 0.000114 ***
      x3            1.1063     0.2482   4.458 8.28e-06 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Residual degrees of freedom: 94
      Log-likelihood: 53.2326
      AIC: -94.4652
      BIC: -78.8342
      

---

    Code
      summary(boot(m), type = "boot")
    Output
      
      Call:
      lmls(location = y ~ x1 + x3, scale = ~x2 + x3, light = FALSE)
      
      Deviance residuals:
          Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
      -2.33600 -0.65790 -0.04923  0.02135  0.67570  2.94200 
      
      Location coefficients (identity link):
                       Mean      2.5%       50% 97.5%
      (Intercept)  0.002901 -0.048806  0.002366 0.052
      x1           0.963803  0.883803  0.965398 1.036
      x3           1.009082  0.906777  1.011978 1.107
      
      Scale coefficients (log link):
                     Mean    2.5%     50%  97.5%
      (Intercept) -3.0303 -3.4694 -3.0158 -2.655
      x2           0.9867  0.4251  0.9911  1.535
      x3           1.1262  0.6139  1.1332  1.620
      
      Residual degrees of freedom: 94
      Log-likelihood: 53.2326
      AIC: -94.4652
      BIC: -78.8342
      

---

    Code
      summary(mcmc(m), type = "mcmc")
    Output
      
      Call:
      lmls(location = y ~ x1 + x3, scale = ~x2 + x3, light = FALSE)
      
      Deviance residuals:
         Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      -1.8680 -0.4159  0.1151  0.2003  0.8222  3.0270 
      
      Location coefficients (identity link):
                       Mean      2.5%       50% 97.5%
      (Intercept)  0.002348 -0.055024  0.003348 0.055
      x1           0.964768  0.884526  0.964412 1.047
      x3           1.010050  0.911696  1.007240 1.112
      
      Scale coefficients (log link):
                     Mean    2.5%     50%  97.5%
      (Intercept) -2.9333 -3.3038 -2.9417 -2.489
      x2           0.9458  0.4510  0.9435  1.455
      x3           1.0850  0.5086  1.0876  1.672
      
      Residual degrees of freedom: 94
      Log-likelihood: 50.4705
      AIC: -88.9411
      BIC: -73.31
      

