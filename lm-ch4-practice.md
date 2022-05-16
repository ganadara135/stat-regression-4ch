회귀분석 4장 문제풀이
================

## 2번 (p.136)

토양에 녹아 있는 붕소(boron)의 양(Y)은 토양의 진흙의 비중(X1)과 토양의
pH(X2)에 관련되어 있다고 한다. 다음 자료는 20가지의 토양을 조사하여
붕소의 양, 진흙의 비중, pH 를 기록한 것이다. <br> 1) 2차
다항회귀모형으로 적합시키고, 적합성 검토 <br> 2) 가설 H0 : B12=0, H1 :
B12 /= 0 을 유의수준 5% 에서 검정 <br> =\> 교호작용은 유의확률 0.46
이므로 0.05 보다 크므로 귀무가설 기각 안됨

``` r
Y  = c(0.62, 0.69, 0.63, 0.61, 0.28, 0.33, 0.31, 0.37, 0.66, 0.70, 0.74, 0.63, 0.52, 0.47, 0.45, 0.42, 0.41, 0.42, 0.42, 0.41)
X1 = c(37,37,37,37,29,29,29,29,29,29,29,29,27,27,27,27,27,27,27,27)
X2 = c(5.3, 5.3, 5.5, 5.7, 5.6, 5.7, 5.7, 5.9, 6.0, 6.0, 6.0, 6.0, 5.5, 5.6, 5.6, 5.7, 5.5, 5.5, 5.5, 5.5)
str(X2)
```

    ##  num [1:20] 5.3 5.3 5.5 5.7 5.6 5.7 5.7 5.9 6 6 ...

``` r
dataF = data.frame(Y, X1, X2)
dataF.lm = lm(Y ~ X1+X2+I(X1^2)+I(X2^2)+X1:X2, data=dataF)
summary(dataF.lm)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X1 + X2 + I(X1^2) + I(X2^2) + X1:X2, data = dataF)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.128912 -0.030792  0.002645  0.029003  0.079928 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  82.697074  20.398454   4.054 0.001184 ** 
    ## X1           -0.895735   0.224292  -3.994 0.001333 ** 
    ## X2          -24.635265   6.395311  -3.852 0.001760 ** 
    ## I(X1^2)       0.011673   0.002849   4.098 0.001087 ** 
    ## I(X2^2)       2.134101   0.476012   4.483 0.000515 ***
    ## X1:X2         0.029347   0.038728   0.758 0.461153    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05755 on 14 degrees of freedom
    ## Multiple R-squared:  0.8807, Adjusted R-squared:  0.8381 
    ## F-statistic: 20.67 on 5 and 14 DF,  p-value: 5.191e-06

## 3번 (p.136)

다음 자료는 미국 와이오밍주에 있는 Snake River 에서 매년 4월1일의 눈에
포함된 수분량(X)과 4월부터 7월까지의 강수량(Y, 단위 inch)을
17년(1919\~1935) 동안 측정한 것이다 <br> 1) X 와 Y의 산점도를 그리라
<br> 2) Xi - X(bar) 이용하여 반응변수 Y 에 대한 1차, 2차, 3차
다항회귀모형을 구하라 <br> 3) 2)의 3개의 다항회귀모형들 중에서 관측된
자료에 가장 적절한 다항회귀모형은 무엇인가? <br>

``` r
X = c(23.1, 32.8, 31.8, 32.0, 30.4, 24.0, 39.5, 24.2, 52.5, 37.9, 30.5, 25.1, 12.4, 35.1, 31.5, 21.1, 27.6)
Y = c(10.5, 16.7, 18.2, 17.0, 16.3, 10.5, 23.1, 12.4, 24.9, 22.8, 14.1, 12.9, 8.8, 17.4, 14.9, 10.5, 16.1)
str(Y)
```

    ##  num [1:17] 10.5 16.7 18.2 17 16.3 10.5 23.1 12.4 24.9 22.8 ...

``` r
dataF = data.frame(X,Y)
plot(dataF)
```

![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
dataF.lm = lm(Y~X, data=dataF)
summary(dataF.lm)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X, data = dataF)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1793 -1.5149 -0.3624  1.6276  3.1973 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.72538    1.54882   0.468    0.646    
    ## X            0.49808    0.04952  10.058 4.63e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.743 on 15 degrees of freedom
    ## Multiple R-squared:  0.8709, Adjusted R-squared:  0.8623 
    ## F-statistic: 101.2 on 1 and 15 DF,  p-value: 4.632e-08

``` r
plot(dataF.lm)
```

![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->

``` r
dataF.lm = lm(Y~X+I(X^2), data=dataF)
summary(dataF.lm)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X + I(X^2), data = dataF)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1865 -1.5803 -0.3889  1.5699  3.1576 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -0.0937778  3.7751930  -0.025   0.9805  
    ## X            0.5528327  0.2342043   2.360   0.0333 *
    ## I(X^2)      -0.0008467  0.0035344  -0.240   0.8141  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.8 on 14 degrees of freedom
    ## Multiple R-squared:  0.8714, Adjusted R-squared:  0.853 
    ## F-statistic: 47.43 on 2 and 14 DF,  p-value: 5.818e-07

``` r
dataF.lm = lm(Y~X+I(X^3), data=dataF)
summary(dataF.lm)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X + I(X^3), data = dataF)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1694 -1.3339 -0.3754  1.4871  3.0753 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -5.786e-01  2.931e+00  -0.197 0.846357    
    ## X            5.629e-01  1.326e-01   4.245 0.000816 ***
    ## I(X^3)      -1.894e-05  3.578e-05  -0.529 0.604970    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.786 on 14 degrees of freedom
    ## Multiple R-squared:  0.8734, Adjusted R-squared:  0.8553 
    ## F-statistic: 48.29 on 2 and 14 DF,  p-value: 5.212e-07

``` r
plot(dataF.lm)
```

![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-6.png)<!-- -->![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-7.png)<!-- -->![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-8.png)<!-- -->![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-9.png)<!-- -->

``` r
dataF.lm = lm(Y~X+I(X^2)+I(X^3), data=dataF)
summary(dataF.lm)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X + I(X^2) + I(X^3), data = dataF)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.88800 -0.90941 -0.01824  0.73015  2.25675 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) 21.1848945  6.8002641   3.115  0.00820 **
    ## X           -1.8588156  0.7223336  -2.573  0.02315 * 
    ## I(X^2)       0.0808675  0.0238868   3.385  0.00488 **
    ## I(X^3)      -0.0008390  0.0002437  -3.442  0.00437 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.351 on 13 degrees of freedom
    ## Multiple R-squared:  0.9327, Adjusted R-squared:  0.9172 
    ## F-statistic: 60.07 on 3 and 13 DF,  p-value: 7.097e-08

``` r
plot(dataF.lm)
```

![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-10.png)<!-- -->![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-11.png)<!-- -->![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-12.png)<!-- -->

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-2-13.png)<!-- -->

## 4번 (p.137)

다음은 미국 경제 자료로서 1940년부터 1950년까지의 가처분소득(Y)과
가계소비지출액(X)을 적어 놓은 것이다. 제2차 세계대전기간(1941\~45)
동안의 기간을 I, 그리고 그 외의 기간을 0으로 하여 가처분소득과
가계소비지출액의 관계를 알아보라

``` r
Y = c(244.0, 277.9, 317.5, 332.1, 343.6, 338.1, 332.7, 318.8, 335.8, 336.8, 362.8)
X = c(299.9, 243.6, 241.1, 248.2, 255.2, 270.9, 301.0, 305.8, 312.2, 319.3, 337.3)
I = c(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
I_d = factor(I, levels=c(0,1), label=c("Line0","Line1"))
str(Y)
```

    ##  num [1:11] 244 278 318 332 344 ...

``` r
dataF = data.frame(Y,X,I_d)

dataF[c(1,2,3),]
```

    ##       Y     X   I_d
    ## 1 244.0 299.9 Line0
    ## 2 277.9 243.6 Line1
    ## 3 317.5 241.1 Line1

``` r
plot(X,Y)
points(X[I_d=="Line1"], Y[I_d=="Line1"], pch=17, col="BLUE")
points(X[I_d=="Line0"], Y[I_d=="Line0"], pch=19, col="RED")
legend("bottomright", legend=levels(I_d), pch=c(19,17), col=c("RED","BLUE"))
```

![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
dataF.lm = lm(Y ~ X+I_d, data=dataF)
summary(dataF.lm)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X + I_d, data = dataF)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -55.625  -9.711   8.852  15.233  31.150 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -225.0999   221.3456  -1.017   0.3389  
    ## X              1.7497     0.7072   2.474   0.0385 *
    ## I_dLine1     106.3739    46.2084   2.302   0.0503 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 28.01 on 8 degrees of freedom
    ## Multiple R-squared:  0.4335, Adjusted R-squared:  0.2919 
    ## F-statistic: 3.061 on 2 and 8 DF,  p-value: 0.103

``` r
dataF.lm = lm(Y ~ X+I_d+X:I_d, data=dataF)
summary(dataF.lm)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X + I_d + X:I_d, data = dataF)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -52.511  -8.650   9.795  14.882  33.994 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -301.8345   292.6733  -1.031   0.3367  
    ## X              1.9952     0.9355   2.133   0.0704 .
    ## I_dLine1     291.0512   427.4711   0.681   0.5178  
    ## X:I_dLine1    -0.6742     1.5503  -0.435   0.6768  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 29.54 on 7 degrees of freedom
    ## Multiple R-squared:  0.4484, Adjusted R-squared:  0.212 
    ## F-statistic: 1.897 on 3 and 7 DF,  p-value: 0.2185

## 5번 (P.137)

한 대학교에서 성별에 따라 교수들의 월급에 차이가 있는가를 보기 위하여
교수들의 월급액(SL), 직위(RK), 근속연수(YR), 최종학위(DG)들을 조사한
결과가 다음과 같다. 단, 직위변수에서는 1=조교수, 2=부교수, 3=정교수로,
최종학위변수에서는 0=석사, 1=박사로, 성별(SEX)변수에서는 1=여자,
0=남자로 기록하였다. <br>

``` r
SEX=c(0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0, 0,1,0,0,1,0,0,1,1,0,1,0,0,0,0,0,0,1,0,0,1,1,0,1,1,1)
RK =c(3,3,3,3,3,3,3,3,3,3,3,2,3,2,3,3,3,2,2,3,1,2,3,3,2,3, 2,3,2,2,1,2,1,2,2,2,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1)
YR =c(25,13,10,7,19,16,0,16,13,13,12,15,9,9,9,7,13,11,10,6,16,8,7,8,9,5, 11,5,3,3,10,11,9,4,6,1,8,4,4,4,3,3,0,3,2,2,2,2,1,1,1,0)
DG =c(1,1,1,1,0,1,0,1,0,0,1,1,1,0,1,1,1,0,0,0,0,0,1,1,1,1, 1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1)
SL =c(36350,35350,28200,26775,33696,28516,24900,31909,31850,32850,27025,24750,28200,23712,25748,29342,31114,24742,22906,24450,19175,20525,27959,38045,24832,25400,24800,25500,26182,23725,21600,23300,23713,20690,22450,20850,18304,17095,16700,17600,18075,18000,20999,17250,16500,16094,16150,15350,16244,16686,15000,20300)
dataF = data.frame(SEX,RK,YR,DG,SL)
dataF$SEX_D = factor(dataF$SEX, levels = c(0,1), label=c("male","female"))
dataF[c(1,2,3),]
```

    ##   SEX RK YR DG    SL SEX_D
    ## 1   0  3 25  1 36350  male
    ## 2   0  3 13  1 35350  male
    ## 3   0  3 10  1 28200  male

1)성별을 표시하는 기호를 이용하여 월급액과 근속연수에 대한 산점도를
그려라

``` r
plot(dataF$YR,dataF$SL)
points(dataF$YR[dataF$SEX_D=="male"], dataF$SL[dataF$SEX_D=="male"], pch=17, col="BLUE")
points(dataF$YR[dataF$SEX_D=="female"], dataF$SL[dataF$SEX_D=="female"], pch=19, col="RED")
```

![](lm-ch4-practice_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> 2)
월급액에 대해서 근속연수와 성별을 설명변수로 하는 회귀모형을 구하라

``` r
dataf.lm = lm(SL~YR+SEX_D,data=dataF)
summary(dataF.lm)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X + I_d + X:I_d, data = dataF)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -52.511  -8.650   9.795  14.882  33.994 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -301.8345   292.6733  -1.031   0.3367  
    ## X              1.9952     0.9355   2.133   0.0704 .
    ## I_dLine1     291.0512   427.4711   0.681   0.5178  
    ## X:I_dLine1    -0.6742     1.5503  -0.435   0.6768  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 29.54 on 7 degrees of freedom
    ## Multiple R-squared:  0.4484, Adjusted R-squared:  0.212 
    ## F-statistic: 1.897 on 3 and 7 DF,  p-value: 0.2185

3)  2)의 결과에 의하면 성별에 따라 월급액과 근속연수의 관계가 다르다고
    할 수 있는가를 설명하라 <br> =\> p-value 가 5% 이하이므로 다르다고
    할 수 없다. <br>
4)  월급액에 대한 근속연수의 회귀모형에 성별, 직위, 최종학력을 포함시킨
    경우와 포함시키지 않는 경우를 비교하는 검정을 실시하라 <br>

``` r
dataf.lm2 = lm(SL~YR+SEX_D+RK,data=dataF)
summary(dataf.lm2)
```

    ## 
    ## Call:
    ## lm(formula = SL ~ YR + SEX_D + RK, data = dataF)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3166.5 -1570.1  -168.0   984.4  9037.0 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 11011.76     966.95  11.388 3.03e-15 ***
    ## YR            393.86      74.53   5.285 3.04e-06 ***
    ## SEX_Dfemale   603.77     811.20   0.744     0.46    
    ## RK           4747.18     452.58  10.489 5.18e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2398 on 48 degrees of freedom
    ## Multiple R-squared:  0.8454, Adjusted R-squared:  0.8358 
    ## F-statistic: 87.51 on 3 and 48 DF,  p-value: < 2.2e-16

``` r
dataf.lm3 = lm(SL~YR+SEX_D+RK+DG,data=dataF)
summary(dataf.lm3)
```

    ## 
    ## Call:
    ## lm(formula = SL ~ YR + SEX_D + RK + DG, data = dataF)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3261.8 -1618.4  -184.2   910.2  9075.6 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 11101.27    1087.12  10.212 1.62e-13 ***
    ## YR            391.84      76.05   5.152 5.02e-06 ***
    ## SEX_Dfemale   608.10     819.80   0.742    0.462    
    ## RK           4753.17     458.31  10.371 9.75e-14 ***
    ## DG           -134.22     715.45  -0.188    0.852    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2422 on 47 degrees of freedom
    ## Multiple R-squared:  0.8455, Adjusted R-squared:  0.8324 
    ## F-statistic: 64.33 on 4 and 47 DF,  p-value: < 2.2e-16

5)  월급액을 반응변수로 하는 최적모형을 유도하고 성별에 의한 월급액의
    차이가 있는지 여부를 판단하라

``` r
dataf.lm4 = lm(SL~YR+SEX+YR:SEX,data=dataF)
summary(dataf.lm4)
```

    ## 
    ## Call:
    ## lm(formula = SL ~ YR + SEX + YR:SEX, data = dataF)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10904.0  -3150.2   -632.2   2896.8  13112.6 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  18222.6     1308.6  13.925  < 2e-16 ***
    ## YR             741.0      126.2   5.870 3.95e-07 ***
    ## SEX           -570.8     2297.2  -0.248    0.805    
    ## YR:SEX         169.1      387.0   0.437    0.664    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4342 on 48 degrees of freedom
    ## Multiple R-squared:  0.4932, Adjusted R-squared:  0.4615 
    ## F-statistic: 15.57 on 3 and 48 DF,  p-value: 3.323e-07

``` r
dataf.lm5 = lm(SL~SEX,data=dataF)
summary(dataf.lm5)
```

    ## 
    ## Call:
    ## lm(formula = SL ~ SEX, data = dataF)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8602.8 -4296.6  -100.8  3513.1 16687.9 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    24697        938  26.330   <2e-16 ***
    ## SEX            -3340       1808  -1.847   0.0706 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5782 on 50 degrees of freedom
    ## Multiple R-squared:  0.0639, Adjusted R-squared:  0.04518 
    ## F-statistic: 3.413 on 1 and 50 DF,  p-value: 0.0706
