Hypothese 3: Kommunikationsfähigkeit
====================================

Je älter eine Person ist, desto mehr sinkt seine Kommunikationsfähigkeit.
-------------------------------------------------------------------------

Korrelation

\#Korrelationen berechnen

    jmv::corrMatrix(datensatz, c("alter", "KOMF"), kendall = TRUE, flag =  TRUE, plots = TRUE)

    ## 
    ##  CORRELATION MATRIX
    ## 
    ##  Correlation Matrix                              
    ##  ─────────────────────────────────────────────── 
    ##                                alter     KOMF    
    ##  ─────────────────────────────────────────────── 
    ##    alter    Pearson's r             —            
    ##             p-value                 —            
    ##             Kendall's Tau B         —            
    ##             p-value                 —            
    ##                                                  
    ##    KOMF     Pearson's r        -0.208        —   
    ##             p-value            < .001        —   
    ##             Kendall's Tau B    -0.118        —   
    ##             p-value            < .001        —   
    ##  ─────────────────────────────────────────────── 
    ##    Note. * p < .05, ** p < .01, *** p < .001

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

![](HypotheseKommunikationsfähigkeit_files/figure-markdown_strict/corrmatrix-1.png)

\#Visualisierung

    datensatz %>% 

      ggplot() + aes(x = alter, y = KOMF) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "black") + cowplot::theme_half_open() + labs(title = "Mit steigendem Alter sinkt die Kommunikationsfähigkeit ", x = "Alter", y = "Kommunikationsfähigkeit",
      "Alter und Kommunikationsfähigkeit korrelieren negativ/positiv")

    ## Warning: Removed 34 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 34 rows containing missing values (geom_point).

![](HypotheseKommunikationsfähigkeit_files/figure-markdown_strict/unnamed-chunk-1-1.png)

\`\`\`
