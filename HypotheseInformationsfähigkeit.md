### Es besteht ein Unterschied zwischen Studierenden und Nicht-Studierenden im Hinblick auf die Informationsfähigkeit.

    datensatz %>%
      ANOVA(dep = "IF", factors = c("taetigkeit"), effectSize = "partEta", postHoc = Informationsfähigkeit ~ taetigkeit, emMeans = ~ taetigkeit, emmPlots = TRUE)

    ## 
    ##  ANOVA
    ## 
    ##  ANOVA                                                                            
    ##  ──────────────────────────────────────────────────────────────────────────────── 
    ##                  Sum of Squares    df     Mean Square    F       p        η²p     
    ##  ──────────────────────────────────────────────────────────────────────────────── 
    ##    taetigkeit              17.2      6          2.871    3.40    0.003    0.049   
    ##    Residuals              331.4    392          0.845                             
    ##  ──────────────────────────────────────────────────────────────────────────────── 
    ## 
    ## 
    ##  POST HOC TESTS
    ## 
    ##  Post Hoc Comparisons - taetigkeit                                                                     
    ##  ───────────────────────────────────────────────────────────────────────────────────────────────────── 
    ##    taetigkeit              taetigkeit          Mean Difference    SE        df     t         p-tukey   
    ##  ───────────────────────────────────────────────────────────────────────────────────────────────────── 
    ##    Schüler            -    Student                     -1.1238    0.3548    392    -3.167      0.027   
    ##                       -    Auszubildender              -1.2024    0.4013    392    -2.996      0.046   
    ##                       -    Angestellter                -0.8302    0.3544    392    -2.343      0.226   
    ##                       -    Selbstständiger             -1.2650    0.4065    392    -3.112      0.032   
    ##                       -    Rentner                     -0.8988    0.5115    392    -1.757      0.578   
    ##                       -    Arbeitssuchender            -0.6571    0.5384    392    -1.221      0.886   
    ##    Student            -    Auszubildender              -0.0786    0.2130    392    -0.369      1.000   
    ##                       -    Angestellter                 0.2937    0.0996    392     2.947      0.052   
    ##                       -    Selbstständiger             -0.1412    0.2227    392    -0.634      0.996   
    ##                       -    Rentner                      0.2250    0.3821    392     0.589      0.997   
    ##                       -    Arbeitssuchender             0.4667    0.4174    392     1.118      0.922   
    ##    Auszubildender     -    Angestellter                 0.3722    0.2123    392     1.754      0.580   
    ##                       -    Selbstständiger             -0.0627    0.2911    392    -0.215      1.000   
    ##                       -    Rentner                      0.3036    0.4256    392     0.713      0.992   
    ##                       -    Arbeitssuchender             0.5452    0.4575    392     1.192      0.897   
    ##    Angestellter       -    Selbstständiger             -0.4349    0.2220    392    -1.959      0.443   
    ##                       -    Rentner                     -0.0687    0.3817    392    -0.180      1.000   
    ##                       -    Arbeitssuchender             0.1730    0.4170    392     0.415      1.000   
    ##    Selbstständiger    -    Rentner                      0.3662    0.4306    392     0.851      0.979   
    ##                       -    Arbeitssuchender             0.6079    0.4621    392     1.315      0.845   
    ##    Rentner            -    Arbeitssuchender             0.2417    0.5567    392     0.434      0.999   
    ##  ─────────────────────────────────────────────────────────────────────────────────────────────────────

![](HypotheseInformationsfähigkeit_files/figure-markdown_strict/ANOVA-1.png)

    mapping <- c("Angestellter" = "Nicht-Studierend",
                 "Auszubildender" = "Nicht-Studierend",
                 "Selbstständiger" = "Nicht-Studierend",
                 "Rentner" ="Nicht-Studierend",
                 "Arbeitssuchender" = "Nicht-Studierend",
                 "Schüler" = "Nicht-Studierend",
                 "Student"="Studierend")
    datensatz$Studienstatus <- mapping[as.character(datensatz$taetigkeit)]
    datensatz %>%
     filter(!is.na(Studienstatus)) %>% 
     group_by(Studienstatus) %>%
     summarise(IF_mean = mean(IF, na.rm = TRUE)-1, 
          count = n(),
          IF_se = std.error(IF)) %>%
    mutate(IF_ci = IF_se * 1.96) %>%
     ggplot() +
     aes(x = Studienstatus, fill = Studienstatus,
       y = IF_mean,
       ymin = IF_mean - IF_ci,
       ymax = IF_mean + IF_ci,
       group = 1) +
     geom_col(fill = c(rwthfarben$lightblue, rwthfarben$bordeaux), width = 0.4) +
     geom_line() +
     geom_errorbar(width = 0.2) + 
     scale_y_continuous(limits = c(0,5), breaks = 0:5) + 
     labs(title = "Studierende haben eine gering höhere Informationsfähigkeit", 
        subtitle = "Mittelwertplot mit 95% Konfidenzintervall",
        x = "Studienstatus ",
        y = "Informationsfähigkeit [0 = nie bis 5 = (fast) immer]", fill = "Studienstatus")

![](HypotheseInformationsfähigkeit_files/figure-markdown_strict/unnamed-chunk-1-1.png)
