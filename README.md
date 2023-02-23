# Agreement with conjoined subjects involving mismatching person features

This is an analyis of experimental acceptability judgment data on variable person agreement in German using R. It includes a model-based analysis of the test-retest reliability of individual differences as well as a power analysis based on previous pilot study data. 

## Experiment

The phenomenon under investigation is a variable agreement pattern in German: when agreeing with a conjoined subject combining a second and a third person feature DP, such as 'you and your brother,' the verb can variably surface with second or third person plural agreement. We investigate how acceptable speakers judge the two possible agreement forms and also whether order of conjuncts influences agreement preferences. We use a 2x3 Latin Square design with the two factors 1) verbal agreement (2Pl, 3Pl and, as an ungrammatical control condition, 1Sg) and 2) order of conjuncts. Participants rated the items on a 5-point Likert scale. The study was hosted on PCIbex.

## Data analysis

Besides a regular regression analysis using a Bayesian mixed-effects ordinal regression model (brms), we investigated the test-retest reliability of individual differences to determine whether there are consistent differences between subjects as to their preferences for second or third person agreement. To this effect, we adopted the model-based analysis in Staub (2021), examining whether the effect of subject in one half of the experiment correlated with the effect of subject in the other half. To determine the number of subjects and items needed for this analysis, we previously conducted a power analysis based on a small pilot study.

## Bibliography

Staub, A. (2021). How reliable are individual differences in eye movements in reading? Journal of Memory and Language, 116, 104190.
