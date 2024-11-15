# [Process monitoring using robust regression control charts]()

This code can be used to reproduce the application in the paper.

# Authors

Eufrásio de Andrade Lima Neto, Cristine Rauber, Hozana Francielle do Nascimento Borges, Luiz M. A. Lima-Filho

# How to cite this work

# Data
The data is related to the daily meteorological observations in January in the city of Sydney, Australia, between 2013 and 2022, totalling 246 observations. 
The weatherAUS application database is available in the rattle library of the R Software. The variable of interest is the temperature in degress Celcius at 3pm. Given that this variable changes throughout the year according to the seasons, we chose January, which corresponds to summer.

# Description of the features used in the fitted model

| Feature      | Description                                                             |
|--------------|-------------------------------------------------------------------------|
|RelHumid3pm   | Relative humidity (%) at 3pm                                            |
|Cloud3pm      | Fraction of sky obscured by cloud at 3pm                                |
|Pressure3pm   | Atmospheric pressure reduced to mean sea level at 3pm                   |
|Rainfall      | The amount of rainfall recorded for the day in mm                       |
|Sunshine      | The number of hours of bright sunshine in the day                       |
|Temperature   | Temperature (degrees Celsius) at 3pm                                    |
|WindGustSpeed | The speed (km/h) of the strongest wind gust in the 24 hours to midnight |
|WindSpeed3pm  | Wind speed (km/hr) averaged over 10 minutes prior to 3pm                |
|RISK_MM       | The amount of rain. A kind of measure of the 'risk'                     |

# Files

## Application 

* application.R: R code for reproducing the application 

# Contact 

Cristine Rauber (rauberoliveira at gmail.com)

# License
[MIT](https://github.com/rauberc/code_betareg/blob/main/LICENSE)
