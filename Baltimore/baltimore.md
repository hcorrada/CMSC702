A quick analysis of Baltimore crime
========================================================

I'm going to do a very simple analysis of Baltimore crime to show off R. We'll use data downloaded from Baltimore City's awesome open data site. 

### Getting data

* Arrest data: https://data.baltimorecity.gov/Crime/BPD-Arrests/3i3v-ibrt 
* CCTV data: https://data.baltimorecity.gov/Crime/CCTV-Locations/hdyb-27ak 

Let's load the data:

```r
arrest_tab = read.csv("BPD_Arrests.csv", stringsAsFactors = FALSE)
cctv_tab = read.csv("CCTV_Locations.csv", stringsAsFactors = FALSE)

# these columns are mislabeled, so fix them
tmp = arrest_tab$sex
arrest_tab$sex = arrest_tab$race
arrest_tab$race = tmp
```


### Exploring data


```r
# dimension of table (data.frame)
dim(arrest_tab)
```

```
## [1] 104528     15
```

```r

# what are the columns
names(arrest_tab)
```

```
##  [1] "arrest"            "age"               "sex"              
##  [4] "race"              "arrestDate"        "arrestTime"       
##  [7] "arrestLocation"    "incidentOffense"   "incidentLocation" 
## [10] "charge"            "chargeDescription" "district"         
## [13] "post"              "neighborhood"      "Location.1"
```

```r

# what is the average arrest age?
mean(arrest_tab$age)
```

```
## [1] 33.2
```

```r

# the range of arrest ages
range(arrest_tab$age)
```

```
## [1]  0 87
```

```r

# how many arrests per sex
table(arrest_tab$sex)
```

```
## 
##           F     M 
##     2 19431 85095
```

```r

# what are the most common offenses
head(sort(table(arrest_tab$incidentOffense), decreasing = TRUE))
```

```
## 
##         Unknown Offense            87-Narcotics       4E-Common Assault 
##                   38649                   24744                    6739 
## 87O-Narcotics (Outside)     97-Search & Seizure                79-Other 
##                    6515                    3670                    3461
```

```r

# range of arrests after removing those w/ age==0
range(arrest_tab$age[arrest_tab$age > 0])
```

```
## [1]  8 87
```


Let's see a table of arrests by sex and race

```r
table(sex = arrest_tab$sex, race = arrest_tab$race)
```

```
##    race
## sex           A     B     H     I     U     W
##         2     0     0     0     0     0     0
##   F     0    37 14663     0    34   183  4514
##   M     0   205 72605     1   184  1566 10534
```


A histogram of age


```r
hist(arrest_tab$age, nc = 100)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r
with(arrest_tab, hist(age[sex == "M"], nc = 100))  # males only
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 

```r
with(arrest_tab, hist(age[sex == "F"], nc = 100))  # females only
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-43.png) 


### Are males and females arrested at different ages on average?

Let's take a look at how age depends on sex. Let's plot age as a function of sex first (notice how we indicate that sex is a `factor`). 


```r
plot(arrest_tab$age ~ factor(arrest_tab$sex))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


One of the neat things about R is that statistical model building and testing is built-in. The model we use is $y_i=\beta_0+\beta_1 x_i$ where $y_i$ is age of sample (example) $i$ and $x_i$ is an indicator variable $x_i \in \{0,1\}$ with $x_i=1$ if the $i$-th record (example) is male. You can check that $\beta_1$ is the difference in mean age between females and males.
We use the formula syntax to build a linear regression model. 


```r
# let's ignore those records with missing sex
fit = lm(age ~ factor(sex), data = arrest_tab, subset = arrest_tab$sex %in% 
    c("M", "F"))
summary(fit)
```

```
## 
## Call:
## lm(formula = age ~ factor(sex), data = arrest_tab, subset = arrest_tab$sex %in% 
##     c("M", "F"))
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -33.39 -10.15  -3.15   9.61  53.85 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   33.3878     0.0847   394.1   <2e-16 ***
## factor(sex)M  -0.2343     0.0939    -2.5    0.013 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.8 on 104524 degrees of freedom
## Multiple R-squared:  5.96e-05,	Adjusted R-squared:  5e-05 
## F-statistic: 6.23 on 1 and 104524 DF,  p-value: 0.0126
```


We see that $\beta_1 \approx -0.2$ meaning that the arrest age for males is about 2.5 months younger. So there is very little difference in the average age (which is what the linear model is testing) but we see that the probability of observing this difference from a sample of this size **when there is no difference in average age** is small $p \approx 0.01$. Since we have a very large number of examples, or records, this testing framework will declare very small differences as *statistically significant*. We'll return to this theme later in class.


### Geographic distribution of arrests.

First we need to extract latitude and longitude from location, we'll use some string functions to do this


```r
tmp = gsub("\\)", "", gsub("\\(", "", arrest_tab$Location))
tmp = strsplit(tmp, split = ",")
arrest_tab$lon = as.numeric(sapply(tmp, function(x) x[2]))
arrest_tab$lat = as.numeric(sapply(tmp, function(x) x[1]))
```


Now let's plot


```r
plot(arrest_tab$lon, arrest_tab$lat, xlab = "Longitude", ylab = "Latitude", 
    main = "Arrests in Baltimore")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


We can also use density estimates to make this nicer:


```r
smoothScatter(arrest_tab$lat, arrest_tab$lon, xlab = "Latitude", ylab = "Longitude", 
    main = "Arrests in Baltimore")
```

```
## KernSmooth 2.23 loaded
## Copyright M. P. Wand 1997-2009
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


Let's make this fancier using the `ggplot2` graphics systems and the `maps` package containing map data.


```r
library(maps)
library(ggplot2)

balto_map = subset(map_data("county", region = "maryland"), subregion == "baltimore city")
plt = ggplot()
plt = plt + geom_polygon(data = balto_map, aes(x = long, y = lat), color = "white", 
    fill = "gray40")
plt = plt + geom_point(data = arrest_tab, aes(x = lon, y = lat), color = "blue", 
    alpha = 0.1)
print(plt)
```

```
## Warning: Removed 40636 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


Now let's add CCTV cameras.


```r
tmp = gsub("\\)", "", gsub("\\(", "", cctv_tab$Location))
tmp = strsplit(tmp, split = ",")
cctv_tab$lon = as.numeric(sapply(tmp, function(x) x[2]))
cctv_tab$lat = as.numeric(sapply(tmp, function(x) x[1]))

plt = ggplot()
plt = plt + geom_polygon(data = balto_map, aes(x = long, y = lat), color = "white", 
    fill = "gray40")
plt = plt + geom_point(data = arrest_tab, aes(x = lon, y = lat), color = "blue", 
    alpha = 0.1)
plt = plt + geom_point(data = cctv_tab, aes(x = lon, y = lat), color = "red")
print(plt)
```

```
## Warning: Removed 40636 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


### A challenge

Is there any relationship between the number of CCTV cameras and the number of arrests? Divide the city into a grid and plot the number of CCTV cameras vs. the number of arrests.


```r
latRange = range(arrest_tab$lat, na.rm = TRUE)
lonRange = range(arrest_tab$lon, na.rm = TRUE)
latGrid = seq(min(latRange), max(latRange), len = 50)
lonGrid = seq(min(lonRange), max(lonRange), len = 50)
latFac = as.numeric(cut(arrest_tab$lat, breaks = latGrid))
lonFac = as.numeric(cut(arrest_tab$lon, breaks = lonGrid))

gridFac = (latFac - 1) * length(latGrid) + (lonFac - 1)

latFac = as.numeric(cut(cctv_tab$lat, breaks = latGrid))
lonFac = as.numeric(cut(cctv_tab$lon, breaks = lonGrid))
cctvGridFac = (latFac - 1) * length(latGrid) + (lonFac - 1)

arrestTab = table(gridFac)
cctvTab = table(cctvGridFac)
m = match(names(cctvTab), names(arrestTab))
plot(arrestTab[m] ~ factor(cctvTab))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


### Extra analyses

As part of HW1 you will add to this analysis. Please use the following template:

#### Your name(s) here
Eric Krokos

What question are you asking?:
What does the distribution of arrests look like for Asians(Blue), Blacks(Red), Whites(Green), and Unknown (Orange)?

What is the code you use to answer it?:


```r
asian = arrest_tab[arrest_tab$race == "A", ]
black = arrest_tab[arrest_tab$race == "B", ]
white = arrest_tab[arrest_tab$race == "W", ]
unknown = arrest_tab[arrest_tab$race == "U", ]
library(maps)
library(ggplot2)
balto_map = subset(map_data("county", region = "maryland"), subregion == "baltimore city")
plt = ggplot()
plt = plt + geom_polygon(data = balto_map, aes(x = long, y = lat), color = "white", 
    fill = "gray40")
plt = plt + geom_point(data = asian, aes(x = lon, y = lat), color = "blue", 
    alpha = 0.1)
plt = plt + geom_point(data = black, aes(x = lon, y = lat), color = "red", alpha = 0.1)
plt = plt + geom_point(data = white, aes(x = lon, y = lat), color = "green", 
    alpha = 0.1)
plt = plt + geom_point(data = unknown, aes(x = lon, y = lat), color = "orange", 
    alpha = 0.1)
print(plt)
```

```
## Warning: Removed 93 rows containing missing values (geom_point).
## Warning: Removed 33454 rows containing missing values (geom_point).
## Warning: Removed 6251 rows containing missing values (geom_point).
## Warning: Removed 749 rows containing missing values (geom_point).
```

![plot of chunk EKrokos](figure/EKrokos.png) 


What did you observe?
=======
The red (Black) dominate the map, but it's interesting that most crimes commited by Whites are done on the southern most part of the city, with Black crime occuring primarily in the upper part. It's difficult to see Asian commited crime. Given that most of the orange dots are in the southern part of the city, I would guess that most of the unknown races are from White people.

#### Andy Garron, Mohit Iyyer, Peter Enns, Fan Du

What question are you asking?: In which months do arrests occur the most? On what day of the week do arrests occur the most?

What is the code you use to answer it?:


```r
arrestDates = as.Date(arrest_tab$arrestDate, "%m/%d/%Y")
dwka <- format(arrestDates, "%a")
# dwka
dwkn <- as.numeric(format(arrestDates, "%w"))  # numeric version
hist(dwkn, main = "Arrests by Day of Week", xlab = "Day of Week", ylab = "Arrest Frequency", 
    breaks = -0.5 + 0:7, labels = unique(dwka[order(dwkn)]))
```

![plot of chunk ClouchPotatoes](figure/ClouchPotatoes1.png) 

```r

dwkm <- format(arrestDates, "%b")  # get month labels
dwkmn <- as.numeric(format(arrestDates, "%m"))  # get numeric data for histogram
hist(dwkmn, main = "Arrests by Month", xlab = "Month of Year", ylab = "Arrest Frequency", 
    breaks = 0:12, labels = unique(dwkm[order(dwkmn)]))
```

![plot of chunk ClouchPotatoes](figure/ClouchPotatoes2.png) 


At the month level, we observe that there is a decline in arrest rates during the winter months and that arrests peak in March and August.

At the day level, we observe that most arrests occur in the middle of the week and that significantly fewer arrests occur on the weekends.

#### Patricia Sazama

Is there a relationship between the age of the arrested individual and the time they are arrested?


```r
# change the time format from hh:mm to hh.mm so it can be converted to a
# numeric value
tmp = gsub(":", ".", arrest_tab$arrestTime)

# convert the arrestTime to numeric
arrestTime = as.numeric(tmp)

# group ages into 5 buckets
ages = arrest_tab$age
ages = cut(ages, 5)

# plot the age buckets as a factor of the numeric arrest times
plot(arrestTime ~ factor(ages))
```

![plot of chunk psazama](figure/psazama.png) 

From these plots we can see slight differences in the average times of day members of these age groups are arrested.  We can observe that as age increases, generally the time of arrest is earlier in the day though this is only a very slight difference.  For all groups the average time of arrest is very close to 15:00.


#### Steven Burgart

What question are you asking?: Is there a relationship between the time a person is arrested and their sex?

What is the code you use to answer it?:


```r

# Make a copy of arrest_tab since we need to modify
arrest_tab_skb <- arrest_tab

# Filter out arrests with unknown sex
arrest_tab_skb <- arrest_tab_skb[arrest_tab_skb$sex == "M" | arrest_tab_skb$sex == 
    "F", ]

# Convert time to just hours as numeric
arrest_tab_skb["arrestTime"] <- lapply(arrest_tab_skb["arrestTime"], function(x) as.numeric(strftime(strptime(x, 
    format = "%H:%M"), "%H")))

# Plot male arrests by hour
hist(arrest_tab_skb[arrest_tab_skb$sex == "M", ]$arrestTime, breaks = 24, main = "Hour of Arrest Frequency for Males", 
    xlab = "Hour of the Day", col = "lightblue", xlim = range(0:23), xaxt = "n")
axis(side = 1, at = seq(0, 23), labels = seq(0, 23))
```

![plot of chunk sburgart](figure/sburgart1.png) 

```r

# Plot female arrests by hour
hist(arrest_tab_skb[arrest_tab_skb$sex == "F", ]$arrestTime, breaks = 24, main = "Hour of Arrest Frequency for Females", 
    xlab = "Hour of the Day", col = "pink", xlim = range(0:23), xaxt = "n")
axis(side = 1, at = seq(0, 23), labels = seq(0, 23))
```

![plot of chunk sburgart](figure/sburgart2.png) 

```r

# Plot relationship between hour of arrest and sex
plot(arrest_tab_skb$arrestTime ~ factor(arrest_tab_skb$sex), main = "Relationship Between Hour of Arrest and Sex", 
    xlab = "Sex", ylab = "Hour of Arrest")
```

![plot of chunk sburgart](figure/sburgart3.png) 



What did you observe?: From the two histograms it isn't readily apparent that there is any relationship between the time a person is arrested and their sex. However, from the third plot we can see that males tend to be arrested slightly later in the day when compared to females.


#### Rob Argue

What question are you asking?:

Do violent crimes occur more in certain areas?


What is the code you use to answer it?:


```r

# The FBI classifies the following as violent crime: - murder and
# nonnegligent manslaughter - forcible rape - robbery - aggravated assault
# as per
# http://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2011/crime-in-the-u.s.-2011/violent-crime/violent-crime

violent = c("1A-Murder", "2A-Rape (Force)", "2B-Rape (Attempt)", "3AF-Robb Hwy-Firearm", 
    "3AJF-Robb Carjack-Firearm", "3AJK-Robb Carjack-Knife", "3AK-Robb Hwy-Knife", 
    "3AO-Robb Hwy-Other Wpn", "3CF-Robb Comm-Firearm", "3CK-Robb Comm-Knife", 
    "3CO-Robb Comm-Other Wpn", "3EF-Robb Gas Station-Firearm", "3EK-Robb Gas Station-Knife", 
    "3GF-Robb Conv Store-Firearm", "3GK-Robb Conv Store-Knife", "3GO-Robb Conv Store-Other Wpn", 
    "3JF-Robb Residence-Firearm", "3JK-Robb Residence-Knife", "3JO-Robb Residence-Other Wpn", 
    "3LF-Robb Bank-Firearm", "3FO-RObb Bank-Other Wpn", "3NF-Robb Misc-Firearm", 
    "3NK-Robb Misc-Knife", "3NO-Robb Misc-Other Wpn", "4A-Agg. Asslt.- Gun", 
    "4B-Agg. Asslt.- Cut", "4C-Agg. Asslt.- Oth.", "4D-Agg. Asslt.- Hand")

plt = ggplot()
plt = plt + geom_polygon(data = balto_map, aes(x = long, y = lat), color = "white", 
    fill = "gray40")
plt = plt + geom_point(data = arrest_tab, aes(x = lon, y = lat), color = "blue", 
    alpha = 0.1)
plt = plt + geom_point(data = arrest_tab[arrest_tab$incidentOffense %in% violent, 
    ], aes(x = lon, y = lat), color = "red", alpha = 0.2)
print(plt)
```

```
## Warning: Removed 40636 rows containing missing values (geom_point).
## Warning: Removed 997 rows containing missing values (geom_point).
```

![plot of chunk Rob Argue](figure/Rob_Argue.png) 


What did you observe?

It appears that there is a heavier concentration of violent crimes in the inner city, particularly in a few hotspots (of which I am unaware the significance of). There also appear to be a couple of specific outlying locations where an unusually large number of violent crimes occur.
#### Hao Zhou(zhhoper), Fang Cheng(Javran)

* What question are you asking?

    Is there any difference between minors and adults
    in terms of time and location?
    (we suppose people under 21 are minors)

* What is the code you use to answer it?
    
    We split data into two parts
    (i.e.  minors (age < 21) and adults (age >= 21)),
    plot the distribution of arrest time 
    and draw scatters according to the location
    for both minors and adults.


```r

# copy data frame, filter out invalid data (age = 0)
arrestTmp <- subset(arrest_tab, arrest_tab$age != 0)

# extract geo info
tmp <- gsub("\\).*", "", gsub(".*\\(", "", arrestTmp$Location))
tmp = strsplit(tmp, split = ",")
arrestTmp$lat <- as.numeric(sapply(tmp, function(x) x[2]))
arrestTmp$lon <- as.numeric(sapply(tmp, function(x) x[1]))

# suppress warnings
arrestTmp <- subset(arrestTmp, !is.na(lon) & !is.na(lat))

# tag data with 'minors' and 'adults' according to their ages
arrestTmp$biAge <- sapply(arrestTmp$age, function(x) {
    if (x < 21) {
        return("minors")
    } else {
        return("adults")
    }
})

# new col: time -> time in Hour
arrestTmp$arrestTimeH <- as.integer(gsub(":[0-9][0-9]", "", arrestTmp$arrestTime))

# plot minors' distribution
minors_tab <- subset(arrestTmp, arrestTmp$biAge == "minors")
minorsArray <- minors_tab$arrestTimeH
minorsTable <- as.integer(table(factor(minorsArray)))
barplot(minorsTable, space = 0, width = 1, xlab = "Time", ylab = "Number of arrset", 
    main = "Number of arrest in each hour (minors)", axes = T)
axis(side = 1, at = seq(0, 23), labels = seq(0, 23))
```

![plot of chunk zhhoper_and_Javran](figure/zhhoper_and_Javran1.png) 

```r

# plot adults' distribution
adults_tab <- subset(arrestTmp, arrestTmp$biAge == "adults")
adultsArray <- adults_tab$arrestTimeH
adultsTable <- as.integer(table(factor((adultsArray))))
barplot(adultsTable, space = 0, width = 1, xlab = "Time", ylab = "Number of arrset", 
    main = "Number of arrest in each hour (adults)", axes = T)
axis(side = 1, at = seq(0, 23), labels = seq(0, 23))
```

![plot of chunk zhhoper_and_Javran](figure/zhhoper_and_Javran2.png) 

```r

plot(arrestTmp$arrestTimeH ~ factor(arrestTmp$biAge), main = "Relationship Between time of Arrest and minor or not", 
    xlab = "age", ylab = "Hour of Arrest")
```

![plot of chunk zhhoper_and_Javran](figure/zhhoper_and_Javran3.png) 

```r

# visualize arrest location for minors & adults
smoothScatter(minors_tab$lat, minors_tab$lon, xlab = "Latitude", ylab = "Longitude", 
    main = "Arrests in Baltimore (minors)")
```

![plot of chunk zhhoper_and_Javran](figure/zhhoper_and_Javran4.png) 

```r
smoothScatter(adults_tab$lat, adults_tab$lon, xlab = "Latitude", ylab = "Longitude", 
    main = "Arrests in Baltimore (adults)")
```

![plot of chunk zhhoper_and_Javran](figure/zhhoper_and_Javran5.png) 

```r

# cleanup
rm(arrestTmp, tmp)
rm(adults_tab, adultsTable, adultsArray)
rm(minors_tab, minorsTable, minorsArray)

```


* What did you observe?

    * Through the first two figures, we observe that
    minors tend to commit less during 1 - 6 a.m., but more during 2 - 8 p.m.

    * There is little difference between the time distribution of minors and adults.

    * From the last two scatters we can tell that the density is slightly different in the northwest part.
    More interesting conclusions could be drawn if we can compare this result with the local map.

#### Hui Miao

What question are you asking?: What is the most common incident offense, and how does it related to sex, race and district?

What is the code you use to answer it?:


```r
old_par <- par(no.readonly = TRUE)

# first let's look at the bar plot of the distribution of incident offense
# types as there are many # of inccident offsense types, I use a filter to
# get rid of 'unknown offense' that we don't care about, and the ones with
# less than 200 cases in order to make the plot clear to understand
library(sqldf)
```

```
## Loading required package: DBI
## Loading required package: gsubfn
## Loading required package: proto
## Loading required namespace: tcltk
## Loading required package: chron
## Loading required package: RSQLite
## Loading required package: RSQLite.extfuns
```

```r
a = sqldf("select incidentOffense, count(*) as cnt from arrest_tab where incidentOffense != 'Unknown Offense' group by incidentOffense having cnt >= 200 order by cnt")
```

```
## Loading required package: tcltk
```

```r
par(las = 2, mar = c(5, 7, 4, 2))
barplot(a$cnt, horiz = TRUE, cex.names = 0.7, names.arg = a$incidentOffense)
```

![plot of chunk imoldcat](figure/imoldcat1.png) 

```r

# as you can see, the most common incident offense is 'Narcotics' (the 1st
# and 3rd row) next, let's analyze the correlations between 'Narcotics'
# inccident type and sex, race and district this is a correlation analysis
# invovling in multiple categorical variables I use a mosaic plot instead of
# the correlogram plot to show their correlations in one plot

library(vcd)
```

```
## Loading required package: grid
```

```r
filtered <- subset(arrest_tab, (race == "B" | race == "W") & sex != "" & district != 
    "" & (incidentOffense == "87-Narcotics" | incidentOffense == "87O-Narcotics (Outside)"))
filtered$district <- gsub("NORTHERN", "N", gsub("NORTHEASTERN", "NE", gsub("NORTHWESTERN", 
    "NW", filtered$district)))
filtered$district <- gsub("SOUTHERN", "S", gsub("SOUTHEASTERN", "SE", gsub("SOUTHWESTERN", 
    "SW", filtered$district)))
filtered$district <- gsub("WESTERN", "W", gsub("EASTERN", "E", gsub("CENTRAL", 
    "C", filtered$district)))
mosaic(~district + sex + race + incidentOffense, data = filtered, shade = TRUE, 
    legend = TRUE)
```

![plot of chunk imoldcat](figure/imoldcat2.png) 

```r
par(old_par)
```


What did you observe?: The first bar plot shows that the 'Narcotics' is the most common inccident offense in this city. Interestingly 'Narcotics (Outside)' is another top3 inccident offense type.  

In the second plot, I use mosaic plot to visualize the correlations among multiple categorical variables (race (b, w), sex (m, f), district (w, sw, se, s, nw, ne, n, e, c)). Rectangle sizes reflect the frequentices of a group, the colors shows the pearson residuals (blue means above expected value, while red means it's below expected value of fitted model).

Here're some interesting findings:

1. From previous map plots with race info, we know that in the southern regions, white cases and black cases are similar. It is the same case for Narcotics. The south regions (SE, S), the numbers are similar (we can see from the heighs of the rectangle). In other regions, black dominants white. 

2. Where you can find narcotics cases? And where does outside Narcotics case happen more often than indoor? North (N), Southerne East (SE) and South (S) regions have the fewest instances of Narcotics cases. In the north eastern (NE) and south western (SW) regions, the Narcotics outside cases are above expected value, while Eastern (E), Northwestern (NW) and Western (W) regions outside cases are below expected value. 

3. Female's narcotics active region is quite different from male. In the central district (C), both female black and white are above the expected values for both inside and outside narcotics cases, while male cases regardless races are below expected value.
