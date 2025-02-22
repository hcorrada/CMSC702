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
```

```
## Warning: Name partially matched in data frame
```

```r
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
```

```
## Warning: Name partially matched in data frame
```

```r
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

![plot of chunk RobArgue](figure/RobArgue.png) 


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
```

```
## Warning: Name partially matched in data frame
```

```r
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
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
## Loading required package: gsubfn
## Loading required package: proto
## Loading required namespace: tcltk
>>>>>>> f520a6901b5f9c89f7fa77a941026b208c6bfe34
=======
## Loading required package: gsubfn
## Loading required package: proto
## Loading required namespace: tcltk
>>>>>>> 0e04f064bb355d87894fd2913c6d15a069b0daf6
=======
## Warning: package 'sqldf' was built under R version 3.0.3
```

```
## Loading required package: gsubfn
```

```
## Warning: package 'gsubfn' was built under R version 3.0.3
```

```
## Loading required package: proto
## Loading required namespace: tcltk
>>>>>>> bee871cb4c4a818b1907e7e4ede2b8f10a7412af
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: RSQLite.extfuns
```

```r
a = sqldf("select incidentOffense, count(*) as cnt from arrest_tab where incidentOffense != 'Unknown Offense' group by incidentOffense having cnt >= 200 order by cnt")
<<<<<<< HEAD
=======
```

```
## Loading required package: tcltk
```

```r
>>>>>>> f520a6901b5f9c89f7fa77a941026b208c6bfe34
par(las = 2, mar = c(5, 7, 4, 2))
barplot(a$cnt, horiz = TRUE, cex.names = 0.7, names.arg = a$incidentOffense)
```

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
![plot of chunk imoldcat](figure/imoldcat.png) 
=======
![plot of chunk imoldcat](figure/imoldcat1.png) 
>>>>>>> f520a6901b5f9c89f7fa77a941026b208c6bfe34
=======
![plot of chunk imoldcat](figure/imoldcat1.png) 
>>>>>>> 0e04f064bb355d87894fd2913c6d15a069b0daf6
=======
![plot of chunk imoldcat](figure/imoldcat1.png) 
>>>>>>> bee871cb4c4a818b1907e7e4ede2b8f10a7412af

```r

# as you can see, the most common incident offense is 'Narcotics' (the 1st
# and 3rd row) next, let's analyze the correlations between 'Narcotics'
# inccident type and sex, race and district this is a correlation analysis
# invovling in multiple categorical variables I use a mosaic plot instead of
# the correlogram plot to show their correlations in one plot

library(vcd)
```

```
<<<<<<< HEAD
=======
## Warning: package 'vcd' was built under R version 3.0.3
```

```
>>>>>>> bee871cb4c4a818b1907e7e4ede2b8f10a7412af
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


#### Ben Klimkowski and Jon Fetter-Degges
What question are you asking?:

What neighborhoods have the highest number of arrests? Of narcotics arrests? What is the nature of the crime in the worst neighborhoods?

What is the code you use to answer it?:

```r
# We begin by counting the total number of arrest records per neighborhood,
# throwing out those where the neighborhood is empty.
total <- table(arrest_tab$neighborhood[arrest_tab$neighborhood != ""])
# Now we do it again, using only narcotics arrests.
narc.arrests <- arrest_tab$neighborhood[grep("narcotics", arrest_tab$incidentOffense, 
    ignore.case = TRUE)]
narc <- table(narc.arrests[narc.arrests != ""])
# We want to put these vectors into the same data frame, so let's throw away
# neighborhoods with no narcotics arrests.
total <- total[names(total) %in% names(narc)]
df <- data.frame(total)
df$narc <- narc
# Now we can sort the frame by total number of arrests.
df <- df[order(df$total, df$narc), ]
df$nonnarc <- df$total - df$narc
# We'll get rid of extra variables; everything we need is in the frame.
rm(total, narc, narc.arrests)
# And generate a plot of neighborhoods with the most arrests, along with how
# many of them are for narcotics.  We looked at Hui Miao's code to find out
# how to save and restore graph parameters.
old_par <- par(no.readonly = TRUE)
par(las = 2, mar = c(4, 9, 3, 2) + 0.1)
barplot(t(as.matrix(df[df$total > 600, 2:3])), horiz = TRUE, col = c("orange", 
    "blue"), cex.names = 0.7, main = "Number of Arrests per Neighborhood--")
legend(2000, 12, legend = c("narcotics", "non-narcotics"), fill = c("orange", 
    "blue"), cex = 0.7)
```

![plot of chunk Klimkowski_and_Fetter_Degges](figure/Klimkowski_and_Fetter_Degges1.png) 

```r
par(old_par)

# Using the code above it, we will now create graphics for the 5
# neighborhoods with the most crime: Downtown, Sandtown-Winchester, Central
# Park Heights, Broadway East, Belair-Edison

library(plotrix)
bmoreHoodAnalyze <- function(arg1) {
    # This function cleans the Baltimore dataset by combining similar crimes and
    # and graphically depicts the most frequently crimes in an area
    a <- table(arrest_tab$incidentOffense[arrest_tab$neighborhood == arg1])
    df <- data.frame(a)
    
    # Combine sums Narcotics
    narcoSum <- sum(df$Freq[df$Var1 == "87O-Narcotics (Outside)" | df$Var1 == 
        "87-Narcotics"])
    df$Freq[df$Var1 == "87-Narcotics"] <- narcoSum
    df <- df[!df$Var1 == "87O-Narcotics (Outside)", ]
    ## Unlabeled crimes
    unkSum <- sum(df$Freq[df$Var1 == "Unknown Offense" | df$Var1 == "79-Other"])
    df$Freq[df$Var1 == "Unknown Offense"] <- unkSum
    df <- df[!df$Var1 == "79-Other", ]
    df <- df[!df$Var1 == "Unknown Offense", ]
    
    # Concatenate the remainder of the crimes
    df <- df[order(-df$Freq), ]
    l <- as.vector(df[, 1])[0:5]
    freq <- as.vector(df[, 2])[0:5]
    freq <- c(freq, sum(as.vector(df[, 2])[6:dim(df)[1]]) + unkSum)
    l <- c(l, "Other")
    title <- paste0("Distribution of Type of Crime in ", arg1)
    pie3D(freq, labels = l, explode = 0.2, theta = pi/2.5, labelcex = 0.7, main = title)
}


bmoreHoodAnalyze("Downtown")
```

![plot of chunk Klimkowski_and_Fetter_Degges](figure/Klimkowski_and_Fetter_Degges2.png) 

```r
bmoreHoodAnalyze("Sandtown-Winchester")
```

![plot of chunk Klimkowski_and_Fetter_Degges](figure/Klimkowski_and_Fetter_Degges3.png) 

```r
bmoreHoodAnalyze("Central Park Heights")
```

![plot of chunk Klimkowski_and_Fetter_Degges](figure/Klimkowski_and_Fetter_Degges4.png) 

```r
bmoreHoodAnalyze("Broadway East")
```

![plot of chunk Klimkowski_and_Fetter_Degges](figure/Klimkowski_and_Fetter_Degges5.png) 

```r
bmoreHoodAnalyze("Belair-Edison")
```

![plot of chunk Klimkowski_and_Fetter_Degges](figure/Klimkowski_and_Fetter_Degges6.png) 

```r

```

What did you observe?:

It is clear from this analysis that narcotics related crimes are the most frequent ones across many neighborhoods in Baltimore.

Unfortunately, the Baltimore database left many entries as "79-Other" or unknown, so specific crimes are hard to correlate.  However, with some cleaning of the data-set, a clear pattern emerged in these problem spots.  After narcotics, the most frequent crime to be encountered would be assault or search/seizure related, followed by towed vehicles.


---
#### Perceptrons: Victoria Cepeda, Ahmed Kosba, Mat?as Marenchino, Mohamed Gunady

What question are you asking?:

    1. As presented earlier by another team, arrests rate on the weekend is not as high as in the middle of the week, although it was expected for crimes to be a bit more in the weekend. So Why is that?

    2. How is the number of vacant houses correlated with the crime rate in a neighborhood? We are concerned with two types of crimes: 1) Violence-related. 2) Narcotic-related. We obtained a dataset for vacant buildings in Baltimore and used it to plot their locations vs. the geographical distribution of each crime category.

What is the code you use to answer it?:

    Test1: Crimes on the weekend.

```r

# Define two types of crimes: Violence-Related, Narcotic-Related
narcotics <- c("87-Narcotics", "87O-Narcotics (Outside)", "97-Search & Seizure")
violentCrimes <- c("102-Questional Death", "104-Malicious Burning", "105-Suspicious Burning", 
    "115-Trespassing", "1A-Murder", "2A-Rape (Force)", "2B-Rape (Attempt)", 
    "2D-Statutory Rape", "2G-Sodomy/Perverson", "2J-Other Sex Offn.", "39-Fire", 
    "3AF-Robb Hwy-Firearm", "3AJF-Robb Carjack-Firearm", "3AJK-Robb Carjack-Knife", 
    "3AK-Robb Hwy-Knife", "3AO-Robb Hwy-Other Wpn", "3B-Robb Highway (Ua)", 
    "3BJ-Robb Carjack(Ua)", "3CF-Robb Comm-Firearm", "3CK-Robb Comm-Knife", 
    "3CO-Robb Comm-Other Wpn", "3D-Robb Comm. (Ua)", "3EF-Robb Gas Station-Firearm", 
    "3EK-Robb Gas Station-Knife", "3F-Robb Gas Sta. (Ua)", "3GF-Robb Conv Store-Firearm", 
    "3GK-Robb Conv Store-Knife", "3GO-Robb Conv Store-Other Wpn", "3H-Robb Conv. Stor.(Ua)", 
    "3JF-Robb Residence-Firearm", "3JK-Robb Residence-Knife", "3JO-Robb Residence-Other Wpn", 
    "3K-Robb Res. (Ua)", "3LF-Robb Bank-Firearm", "3FO-RObb Bank-Other Wpn", 
    "3M-Robb Bank (Ua)", "3NF-Robb Misc-Firearm", "3NK-Robb Misc-Knife", "3NO-Robb Misc-Other Wpn", 
    "3P-Robb Misc. (Ua)", "49-Family Disturbance", "4A-Agg. Asslt.- Gun", "4B-Agg. Asslt.- Cut", 
    "4C-Agg. Asslt.- Oth.", "4D-Agg. Asslt.- Hand", "4E-Common Assault", "4F-Assault By Threat", 
    "52A-Animal Cruelty", "56-Missing Person", "75-Destruct. Of Property", "76-Child Abuse-Sexual", 
    "7A-Stolen Auto")

# Arrest data for the two types of crimes, and crimes with unknown offense
violent_arrests <- arrest_tab[arrest_tab$incidentOffense %in% violentCrimes, 
    ]
narcotic_arrests <- arrest_tab[arrest_tab$incidentOffense %in% narcotics, ]
unknown_arrests <- arrest_tab[arrest_tab$incidentOffense == "Unknown Offense", 
    ]

# Histogram function for crimes over the days of week
hist_dayofweek <- function(dataDates, dataName) {
    dates = as.Date(dataDates, "%m/%d/%Y")
    dayofweek <- format(dates, "%a")
    dayofweek_nums <- as.numeric(format(dates, "%w"))  # numeric version
    hist(dayofweek_nums, main = paste(dataName, " by Day of Week"), xlab = "Day of Week", 
        ylab = "Frequency", breaks = -0.5 + 0:7, labels = unique(dayofweek[order(dayofweek_nums)]))
}

# Plot histogram of violence-related arrests over the week
hist_dayofweek(violent_arrests$arrestDate, "Violence-Related Arrests")
```

![plot of chunk Perceptrons-Test1](figure/Perceptrons-Test11.png) 

```r
# Plot histogram of narcotic-related arrests over the week
hist_dayofweek(narcotic_arrests$arrestDate, "Narcotic-Related Arrests")
```

![plot of chunk Perceptrons-Test1](figure/Perceptrons-Test12.png) 

```r
# Plot histogram of unknown-offense arrests over the week
hist_dayofweek(unknown_arrests$arrestDate, "Unknown Offense Arrests")
```

![plot of chunk Perceptrons-Test1](figure/Perceptrons-Test13.png) 


    Test2: Vacant buildings vs. Crime areas.

```r

# Prepare the dataset of vacant buildings
vacant_tab <- read.csv("Vacant_Buildings.csv", stringsAsFactors = FALSE)

# prepare long/lat comlumns
tmp = gsub("\\)", "", gsub("\\(", "", vacant_tab$Location))
```

```
## Warning: Name partially matched in data frame
```

```r
tmp = strsplit(tmp, split = ",")
vacant_tab$lon = as.numeric(sapply(tmp, function(x) x[2]))
vacant_tab$lat = as.numeric(sapply(tmp, function(x) x[1]))

# Plot the geographical distribution of vacant buildings vs. arrests
library(ggplot2)
library(ggmap)
<<<<<<< HEAD
=======
```

```
## Warning: package 'ggmap' was built under R version 3.0.3
```

```r
>>>>>>> bee871cb4c4a818b1907e7e4ede2b8f10a7412af

# Function to plot datapoints using GoogleMaps API
plot_map <- function(map, dataPoints, dataPoints2) {
    googleMap = ggmap(map)
    googleMap = googleMap + geom_point(data = dataPoints, aes(x = lon, y = lat), 
        color = "black", alpha = 0.1, size = 3)
    if (!missing(dataPoints2)) {
        googleMap = googleMap + geom_point(data = dataPoints2, aes(x = lon, 
            y = lat), color = "yellow", alpha = 0.1, size = 1.5)
    }
    googleMap
}

# Get Baltimore city map from GoogleMaps
map = get_map(location = c(lon = -76.62, lat = 39.3), zoom = 12, maptype = "terrain")
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=39.3,-76.62&zoom=12&size=%20640x640&scale=%202&maptype=terrain&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
```

```r

# Plot violence-related arrests vs. vacant buildings locations
plot_map(map, violent_arrests, vacant_tab)
```

```
## Warning: Removed 2588 rows containing missing values (geom_point).
```

![plot of chunk Perceptrons-Test2](figure/Perceptrons-Test21.png) 

```r
# Plot narcotic-related arrests vs. vacant buildings locations
plot_map(map, narcotic_arrests, vacant_tab)
```

```
## Warning: Removed 2744 rows containing missing values (geom_point).
```

![plot of chunk Perceptrons-Test2](figure/Perceptrons-Test22.png) 


What did you observe?:

    1. For Q1: 
      - As observed from the figures, violence-related arrests are high in weekends (with a peak on Saturday) as anticipated.
      - Both narcotic-related arrests and arrests with unknown-offense have their peak in the middle of the week, with a lower rate on weekends.
      - As the number of records for narcotic-related and unknown offense arrests are much higher than violence-related arrests, the overall total crime rate has its peak on Wednesday, not in weekends.

    2. For Q2:
      As predicted, the figures show that the vacant buildings in a neighborhood are correlated with its crime rate, with a bit more correlation with neighborhoods where arrests due to violence occurred than the narcotic-related arrests.
---

#### Xiyang Dai

What question are you asking?:

  Q1. Is there a relationship between arrest time and crime type? 
  
  Q2. Is there a relationship between arrest time and arrest location (longitude, latitude)?

What is the code you use to answer it?:


```r
library(ggmap)
library(ggplot2)

# Define function that converts time to numerical value for calculation
time2num = function(x) {
    tmp = as.numeric(gsub("\\:", "", x))
    x = tmp/100 + tmp%%100/60
}

# Get subtable from the original table
arrest_tab_tmp1 = subset(arrest_tab, incidentOffense != "", select = c(arrestTime, 
    incidentOffense))
arrest_tab_tmp1 = arrest_tab_tmp1[complete.cases(arrest_tab_tmp1), ]
arrest_tab_tmp1$arrestTime = sapply(arrest_tab_tmp1$arrestTime, time2num)

# Plot the the relationship between arrest time and crime type, reorder them
# based on the median of arrest time value
arrest_tab_tmp1$incidentOffense = with(arrest_tab_tmp1, reorder(incidentOffense, 
    arrestTime, median))
qplot(factor(arrest_tab_tmp1$incidentOffense), arrest_tab_tmp1$arrestTime, main = "Relationship Between Arrest Time and Crime Type", 
    xlab = "Crime Type", ylab = "Arrest Time", geom = "boxplot", asp = 2) + 
    coord_flip()
```

![plot of chunk Xiyang, Q1](figure/Xiyang__Q1.png) 



```r
# Get subtable from the original table
arrest_tab_tmp2 = subset(arrest_tab, select = c(arrestTime, lon, lat))
arrest_tab_tmp2 = arrest_tab_tmp2[complete.cases(arrest_tab_tmp2), ]
arrest_tab_tmp2$arrestTime = sapply(arrest_tab_tmp2$arrestTime, time2num)

# Shift time axis to start at 6am for better representation
shift_axis_shift = function(x) {
    if (x < 6) {
        x = x + 24
    } else {
        x = x
    }
}
arrest_tab_tmp2$arrestTime = sapply(arrest_tab_tmp2$arrestTime, shift_axis_shift)

# Cut number into bins for color visualization
rg_pal = colorRampPalette(c(c("light green", "yellow", "red")), bias = 1)
a_color = rg_pal(12)[as.numeric(cut(arrest_tab_tmp2$arrestTime, breaks = 12))]

# Get Baltemore map from Google Map
map = get_map(location = c(lon = -76.62, lat = 39.3), zoom = 12, maptype = "roadmap")
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=39.3,-76.62&zoom=12&size=%20640x640&scale=%202&maptype=roadmap&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
```

```r
plt = ggmap(map)

# Visualize arrest time on map
plt = plt + geom_point(data = arrest_tab_tmp2, aes(x = arrest_tab_tmp2$lon, 
    y = arrest_tab_tmp2$lat), color = a_color, alpha = 0.1)
plt = plt + guides(title = "Arrest Time", fill = guide_colorbar())
print(plt)
```

```
## Warning: Removed 61 rows containing missing values (geom_point).
```

![plot of chunk Xiyang, Q2](figure/Xiyang__Q2.png) 


What did you observe?

Q1: As expected, there is a obvious relationship between arrest time and crime type. From the figure, we can conclude the tendency for specific crime. For example, rob stores or gas stations usually happen in the late night. However, Rob banks usually happen in the afternoon.

Q2: It seems that there is a relationship between arrest time and arrest location. In places near the downtown, crimes trend to happen in night. In places far from downtown, crimes trend to happen in daytime. But this relationship still needs to be further analyzed commbing with specific geographic features.  
  
---

#### Ruofei Du
What question are you asking?:

    Is there a relationship between top crime types and age of the criminals? For instance, the younger criminals tend to commit more aggressive offence while the older tend to commit more stealthy offence.

What is the code you use to answer it?:

```r
# Remove the data with unknown offense or other type
arrestData <- subset(arrest_tab, age > 0 & incidentOffense != "Unknown Offense" & 
    incidentOffense != "79-Other")
arrestData <- subset(arrestData, select = c(age, incidentOffense))

# Calculate the frequency of each crime type
offenseFreq <- table(arrestData$incidentOffense)
offenseFreq <- sort(offenseFreq, decreasing = TRUE)
N <- 15
topN <- data.frame(IncidentOffense = names(offenseFreq[1:N]), Freq = as.integer(offenseFreq[1:N]))
arrestData$topIncident <- sapply(arrestData$incidentOffense, function(x) {
    id <- 0
    for (y in names(offenseFreq[1:N])) {
        id <- id + 1
        if (x == y) {
            return(id)
        }
    }
    return(-1)
})
arrestData <- subset(arrestData, arrestData$topIncident != -1)
# Sort y axis by median of age; sort color by frequency
arrestData$incidentOffense = with(arrestData, reorder(incidentOffense, age, 
    median))
arrestData$topOffense = with(arrestData, reorder(incidentOffense, topIncident, 
    median))
Top_15_Offenses <- arrestData$topOffense
qplot(factor(arrestData$incidentOffense), arrestData$age, main = "Relationship Between Age and Crime Type", 
    xlab = "Crime Type", ylab = "Age", geom = "boxplot", asp = 2, col = Top_15_Offenses) + 
    coord_flip()
```

![plot of chunk Ruofei_Du](figure/Ruofei_Du.png) 


What did you observe?:

    There's indeed a weak relationship between age and criminal type. The older criminals tend to commit crimes with less violence such as shop lifting and burglary. E.G. the median of shoplifting criminals is of age 40; the younger criminals ten to commit crimes with more violence and sabotage such as trespassing, disorder, destrction of property.

    However, the age range of criminals can be adolescents to relatively old poeple. Few people commit destrction of property after the age of 60.

---
<<<<<<< HEAD
#### Will Armstrong

What question are you asking?: Is the difference in arrest numbers between "black" and "white" races due to population demographics?

What is the code you use to answer it?:


```r
#### Plot distribution of race for arrests based on neighborhood, based on code
#### by Ben Klimowski and Jon Fetter-Degges
total <- table(arrest_tab$neighborhood[arrest_tab$neighborhood != ""])
black.arrests <- arrest_tab$neighborhood[arrest_tab$race == "B"]
black <- table(black.arrests[black.arrests != ""])
white.arrests <- arrest_tab$neighborhood[arrest_tab$race == "W"]
white <- table(white.arrests[white.arrests != ""])

# 0-fill unused neighborhoods and resort
black[names(total)[!(names(total) %in% names(black))]] <- 0
black <- black[order(names(black))]
white[names(total)[!(names(total) %in% names(white))]] <- 0
white <- white[order(names(white))]

# Combine into data frame
df <- data.frame(total)
df$black <- black
df$white <- white
df <- df[order(df$Freq, df$black, df$white), ]
df$other <- df$Freq - df$black - df$white

#### Load population demographics
census_tab <- read.csv("Census_Demographics_2010.csv", stringsAsFactors = FALSE)

grepFun <- function(neighborhood) {
    ns <- unlist(strsplit(neighborhood, "/"))
    gr <- grep(ns[1], census_tab$CSA2010)
    index <- 0
    if (any(gr)) 
        index <- gr[1] else if (length(ns) > 1) {
        gr <- grep(ns[2], census_tab$CSA2010)
        if (any(gr)) 
            index <- gr[1]
    }
    index
}

# Look up census information for each neighborhood
df$census_ind <- apply(as.matrix(df$Var1), 1, grepFun)
# Remove neighborhoods not found in census
df <- df[df$census_ind > 0, ]
df$peraa <- census_tab$peraa10[df$census_ind]
df$perwhite <- census_tab$perwhite10[df$census_ind]
df$expected_b <- df$peraa * (df$Freq/100)
```

```
## Error: non-numeric argument to binary operator
```

```r
df$expected_w <- df$perwhite * (df$Freq/100)
```

```
## Error: non-numeric argument to binary operator
```

```r

#### Plot Results

rm(total, black, black.arrests, white, white.arrests, census_tab)
# Generate a plot of neighborhoods with the racial distribution of, along
# with points for the expected values.
old_par <- par(no.readonly = TRUE)
par(las = 2, mar = c(4, 9, 3, 2) + 0.1)
toPlot <- df$Freq > 600
df.bar <- barplot(t(as.matrix(df[toPlot, 3:4])), names.arg = df$Var1[toPlot], 
    horiz = TRUE, col = c("green", "red"), cex.names = 0.7, main = "Race of Arrests by Neighborhood")
points(x = df$expected_b[toPlot], y = df.bar, col = "blue", pch = 18)
```

```
## Error: 'x' and 'y' lengths differ
```

```r
points(x = df$expected_w[toPlot] + df$black[toPlot], y = df.bar, col = "black", 
    pch = 18)
```

```
## Error: 'x' and 'y' lengths differ
```

```r
legend(2000, 5, legend = c("black", "white", "expected black", "expected white"), 
    fill = c("green", "red", "blue", "black"), cex = 0.7)
```

![plot of chunk armstrow](figure/armstrow.png) 

What did you observe?
=======
Here I am making a big assumption that people tend to get arrested in the same neighborhood in which they live.  Therefore the results may not apply as well in a neighborhood like "Downtown" where more people commute to the neighborhood than live there.  For about half of the listed neighborhoods, black arrests are higher than mere population demographics would predict and white arrests are lower. This could be due to a variety of factors, not least of which is potential racism among the police force.  The other half follow the expected results very well, and Cherry Hill even reverses the finding.  Therefore I conclude that race of arrest is at leasty partially influenced by factors other than from population demographics.

<<<<<<< HEAD
<<<<<<< HEAD
---
<<<<<<< HEAD
### Bharat and Michael

What question are you asking?

We are looking for information gain between multiple attributes in the dataset. In the following code we analyse the dependencies between age, sex, race, incident location, district, neighborhood, date, and time of arrest. Finally, we plot a graph depicting the relationships between the attributes. The strength of relationship is quantified by the color of the edges, the more red the edges, the greater the strength.

What is the code you used to answer it?


```r
library(FSelector)
arrest_tab = read.csv("BPD_Arrests.csv", stringsAsFactors = FALSE)
mod_arrest_tab <- arrest_tab
notneeded <- c("arrest", "charge", "chargeDescription", "Location.1", "arrestLocation", 
    "incidentOffense", "post")
mod_arrest_tab <- mod_arrest_tab[, !(names(mod_arrest_tab) %in% notneeded)]
mod_arrest_tab$arrestDate <- unlist(lapply(mod_arrest_tab$arrestDate, function(x) substr(x, 
    1, 2)))
mod_arrest_tab$arrestTime <- unlist(lapply(mod_arrest_tab$arrestTime, function(x) substr(x, 
    1, 2)))

wage <- information.gain(age ~ ., mod_arrest_tab)
wsex <- information.gain(sex ~ ., mod_arrest_tab)
wrace <- information.gain(race ~ ., mod_arrest_tab)
wincidentLocation <- information.gain(incidentLocation ~ ., mod_arrest_tab)
wdistrict <- information.gain(district ~ ., mod_arrest_tab)
wneighborhood <- information.gain(neighborhood ~ ., mod_arrest_tab)
warrestDate <- information.gain(arrestDate ~ ., mod_arrest_tab)
warrestTime <- information.gain(arrestTime ~ ., mod_arrest_tab)

require(igraph)
```

```
## Loading required package: igraph
```

```
## Warning: there is no package called 'igraph'
```

```r
# 1 - Age 2 - Sex 3 - Race 4 - Incident Location 5 - District 6 -
# Neighborhood 7 - Date 8 - Time
g1 <- graph(c(1, 2, 1, 3, 1, 4, 1, 5, 1, 6, 1, 7, 1, 8, 2, 3, 2, 4, 2, 5, 2, 
    6, 2, 7, 2, 8, 3, 4, 3, 5, 3, 6, 3, 7, 3, 8, 4, 5, 4, 6, 4, 7, 4, 8, 5, 
    6, 5, 7, 5, 8, 6, 7, 6, 8, 7, 8), directed = FALSE)
```

```
## Error: could not find function "graph"
```

```r

V(g1)$label <- c("Age", "Sex", "Race", "IncidentLocation", "District", "Neighborhood", 
    "ArrestDate", "ArrestTime")
```

```
## Error: object 'g1' not found
```

```r

colors = heat.colors(1000)

# set colors for Age node
E(g1)[1]$color <- colors[999 - as.integer(200 * wage["sex", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[2]$color <- colors[999 - as.integer(200 * wage["race", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[3]$color <- colors[999 - as.integer(200 * wage["incidentLocation", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[4]$color <- colors[999 - as.integer(200 * wage["district", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[5]$color <- colors[999 - as.integer(200 * wage["neighborhood", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[6]$color <- colors[999 - as.integer(200 * wage["arrestDate", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[7]$color <- colors[999 - as.integer(200 * wage["arrestTime", 1])]
```

```
## Error: object 'g1' not found
```

```r

# set colors for Sex node
E(g1)[8]$color <- colors[999 - as.integer(200 * wsex["race", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[9]$color <- colors[999 - as.integer(200 * wsex["incidentLocation", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[10]$color <- colors[999 - as.integer(200 * wsex["district", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[11]$color <- colors[999 - as.integer(200 * wsex["neighborhood", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[12]$color <- colors[999 - as.integer(200 * wsex["arrestDate", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[13]$color <- colors[999 - as.integer(200 * wsex["arrestTime", 1])]
```

```
## Error: object 'g1' not found
```

```r

# set colors for Race node
E(g1)[14]$color <- colors[999 - as.integer(200 * wrace["incidentLocation", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[15]$color <- colors[999 - as.integer(200 * wrace["district", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[16]$color <- colors[999 - as.integer(200 * wrace["neighborhood", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[17]$color <- colors[999 - as.integer(200 * wrace["arrestDate", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[18]$color <- colors[999 - as.integer(200 * wrace["arrestTime", 1])]
```

```
## Error: object 'g1' not found
```

```r

E(g1)[19]$color <- colors[999 - as.integer(200 * wincidentLocation["district", 
    1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[20]$color <- colors[999 - as.integer(200 * wincidentLocation["neighborhood", 
    1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[21]$color <- colors[999 - as.integer(200 * wincidentLocation["arrestDate", 
    1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[22]$color <- colors[999 - as.integer(200 * wincidentLocation["arrestTime", 
    1])]
```

```
## Error: object 'g1' not found
```

```r

E(g1)[23]$color <- colors[999 - as.integer(200 * wdistrict["neighborhood", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[24]$color <- colors[999 - as.integer(200 * wdistrict["arrestDate", 1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[25]$color <- colors[999 - as.integer(200 * wdistrict["arrestTime", 1])]
```

```
## Error: object 'g1' not found
```

```r

E(g1)[26]$color <- colors[999 - as.integer(200 * wneighborhood["arrestDate", 
    1])]
```

```
## Error: object 'g1' not found
```

```r
E(g1)[27]$color <- colors[999 - as.integer(200 * wneighborhood["arrestTime", 
    1])]
```

```
## Error: object 'g1' not found
```

```r

E(g1)[28]$color <- colors[999 - as.integer(200 * warrestDate["arrestTime", 1])]
```

```
## Error: object 'g1' not found
```

```r

plot(g1)
```

```
## Error: object 'g1' not found
```

```r

# 1 - Age 2 - Sex 3 - Race 4 - Date 5 - Time
g2 <- graph(c(1, 2, 1, 3, 1, 4, 1, 5, 2, 3, 2, 4, 2, 5, 3, 4, 3, 5, 4, 5), directed = FALSE)
```

```
## Error: could not find function "graph"
```

```r

V(g2)$label <- c("Age", "Sex", "Race", "ArrestDate", "ArrestTime")
```

```
## Error: object 'g2' not found
```

```r

# set colors for Age node
E(g2)[1]$color <- colors[999 - as.integer(60000 * wage["sex", 1])]
```

```
## Error: object 'g2' not found
```

```r
E(g2)[2]$color <- colors[999 - as.integer(60000 * wage["race", 1])]
```

```
## Error: object 'g2' not found
```

```r
E(g2)[3]$color <- colors[999 - as.integer(60000 * wage["arrestDate", 1])]
```

```
## Error: object 'g2' not found
```

```r
E(g2)[4]$color <- colors[999 - as.integer(60000 * wage["arrestTime", 1])]
```

```
## Error: object 'g2' not found
```

```r

# set colors for Sex node
E(g2)[5]$color <- colors[999 - as.integer(60000 * wage["race", 1])]
```

```
## Error: object 'g2' not found
```

```r
E(g2)[6]$color <- colors[999 - as.integer(60000 * wage["arrestDate", 1])]
```

```
## Error: object 'g2' not found
```

```r
E(g2)[7]$color <- colors[999 - as.integer(60000 * wage["arrestTime", 1])]
```

```
## Error: object 'g2' not found
```

```r

# set colors for Race node
E(g2)[8]$color <- colors[999 - as.integer(60000 * wage["arrestDate", 1])]
```

```
## Error: object 'g2' not found
```

```r
E(g2)[9]$color <- colors[999 - as.integer(60000 * wage["arrestTime", 1])]
```

```
## Error: object 'g2' not found
```

```r

# set colors for Date node
E(g2)[9]$color <- colors[999 - as.integer(60000 * wage["arrestTime", 1])]
```

```
## Error: object 'g2' not found
```

```r

plot(g2)
```

```
## Error: object 'g2' not found
```


What did you observe?

We observed a strong relationship between atrributes pertaining to location and all other attributes. This should be expected as location is a fairly unique attribute of any arrest, and therefore predictive of other attributes.
=======

#### Shangfu Peng
What question are you asking?:

    Which types of arrested crimes can benefit from CCTV cameras? Is there any relationship between the number of CCTV cameras and the number of arrests on specific incident offense? In addition, what is the difference of the performance on sex?
=======

---

#### Zebao Gao
What question are you asking?:

    What are the major types of criminals for males/females in comparison?
>>>>>>> 2574bd065679b3b24b0d520209815fbcdab1d612

What is the code you use to answer it?:

```r
<<<<<<< HEAD

narcotics <- c("87-Narcotics", "87O-Narcotics (Outside)", "97-Search & Seizure")
violent <- c("1A-Murder", "2A-Rape (Force)", "2B-Rape (Attempt)", "3AF-Robb Hwy-Firearm", 
    "3AJF-Robb Carjack-Firearm", "3AJK-Robb Carjack-Knife", "3AK-Robb Hwy-Knife", 
    "3AO-Robb Hwy-Other Wpn", "3CF-Robb Comm-Firearm", "3CK-Robb Comm-Knife", 
    "3CO-Robb Comm-Other Wpn", "3EF-Robb Gas Station-Firearm", "3EK-Robb Gas Station-Knife", 
    "3GF-Robb Conv Store-Firearm", "3GK-Robb Conv Store-Knife", "3GO-Robb Conv Store-Other Wpn", 
    "3JF-Robb Residence-Firearm", "3JK-Robb Residence-Knife", "3JO-Robb Residence-Other Wpn", 
    "3LF-Robb Bank-Firearm", "3FO-RObb Bank-Other Wpn", "3NF-Robb Misc-Firearm", 
    "3NK-Robb Misc-Knife", "3NO-Robb Misc-Other Wpn", "4A-Agg. Asslt.- Gun", 
    "4B-Agg. Asslt.- Cut", "4C-Agg. Asslt.- Oth.", "4D-Agg. Asslt.- Hand")


IncidentViaCCTV <- function(offenses, sexual) {
    subsetvio = subset(arrest_tab, arrest_tab$incidentOffense %in% offenses)
    if (sexual == "M" || sexual == "F") 
        subsetsex = subset(subsetvio, subsetvio$sex == sexual) else subsetsex = subsetvio
    
    latRange = range(arrest_tab$lat, na.rm = TRUE)
    lonRange = range(arrest_tab$lon, na.rm = TRUE)
    latGrid = seq(min(latRange), max(latRange), len = 50)
    lonGrid = seq(min(lonRange), max(lonRange), len = 50)
    latFac = as.numeric(cut(subsetsex$lat, breaks = latGrid))
    lonFac = as.numeric(cut(subsetsex$lon, breaks = lonGrid))
    
    gridFac = (latFac - 1) * length(latGrid) + (lonFac - 1)
    
    latFac = as.numeric(cut(cctv_tab$lat, breaks = latGrid))
    lonFac = as.numeric(cut(cctv_tab$lon, breaks = lonGrid))
    cctvGridFac = (latFac - 1) * length(latGrid) + (lonFac - 1)
    
    arrestTab = table(gridFac)
    cctvTab = table(cctvGridFac)
    m = match(names(cctvTab), names(arrestTab))
    plot(arrestTab[m] ~ factor(cctvTab))
}

IncidentViaCCTV(narcotics, "M")
```

![plot of chunk Shangfu_Peng](figure/Shangfu_Peng1.png) 

```r
IncidentViaCCTV(narcotics, "F")
```

![plot of chunk Shangfu_Peng](figure/Shangfu_Peng2.png) 

```r

IncidentViaCCTV(violent, "M")
```

![plot of chunk Shangfu_Peng](figure/Shangfu_Peng3.png) 

```r
IncidentViaCCTV(violent, "F")
```

![plot of chunk Shangfu_Peng](figure/Shangfu_Peng4.png) 

```r


IncidentViaCCTV(c("4E-Common Assault"), "M")
```

![plot of chunk Shangfu_Peng](figure/Shangfu_Peng5.png) 

```r
IncidentViaCCTV(c("4E-Common Assault"), "F")
```

![plot of chunk Shangfu_Peng](figure/Shangfu_Peng6.png) 


What did you observe?:

    From previous "challenge" example, we know that the relationship between the number of CCTV cameras and the number of arrests is not obvious. However, I guess that there should be some certain incident offenses that woule benefit from CCTV caremas. (Otherwise, we can give up CCTV caremas.)
    
    First I tested on narcotics offenses. It is not influensed by the number of cameras. I guess it is because most people have drug in the houses, such that would not be photoed by cameras.
    
    Second I tested on violent crimes. Ignoing the small cases, the number of arrested crimes roughly increases as the number of caremas, especially on female. Obviously, CCTV cameras can catch violent crimes.  
    
    Furthermore, I tested on the certain offenses, "4E-Common Assault". The relationship is more obvious. 
    
    So using this function, we can know the which types of crimes would benefit from CCTV caremas.
>>>>>>> f520a6901b5f9c89f7fa77a941026b208c6bfe34
=======
#### Phil Nguyen

Question: What is the distribution of arrest time for each neighborhood?

Code I use:

```r
# convert arrest time to numeric, e.g. 11:45 -> 11.75
time.to.num <- function(s) {
    x <- as.numeric(gsub(":", "", s))
    (x%/%100) + (x%%100)/60
}

# abbreviate district names
abbrev <- function(s) {
    gsub("CENTRAL", "C", gsub("EASTERN", "E", gsub("WESTERN", "W", gsub("NORTHERN", 
        "N", gsub("NORTHWESTERN", "NW", gsub("SOUTHEASTERN", "SE", gsub("SOUTHERN", 
            "S", gsub("SOUTHWESTERN", "SW", gsub("NORTHEASTERN", "NE", s)))))))))
}

# filter out empty districts
dat <- arrest_tab[arrest_tab$district != "", ]

plot(time.to.num(dat$arrestTime) ~ factor(abbrev(dat$district)), xlab = "Neighborhood", 
    ylab = "Arrest Time")
```

![plot of chunk philnguyen](figure/philnguyen.png) 

```r

# earliest and latest arrest time median
median(time.to.num(dat$arrestTime[dat$district == "NORTHWESTERN"]))
```

```
## [1] 17.07
```

```r
median(time.to.num(dat$arrestTime[dat$district == "CENTRAL"]))
```

```
## [1] 14.33
```


What I observed: Overall, more than half of the arrests happen between 10AM and 8PM.
There is no siginificant difference between arrest time in different neighborhoods,
although arrests happen slightly earlier in central and the southern areas compared to northern areas.
>>>>>>> 0e04f064bb355d87894fd2913c6d15a069b0daf6
=======
# get arrest data related to sex and incidenOffense, and remove unknown
# offense types
arrest_data <- subset(arrest_tab, select = c(sex, incidentOffense))
arrest_data <- subset(arrest_data, !(incidentOffense %in% c("79-Other", "Unknown Offense")))
# calculate frequence of criminals by males or females for each criminal
# type
arrest_table <- table(arrest_data$incidentOffense, arrest_data$sex)
arrest_frame <- as.data.frame.table(arrest_table)
arrest_frame_m <- subset(arrest_frame, arrest_frame$Var2 == "M")
arrest_frame_f <- subset(arrest_frame, arrest_frame$Var2 == "F")
arrest_frame_all <- data.frame(incidentOffense = arrest_frame_m$Var1, M = arrest_frame_m$Freq, 
    F = arrest_frame_f$Freq)
# remove criminal types occured for very few times
arrest_frame_all <- subset(arrest_frame_all, arrest_frame_all$M > 2 & arrest_frame_all$F > 
    2)
arrest_frame_all$MRatio = 100 * arrest_frame_all$M/(arrest_frame_all$M + arrest_frame_all$F)
arrest_frame_all$FRatio = 100 * arrest_frame_all$F/(arrest_frame_all$M + arrest_frame_all$F)
# remove unusable data
rm(arrest_table, arrest_frame, arrest_frame_m, arrest_frame_f)
arrest_frame_all <- arrest_frame_all[order(arrest_frame_all$MRatio), ]
par(las = 2, mar = c(4, 9, 3, 2) + 0.1)
barplot(t(as.matrix(arrest_frame_all[arrest_frame_all$MRatio > 87, 4:5])), horiz = TRUE, 
    col = c("blue", "red"), cex.names = 0.7, main = "Major Criminal Types for Males", 
    names.arg = arrest_frame_all$incidentOffense[arrest_frame_all$MRatio > 87])
```

![plot of chunk Zebao_Gao](figure/Zebao_Gao1.png) 

```r
arrest_frame_all <- arrest_frame_all[order(arrest_frame_all$FRatio), ]
barplot(t(as.matrix(arrest_frame_all[arrest_frame_all$FRatio > 20, 5:4])), horiz = TRUE, 
    col = c("red", "blue"), cex.names = 0.7, main = "Major Criminal for Females", 
    names.arg = arrest_frame_all$incidentOffense[arrest_frame_all$FRatio > 20])
```

![plot of chunk Zebao_Gao](figure/Zebao_Gao2.png) 

```r
rm(arrest_frame_all)
```


What did you observe?:
=======
    In the two charts, the criminal types which are mainly committed by males/females are listed. The blue/red parts stand for the percentage of cases committed by males/females.
    There's a difference between the major types of criminals for males and females. Although males take a larger percentage in most of the criminal types, most of the major types are still kind of voilent. The characteristics of criminal types for females are more related to sex, children and disabilites.
>>>>>>> 2574bd065679b3b24b0d520209815fbcdab1d612
=======

---
### Hao Li
What question are you asking?: 

Q1: I heared the area around the Johns Hopkins University is not safe. Is that true?
Q1: Where is the best place to live around JHU?
  
What is the code you use to answer it?:


```r
library(ggplot2)
library(ggmap)

# Function to plot datapoints using GoogleMaps API
plot_map <- function(center, square) {
    map = ggmap(get_map(location = center, zoom = 14, maptype = "roadmap"))
    map = map + geom_point(data = arrest_tab, aes(x = lon, y = lat), color = "red", 
        alpha = 0.1, size = 2)
    map = map + geom_rect(aes(xmin = square[1], xmax = square[2], ymin = square[3], 
        ymax = square[4]), color = "yellow", alpha = 0.05)
    map
}

# JHU Homewood campus
center = c(lon = -76.620644, lat = 39.329522)
square = c(-76.624283, -76.617685, 39.324571, 39.335874)  # bounding box of the campus
map = plot_map(center, square)
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=39.329522,-76.620644&zoom=14&size=%20640x640&scale=%202&maptype=roadmap&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
```

```r
map = map + geom_leg(aes(x = -76.609546, y = 39.339051, xend = -76.609504, yend = 39.315514), 
    colour = "blue", alpha = 0.1, size = 2)
map
```

```
## Warning: Removed 96502 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-131.png) 

```r

# JHU School of Medicine
center = c(lon = -76.588595, lat = 39.298584)
square = c(-76.59735, -76.58793, 39.293952, 39.300777)  # bounding box of the campus
map = plot_map(center, square)
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=39.298584,-76.588595&zoom=14&size=%20640x640&scale=%202&maptype=roadmap&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
```

```r
map
```

```
## Warning: Removed 88292 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-132.png) 


What did you observe?:

1. The area around the Homewood campus(main campus) of JHU is relatively safe comparing to the campus of JHU School of Medicine locates, which locates in downtown and has higher crime density.

2. It is interesting to see that there is clear contrast between the west and the east of Greenmount Road(blue line) near the Homewood campus and there are quite few crimes in the north of the campus, which could be a safe neighborhood to live. If you attend the school of medicine, it's better to live elsewhere and take the shuttle:)
>>>>>>> bee871cb4c4a818b1907e7e4ede2b8f10a7412af
