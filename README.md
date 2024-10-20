# Lab 3: Spatial Autocorrelation Tutorial
Joshua Haberer 

2024-10-19

## Introduction

Spatial autocorrelation is an important concept in a spatial analyst’s toolbox. It refers to the degree to which spatial events are correlated across an area (O'Sullivan & Unwin, 2014). In basic terms, it is the idea that spatial events from closer locations are more likely to be similar compared to spatial events that are farther apart (Tobler, 2002). This law of geography is crucial in geographical studies where spatial distribution can influence analytical outcomes. Positive spatial autocorrelation indicate that similar events are closer together, while negative spatial autocorrelation describes a dispersed pattern.

The following tutorial will demonstrate how to conduct a spatial autocorrelation analysis within RStudio using real world data. As an example, we will look at the spatial patterns for two variables within Kingston, Ontario: (1) the percentage of individuals with French knowledge, and (2) the mean total income. These variables were obtained from Canadian Census data (Government of Canada, Statistics Canada, 2022). We will use specific statistical techniques such as Moran’s I and Local Spatial Autocorrelation to determine the degree of spatial autocorrelation within the two datasets. As well, we will explore how to represent these calculated statistics through visual aids such as maps and scatterplots.

By the end of this tutorial, you will have the ability to apply spatial autocorrelation techniques to other spatial datasets and understand the significance of spatial dependency in geography studies.

### Data Set-Up

Prior to any analysis, we will have to utilize multiple libraries within RStudio. These can be opened using the following code:

```
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "/Users/joshua/Desktop/GEOG418_GeoStatistics")

#install libraries (if not already)
install.packages("knitr") #to help display tables for descriptive statistics
install.packages("tmap") #used for visualizing spatial data and analysis
install.packages("spdep") #provides tools for spatial dependence analysis - calculating autocorrelation metrics
install.packages("e1071") #provides functions for statistical operations
install.packages("sf") #used for handelling and analyzing the shapefiles used in this study

#load in libraries
library("knitr") 
library("tmap")
library("spdep")
library("e1071") 
library("sf") 
```

Once the libraries have been loaded, you can start by setting your working directory and bringing in your shapefile as a st dataframe and your data as a dataframe. For this example, I have uploaded a .shp of Canada and the census .csv file from my working directory.

```
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "/Users/joshua/Desktop/GEOG418_GeoStatistics")

dir <- "~/Desktop/GEOG418_GeoStatistics" #will have to adjust accordingly
setwd(dir)

#bring in census .csv dataframe
csv <- read.csv("./ucgsJQnBVLvP_data.csv")

#bring in .shp files
shp <- st_read("./lda_000a16a_e.shp")
#shp <- st_transrom(shp, crs = ) #make sure to set your crs to the correct projection
```

Next, we can clean up the dataset to make it easier to analyze. We will begin by creating new names for the columns to make to make the dataset easier to understand. Once renamed, we can apply these names to the dataset.

```
#new column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
          "CD name", "DA name", "Population", "Land area", 
          "Median total income", "Income Sample Size", "French Knowledge", 
          "Language Sample Size")

#apply names to the dataframe
colnames(csv) <- cols
```

Second, we create a new column called len that counts the number of charcters within the GEO UID column. This ensures that the correct geographic scale is set throughout the dataset. These geographic units should have 8 characters so we should remove any rows where GEO UID does not equal 8.

```
#add column to count the number of characters in the GEO UID
csv$len <- nchar(csv$`GEO UID`)

#remove IDs with less than 8 characters
csv_clean <- subset(csv, len == 8)
```

Now we can combine the cleaned census data with the shapefile using the following code:

```
#merge spatial and spatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)
```

Since Kingston Ontario is the main focus for this analysis, we can subset the merged data to only include the geographic areas within the Kingston Boundary. This gives us a much smaller dataset which is specific to the area of focus.

```
#subset for Kingston Ontario
Municip <- subset(census_DAs, census_DAs$CMANAME == "Kingston")
```

Next, we can calculate the percent of French speakers by using the French language knowledge and language sample size fields within the ‘Municip’ dataset. This data is then directed to a new column called ‘PercFrench’ within the ‘Municip’ dataframe.

```
#convert to rate (percentage of people with French knowledge)
Municip$PercFrench <- (Municip$`French Knowledge` / Municip$`Language Sample Size`) * 100
```

Finally, we create two separate datasets for median total income and French knowledge data. As well, we remove any rows where we have missing values for either variable.

```
#remove rows where 'Median total income' is NA
Income_noNA <- Municip[which(!is.na(Municip$`Median total income`)),]

#remove rows where 'PercFrench' is NA
French_noNA <- Municip[which(!is.na(Municip$PercFrench)),]
```

### Data Visualization

Prior to autocorrelation analysis, we can take a deeper dive into the median total income and percentage of respondents with French language knowledge dataframes. We can begin by looking at some basic descriptive statistics including the mean, standard deviation, median, and skewness. As well, we can do a final check to make sure there are no more NA values or any major outliers within either of the datasets. This ensures that we cleaned up the data well enough for us to continue to the spatial autocorrelation analysis.

```
#calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`, na.rm = TRUE)
stdevIncome <- sd(Income_noNA$`Median total income`, na.rm = TRUE)
skewIncome <- skewness(Income_noNA$`Median total income`, na.rm = TRUE)
medIncome <- median(Income_noNA$`Median total income`, na.rm = TRUE)

#calculate descriptive stats for French knowledge percentage
meanFrench <- mean(French_noNA$PercFrench, na.rm = TRUE)
stdevFrench <- sd(French_noNA$PercFrench, na.rm = TRUE)
skewFrench <- skewness(French_noNA$PercFrench, na.rm = TRUE)
medFrench <- median(French_noNA$`PercFrench`, na.rm = TRUE)

#create dataframe for display in a table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome, 2), round(meanFrench, 2)),
                   StandardDeviation = c(round(stdevIncome, 2), round(stdevFrench, 2)),
                   Skewness = c(round(skewIncome, 2), round(skewFrench, 2)),
                   Median = c(round(medIncome, 2), round(medFrench, 2)))

#produce table (using kable function from knitr package)
kable(data, caption = paste0("Descriptive statistics for selected ",
                             2016, " census variables"))
```

Along with numerically looking at the data through descriptive statistics, we can create maps of the study area using the attributes of interest. The following code allows us to visualize the data distribution for both percent French knowledge and mean total income.

```
#map for median income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks",  # Jenks Natural Breaks
              palette = "BuGn", # Sequential palette
              n = 6,            # Number of bins
              border.alpha = 0, # No borders
              colorNA = NA) +  # Grey for NA values
  tm_layout(legend.position = c("LEFT", "TOP"))

#map for French knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "RdBu", 
              n = 6, 
              border.alpha = 0, 
              colorNA = NA) + 
  tm_layout(legend.position = c("LEFT", "TOP")) + tm_scale_bar() +
  tm_compass(position = c("RIGHT", "TOP")) #adding map elements

#display both maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```
![image](https://github.com/user-attachments/assets/894c876d-b016-4df8-9811-f0db07205fc8)

These steps allow us to gain a preliminary understanding of the data.

## Neighborhood Matrix

Now that we have gotten our data ready for further analysis, we can begin the spatial autocorrelation process. In order to conclude whether our attributes are spatially autocorrelated or not, the neighbors of each dataset must be defined. Once defined, each neighbor can be assigned a specific weight that distinguishes them from the rest of the spatial events. By assigning these weights to the neighbors, we can define how spatially correlated the dataset is with itself across the study area (O'Sullivan & Unwin, 2014). In this tutorial, we will go over two methods for defining our neighbors: rooks and queens wights.

#### Rooks

Similar to how a rook moves on a chess board (vertically and horizontally, not diagonally), this method defines neighbors based on direct edge-to-edge connections.

#### Queens

This method defines neighbors based on shared edges and corners, like how a queen moves in chess. This method is overall more inclusive compared to the rooks

To create a list of neighbors with our datasets, we can use the ‘spdep’ package, specifically the ‘poly2nb()’ function. If we want to switch between queen and rook weighting, we can simply select ‘queen = TRUE’ or ‘queen = FALSE’, as shown in the code below:

```
#Income Neighbors - queens weight
Income.nb <- poly2nb(Income_noNA)
Income.net <- nb2lines(Income.nb, coords = st_geometry(Income_noNA))
st_crs(Income.net) <- st_crs(Income_noNA)

#Income Neighbors - rooks wieght
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_geometry(Income_noNA))
st_crs(Income.net2) <- st_crs(Income_noNA)

#French Neighbors - queens wight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_geometry(French_noNA))
st_crs(French.net) <- st_crs(French_noNA)

#French Neighbors - rooks wight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb, coords = st_geometry(French_noNA))
st_crs(French.net2) <- st_crs(French_noNA)
```

If you want a visual aid to see how the queens and rooks weighting method change the selection of neighbors, you can print separate maps as shown below:

```
# Queens map
IncomeQueen <- tm_shape(Income_noNA) + 
  tm_borders(col='lightgrey') + 
  tm_shape(Income.net) + 
  tm_lines(col='blue') +
  tm_layout(title = "Queens", title.position = c("left", "top")) +
  tm_layout(legend.position = c("left", "bottom"))

# Rooks map
IncomeRook <- tm_shape(Income_noNA) + 
  tm_borders(col='lightgrey') + 
  tm_shape(Income.net2) + 
  tm_lines(col='red', lwd = 1) +
  tm_layout(title = "Rooks", title.position = c("left", "top"))

# Combined map
IncomeBoth <- tm_shape(Income_noNA) + 
  tm_borders(col='lightgrey') + 
  tm_shape(Income.net) + 
  tm_lines(col='blue', lwd = 1) +
  tm_shape(Income.net2) + 
  tm_lines(col='red', lwd = 1) +
  tm_scale_bar() + 
  tm_layout(title = "Combined", title.position = c("left", "top")) +
  tm_compass(position = c("right", "top"))

# Print maps in a three-pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)

```
![image](https://github.com/user-attachments/assets/2f5ab8e2-e64b-463b-8982-90942d18210e)

```
# Queens map
FrenchQueen <- tm_shape(French_noNA) + 
  tm_borders(col='lightgray') +
  tm_shape(French.net) + 
  tm_lines(col='purple', lwd = 1) +
  tm_layout(title = "Queens")

# Rooks map
FrenchRook <- tm_shape(French_noNA) + 
  tm_borders(col='lightgray') +
  tm_shape(French.net2) + 
  tm_lines(col='orange', lwd = 1) +
  tm_layout(title = "Rooks")

# Combined map
FrenchBoth <- tm_shape(French_noNA) + 
  tm_borders(col='lightgrey') +
  tm_shape(French.net) + 
  tm_lines(col='purple', lwd = 1) +
  tm_shape(French.net2) + 
  tm_lines(col='orange', lwd = 1) +
  tm_scale_bar() + 
  tm_compass(position = c("right", "top")) +
  tm_layout(title = "Combined")

# Print maps in a three-pane figure
tmap_arrange(FrenchQueen, FrenchRook, FrenchBoth, ncol = 3, nrow = 1)
```
![image](https://github.com/user-attachments/assets/2f08e8da-a264-4743-a657-fa1c35d86221)


### Creating a Weights Matrix

After our neighbors have been assigned through either queens or rook’s weights, we can implement a weight matrix that will quantify the influence of each spatial unit onto the neighboring polygons (Chen, 2021). This matrix assigns weights to each neighboring polygon which determines how much influence each neighbor has. Mathematicall, the neighborhood matrix is represented as W, where each element of W_ij defines the spatial relationship between unit i and unit j (O'Sullivan & Unwin, 2014). If the two units are considered neighbors by the chosen method of either queens or rooks, then W_ij = 1. Otherwise, W_ij = 0. This matrix is the foundation for calculating further autocorrelation statistics such as Morans I.

This weight matrix can be mathematically visualized as followed:

$$
\mathbf{W} = \begin{bmatrix}
w_{11} & w_{12} & \cdots & w_{1n} \\
w_{21} & w_{22} & \cdots & w_{2n} \\
\vdots & \vdots & \ddots & \vdots \\
w_{n1} & \cdots & \cdots & w_{nn}
\end{bmatrix}
$$

In the case of our analysis, we can use the nb2listw function from the spdep package to transform the neighbor list into spatial weights. These produced weight objects allow us to calculate spatial autocorrelation metrics later on in the analysis.

```
#create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

Income.lw$weights[1] #see the weight of the first polygon’s neighbors type
```


## Global Moran's I

Now that we have out weight matrices all set up, we can continue onto the meat of this spatial analysis. The Global Moran’s I assess whether the spatial distribution of a variable is clustered, dispersed or random throughout a study area. More specifically, it determines the degree to which similar values within a dataset, such as income or French knowledge, are spatially clustered (O'Sullivan & Unwin, 2014). A positive Moran’s I value tells us that similar values are clustered together, while a negative value indicates dispersion. Conversely, a Moran’s I value equal to or close to zero tells us that the pattern is likely random. Mathematically, the Moran’s I can be determined using the following equation: 

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

Where, $W_{i,j}$ represents the spatial weight matrix calculated in the previous section of this tutorial. This weight defines the relationship between spatial units $i$ and $j$. $x_i$ and $x_j$ are the observed values for locations $i$ and $j$. $\bar{x}$ is the mean of the variable across the study area, and $n$ is the total number of spatial units.

In this equation, the numerator is measuring the covariance between neighboring spatial units based on the previously calculated weight matrix, while the denominator is standardizing the attributes through normalizing the values based on the variations in the dataset (Babish, 2006). Therefore, high values of $I$ represent positive spatial autocorrelation while low values indicate negative spatial autocorrelation.

To calculate the Moran’s $I$ within R, the moran.test function can be used. The following code segment demonstrates how $I$ was calculated for both median total income and percent French knowledge variables. 

```
#calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]] #moran's I statistic
eIIncome <- miIncome$estimate[[2]] #expected value
varIncome <- miIncome$estimate[[3]] #variance

#calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```

#### Calculating the Range of Moran's I

We can also determine the theoretical range of Moran’s $I$, which is dependent on the spatial weight matrices. This range tells us the possible minimum and maximum values that Moran’s I could be for the given datasets (Babish, 2006). This range was calculated using the following code segment:

```
#function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#calculate the range for the Income variable
range_I <- moran.range(Income.lw)
minRange_I <- range_I[1]
maxRange_I <- range_I[2]

#calculate the range for the French variable
range_F <- moran.range(French.lw)
minRange_F <- range_F[1]
maxRange_F <- range_F[2]
```

#### Calculating Z-test for Significance

Like many other statistical tests, we can test for the significance of our Moran’s $I$ result. We will perform a z-test by calculating a z-score, which compares the compares the observed Moran’s $I$ to the expected Moran’s $I$ and adjusts for variance (Moraga, 2020). This was done in R using the following code:

```
#calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```

#### Moran's I Results 

To display our results for the Global Moran's $I$, we can create a table using the following code:

```
#create a dataframe for displaying the results in a table format
moransI_results <- data.frame(
  Variable = c("Income", "French"),
  Moran_I = c(mIIncome, mIFrench),
  Expected_Value = c(eIIncome, eIFrench),
  Variance = c(varIncome, varFrench),
  Range_Min = c(minRange_I, minRange_F),
  Range_Max = c(maxRange_I, maxRange_F),
  Z_score = c(zIncome, zFrench)
)

#display the table
kable(moransI_results, 
      col.names = c("Variable", "Moran's I", "Expected Value", "Variance", "Range (Min)", "Range (Max)", "Z-score"),
      caption = "Global Moran's I Results for Income and French Variables")
```
![image](https://github.com/user-attachments/assets/1b2ca0fb-72f4-42e2-8640-d4748c0d5e64)

The results for our income layer show a strong positive spatial autocorrelation, with an $I$ value of 0.6197. This tells us that areas with similar income values are clustered together rather than randomly distributed. The expected value for random is close to zero which is what we expect for no spatial autocorrelation. A variance of 0.0015 tells us that the actual distribution of income values deviates from a random distribution. With a high z-score of 16.01, we can conclude that the observed Moran’s $I$ value is far from what is expected under the null hypothesis of no spatial autocorrelation. Therefore, it is safe to conclude that there is spatial clustering within our income layer.

Similar results are shown for the French layer. The Moran’s $I$ value shows a smaller positive spatial autocorrelation compared to the income layer however, the same conclusions can be made as it is still significantly positive.

## Local Moran's I

Like the Global Moran’s $I$, Local Moran’s $I$ can be a great way to test a dataset for spatial autocorrelation. However, instead of focusing on the entire dataset, like the global statistic, Local Moran’s $I$ is analyzing individual polygons by computing the value for one polygon at a time and comparing said value with its neighbors’ (O'Sullivan & Unwin, 2014). Therefore, we can determine specific areas where data is clusters and disperses. Similar to the Global Moran’s $I$ results, positive values for Local Moran’s $I$ tells us that there is a positive spatial autocorrelation while negative indicates negative spatial autocorrelation. Local Moran’s $I$ is very useful in that we can apply a tool known as Local Indicators of Spatial Autocorrelation (LISA). This is used to detect patterns at a local scale within the study area (Scharf, 2022).

The Local Moran’s $I$ statistic can be calculated using the following equation:

$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1}
$$

This equation is similar in a sense to the Global Moran’s $I$ however, instead of calculating a single $I$ for the entire dataset, we are calculating a value for each individual location as well as a variance, z-value, and expected $I$ (Scharf, 2022).

#### Calculations
Since there are going to be a lot of individual calculations to do, we can use the localmoran() function in R to make our lives a lot easier. We just have to ensure that we input our weighting scheme with the correct variable. This can be done using the following code chunk:

```
#calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```

#### Mapping
Unlike the Global Moran's $I$ statistic, this data would be incredibly hard and tedious to analyze within a table. This is because a unique statistic was calculated for each polygon. Therefore, we can utilize other visual interpretation methods such as maps and scatterplots. The code segment below shows us how to create a map showing z-score values for each individual polygon. This same output can be created for the other calculated statistics such as $I$, variance, and expected $I$.

```
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores: Median Income",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = 'grey',
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "RdBu", n = 3)+
  tm_legend(position = c("left", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores: French Knowledge",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = 'grey',
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "RdBu", n = 3)+
  tm_compass(position=c("right", "top"))+
  tm_scale_bar(position=c("right", "bottom"))+
  tm_legend(position = c("left", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```
![image](https://github.com/user-attachments/assets/730290f1-dd30-4ad7-aed9-f2afd6cf2452)


#### Plotting
Similarly, we can present our findings in a scatterplot to show the relationship between each location and it's mean. This plot can be very useful to us in that in helps identify patterns in how similar and dissimilar values are distributed across the study area. This will show us where clusters of similar values occur (either low or high) as well as pointing out where outliers occur (Babish, 2006). In this plot, the x-axis represents the actual value of the variable at each location while the y-axis represents the spatial lag of that value (average of the variable in neighboring locations).

The values that fall in the top right corner of the graph represent locations where both location $i$ and its surrounding neighbors have high values indicating positive spatial autocorrelation (clustering). Points that fall within the bottom left of the graph shows a low value at location $i$ as well as its surrounding neighbors, also indicating positive spatial autocorrelation (clustering). The top left and bottom right of the plot hold values that indicate local outliers (Babish, 2006). It is worth noting that points of significance are marked with a diamond and labeled with their id. These points represent loca5ions where the local Moran’s $I$ statistic is statistically significant. 

To create these scatterplots in R we can utilize the moran.plot function:


```
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL,
           labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL,
           plot = TRUE, return_df = TRUE)
```
![image](https://github.com/user-attachments/assets/a65588bc-a7f1-4f90-a8a8-115813a97024)


This plot suggests that high income areas tend to be surrounded by other areas of high income and vice versa, indicating a positive spatial autocorrelation.

```
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL,
           labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL,
           plot = TRUE, return_df = TRUE)
```
![image](https://github.com/user-attachments/assets/81f49723-0288-413e-83ba-32aaceac11f3)


Similarly, this plot also suggests that areas with high French knowledge are surrounded by other areas of high French knowledge and vice versa.

## Summary

Throughout this tutorial, we conducted a spatial autocorrelation analysis using census data from Kingston, Ontario focusing on percentage of individuals with French knowledge and mean total income. Through both the Global and Local Moran’s $I$ statistic, we determined that there is significant positive spatial autocorrelation in both data datasets, indicating that areas with similar income levels and French knowledge tend to be clustered together rather than randomly distributed. This tutorial should set you up well for future spatial analysis and is overall a great tool to have in your back pocket. For further analysis, you could even try more advanced spatial statistics such as Geographically Weighted Regression (Babish, 2006). This could provide insights to how two variables, such as income and French knowledge, are related to each other over space. By expanding the scope of analysis and incorporating more diverse datasets, we can deepen our understanding of spatial dynamics and inform policy decisions that address real-world issues.

## References

Babish, G. (2006). Geostatistics without tears. Environment Canada. 

Chen, Y. (2021). An analytical process of spatial autocorrelation functions based on Moran’s index. PLOS ONE, 16(4). [https://doi.org/10.1371/journal.pone.0249589]()
 
Government of Canada, Statistics Canada. (2022, June 14). Index to the latest information from the census of population. this survey conducted by Statistics Canada provides a statistical portrait of Canada and its people. the census is a reliable source designed to provide information about people and housing units in Canada by their demographic, social and economic characteristics. information from previous censuses is also available. Census of Population. [https://www12.statcan.gc.ca/census-recensement/index-eng.cfm]()
 
Moraga, P. (2020). Spatial statistics for Data Science: Theory and practice with R. Chapter 8 Spatial autocorrelation. [https://www.paulamoraga.com/book-spatial/spatial-autocorrelation.html]() 
 
O’Sullivan, D., & Unwin, D. (2014). Geographic information analysis. John Wiley & Sons, Incorporated. 

Scharf, H. (2022). Local indicators of Spatial Association (lisa). Wiley StatsRef: Statistics Reference Online, 1–9. [https://doi.org/10.1002/9781118445112.stat08399]()
 
Tobler, W. (2002). Global Spatial Analysis. Computers, Environment and Urban Systems, 26(6), 493–500. [https://doi.org/10.1016/s0198-9715(02)00017-0]()  











