################################################################
# This script contains XXXXXXXXXXXXX function that make XXXXXXXXXXXXXXXXXXXXXXX different plots out
# of the California Health Information Survey (CHIS) data set

# The CHIS is the largest state health survey in the US

# # To work properly these functions require:
#   ADULT.sav
#   ggplot2 to be installed
#   haven package
#   dplyr package
#   reshape2
#   ggthemes

### Arguments:
#   titanic_originaldf : data frame with purchases information
#                          (pclass, survived, name, sex, age, sibsp, parch, ticket, fare, cabin, embarked, boat, body, home.dest)

### Return: 
#   Nothing. The plots are saved as pdf files.

### Usage:
#   To run all script do > 

# Author: Paula Andrea Ortiz Otalvaro
# Created:  30-08-2019
# Last modified:   30-08-2019
#
################################################################

# --------------------------------------------------------------
#                        LOAD PACKAGES
# --------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(haven) #to read .sav file
library(reshape2)
library(ggthemes)

# --------------------------------------------------------------
#                        FUNCTIONS
# --------------------------------------------------------------

# ********* Function to get csv file from excel file *************
xls_to_csv <- function(xlsfile, csvfile){
  
  exceldata <- xlsx::read.xlsx(xlsfile, sheetIndex = 1)
  data.table::fwrite(exceldata, file = csvfile)
  
}


# ********* Function: filter data *************



# ****** function: plot distribution of one of the features *******

plot_distribution_titanic <- function(titanic_originaldf, feature_x, feature_fill){
    
    # Print the plot to a pdf file
    pdf( paste0(feature_x,"_distribution_withfill_",feature_fill,".pdf")  )
  
    # make plot  
    plot_featuredistribution <- ggplot(titanic, aes_string(x = feature_x , fill = feature_fill)) +
                                geom_bar(position = "dodge")
    
    print(plot_featuredistribution)
    dev.off()
}














# --------------------------------------------------------------
#             CALL FUNCTION 
# --------------------------------------------------------------

# *********************** Get csv file (uncomment when needed) ***********************
#xls_to_csv(xlsfile = "titanic3.xls", csvfile = "titanic_original.csv")

# *********************** Load data ***********************
chis09 <- haven::read_sav("./chis09_adult_spss/ADULT.sav")

#*********************** wrangle data *********************
# NOTE: Ethnicities RACEPR2
# 1 LATINO 
# 4 ASIAN 
# 5 AFR. AMER. 
# 6 WHITE

chis_subset <- chis09 %>% 

              #subset data to columns of interest
              select(RBMI, BMI_P, RACEHPR2, SRSEX, SRAGE_P, MARIT2, AB1, ASTCUR, AB51, POVLL) %>%

              #get rows with required ethnicities
              filter(RACEHPR2 %in% c(1,4,5,6)) 




# *********************** Look at data structure ***********************
# I. Plot the univariate statistics of interest:

# 1. age Histogram: simple
pdf("./chis09_plots/1_age_histogram_simple.pdf")
ggplot(chis_subset, aes(x = SRAGE_P) ) + geom_histogram()
dev.off()

# 2. BMI Histogram: simple
pdf("./chis09_plots/2_bmi_histogram_simple.pdf")
ggplot(chis_subset, aes(x = BMI_P) ) + geom_histogram()
dev.off()


# 3. relation between BMI=weight/height^2 and age
pdf("./chis09_plots/3_age_vs_bmi_simple.pdf")
ggplot(chis_subset, aes(x = SRAGE_P, y = BMI_P) ) + geom_point()
dev.off()



# ***********************  Clean data ***********************

# Keep adults younger than or equal to 84
chis_subset <- chis_subset[chis_subset$SRAGE_P <= 84, ] 

# Keep chis_subsets with BMI at least 16 and less than 52
chis_subset <- chis_subset[chis_subset$BMI_P >= 16 & chis_subset$BMI_P < 52, ]

# Relabel the race variable
chis_subset$RACEHPR2 <- factor(chis_subset$RACEHPR2, labels = c("Latino", "Asian", "African American", "White"))

# Relabel the BMI categories variable
chis_subset$RBMI <- factor(chis_subset$RBMI, labels = c("Under-weight", "Normal-weight", "Over-weight", "Obese"))


# ************ Customizations: Color and theme **************

# The color scale used in the plot
BMI_fill <- scale_fill_brewer("BMI Category", palette = "Reds")

# Theme to fix category display in faceted plot
fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                    strip.background = element_blank(),
                    legend.position = "none")



# ************ Histograms using customizations **************

# 4. age histogram , separate according to BMI categories and using BMI_fill color palette
pdf("./chis09_plots/4_age_historam_withBMIcategories.pdf")
ggplot(chis_subset, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1) +
  BMI_fill
dev.off()

# 5. age density histogram , separate according to BMI categories and using BMI_fill color palette
pdf("./chis09_plots/5_age_densityhistoram_withBMIcategories.pdf")
ggplot(chis_subset, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  BMI_fill
dev.off()

# ******************  Multiple Histograms *******************

# 6. Histogram, add BMI_fill and customizations
pdf("./chis09_plots/6_age_facetedhistograms_withBMIcategories.pdf")
ggplot(chis_subset, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1) +
  fix_strips +
  BMI_fill +
  facet_grid(RBMI ~ .) +
  theme_classic()
dev.off()


# 7. Faceted density histogram
pdf("./chis09_plots/7_age_faceted_densityhistograms_withBMIcategories.pdf")
ggplot(chis_subset, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  BMI_fill +
  facet_grid(RBMI ~ .)
dev.off()



# 8. Density histogram with position = "fill"
pdf("./chis09_plots/8_age_densityhistograms_withBMIcategories_posfill.pdf")
ggplot(chis_subset, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, position="fill") +
  BMI_fill
dev.off()



# ******************  Facet Frequency Histogram *******************

# An attempt to facet the accurate frequency histogram from before (failed)
#ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
#  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, position = "fill") +
#  BMI_fill +
#  facet_grid(RBMI ~ .)

# Create DF with table()
DF <- table(chis_subset$RBMI, chis_subset$SRAGE_P)

# Use apply on DF to get frequency of each group
DF_freq <- apply(DF, 2, function(x) x/sum(x))

# Load reshape2 and use melt on DF to create DF_melted
library(reshape2)
DF_melted <- melt(DF_freq)

# Change names of DF_melted
names(DF_melted) <- c("FILL", "X", "value")

# Add code to make this a faceted plot
pdf("./chis09_plots/9_age_faceted_densityhistograms_withBMIcategories_posstack.pdf")
ggplot(DF_melted, aes(x = X, y = value, fill = FILL)) +
  geom_col(position = "stack") +
  BMI_fill + 
  facet_grid(FILL ~ .) # Facets
dev.off()

# ****************** Mosaic plot 1 *******************
# The initial contingency table
DF <- as.data.frame.matrix(table(chis_subset$SRAGE_P, chis_subset$RBMI))

# Create groupSum, xmax and xmin columns
DF$groupSum <- rowSums(DF)
DF$xmax <- cumsum(DF$groupSum)
DF$xmin <- DF$xmax - DF$groupSum
# The groupSum column needs to be removed; don't remove this line
DF$groupSum <- NULL

# Copy row names to variable X
DF$X <- row.names(DF)

# Melt the dataset
#library(reshape2)
DF_melted <- melt(DF, id.vars = c("X", "xmin", "xmax"), variable.name = "FILL")

# dplyr call to calculate ymin and ymax - don't change
#library(dplyr)
DF_melted <- DF_melted %>%
  group_by(X) %>%
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))

# Plot rectangles
pdf("./chis09_plots/10_age_bmi_mosaic_simple.pdf")
#library(ggthemes)
ggplot(DF_melted, aes(ymin = ymin,
                      ymax = ymax,
                      xmin = xmin,
                      xmax = xmax,
                      fill = FILL)) +
  geom_rect(colour = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  BMI_fill +
  theme_tufte()
dev.off()


# ****************** Mosaic plot 2 *******************
# Perform chi.sq test (RBMI and SRAGE_P)
results <- chisq.test(table(chis_subset$RBMI, chis_subset$SRAGE_P))

# Melt results$residuals and store as resid
resid <- melt(results$residuals)

# Change names of resid
names(resid) <- c("FILL", "X", "residual")

# merge the two datasets:
DF_all <- merge(DF_melted, resid)

# Position for labels on y axis (don't change)
index <- DF_all$xmax == max(DF_all$xmax)
DF_all$yposn <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2


# Position for labels on x axis
DF_all$xposn <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2



# Update plot command
pdf("./chis09_plots/11_age_bmi_mosaic_withchisquare.pdf")
library(ggthemes)
ggplot(DF_all, aes(ymin = ymin,
                   ymax = ymax,
                   xmin = xmin,
                   xmax = xmax,
                   fill = residual)) +
  geom_rect() +
  scale_fill_gradient2() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_tufte() + #%+%
  
  #DF_all + 
  geom_text(aes(x = max(xmax), 
                y = yposn,
                label = FILL),
            size = 3, hjust = 1,
            show.legend  = FALSE) + # %+%


  # geom_text for ages (i.e. the x axis)
  #DF_all + 
  geom_text(aes(x = xposn, label = X),
            y = 1, angle = 90,
            size = 3, hjust = 1,
            show.legend = FALSE)

  
dev.off()



################
# Function to make the mosaic plot

# function(data, X, FILL) {
#   # Proportions in raw data
#   DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
#   DF$groupSum <- rowSums(DF)
#   DF$xmax <- cumsum(DF$groupSum)
#   DF$xmin <- DF$xmax - DF$groupSum
#   DF$X <- row.names(DF)
#   DF$groupSum <- NULL
#   DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
#   DF_melted <- DF_melted %>%
#     group_by(X) %>%
#     mutate(ymax = cumsum(value/sum(value)),
#            ymin = ymax - value/sum(value))
#   
#   # Chi-sq test
#   results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
#   resid <- melt(results$residuals)
#   names(resid) <- c("FILL", "X", "residual")
#   
#   # Merge data
#   DF_all <- merge(DF_melted, resid)
#   
#   # Positions for labels
#   DF_all$xposn <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
#   index <- DF_all$xmax == max(DF_all$xmax)
#   DF_all$yposn <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
#   
#   # Plot
#   g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin,
#                           xmax = xmax, fill = residual)) +
#     geom_rect(col = "white") +
#     geom_text(aes(x = xposn, label = X),
#               y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
#     geom_text(aes(x = max(xmax),  y = yposn, label = FILL),
#               size = 3, hjust = 1, show.legend = FALSE) +
#     scale_fill_gradient2("Residuals") +
#     scale_x_continuous("Individuals", expand = c(0,0)) +
#     scale_y_continuous("Proportion", expand = c(0,0)) +
#     theme_tufte() +
#     theme(legend.position = "bottom")
#   print(g)
# }



