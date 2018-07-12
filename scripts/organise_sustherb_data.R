#### TOPP ####

## A script to organise the sustherb data ##


# Working directories
Anders <- "M:\\Anders L Kolstad\\R\\R_projects\\disentangle"
setwd(Anders)


# Packages
library(readxl)
library(dplyr)
library(reshape2)


# Import data
dat <-  read_excel("data/Dump_sustherbData_12072018.xlsx", 
                                         sheet = "Dump")


# Select and filter (using piping (%>%) and dplyr functions)
dat2 <- dat %>%  
          select(Region,
               LocalityName,
               Treatment,
               Plot,
               Date ='_Date',
               DataTreeID,
               TaxaName,
               Height_cm,
               Diameter_at_base_mm,
               No_oubrowsed_shoots = Unbrowsed,
               Newly_browsed_shoots = Browsed_new,
               Old_brosed_shoots = Browsed_old,
               Browsing_percent)  %>%    # this is total browsed shoots (old and new) divided by total number of counted shoots)
          filter(Region %in% c("Trøndelag", "Telemark",  "Hedmark")) %>%
          mutate(DataTreeID = as.numeric(DataTreeID)) %>%
          mutate(Height_cm  = as.numeric(Height_cm)) %>%
          mutate(Date       = as.Date(Date, "%d.%m.%Y")) %>%
          mutate(Year       = format(Date, "%Y"))
  
#View(dat2)
rm(dat)

# Note:
# Browsing pressure on Juneperus and Picea are never counted (old and new browsed are just recorded as NA) but visually estimated in percentage 


dat2$Treatment[dat2$Treatment == "B"] <- "Open plots"
dat2$Treatment[dat2$Treatment == "UB"] <- "Exclosures"






## ***************************************##
# Quality Control ####
## ***************************************##



# Cheching data for balance and errors etc.:
table(dat2$Region, dat2$Year)
unique(dat2$Region)
table(dat2$LocalityName[dat2$Region=="Hedmark"], dat2$Year[dat2$Region=="Hedmark"])
table(dat2$LocalityName[dat2$Region=="Trøndelag"], dat2$Year[dat2$Region=="Trøndelag"])
table(dat2$LocalityName[dat2$Region=="Telemark"], dat2$Year[dat2$Region=="Telemark"])
  # Looks like we-re still missing lots of data from 2018. This script work for any newly downloaded data from the sustherb database. To start working on a newer data version just export 'as in the dump' from the database and import as 'dat'.



table(dat2$LocalityName, dat2$Treatment)#OK
table(dat$Region, dat$TaxaName) 


table(dat2$LocalityName, dat2$Plot)   
# too many plot names. The once based on compass direction are wrong. I change them to match the others
dat2$Plot[dat2$Plot == "NE"] <- "ØH"
dat2$Plot[dat2$Plot == "NW"] <- "ØV"
dat2$Plot[dat2$Plot == "SE"] <- "NH"
dat2$Plot[dat2$Plot == "SW"] <- "NV"
table(dat2$LocalityName, dat2$Plot)   


# Get numer of recorded tress per year:
tapply(dat2$Height_cm, dat2$Year, FUN = length)  



# Look for duplicate trees:
qq <- dcast(dat2,
            value.var = "Height_cm",
            DataTreeID~Year,
            fun.aggregate = length)
head(qq, 50)
any(qq == 2) # check that jus 1 value/tree/year
#View(qq[which(qq ==2),])
# data ok


# for simplicity:
dat <- dat2;rm(dat2)


# Standardize year (since starting measuremnts; not years since exclosing because differetn timing in different sites...)
dat$nYear <- as.numeric(dat$Year)
dat$std_year <- ""
dat$std_year<- ifelse(dat$Region == "Trøndelag", dat$nYear-2008, dat$std_year)
dat$std_year<- ifelse(dat$Region == "Telemark",  dat$nYear-2009, dat$std_year)
dat$std_year <-ifelse(dat$Region == "Hedmark",   dat$nYear-2012, dat$std_year)
#View(dat)
dat$std_year_n <- as.numeric(dat$std_year)
table(dat$Region, dat$std_year_n)


# Create dataset for individual trees (for quality control only):
wide <- dcast(dat,
              value.var = "Height_cm",
              DataTreeID~std_year_n,
              fun.aggregate = sum)
head(wide)
# unsolved question: what to do with zeros and NAs





# Create dataset with annual tree growth
wide2 <- wide
for(i in 3:ncol(wide)){
  wide2[,i] <- wide[,i]-wide[,i-1]
}
head(wide2)

long <- melt(wide2, 
             id.vars = c("DataTreeID"),
             measure.vars = c("2",  "3",  "4",  "5",  "6",  "7",   "8",  "9",   "10"),    # not carring the first year data...
            value.name = "last_years_growth_cm",
             variable.name = "std_year_n")
head(long)



# Now we need to get this data into the full dataset using match():
# First we need a common varaible, a link between the two datasets.
dat$link <- paste(dat$DataTreeID, dat$std_year_n, sep = " + ")
any(duplicated(dat$link)) #hmm...
length(dat$link)
length(unique(dat$link))   # two duplicates
View(dat[duplicated(dat$link),])   #I don't get it - these are unique....
duplicated(dat$link)
#...ignoring the problem....

long$link <- paste(long$DataTreeID, long$std_year_n, sep = " + ")
any(duplicated(long$link)) # good

dat$last_years_growth_cm <- long$last_years_growth_cm[match(dat$link, long$link)]


# we're getting somewhere now...

# Aggretaing to plot level:
# Here you (Kata) need to make some choices beforehand about what species to includ. I just run this now for all taxa combined (even the 'No occurences' taxa).
aggDat <- aggregate(data = dat,
                    last_years_growth_cm~
                        Region+
                        LocalityName+
                        Treatment+
                        Plot+
                        Year,
                    FUN = sum)
aggDat$sum_of_last_years_growth_cm_per_m2 <- aggDat$last_years_growth_cm/(pi*2^2)
aggDat <- select(aggDat, -last_years_growth_cm)

head(aggDat)
save(aggDat, file= "data\\dataFromAnders.RData")
