# Lake dataset A
# Data import and cleaning
# Last updated 21 April 2022
# Colette Ward


library(tidyverse); library(readxl); library(lubridate); library(tidyxl); library(unpivotr)



#######################################
#######################################
#######################################

# 2. Morphometrics data (lake depth)

a.morph.raw <- read_csv("../data/dataset-a/morph.csv")

a.morph <- a.morph.raw %>% 
  select(site = `Sample\rSite`, year = Year, date = `Date\r(m/d/y)`, 
         profileDepth = `Profile\rDepth (m)`, maxDepth = `Max Depth`) %>%
  mutate(date = dmy(date),
         profileDepth = str_replace(profileDepth, "-", NA_character_),
         profileDepth = as.numeric(profileDepth),
         maxDepth = str_replace(maxDepth, "-|not recorded", NA_character_),
         maxDepth = as.numeric(maxDepth)) %>% 
  
  # group sites within lakes, to calculate max depth lake-wide
  mutate(lake = case_when(site == "CL" ~ "C Lake",
                          str_detect(site, "H..") ~ "H Lake",
                          str_detect(site, "WB.+") ~ "W 7",
                          str_detect(site, "UTR.*") ~ "T River",
                          site %in% c("North Basin", "South Basin") ~ "?")) %>%
  
  # fix typo for H-1, 7-Sept-2017; 
  mutate(maxDepth = case_when(maxDepth == 163 ~ 16.3,
                              TRUE ~ maxDepth)) %>%
  
  # find maximum depth
  # take max depth, or use largest number in Profile Depth column:
  group_by(lake, year) %>% 
  summarize(across(c(profileDepth, maxDepth), ~ max(.x, na.rm = T))) %>%
  ungroup() %>% 
  mutate(across(c(profileDepth, maxDepth), ~ na_if(.x, -Inf))) %>% 
  mutate(lakeMaxDepth = case_when(!is.na(maxDepth) ~ maxDepth,
                                  is.na(maxDepth) ~ profileDepth)) %>% # else use max in profileDepth column
  
  filter(!lake %in% c("T River", "?")) %>% 
  select(lake, year, lakeMaxDepth) %>% 
  rename(maxDepth = lakeMaxDepth) %>% # rename to match other datasets
  mutate(surfaceAreaHa = case_when(lake == "H Lake" ~ 76.7),
         meanDepth = case_when(lake == "H Lake" ~ 7.2))

#######################################
#######################################
#######################################

# 3. Nutrients & Chlorophyll a

# TP, TKN data are available from several programs sometimes on the same dates
# use Analyte Category == "Nutrients and BOD" (or "Major Ions" when the former is not available) for TP because detection limit is lower than for TP metric in "Total Metals"
# do not use data when Result Flag == "<"

# use Analyte Category == "Nutrients and BOD" for all parameters - most consistent
# except pH, which is in Analyte Category == "Conventional Parameters"

a.nutChl.raw <- read_excel("../data/dataset-a/nut.xlsx", 
                                 sheet = "H_2008 to 2021_data", 
                                 col_types = c("text", "text", "text", "date", 
                                               "numeric", "text", "text", "text", 
                                               "text", "text", "numeric", "text", "text"))

a.nutChl <- a.nutChl.raw %>% 
  rename_with(tolower) %>%
  rename(lake = waterbody) %>% 
  mutate(date = ymd(date),
         month = month(date)) %>% 
  mutate(lake = case_when(lake == "WB-7" ~ "W 7",
                          TRUE ~ lake)) %>% 
  filter(lake != "T River",
         `analyte category` == "Nutrients and BOD") %>% 
  mutate(analyte = case_when(analyte %in% c("chlorophyll a", "Chlorophyll a") ~ "chl",
                             analyte %in% c("nitrogen - Kjeldahl", "Nitrogen - Kjeldahl", "Nitrogen – Kjeldahl", "Total Kjeldahl Nitrogen", "Total Total Kjeldahl Nitrogen") ~ "TKN",
                             analyte %in% c("phosphorus - total", "Phosphorus - total", "Phosphorus – total", "Phosphorus (P)-Total", "Total Phosphorus", "Total Phosphorus (P)") ~ "TP",
                             TRUE ~ analyte)) %>% 
  filter(analyte %in% c("chl", "TKN", "TP")) %>% 
  select(lake, year, season, month, date, analyte, result) %>%
  
  # calculate means of replicates
  group_by(lake, year, season, month, date, analyte) %>% 
  summarize(meanDailyValue = mean(result)) %>% 
  ungroup() %>% 
  
  filter(month %in% seq(5, 10, 1)) %>% # keep only May - October data
  
  # stop here to run data vis for sampling frequency
  
  # calculate monthly means
  group_by(lake, year, month, analyte) %>% 
  summarize(monthlyMeanValue = mean(meanDailyValue)) %>%
  ungroup() %>% 
  
  spread(key = analyte, value = monthlyMeanValue)


#######################################
#######################################
#######################################

# 4. Phytoplankton

a.phyto.path <- "../data/dataset-a/phyto.xlsx"

a.phyto.1 <- xlsx_cells(a.phyto.path) %>%
  filter(!(sheet %in% c("2008 H working", "2009 H working", "2010 working", "2011 working", "2012", "2013", "2014 working",
                        "2015 working", "2016 working", "2017 working", "2018 working"))) %>% 
  mutate(sheet = str_trim(sheet)) %>%
  
  filter(case_when(sheet == "2006" ~ col <= 13 & row %in% c(3:51),
                   sheet == "2007" ~ col <= 13 & row %in% c(2, 4:47),
                   sheet == "2008 CL" ~ col <= 7 & row %in% c(3, 5:107),
                   sheet == "2008 H" ~ col <= 55 & row %in% c(2, 4:106),
                   sheet == "2009 CL" ~ col <= 7 & row %in% c(2, 3, 5:104),
                   sheet == "2009 H" ~ col <= 67 & row %in% c(2, 3, 5:135),
                   sheet == "2010" ~ col <= 25 & row %in% c(3, 4, 6:77),
                   sheet == "2011" ~ col <= 116 & row %in% c(1, 2, 4:92),
                   sheet == "2012 working" ~ col <= 37 & row %in% c(2, 3, 5:110),
                   sheet == "2013 working" ~ col <= 19 & row %in% c(2, 3, 5:82),
                   sheet == "2014" ~ col <= 26 & row %in% c(2, 3, 5, 8:19, 21:30, 32:38, 40:45, 47:70, 72:74, 76, 77),
                   sheet == "2015" ~ col <= 19 & row %in% c(2, 3, 5:75),
                   sheet == "2016" ~ col <= 21 & row %in% c(2, 3, 5:80),
                   sheet == "2017" ~ col <= 19 & row %in% c(2, 3, 5:93),
                   sheet == "2018" ~ col <= 19 & row %in% c(2, 3, 5:95),
                   sheet == "2019" ~ col <= 20 & row %in% c(1, 3, 4, 7:97),
                   sheet == "2020" ~ col <= 7 & row %in% c(2, 4:23)
  ))




# (a) Function to unpivot 2006-11, 2016-18 data:
unpivot.g1.function <- function(x) {
  dat <- x %>% 
    filter(!is_blank,
           !character %in% c("Site/Lake ID", "Sample ID", "Lake ID", "Sample ID", "Date sampled", "Date Sampled",
                             "BACILLARIOPHYCEAE", "CHLOROPHYCEAE", "CHLOROPHYTES", "CHRYSOPHTYES", "CHRYSOPHYCEAE", 
                             "CRYPTOPHYCEAE", "CRYPTOPHYTES", "CYANOBACTERIA", "DIATOMS", "DINOFLAGELLATES", "DINOPHYCEAE",
                             "EUGLENOPHYCEAE", "EUGLENOPHYTES", "XANTHOPHYCEAE", "Taxon", 
                             "Bacillariophyceae", "Chlorophyceae", "Chrysophyceae", "Cryptophyceae", "Cyanophyceae", "Euglenophyceae")) %>% 
    behead("up-left", "site") %>%
    behead("up-left", "samplingDate") %>% 
    mutate(character = str_trim(character, side = c("both"))) %>% # remove white space before & after Density and Biomass
    behead_if(character %in% c("Density", "Biomass", "Abundance"),
              direction = "up", name = "metric") %>% 
    behead("left", "taxon") %>%
    mutate(date1 = as.character(samplingDate)) %>% # convert to character so I can fix dates below
    select(sheet, site, date1, taxon, metric, value = numeric)
}
a.phyto.1.g1 <- a.phyto.1 %>% 
  filter(sheet %in% c("2006", "2007", "2008 CL", "2008 H", "2009 CL", "2009 H", "2010", "2011", "2016", "2017", "2018"))
a.phyto.g1_list <- split(a.phyto.1.g1, f=a.phyto.1.g1$sheet)
a.phyto.g1.proc <- map_dfr(a.phyto.g1_list, unpivot.g1.function)


# (b) Function to unpivot 2012, '13, '19 data:
unpivot.g2.function <- function(x) {
  dat <- x %>% 
    filter(!is_blank,
           !character %in% c("Date sampled", "Sample ID", "Station", "Variable",
                             "CHLOROPHYTES", "CHRYSOPHTYES", "CRYPTOPHYTES", "CYANOBACTERIA", 
                             "DIATOMS", "DINOFLAGELLATES", "EUGLENOPHYTES",
                             "CHLOROPHYCEAE", "CHRYSOPHYCEAE", "CRYPTOPHYCEAE", "DINOPHYCEAE", "EUGLENOPHYCEAE")) %>%
    behead("up", "site") %>%
    behead("up", "samplingDate") %>% 
    mutate(character = str_trim(character, side = c("both"))) %>% # remove white space before & after Abundance and Biomass
    behead_if(character %in% c("Abundance", "Biomass"),
              direction = "up", name = "metric") %>%
    behead("left", "taxon") %>%
    mutate(date1 = as.character(samplingDate)) %>% # convert to character so I can fix dates below
    select(sheet, site, date1, taxon, metric, value = numeric)
}
a.phyto.1.g2 <- filter(a.phyto.1, sheet %in% c("2012 working", "2013 working", "2019"))
a.phyto.g2_list <- split(a.phyto.1.g2, f=a.phyto.1.g2$sheet)
a.phyto.g2.proc <- map_dfr(a.phyto.g2_list, unpivot.g2.function)



# (c) Function to unpivot 2014 & 2015 data:
unpivot.g3.function <- function(x) {
  dat <- x %>% 
    filter(!is_blank,
           !character %in% c("Date sampled", "Sample ID", "BACILLAIROPHYCEAE", "CHLOROPHYCEAE", "CHRYSOPHYCEAE", 
                             "CRYPTOPHYCEAE", "CYANOBACTERIA", "DIATOMS", "DINOPHYCEAE", "EUGLENOPHYCEAE")) %>% 
    behead("up-left", "samplingDate") %>%
    behead("up-left", "site") %>%
    mutate(character = str_trim(character, side = c("both"))) %>% # remove white space before & after Density and Biomass
    behead_if(character %in% c("Density", "Biomass", "Abundance"),
              direction = "up", name = "metric") %>%
    behead("left", "taxon") %>%
    mutate(date1 = as.character(samplingDate)) %>% # convert to character so I can fix dates below
    select(sheet, site, date1, taxon, metric, value = numeric)
}
a.phyto.1.g3 <- filter(a.phyto.1, sheet %in% c("2014", "2015"))
a.phyto.g3_list <- split(a.phyto.1.g3, f=a.phyto.1.g3$sheet)
a.phyto.g3.proc <- map_dfr(a.phyto.g3_list, unpivot.g3.function)




# (d) Function to unpivot 2020 data:
unpivot.g4.function <- function(x) {
  dat <- x %>% 
    filter(!is_blank,
           !character %in% c("Site ID", "CHLOROPHYCEAE", "CHRYSOPHYCEAE", "CRYPTOPHYCEAE", "DIATOMS")) %>% 
    behead("up-left", "site") %>%
    mutate(character = str_trim(character, side = c("both"))) %>% # remove white space before & after Density and Biomass
    behead_if(character %in% c("Density", "Biomass"),
              direction = "up", name = "metric") %>%
    behead("left", "taxon") %>%
    mutate(date1 = "2020-09-20") %>% 
    select(sheet, site, date1, taxon, metric, value = numeric)
}
a.phyto.1.g4 <- filter(a.phyto.1, sheet == "2020")
a.phyto.g4_list <- split(a.phyto.1.g4, f=a.phyto.1.g4$sheet)
a.phyto.g4.proc <- map_dfr(a.phyto.g4_list, unpivot.g4.function)



# bind all df's back together and continue with processing: 
a.phyto.proc <- a.phyto.g1.proc %>% 
  bind_rows(a.phyto.g2.proc) %>% 
  bind_rows(a.phyto.g3.proc) %>%
  bind_rows(a.phyto.g4.proc) %>%
  
  # fix dates
  mutate(date1 = case_when(date1 == "2001-09-21" ~ "2009-09-21", # sheet == "2009 HZL"
                           date1 == "2012-07-11" ~ "2011-07-12", # for 2011
                           date1 == "2014-06-11" ~ "2011-06-14", # for 2011
                           TRUE ~ date1)) %>%
  
  mutate(date = parse_date_time(date1, c("dmy", "Ymd"))) %>% # convert to date format
  mutate(year = year(date),
         month = month(date)) %>%
  
  mutate(lake = case_when(str_detect(site, "HL|Hl|HI") ~ "H Lake",
                          site %in% c("Rep 1", "Rep 2", "Rep 3") ~ "H Lake", # for 2019
                          str_detect(site, "CL") ~ "C Lake",
                          str_detect(site, "WB7") ~ "W 7")) %>% 
  
  mutate(replicate = case_when(str_detect(site, "1$|A") ~ "1",
                               str_detect(site, "2|B$") ~ "2",
                               str_detect(site, "3|C$") ~ "3"
  )) %>% 
  
  filter(metric == "Biomass") %>% 
  
  # calculate total biomass for each lake, year, month, date, replicate combo
  group_by(lake, year, month, date, replicate) %>% 
  summarize(totPhyto.replicate = sum(value)) %>% 
  ungroup() %>% 
  
  # calculate mean across replicates
  group_by(lake, year, month, date) %>%
  summarize(totPhyto.date = mean(totPhyto.replicate)) %>% 
  ungroup() %>% 
  
  # stop here to run data vis for sampling frequency (line 570)
  
  # calculate monthly means
  group_by(lake, year, month) %>%
  summarize(meanMonthlyPhyto = mean(totPhyto.date)) %>% 
  ungroup()


#######################################
#######################################
#######################################

# 5. Zooplankton

a.zoop.path <- "../data/dataset-a/zoop.xlsx"


a.zoop.1 <- xlsx_cells(a.zoop.path) %>% 
  filter(!(sheet %in% c("2008 working", "2009 H working", "2010 working", "2011", "2012",
                        "2013", "2014", "2015 working", "2016 working", "2017 working", "2018 working"))) %>% 
  filter(case_when(sheet == "2006 CL WB-7" ~ col <= 18 & row %in% c(3:43),
                   sheet == "2007 CL WB7" ~ col <= 18 & row %in% c(2:37),
                   sheet == "2008 CL WB7" ~ col <= 58 & row %in% c(2, 3, 5:27, 30, 31, 34:36, 39:41, 44:49),
                   sheet == "2008 H" ~ col %in% c(1:13, 15:24, 26:35, 37:46, 48:57, 59:64) & row %in% c(2, 3, 5:50),
                   sheet == "2009 CL" ~ col <= 7 & row %in% c(2:53),
                   sheet == "2009 H" ~ col <= 57 & row %in% c(2:31, 35:38, 42:45, 49:56),
                   sheet == "2010" ~ col <= 31 & row %in% c(3, 4, 6:43),
                   sheet == "2011 working" ~ col <= 31 & row %in% c(1, 2, 4:43),
                   sheet == "2012 working" ~ col <= 37 & row %in% c(1, 2, 4:42),
                   sheet == "2013 working" ~ col <= 19 & row %in% c(1, 2, 4:43),
                   sheet == "2014 working" ~ col <= 19 & row %in% c(2, 3, 5, 8:22, 26:28, 32:34, 38:43),
                   sheet == "2015" ~ col <= 21 & row %in% c(2, 3, 5:37),
                   sheet == "2016" ~ col <= 27 & row %in% c(2, 3, 5:37),
                   sheet == "2017" ~ col <= 27 & row %in% c(2, 3, 5:43),
                   sheet == "2018" ~ col <= 19 & row %in% c(2, 3, 5:35),
                   sheet == "2019" ~ col %in% c(1:17, 20:21) & row %in% c(2:4, 7:11, 15:17, 20:23, 26:27, 30:49),
                   sheet == "2020" ~ col <= 7 & row %in% c(2, 4:24)
  ))


# (a) Function to unpivot 2006-10, 2015-18 data:
unpivot.zoop.g1.function <- function(x) {
  dat <- x %>% 
    filter(!is_blank,
           !character %in% c("Date", "Date sampled", "Date Sampled", "Lake ID", "Lake/Site ID", "Sample ID", "Site ID",
                             "Calanoida", "CALANOIDA", "CILIOPHORA", "Cladocera", "CLADOCERA", "CLADOCERA ", 
                             "Cyclopoida", "CYCLOPOIDA", "OTHERS (CILIOPHORA)", "Rotifera", "ROTIFERA", "Taxon")) %>%
    
    behead("up-left", "site") %>%
    behead("up-left", "samplingDate") %>% 
    behead_if(character %in% c("Density", "Biomass"),
              direction = "up", name = "metric") %>% 
    behead("left", "taxon") %>%
    
    # 2010: samplingDate is a character, format = 9‐Jun‐10
    # rest: samplingDate is POSIXct, format = 2006-10-01
    
    mutate(date1 = as.character(samplingDate)) %>% # convert to character so I can fix dates below
    select(sheet, site, date1, taxon, metric, value = numeric)
}

a.zoop.1.g1 <- a.zoop.1 %>% 
  filter(sheet %in% c("2006 CL WB-7", "2007 CL WB7", "2008 CL WB7", "2008 H", "2009 CL", "2009 H", "2015", "2016", "2017", "2018"))
a.zoop.g1_list <- split(a.zoop.1.g1, f=a.zoop.1.g1$sheet)
a.zoop.g1.proc <- map_dfr(a.zoop.g1_list, unpivot.zoop.g1.function)


a.zoop.1.g2 <- a.zoop.1 %>% filter(sheet == "2010") # run separately because of date formatting error
a.zoop.g2_list <- split(a.zoop.1.g2, f=a.zoop.1.g2$sheet)
a.zoop.g2.proc <- map_dfr(a.zoop.g2_list, unpivot.zoop.g1.function)




# (b) Function to unpivot 2014 data
unpivot.zoop.g3.function <- function(x) {
  dat <- x %>% 
    filter(!is_blank,
           !character %in% c("Sample ID", "Date sampled")) %>%
    
    behead("up-left", "samplingDate") %>% 
    behead("up", "site") %>%
    behead_if(character %in% c("Abundance", "Biomass"),
              direction = "up", name = "metric") %>%
    behead("left", "taxon") %>% 
    
    mutate(date1 = as.character(samplingDate)) %>% # convert to character so I can fix dates below
    select(sheet, site, date1, taxon, metric, value = numeric)
}


a.zoop.1.g3 <- filter(a.zoop.1, sheet == "2014 working")
a.zoop.g3_list <- split(a.zoop.1.g3, f=a.zoop.1.g3$sheet)
a.zoop.g3.proc <- map_dfr(a.zoop.g3_list, unpivot.zoop.g3.function)



# (c) Unpivot 2019 data
a.zoop.1.g4 <- filter(a.zoop.1, sheet == "2019")
a.zoop.g4_list <- split(a.zoop.1.g4, f=a.zoop.1.g4$sheet)

a.zoop.g4.proc <- map_dfr(a.zoop.g4_list, unpivot.zoop.g1.function)



# (d) Function to unpivot 2020 data
unpivot.zoop.g5.function <- function(x) {
  dat <- x %>% 
    filter(!is_blank,
           !character %in% c("Sample ID",
                             "CALANOIDA", "CLADOCERA ", "CYCLOPOIDA", "OTHERS (CILIOPHORA)", "ROTIFERA")) %>% 
    
    behead("up-left", "site") %>%
    behead_if(character %in% c("Density", "Biomass"),
              direction = "up", name = "metric") %>%
    behead("left", "taxon") %>% 
    
    mutate(date1 = "2020-09-20") %>%
    select(sheet, site, date1, taxon, metric, value = numeric)
}


a.zoop.1.g5 <- filter(a.zoop.1, sheet == "2020")
a.zoop.g5_list <- split(a.zoop.1.g5, f=a.zoop.1.g5$sheet)
a.zoop.g5.proc <- map_dfr(a.zoop.g5_list, unpivot.zoop.g5.function)




# (e) Function to unpivot 2011-13 data:
unpivot.zoop.g6.function <- function(x) {
  dat <- x %>% 
    filter(!is_blank,
           !character %in% c("Sample ID", "Site ID", "Date sampled",
                             "CALANOIDA", "CILIOPHORA", "CILIPHORA", "CLADOCERA", "CYCLOPOIDA", 
                             "ROTIFERA")) %>% 
    
    behead("up", "site") %>%
    behead("up", "samplingDate") %>% 
    behead_if(character %in% c("Abundance", "Biomass"),
              direction = "up", name = "metric") %>% 
    behead("left", "taxon") %>%
    
    mutate(date1 = as.character(samplingDate)) %>% # convert to character so I can fix dates below
    select(sheet, site, date1, taxon, metric, value = numeric)
}

a.zoop.1.g6 <- filter(a.zoop.1, sheet %in% c("2011 working", "2012 working", "2013 working"))
a.zoop.g6_list <- split(a.zoop.1.g6, f=a.zoop.1.g6$sheet)
a.zoop.g6.proc <- map_dfr(a.zoop.g6_list, unpivot.zoop.g6.function)


######################


# create lists of rotifer & ciliophore genuses
rotifera <- c("Ascomorpha", "Asplanchna", "Brachionus", "Conochilus", "Collotheca", "Euchlanis",
              "Filinia", "Gastropus", "Kellicotia", "Kellicottia", "Keratella", "Lecane", "Lepadella", 
              "Lophocharis", "Monostyla", "Mytlilina", "Notholca", "Notomata", "Operculata", "Ploesoma", 
              "Polyarthra", "Polyathra", "Pompholyx", "Synchaeta", "Trichocerca", "Testudinella", "Trichotria")
ciliophora <- c("Opercularia", "Vorticella")

taxonomists <- c("Fischer", "\\(O.F. Muller\\)", "O.F. Muller", "Birge", 
                 "S. A. Forbes", "S.A. Forbes", "Sars", "Coker", "Leydig", 
                 "Lilljeborg\\(c\\)", "Lilljeborg", "Focke")


######################


# load zooplankton mean weight file
# units are ug DRY
load("../formattedData/zoop.conversions.RData")

# change taxon to assignedName to faciliate joining to field data below
zoop.conversions <- rename(zoop.conversions, assignedName = taxon)


######################

# bind the partially-processed dataframes and continue with data cleaning:
a.zoop.proc <- a.zoop.g1.proc %>% 
  bind_rows(a.zoop.g2.proc) %>% 
  bind_rows(a.zoop.g3.proc) %>%
  bind_rows(a.zoop.g4.proc) %>%
  bind_rows(a.zoop.g5.proc) %>%
  bind_rows(a.zoop.g6.proc) %>%
  
  # fix dates for 2008 H spreadsheet
  mutate(date1 = case_when((date1 == "2011-08-08" & sheet == "2008 H") ~ "2008-08-11",
                           date1 == "2014-10-08" ~ "2008-10-14",
                           date1 == "2016-09-08" ~ "2008-09-16",
                           date1 == "2021-07-08" ~ "2008-07-21",
                           date1 == "2021-08-08" ~ "2008-08-21",
                           date1 == "2028-09-08" ~ "2008-09-28",
                           date1 == "2028-10-08" ~ "2008-10-28",
                           TRUE ~ date1)) %>%
  
  mutate(date = parse_date_time(date1, c("Ymd", "dmy"))) %>% # convert to date format
  
  mutate(year = year(date),
         month = month(date)) %>%
  
  mutate(lake = case_when(str_detect(site, "HL|Hl|Rep") ~ "H Lake",
                          str_detect(site, "CL") ~ "C Lake",
                          str_detect(site, "WB") ~ "W 7")) %>% 
  filter(!is.na(lake)) %>% 
  
  mutate(replicate = case_when(str_detect(site, "1$|A") ~ "1",
                               str_detect(site, "2|B$") ~ "2",
                               str_detect(site, "3|C$") ~ "3"
  )) %>%
  
  filter(metric %in% c("Density", "Abundance")) %>% 
  
  # convert #/m3 to #/L for all years except 2014 & 2019
  mutate(value = case_when(year %in% c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 
                                       2015, 2016, 2017, 2018, 2020) ~ value/1000,
                           TRUE ~ value)) %>% 
  
  # remove rotifers and ciliophora
  filter(!str_detect(taxon, paste(rotifera, collapse = "|")),
         !str_detect(taxon, paste(ciliophora, collapse = "|"))) %>% 
  
  
  # clean up taxonomic names to match the zoop mean weight file:
  mutate(taxon = str_remove(taxon, paste(taxonomists, collapse = "|")), # remove taxonomist names
         taxon = str_remove_all(taxon, "\\(|\\)"), # remove brackets
         taxon = str_squish(taxon), # remove trailing spaces and extra spaces between words
         
         # fix typos:
         taxon = str_replace(taxon, "Cerodaphnia", "Ceriodaphnia"),
         taxon = str_replace(taxon, "Chydorus sphericus", "Chydorus sphaericus"),
         taxon = str_replace(taxon, "Daphnia geleatea", "Daphnia galeatea"),
         taxon = str_replace(taxon, "Daphnia rosa", "Daphnia rosea"),
         taxon = str_replace(taxon, "Dicyclops bicuspidatus", "Diacyclops bicuspidatus"),
         taxon = str_replace(taxon, "Diaphanososma", "Diaphanosoma"),
         taxon = str_replace(taxon, "Leptodora kindti$", "Leptodora kindtii"),
         taxon = str_replace(taxon, "Pleroxus sp", "Pleuroxus sp."),
         taxon = str_replace(taxon, "Diaptomus oregonensis", "Skistodiaptomus oregonensis"), # update to valid name
         
         # make taxonomic names match those in mean weight file:
         taxon = str_replace(taxon, "Ceriodaphnia sp", "Ceriodaphnia sp."),
         taxon = str_replace(taxon, "Diaphanosoma sp.", "Diaphanosoma"),
         taxon = str_replace(taxon, "Calanoid copepodid", "Calanoida copepodites"),
         taxon = str_replace(taxon, "Calanoid nauplii", "Calanoida nauplii"),
         taxon = str_replace(taxon, "Cyclopoid copepodid", "Cyclopoida copepodites"),
         taxon = str_replace(taxon, "Cyclopoid nauplii", "Cyclopoida nauplii")) %>% 
  
  # "Cladocera immature", "Immature Cladocera"
  # ignore these because (i) they're a very minor component of total zoop, and (ii) they're sometimes counted, sometimes not
  
  
  # assign taxa without formulas to other groups (e.g. by genus):
  # create new column called assignedName (this will be species name used for biomass conversion)
  mutate(assignedName = taxon) %>% # copy over all entries from taxon to assignedName
  
  # then replace species names without a mean weight with the name of the group to which they are assigned
  mutate(assignedName = case_when(
    taxon == "Cyclops bicuspidatus thomasi" ~ "Cyclops",
    taxon == "Diacyclops bicuspidatus" ~ "Diacyclops thomasi",
    taxon == "Diaptomus leptopus" ~ "diaptomid",
    TRUE ~ assignedName)) %>% 
  
  # add mean weights from zoop.conversions file
  left_join(zoop.conversions, by = "assignedName") %>% 
  select(-dataset) %>%
  
  mutate(meanWt = case_when(taxon == "Daphnia galeatea and rosea" ~ 6.07, # assign to mean weight of Daphnia galeatea mendodate and D. rosea
                            TRUE ~ meanWt)) %>% 
  
  # convert density to biomass - output units are ug/L DRY
  mutate(biomass = value*meanWt) %>%
  
  # calculate total biomass for each lake, year, month, date, replicate combo
  group_by(lake, year, month, date, replicate) %>%
  summarize(totZoop.replicate = sum(biomass, na.rm = T)) %>% 
  ungroup() %>% 
  
  # calculate mean across replicates
  group_by(lake, year, month, date) %>%
  summarize(totZoop.date = mean(totZoop.replicate)) %>% 
  ungroup() %>%
  
  # calculate monthly means
  group_by(lake, year, month) %>%
  summarize(meanMonthlyZoop = mean(totZoop.date)) %>% 
  ungroup() %>% 
  
  # remove years after methods shift
  # zoop were sampled from Whole water column until 2016, and from Euphotic zone at least 2017 & 2018
  filter(!year %in% seq(2017, 2020, 1))



#######################################
#######################################
#######################################

# Visualize sampling frequency:
lk_list <- split(a.zoop.proc, f=a.zoop.proc$lake)
lk_freq <- list()
for(i in seq_along(lk_list)) {
  lk_freq[[i]] <- ggplot(lk_list[[i]], aes(x = year, y = month)) +
    geom_point() +
    #geom_count(alpha = 0.5) +
    ggtitle(paste(lk_list[[i]]$lake))
}
lk_freq


#######################################
#######################################
#######################################

# 6. Bind the datasets

a.lfw <- full_join(a.nutChl, a.phyto.proc, by = c("lake", "year", "month")) %>% 
  full_join(a.zoop.proc, by = c("lake", "year", "month")) %>% 
  full_join(a.morph, by = c("lake", "year")) %>% 
  
  # fill in missing depth info
  mutate(maxDepth = case_when(lake == "C Lake" ~ 0.95,
                              (lake == "W 7" & year == 2007) ~ 1.1,
                              (lake == "W 7" & year == 2014) ~ 1.25,
                              TRUE ~ maxDepth)) %>% 
  
  # move to long format:
  gather(key = metric, value = value, chl:meanMonthlyZoop) %>% 
  
  # add unit info:
  mutate(units = case_when(metric == "TP" ~ "mg/L",
                           metric == "TKN" ~ "",
                           metric == "chl" ~ "ug/L",
                           metric == "meanMonthlyPhyto" ~ "mg/m^3 wet",
                           metric == "meanMonthlyZoop" ~ "ug/L dry"))


save(a.lfw, file = "./formattedData/a-lfw.RData")


#######################################
#######################################
#######################################

# Reproducibility

sessionInfo()

#R version 3.5.1 (2018-07-02)
#Platform: x86_64-apple-darwin15.6.0 (64-bit)
#Running under: OS X El Capitan 10.11.6

#Matrix products: default
#BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
#LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib

#locale:
#[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#[1] unpivotr_0.6.1   tidyxl_1.0.7     lubridate_1.7.10 readxl_1.3.1     forcats_0.5.1   
#[6] stringr_1.4.0    dplyr_1.0.5      purrr_0.3.4      readr_1.4.0      tidyr_1.1.3     
#[11] tibble_3.1.1     ggplot2_3.3.3    tidyverse_1.3.1 

#loaded via a namespace (and not attached):
#[1] Rcpp_1.0.6       cellranger_1.1.0 pillar_1.6.0     compiler_3.5.1   dbplyr_2.1.1    
#[6] plyr_1.8.4       tools_3.5.1      digest_0.6.23    jsonlite_1.7.2   lifecycle_1.0.0 
#[11] gtable_0.2.0     pkgconfig_2.0.1  rlang_0.4.10     reprex_2.0.0     cli_2.5.0       
#[16] rstudioapi_0.13  DBI_1.0.0        yaml_2.1.19      haven_2.4.1      xml2_1.3.2      
#[21] withr_2.4.1      httr_1.4.2       fs_1.3.1         generics_0.1.0   vctrs_0.3.8     
#[26] hms_1.0.0        grid_3.5.1       tidyselect_1.1.1 glue_1.4.2       R6_2.4.1        
#[31] fansi_0.4.0      modelr_0.1.8     magrittr_2.0.1   backports_1.1.2  scales_0.5.0    
#[36] ellipsis_0.3.1   rvest_1.0.0      assertthat_0.2.0 colorspace_1.3-2 utf8_1.1.4      
#[41] stringi_1.2.3    munsell_0.5.0    broom_0.7.6      crayon_1.4.1  