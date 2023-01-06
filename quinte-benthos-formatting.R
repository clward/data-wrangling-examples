# Bay of Quinte Benthic Invertebrate Data
# Data Formatting script
# Colette Ward (colette.ward  - at -  dfo-mpo.gc.ca)
# last updated 2 April 2019



library(plyr); library(tidyverse); library(readxl); library(tidyxl); library(unpivotr); library(lubridate)


########################################################################
########################################################################
########################################################################


# Load and format 2008-2011 raw data

# The following function loads and cleans the data in multiple .xlsx files, each with multiple spreadsheets:
rawFiles <- list.files(path = "your path here", pattern = "*.xlsx", full.names = T)


# 1. Data load function:
dataLoadFunction <- function(x) {
  
  sheetNames <- readxl::excel_sheets(x) # extract sheet names within the worksheet
  sheetPattern <- "\\(DB\\)$" # use spreadsheets ending "(DB)", not "_sum". The latter have functional group totals added
  sheet <- str_subset(sheetNames, sheetPattern)
  
  all_cells <-
    tidyxl::xlsx_cells(x, sheets = sheet) %>%
    filter(col <= 11 & row >= 6) %>%
    filter(!is_blank) %>%
    filter(!character %in% c("n.a.", "n.a", "na.","")) %>%
    select(sheet, row, col, data_type, character, numeric) %>%
    split(.$sheet) # split into a list of dataframes
  
}

dataLoad1 <- lapply(rawFiles, dataLoadFunction)
dataLoad2 <- flatten(dataLoad1) # remove extra level of listing at the top level (grouped by year)
dataLoad <- lapply(dataLoad2, function(x) {select(x, -sheet)})



# 2. Metadata load function:
metadataLoadFunction <- function(x) {
  
  sheetNames <- readxl::excel_sheets(x) # extract sheet names within the worksheet
  sheetPattern <- "\\(DB\\)$"
  sheet <- str_subset(sheetNames, sheetPattern)
  
  all_cellsMeta <- tidyxl::xlsx_cells(x, sheets = sheet) %>%
    filter(col <= 13 & row <=5) %>%
    select(sheet, character) %>%
    split(.$sheet)
  
}

metadatLoad1 <- lapply(rawFiles, metadataLoadFunction)
metadataLoad2 <- flatten(metadatLoad1) # remove extra level of listing at the top level
metadataLoad <- lapply(metadataLoad2, function(x) {select(x, character)}) # keep only the column (called "character") which contains the metadata



# 3. 
dataFormatFunction <- function(x, y) {
  
  dat <- x %>%
    
    # remove rows after Hydrolimax griseus (the last data entry in each spreadsheet):
    filter(character == "Hydrolimax griseus") %>%
    pull(row) %>%
    {filter(x, row <= .)} %>%
    
    # unpivot the tables:
    behead("NNW", "replicate") %>%
    behead("N", "measurement") %>%
    behead("W", "taxon") %>%
    
    filter(data_type != "character") %>%
    
    select(-row, -col) %>%
    
    mutate(measurement = ifelse((measurement %in% c("# /Ekm", "#'s", "num /Ekm")), "numPerEkman",
                                ifelse((measurement %in% c("mg.wetwt.", "wt.", "mg wetwt/Ekm +shell.", "wt. mg/Ek")), "mgWetWt",
                                       measurement
                                ))) %>%
    
    spatter(measurement) %>%
    
    select(replicate, taxon, mgWetWt, numPerEkman) %>%
    
    
    mutate(mgWetWt = as.numeric(mgWetWt),
           numPerEkman = as.numeric(numPerEkman),
           replicate = as.numeric(replicate),
           
           # sometimes there are NAs in mgWetWt (e.g. Conway 2008, all replicates, Cypria)
           # this is OK - no weights are reported in raw data, only numbers
           
           mgWetWt = ifelse((is.na(mgWetWt)), 0, mgWetWt), # when mgWetWt == NA, change this to 0 (this only occurs when numPerEkman == 0)
           
           taxon = str_trim(taxon), # trim leading and trailing whitespaces
           taxon = ifelse((taxon == "NEMATODA:"), "Nemata",
                          ifelse((taxon == "CERATOPOGONIDAE:"), "Ceratopogonidae",
                                 ifelse((taxon == "Very Immature"), "Dreissena",
                                        ifelse((taxon %in% c("Dreissena bugensis \"quagga\"", "Very Immature \"quagga\"", "D. bugensis (yoy)", "Dreissena bugensis \"immature\"", "Dreissena bugensis \"yoy\"")), "Dreissena bugensis",
                                               ifelse((taxon %in% c("Immatures without hairs", "Immature with hairs")), "Tubificidae",
                                                      ifelse((taxon == "Planariidae(Dugesia tigrina)"), "Dugesia tigrina",
                                                             ifelse((taxon %in% c("\"microturbellaria\"", "\"microturbellaria\" Neorhabididae")), "Neorhabdocoela",
                                                                    ifelse((taxon == "Dero (Aulophorus) flabelliger"), "Dero flabelliger",
                                                                           ifelse((taxon == "Dicrotendipes modestum"), "Dicrotendipes modestus",
                                                                                  ifelse((taxon == "Stylodrilus herringianus"), "Stylodrilus heringianus",
                                                                                         ifelse((taxon == "Hydrolimax griseus"), "Hydrolimax grisea", # fix spelling error
                                                                                                ifelse((taxon == "Paralauterbourniella"), "Paralauterborniella", # fix spelling error
                                                                                                       ifelse((taxon == "Potamopyrgus antipodurum"), "Potamopyrgus antipodarum", # fix spelling error
                                                                                                              ifelse((taxon == "Quistadrilus multisetosus"), "Quistradrilus multisetosus", # fix spelling error
                                                                                                                     ifelse((taxon == "Slavinia appendiculata"), "Slavina appendiculata", # fix spelling
                                                                                                                            ifelse((taxon == "Amnicola limosa"), "Amnicola limosus", # fix spelling
                                                                                                                                   ifelse((taxon == "Probythinia lacustris"), "Probythinella lacustris", # fix spelling
                                                                                                                                          ifelse((taxon == "Peloscolex ferox"), "Spirosperma ferox", # update to valid ITIS name
                                                                                                                                                 ifelse((taxon == "Chironomus plumosus (semireductus)"), "Chironomus plumosus", # update to valid ITIS name
                                                                                                                                                        ifelse((taxon == "Chironomus semireductus"), "Chironomus plumosus", # update to valid ITIS name
                                                                                                                                                               taxon
                                                                                                                                                        )))))))))))))))))))),
           taxon = gsub(" sp\\.", "", taxon),
           taxon = gsub(" spp\\.", "", taxon)) %>% # "Chironomus spp." # genus name; used when individuals can't be ID'd to species level; retain because it's one of the lowest possible ID levels
    
    # remove the following because they are subtotals for data that are already in spreadsheets:
    filter(!taxon %in% c("TOTALS", "EPHEMEROPTERA: sum", "TRICHOPTERA: sum", "NAIDIDAE: sum",
                         "total Non Dreiss.", "total Non Dreiss.", "DREISSENIDAE:", "DREISSENIDAE: sum",
                         "ACARINA:", "ACARINA:  (total)", "ACARINA:  sum", "ACARINA:  Total", "ACARINA: sum",
                         "AMPHIPODA:", "AMPHIPODA:  sum", "AMPHIPODA: sum", "AMPHIPODA: Total",
                         "CHIRONOMIDAE:(total)", "CHIRONOMINI: sum", "HEPTAGENIIDAE:",
                         "MOLLUSCA:GASTROPODA:", "MOLLUSCA:GASTROPODA: sum", "SPHAERIIDAE: (total)",
                         "OLIGOCHAETA (total)", "ORTHOCLADIINAE:sum", "ORTHOCLADIINAE: sum",
                         "Ostracoda:", "Ostracoda:  (total)", "Ostracoda:  sum", "Ostracoda: sum",
                         "PLATYHELMINTHES:", "PLATYHELMINTHES: sum",
                         "TANYPODINAE:  sum", "TANYPODINAE: sum", "TANYTARSINI:  sum", "TANYTARSINI: sum",
                         "TUBIFICIDAE: sum"))
  
  
  # extract date information
  dateInfo <- unique(str_subset(y$character, "^DATE"))
  datePattern <- "\\s+.+\\s\\d+\\,*\\s+\\d+$"
  date <- str_extract(dateInfo, datePattern) %>% 
    str_trim(side = "left") %>%
    unique()
  
  
  # extract sieve information
  sieveInfo <- unique(str_subset(y$character, "^Screened"))
  sievePattern <- "\\d{3}\\s+.."
  sieve <- str_extract(sieveInfo, sievePattern) %>%
    str_replace("([^ ]+) ([^ ]+)", "\\1 \\2 mesh sieve")
  
  
  # extract sampling gear information
  methodInfo <- unique(str_subset(y$character, "9\" Ekmans"))
  methodPattern <- "\\d\"\\s......"
  method <- str_extract(methodInfo, methodPattern) %>%
    str_replace("\"", " inch") %>%
    str_replace("Ekmans", "Ekman dredge")
  
  
  # extract depth information
  depthInfo <- unique(str_subset(y$character, "^DEPTH"))
  depthPattern <- "\\d+\\.*\\d*"
  depth <- as.numeric(str_extract(depthInfo, depthPattern)) 
  
  
  # extract site information
  locInfo <- c("Big Bay", "Conway", "Glenora", "LOX")
  locsToMatch <- str_c(locInfo, collapse = "|")
  location <- unique(str_subset(y$character, locsToMatch)) %>%
    str_replace(".*Big Bay.*", "Big Bay") %>% str_replace(".*Conway.*", "Conway") %>% str_replace(".*Glenora.*", "Glenora") %>% str_replace(".*LOX.*", "LOX") %>%
    unique()
  
  
  # bind metadata into a single dataframe
  meta <- as.data.frame(cbind(location, date, depth, method, sieve), stringsAsFactors = F)
  m <- meta %>% 
    mutate(date = mdy(date),
           depth = as.numeric(depth)) %>% 
    slice(rep(1:n(), nrow(dat))) # expand the metadata df to have the same number of rows as the data df
  
  
  data <- bind_cols(m, dat) # merge data and metadata
  
  
}


data2008_2011 <- mapply(dataFormatFunction, x = dataLoad, y = metadataLoad, SIMPLIFY = F)
df2008_2011 <-bind_rows(data2008_2011[]) # combine all sites into a single df




########################################################################
########################################################################
########################################################################


# Load 2007 data:

dat2007 <- "your-path-here/BQBEN2007.xlsx"


# 1. Data load function:
dataLoadFunction_2007 <- function(x) {
  
  all_cells <-
    tidyxl::xlsx_cells(x) %>%
    filter(col <= 11 & row >= 4) %>%
    filter(!is_blank) %>% 
    filter(!character %in% c("n.a.", "n.a", "na.","")) %>%
    select(sheet, row, col, data_type, character, numeric) %>%
    split(.$sheet) # split into a list of df's
  
}

dataLoad1_2007 <- lapply(dat2007, dataLoadFunction_2007)
dataLoad2_2007 <- flatten(dataLoad1_2007) # remove extra level of listing at the top level (grouped by year)
dataLoad_2007 <- lapply(dataLoad2_2007, function(x) {select(x, -sheet)})



# 2. Metadata load function:
metadataLoadFunction_2007 <- function(x) {
  
  all_cellsMeta <- tidyxl::xlsx_cells(x) %>%
    filter(col <= 13 & row <=3) %>%
    select(sheet, character) %>%
    split(.$sheet) # split into a list of df's
  
}

metadatLoad1_2007 <- lapply(dat2007, metadataLoadFunction_2007)
metadataLoad2_2007 <- flatten(metadatLoad1_2007) # remove extra level of listing at the top level
metadataLoad_2007 <- lapply(metadataLoad2_2007, function(x) {select(x, character)}) # keep only the column (called "character") which contains the metadata


# use dataFormatFunction from above to format 2007 data:
data_2007 <- mapply(dataFormatFunction, x = dataLoad_2007, y = metadataLoad_2007, SIMPLIFY = F)
df2007 <- bind_rows(data_2007[]) # combine all sites into a single df



########################################################################
########################################################################
########################################################################


# Join and minor formatting of dataframes:

df2007_2011 <- bind_rows(df2007, df2008_2011)  # bind 2007-2011 data

recent <- df2007_2011 %>%
  mutate(day = yday(date),
         month = month(date),
         year = year(date)) %>%
  select(-depth)


########################################################################
########################################################################
########################################################################


# Load and format Dasha Martchenko's file for 1967-2006 data

benthos67to06 <- read.csv("your-path-here/file-name.csv", stringsAsFactors = FALSE)


colnames(benthos67to06) <- tolower(colnames(benthos67to06))

ben <- benthos67to06 %>%
  select(location, date = sampling.date, replicate, taxon = taxonomic.name, numPerEkman = taxon.count,  mgWetWt = taxon.biomass, method = sampling.method, sieve = sieve.method) %>% # taxon.count.unit, taxon.biomass.unit
  mutate(date = dmy(date),
         date = gsub("2067-08-01", "1967-08-01", date), # fix dates
         date = gsub("2067-08-17", "1967-08-17", date),
         date = gsub("2068-08-01", "1968-08-01", date),
         date = gsub("2068-08-16", "1968-08-16", date),
         date = gsub("2068-08-21", "1968-08-21", date),
         date = ymd(date),
         day = yday(date),
         month = month(date),
         year = year(date),
         
         # rename sites to match 2007-11 data:
         location = gsub("BB_Big Bay", "Big Bay", location),
         location = gsub("C_Conway", "Conway", location),
         location = gsub("GL_Glenora_benthic", "Glenora", location),
         location = gsub("LOX_Lake Ontario Exit", "LOX", location),
         
         taxon = str_trim(taxon, side = "right"),
         taxon = ifelse((taxon == "Peloscolex ferox"), "Spirosperma ferox", # update to valid ITIS name
                        ifelse((taxon == "Chironomus semireductus"), "Chironomus plumosus", # update to valid ITIS name
                               taxon
                        ))) %>%
  
  filter(location %in% c("Big Bay", "Conway", "Glenora", "LOX"))



########################################################################
########################################################################
########################################################################


# Join data for all years:

allBenthos <- bind_rows(ben, recent) # join the '67-'06 and '07-'11 datasets


########################################################################
########################################################################
########################################################################


# Assign Ron Dermott's taxonomic groups (e.g. Amphipods, Chironomids) to each observation.

# These groupings come from:
# (i) spreadsheet layout in the raw 2007-2011 files, 
# (ii) "Benthic_Taxonomic_Hierarchy_sort&Wet_Dry_Conversion_Mar15 - edited Sep16.xls"
# (iii) consulting itis.gov
# (iv) matching grouping decisions in Dasha Marchenko's file

allBenthosGrouped <- allBenthos %>%
  
  filter(!is.na(taxon)) %>% # remove unnecessary rows without taxon from Conway 2009 data (in raw excel spreadsheet "BQCON2009_sum", formula in macro indicates these rows are the sum of all Tubificidae; we don't want sums in the data)
  
  mutate(taxGroup = ifelse((str_detect(taxon, "Diporeia")), "amphipoda",
                           ifelse((str_detect(taxon, "Gammarus")), "amphipoda",
                                  ifelse((str_detect(taxon, "Hyalella")), "amphipoda",
                                         ifelse((taxon %in% c("Crangonyx gracilis", "Echinogammarus ischnus")), "amphipoda",
                                                
                                                ifelse((str_detect(taxon, "Chironomus")), "chironomidae", # for all Chironomus sp.
                                                       ifelse((str_detect(taxon, "Cryptochironomus")), "chironomidae",
                                                              ifelse((str_detect(taxon, "Dicrotendipes")), "chironomidae",
                                                                     ifelse((str_detect(taxon, "Endochironomus")), "chironomidae",
                                                                            ifelse((str_detect(taxon, "Harnischia")), "chironomidae",
                                                                                   ifelse((str_detect(taxon, "Heterotrissocladius")), "chironomidae",
                                                                                          ifelse((str_detect(taxon, "Microchironomus")), "chironomidae",
                                                                                                 ifelse((str_detect(taxon, "Parachironomus")), "chironomidae",
                                                                                                        ifelse((str_detect(taxon, "Paratendipes")), "chironomidae",
                                                                                                               ifelse((str_detect(taxon, "Polypedilum")), "chironomidae",
                                                                                                                      ifelse((str_detect(taxon, "Procladius")), "chironomidae",
                                                                                                                             ifelse((taxon %in% c("Ablabesmyia", "Chironomidae", "Chironomini", "Cladopelma", "Clinotanypus", "Coelotanypus", "Conchapelopia", "Cricotopus", "Cryptotendipes", "Einfeldia", "Glyptotendipes", "Micropsectra", "Microtendipes", "Nilothauma", "Paracladopelma", "Paralauterborniella", "Paramerina", "Phaenopsectra", "Potthastia longimana", "Psectrotanypus", "Sergentia coracina", "Stempellina", "Stempellinella", "Stictochironomus", "Tanypodinae", "Tanytarsus", "Tribelos")), "chironomidae",
                                                                                                                                    
                                                                                                                                    ifelse((str_detect(taxon, "Dreissena")), "dreissena",
                                                                                                                                           
                                                                                                                                           ifelse((str_detect(taxon, "Amnicola")), "gastropoda",
                                                                                                                                                  ifelse((str_detect(taxon, "Fossaria")), "gastropoda",
                                                                                                                                                         ifelse((str_detect(taxon, "Valvata")), "gastropoda",
                                                                                                                                                                ifelse((taxon %in% c("Bithynia tentaculata", "Helisoma anceps", "Ferrissia parallela", "Gastropoda", "Gyraulus parvus", "Helisoma anceps", "Marstonia decepta", "Physella gyrina", "Pleurocera acuta", "Potamopyrgus antipodarum", "Probythinella lacustris", "Pseudosuccinea columella")), "gastropoda", # "Probythinia lacustris", 
                                                                                                                                                                       
                                                                                                                                                                       ifelse((str_detect(taxon, "Chaoborus")), "insectaNoChiron",
                                                                                                                                                                              ifelse((taxon %in% c("Agrypnia", "Bezzia", "Caenis", "Ceratopogonidae", "Cheumatopsyche", "Coleoptera", "Dubiraphia", "Enallagma", "Hexagenia", "Insecta", "Lepidostoma", "Nectopsyche", "Oecetis", "Oxyethira", "Phylocentropus", "Polycentropus", "Stenelmis", "Trichoptera", "Sialis")), "insectaNoChiron", # includes Trichoptera, Ceratopogonid, Ephemeroptera, Chaoborus, Coleoptera, Odonata, # Megaloptera
                                                                                                                                                                                     
                                                                                                                                                                                     ifelse((str_detect(taxon, "Caecidotea")), "isopoda",
                                                                                                                                                                                            ifelse((taxon %in% c()), "isopoda",
                                                                                                                                                                                                   
                                                                                                                                                                                                   ifelse((taxon %in% c()), "miscellaneous",
                                                                                                                                                                                                          
                                                                                                                                                                                                          ifelse((str_detect(taxon, "Aulodrilus")), "oligochaeta",
                                                                                                                                                                                                                 ifelse((str_detect(taxon, "Dero")), "oligochaeta", 
                                                                                                                                                                                                                        ifelse((str_detect(taxon, "Limnodrilus")), "oligochaeta",
                                                                                                                                                                                                                               ifelse((str_detect(taxon, "Nais")), "oligochaeta",
                                                                                                                                                                                                                                      ifelse((str_detect(taxon, "Potamothrix")), "oligochaeta",
                                                                                                                                                                                                                                             ifelse((str_detect(taxon, "Stylodrilus")), "oligochaeta",
                                                                                                                                                                                                                                                    ifelse((taxon %in% c("Arcteonais lomondi", "Chaetogaster diaphanus", "Ilyodrilus templetoni", "Lumbriculidae", "Naididae", "Oligochaeta", "Peloscolex ferox", "Quistradrilus multisetosus", "Slavina appendiculata", "Specaria josinae", "Spirosperma ferox", "Stylaria lacustris", "Tubifex tubifex", "Tubificidae", "Uncinais uncinata", "Vejdovskyella intermedia")), "oligochaeta",
                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                           ifelse((str_detect(taxon, "Pisidium")), "sphaeriidae",
                                                                                                                                                                                                                                                                  ifelse((str_detect(taxon, "Musculium")), "sphaeriidae",
                                                                                                                                                                                                                                                                         ifelse((str_detect(taxon, "Sphaerium")), "sphaeriidae",
                                                                                                                                                                                                                                                                                ifelse((taxon %in% c("Pisidiidae")), "sphaeriidae",
                                                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                                                       ifelse((taxon %in% c("Calanoida", "Cyclopoida", "Harpacticoida", # copepods
                                                                                                                                                                                                                                                                                                            "Hemimysis anomala", "Mysis relicta", # mysids
                                                                                                                                                                                                                                                                                                            "Candona", "Cypria", "Cyprididae", "Cytherissa", "Cytherissa lacustris", "Ostracoda", # Ostracods
                                                                                                                                                                                                                                                                                                            "Gloiobdella elongata", "Glossiphonia", "Glossiphonia complanata", "Helobdella", "Helobdella stagnalis", "Mooreobdella fervida", # Annelids / Hirudinea
                                                                                                                                                                                                                                                                                                            "Hydra",
                                                                                                                                                                                                                                                                                                            "Hydrolimax grisea", "Neorhabdocoela", "Turbellaria", "Dugesia tigrina", "Procotyla fluviatilis", # Platyhelminthes / Turbellaria
                                                                                                                                                                                                                                                                                                            "Hydrachnidae", "Hygrobates", "Krendowskia", "Lebertia", "Limnesia", "Piona", "Unionicola", "Unionicolidae", "Forelia", "Neumania", "Prostigmata", # Acarina
                                                                                                                                                                                                                                                                                                            "Manayunkia speciosa", # Polychaete
                                                                                                                                                                                                                                                                                                            "Mermithidae", "Nemata", # Nemata
                                                                                                                                                                                                                                                                                                            "Planaria", 
                                                                                                                                                                                                                                                                                                            "Prostoma", "Prostoma canadensis" # Nemertea
                                                                                                                                                                                                                                                                                       )), "other",
                                                                                                                                                                                                                                                                                       NA
                                                                                                                                                                                                                                                                                       )))))))))))))))))))))))))))))))))))))))


########################################################################
########################################################################
########################################################################


# Final formatting
# Convert to aerial estimates, Calculate annual means


# (a) Convert to aerial estimates:

allBenthosAerial <- allBenthosGrouped %>%
  
  # 1. omit samples which used 250um mesh sieve (only 1 replicate at Glenora & Conway on 1 date in 1992). All other sampling used 580um mesh sieve
  filter(sieve != "250 um mesh sieve") %>%
  
  # 2. convert data to aerial estimates (/m2)
  mutate(abundAerial = ifelse((method == "9 inch Ekman dredge"), numPerEkman*20, #from raw 2008 file: " 9" Ekmans (=1/20 m2)"
                              ifelse((method == "9 inch Ponar dredge"), numPerEkman*18.9, # from raw 1993 & 1995 files: " 9 Inch Ponars (=1/18.9 m2)"
                                     NA
                              )),
         biomassAerial = ifelse((method == "9 inch Ekman dredge"), mgWetWt*20,
                                ifelse((method == "9 inch Ponar dredge"), mgWetWt*18.9,
                                       NA
                                ))) %>%
  
  select(-numPerEkman, -mgWetWt, -method, -sieve)




# ***Save this output (allBenthosAerial) if you wish to have a file with data at the lowest taxonomic level possible.***



# (b) Calculate annual mean biomass and abundance for each taxonomic group across replicates, for each site 
# *including samples with zero abundance/biomass*:


allBenthosAerialMeans <- allBenthosAerial %>%  
  
  # 1. calculate functional group totals (per replicate and site)
  group_by(location, year, month, day, replicate, taxGroup) %>%
  summarize(totAbundAerial = sum(abundAerial, na.rm = T),
            totBiomassAerial = sum(biomassAerial, na.rm = T)) %>% # fyi this replaces all resulting NAs before 1985 with 0, which is not correct; these are true NAs
  ungroup() %>%
  
  
  # 2. add missing zeros
  # *** Note these are added AFTER grouping by functional groups (ie not at the individual taxon level) ***
  
  unite(locYearMonthDayRep, location, year, month, day, replicate, sep = ",") %>%
  tidyr::complete(taxGroup, locYearMonthDayRep, fill = list(totAbundAerial = 0, totBiomassAerial = 0)) %>%
  separate(locYearMonthDayRep, into = c("location", "year", "month", "day", "replicate"), sep = ",", convert = T) %>%
  
  
  # 3. For years before 1985, replace all zeros with NA in biomass column (because no weights were recorded in these years)
  mutate(totBiomassAerial = ifelse((year <= 1984), NA, totBiomassAerial)) %>%
  
  # 4. Calculate annual mean biomass and abundance for each taxonomic group across replicates, for each site
  group_by(location, year, month, day, taxGroup) %>%
  summarize(meanAbundAerial = mean(totAbundAerial),
            meanBiomassAerial = mean(totBiomassAerial)) %>%
  ungroup()



#################################################################
#################################################################
#################################################################


# Add Ron Dermott's back-calculated biomass values for 1967-1984 (from ... \Zzdata_bases\Bqindex\BQ70S\MJBQWTm2_OK.xls)
# these are back-calculated from volumetric displacement data in Johnson & McNeil 1986.special CJFAS 86: p180
# there are no data for 1972
# units are g/m^2


earlyBiomass <- read_csv("your-path-here/quinteIndexBenthos_biomass_67-84.csv", skip = 2)


earlyBiomass1 <- earlyBiomass %>%
  select(-matches("SE$")) %>%
  gather(taxGroup, meanBiomassGM2, chironomidMean:isopodMean) %>%
  mutate(meanBiomassAerial = meanBiomassGM2*1000) %>% # convert from g/m^2 to mg/m^2
  select(-meanBiomassGM2) %>%
  mutate(taxGroup = recode(taxGroup, chironomidMean = "chironomidae", oligochaeteMean = "oligochaeta",
                           sphaeriidMean = "sphaeriidae", amphipodMean = "amphipoda", isopodMean = "isopoda"))




# replace NAs in in 1967-2011 biomass data with back-calculated biomass values
# split allBenthosAerialMeans at 1984/85; in the first half of the data, remove meanBiomassAerial (should be all NAs), and replace with earlyBiomass1$meanBiomassAerial
# then rejoin first and second halves of the data, and do some formatting:

allBenthosAerialMeans_early <- allBenthosAerialMeans %>%
  filter(year <= 1984) %>%
  select(-meanBiomassAerial) %>% # remove the old biomass vector containing NA
  left_join(earlyBiomass1, by = c("location", "year", "taxGroup")) # merge in earlyBiomass1$meanBiomassAerial


allBenthosAerialMeans_later <- filter(allBenthosAerialMeans, year >= 1985)


benthos <- bind_rows(allBenthosAerialMeans_early, allBenthosAerialMeans_later) %>% # rejoin first and second halves of the data
  select(-month, -day) %>%
  rename(stn = location) %>%
  filter(stn != "LOX") %>%
  
  # convert [wet weights with shells] to [wet weights without shells]
  # use conversion factors from Ron Dermott's file "Benthic_Taxonomic_Hierarchy_sort&Wet_Dry_Conversion_Mar15 - edited Sep16.xls"
  mutate(meanBiomassAerial_shellFree = ifelse((taxGroup == "dreissena"), meanBiomassAerial*0.56,
                                              ifelse((taxGroup %in% c("sphaeriidae", "gastropoda")), meanBiomassAerial*0.3,
                                                     meanBiomassAerial
                                              ))) %>%
  
  select(-meanBiomassAerial)


# for 1967, '68, '77, '82-'84, biomass was only measured for the five most common functional groups (amphipoda, chironomidae, isopoda, oligochaeta, sphaeriidae) 
# realistically, these NAs could all be recoded to zero by adding the following to the pipe that creates the benthos df
# because their abundance estimates were zero or because abundances suggest they were a minor fraction of total benthic biomass:
#replace_na(list(meanBiomassAerial = 0))


# NB for 1972 there are abundance data but no back-calculated biomass


save(benthos, file="your-file-name.RData")


#################################################################
#################################################################
#################################################################

# Computing environment / Reproducibility:

# note this script was created using unpivotr version 0.4.0

sessionInfo()

# R version 3.5.1 (2018-07-02)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: OS X El Capitan 10.11.6

# Matrix products: default
# BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib

# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
# [1] bindrcpp_0.2.2  lubridate_1.7.4 unpivotr_0.4.0  tidyxl_1.0.3    readxl_1.1.0    forcats_0.3.0  
# [7] stringr_1.3.1   dplyr_0.7.6     purrr_0.3.4     readr_1.1.1     tidyr_0.8.1     tibble_2.1.3   
# [13] ggplot2_3.0.0   tidyverse_1.2.1 plyr_1.8.4     

# loaded via a namespace (and not attached):
# [1] Rcpp_0.12.17     cellranger_1.1.0 pillar_1.4.2     compiler_3.5.1   bindr_0.1.1      tools_3.5.1     
# [7] jsonlite_1.7.2   nlme_3.1-137     gtable_0.2.0     lattice_0.20-35  pkgconfig_2.0.1  rlang_0.4.10    
# [13] cli_2.3.0        rstudioapi_0.13  yaml_2.1.19      haven_1.1.2      withr_2.4.1      xml2_1.3.2      
# [19] httr_1.4.2       hms_0.4.2        grid_3.5.1       tidyselect_0.2.4 glue_1.3.1       R6_2.4.1        
# [25] modelr_0.1.2     magrittr_1.5     backports_1.1.2  scales_0.5.0     rvest_0.3.2      assertthat_0.2.0
# [31] colorspace_1.3-2 stringi_1.2.3    lazyeval_0.2.1   munsell_0.5.0    broom_0.5.0      crayon_1.3.4