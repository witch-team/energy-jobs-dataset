# Script to process energy jobs data to create the jobs intensity dataset by country, jobs category, and technology
# All values are jobs [FTE]/GW or jobs [FTE]/PJ (all latest available data, so aobut 2017-2020)
# First version: 10 October 2019
# Authors: Johannes Emmerling and Sandeep Pai

#requires the input file in the same directory: 
energy_jobs_database_file <- "energy_jobs_database.xlsx"
#directory to store graphs and output
resdir <- "results"; if(!dir.exists(resdir)){dir.create(resdir)}

#Install and load required packages
require_package <- function(package, loading = T){
  if (!package %in% rownames(installed.packages())) try(install.packages(package, repos = "http://cran.rstudio.com"), silent = TRUE)
  if (loading) suppressPackageStartupMessages(library(package,character.only = T, quietly = TRUE))
}
pkgs <- c('tidyverse', 'openxlsx', 'countrycode', 'data.table')
res <- lapply(pkgs, require_package)
### END OF FUNCTIONS ###


#Load main Excel file
loaddata = read.xlsx(xlsxFile = energy_jobs_database_file, sheet = "FinalAllcountries", startRow = 2)
loaddata <- loaddata %>% select(-Comments, -Job.Types)

rawdata <- loaddata %>% filter(!is.na(`Country.-.region.name`)) 
rawdata <- rawdata %>% mutate(country = sub("\\-.*", "", `Country.-.region.name`)) %>% mutate(iso3 = countrycode(sourcevar = country, origin = "country.name", destination = "iso3c", warn = F)) %>% select(-`Country.-.region.name`, -country)


#get good category names
catfuelnames <- paste0(t(rawdata[1:2,])[,1],"-",t(rawdata[1:2,])[,2])
names(rawdata) <- sub("NA-NA", "iso3", catfuelnames)
rawdata <- rawdata[-(1:2), , drop = FALSE]

#convert to long format
rawdata_long <- reshape2::melt(rawdata, id.vars = "iso3", variable.name = "variable")  
rawdata_long <- rawdata_long %>% mutate(value = as.numeric(value)) 

rawdata_long <- rawdata_long %>% mutate(fullname = variable) %>% separate(variable, c("fuel","category"), sep = "-")

#For wind_offshore take wind_onshore if 0 (=NA)
rawdata_long <- rawdata_long %>% group_by(iso3, category) %>% mutate(value=ifelse(fuel=="wind_off",ifelse(value==0, mean(value[fuel=="wind_on"]), value), value))

#All zeros are missing values
rawdata_long <- rawdata_long %>% filter(value!=0)


#Convert all units to PJ and GW
#here just extraction for all fuels, and only  refinery
#refinery, .0611     # kbarrels/day -> PJ/day
fuel_conv_units <- "fuel, conv
coal_hard, 22.74       # Mt -> PJ
coal_lignite, 10    # Mt -> PJ
oil, 0.0418         # ktoe -> PJ
oil_unconv, 0.0418  # ktoe -> PJ
gas, 0.0418         # ktoe -> PJ
gas_unconv, 0.0418  # ktoe -> PJ
biodiesel, 0.0327   # Ml -> PJ
ethanol, 0.0214     # Ml -> PJ
"
fuel_conv_units <- read_csv(fuel_conv_units, comment = "#")
rawdata_long <- merge(rawdata_long, fuel_conv_units, by="fuel", all=T)
rawdata_long <- rawdata_long %>%
  mutate(value = ifelse(category=="extraction" & fuel %in% fuel_conv_units$fuel, value / conv, value)) %>%
  mutate(value = ifelse(category == "refinery", value / (.00601 *365), value)) %>%
  select(fuel,category,iso3,value)

Q_OUT_TYPE <- read.xlsx(xlsxFile = energy_jobs_database_file, sheet = "Q_OUT_TYPE", startRow = 1)
conv_unconv <- Q_OUT_TYPE %>% filter(year == 2016) %>% mutate(fuel=case_when(f=="oil" & type=="conv" ~ "oil", f=="oil" & type=="nonconv" ~ "oil_unconv", f=="coal" & type=="nonconv" ~ "coal_lignite", f=="coal" & type=="conv" ~ "coal_hard")) %>% select(-f, -type, -year) %>% rename(share=value)
rawdata_long <- rawdata_long %>% left_join(conv_unconv) %>% mutate(share=ifelse(is.na(share), 1, share)) %>% mutate(fuel = ifelse(str_detect(fuel, "solar|wind|hydro"), fuel, str_replace(fuel,"\\_.*",""))) %>% group_by(iso3,fuel,category) %>% dplyr::summarize(value=weighted.mean(value, w = share, na.rm = T)) %>% ungroup() %>% as.data.frame()

#For hydro, compute weighted average using shares of large and small hydro where available:
hydro_shares <- read.xlsx(xlsxFile = energy_jobs_database_file, sheet = "Hydro", rows = seq(31,36), cols = c(1, 2, 3)) 
continents <- as.data.frame(codelist$iso3c)
continents <- suppressWarnings(continents %>% dplyr::rename(iso3c="codelist$iso3c") %>% filter(!is.na(iso3c)) %>% mutate(continent=countrycode(iso3c, origin = "iso3c", destination = "continent")) %>% dplyr::rename(iso3=iso3c) %>% mutate(iso3=as.character(iso3)))
hydro_shares <- continents %>% full_join(melt(as.data.table(hydro_shares), id.vars = "continent", variable.name = "fuel", value.name = "share") %>% mutate(fuel=as.character(fuel))) %>% select(-continent)
#apply Hydro relative shares
rawdata_long <- rawdata_long %>% left_join(hydro_shares) %>% mutate(share=ifelse(is.na(share), 1, share)) %>% mutate(fuel = ifelse(str_detect(fuel, "hydro"), str_replace(fuel,"\\_.*",""), fuel)) %>% group_by(iso3,fuel,category) %>% dplyr::summarize(value=weighted.mean(value, w = share, na.rm = T)) %>% ungroup() %>% as.data.frame()

#biofuels: assign as trbiofuel (traditional biofuel)
rawdata_long <- rawdata_long %>% mutate(fuel = ifelse(str_detect(fuel, "ethanol|biodiesel"), "trbiofuel", fuel)) %>% 
  group_by(iso3,fuel,category) %>% dplyr::summarize(value=mean(value, na.rm = T)) %>% ungroup() %>% as.data.frame()

#Get Job units correct
# Apply 17% of .17 for ethanol and biodiesel jobs to only have direct jobs (not indirect nor induced)
#Based on Urbanchuk, J. M. (2019) paper. We consider only Agriculture & Ethanol production jobs  
rawdata_long <- rawdata_long %>% mutate(value = ifelse(fuel %in% c("biodiesel", "ethanol") & category=="refinery", value * 0.17, value))


add_fossil_manufacturing = T
#Add manufacturing for coal, gas, and nuclear power plants (based on Dominich et al. https://link.springer.com/chapter/10.1007/978-3-030-05843-2_10) Table 10.1
#Job years/MW: Coal	5.1, Gas	2.9, Nuclear	1.3
#assign for USA and China (oecd/non-oecd) which then will be assigned to the total world number
#For oil, assume it is the same as gas power plants
manufacturing_dominich <-    fread("fuel, category, value
                                    coal,    manufacturing, 5.1
                                    gas,     manufacturing, 2.9
                                    oil,     manufacturing, 2.9
                                    nuclear, manufacturing, 1.3")
manufacturing_dominich <- rbind(manufacturing_dominich %>% mutate(iso3="USA"), manufacturing_dominich %>% mutate(iso3="CHN"))
if(add_fossil_manufacturing) rawdata_long <- rbind(rawdata_long, manufacturing_dominich %>% mutate(value=value*1e3))
#For Construction & Manufacturing: convert job years to jobs. The below is the construction times (Rutovitz, 2015) (for wind: wind_on:2, wind_off 4: take value of 3)
# https://opus.lib.uts.edu.au/bitstream/10453/43718/1/Rutovitzetal2015Calculatingglobalenergysectorjobsmethodology.pdf
construction_time <- "fuel, years
coal, 5 
gas, 2 
oil, 2
nuclear, 10
biomass, 2
hydro, 2
solar_pv, 1
solar_csp, 1
wind_on, 2
wind_off, 4
geothermal, 2"
construction_time <- read_csv(construction_time)
rawdata_long <- merge(rawdata_long, construction_time, by = "fuel", all=T)
rawdata_long <- rawdata_long %>% mutate(value=ifelse(category %in% c("manufacturing", "construction"), value / years, value)) %>% select(-years) %>% filter(!is.na(value))
setcolorder(rawdata_long,c('iso3','category','fuel','value'))
energy_jobs_intensity <- rawdata_long


############Manufacturing (el-pv) & Refinery (oil) & Extraction (uranium): Check how global pool values should be assigned to countries ########
#Oil refinery: Correlation of oil refinery with consumption is 0.916, with extraction only 0.094 => leave it at consumption level, no global pool!
#Solar PV Panels 
global_pool_share <- read.xlsx(xlsxFile = energy_jobs_database_file, sheet = "Global_Manufacturing_solar_pv", startRow = 7,rows = seq(7,17))
global_pool_share <- global_pool_share %>% mutate(iso3 = countrycode(X1, "country.name", "iso3c", warn = F))
#Europe is mostly Germany
global_pool_share <- global_pool_share %>% mutate(iso3=ifelse(X1=="Europe", "DEU", iso3)) %>% select(-Region,-Worldwide)
global_pool_share <- global_pool_share %>% select(-X1) %>% filter(!is.na(X2)) %>% rename(value=X2)
#Remove other (8% of PV manufacturing and recompute shares)
global_pool_share <- global_pool_share %>% filter(!is.na(iso3)) %>% mutate(value=value / sum(value) * 100) %>% mutate(fuel="solar_pv", category="manufacturing")
setcolorder(global_pool_share,c('iso3','fuel','value'))
#Now add uranium extraction (shares based on last year extraction, for now not reserves)
uranium_share <- read.xlsx(xlsxFile = energy_jobs_database_file, sheet = "Uranium",rows = seq(39,46), cols = c(1,2))
uranium_share <- uranium_share %>% mutate(iso3 = countrycode(country, "country.name", "iso3c", warn = F)) %>% select(-country) %>% dplyr::rename(value = extraction_share) %>% mutate(fuel="nuclear", category="extraction")
global_pool_share <- rbind(global_pool_share, uranium_share)
 

############### Save data for modeling input ##########
write.csv(energy_jobs_intensity, file.path(resdir, "energy_jobs_intensity.csv"), row.names = F)
write.csv(global_pool_share, file.path(resdir, "global_production_shares.csv"), row.names = F)

# using the WITCH module logic, complete a full set of iso3 data using the same extrapolations as in the module for global coverage also for the historical data
energy_jobs_intensity_iso3_completed <- energy_jobs_intensity %>% 
complete(iso3, nesting(category, fuel))
#add oil construction and oem based on gas
energy_jobs_intensity_iso3_completed <- rbind(energy_jobs_intensity_iso3_completed, energy_jobs_intensity_iso3_completed %>% filter(category %in% c("construction", "oem") & fuel=="gas") %>% mutate(fuel="oil"))
#now add missing ISO3 values
energy_jobs_intensity_iso3_completed <- energy_jobs_intensity_iso3_completed %>% complete(iso3=countrycode::codelist$iso3c[!is.na(countrycode::codelist$iso3c)], nesting(category, fuel))
#original dataset: 544 datapoints
#completed dataset: 249(iso3)*36(combinations of technologies and categories)=8964 data points

#Add OECD and witch regions
energy_jobs_intensity_iso3_completed <- energy_jobs_intensity_iso3_completed %>% mutate(oecd=ifelse(iso3 %in% c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA"), "oecd", "non-oecd"))
energy_jobs_intensity_iso3_completed <- energy_jobs_intensity_iso3_completed %>% left_join(read.xlsx(energy_jobs_database_file, sheet = "witch17regmap"))

#first take (weighted) mean across WITCH17 regions (like using witchtools)
energy_jobs_intensity_iso3_completed <- energy_jobs_intensity_iso3_completed %>% group_by(fuel, category, witch17) %>% mutate(value = ifelse(is.na(value), mean(value, na.rm = T), value))

# for NA values take corresponding oecd/non oecd mean
energy_jobs_intensity_iso3_completed <- energy_jobs_intensity_iso3_completed %>% group_by(fuel, category, oecd) %>% mutate(value = ifelse(is.na(value), mean(value, na.rm = T), value))

#construction & OEM of oil fired power plants equal to gas
energy_jobs_intensity_iso3_completed <- energy_jobs_intensity_iso3_completed %>% group_by(category, iso3) %>% mutate(value = ifelse(category %in% c("construction", "oem"), ifelse(fuel=="oil", value[fuel=="gas"], value), value))
#no oem/construction power plants for biogas nor trbiofuel
energy_jobs_intensity_iso3_completed <- energy_jobs_intensity_iso3_completed %>% mutate(value = ifelse(category %in% c("construction", "oem"), ifelse(fuel %in% c("biogas", "trbiofuel"), 0, value), value))
#manufacturing only for biomass, hydro, solar, and wind
manufacturing_sectors <- c("biomass", "hydro", "solar_pv", "solar_csp", "wind_on", "wind_off")
if(add_fossil_manufacturing) manufacturing_sectors <- c("biomass", "hydro", "solar_pv", "solar_csp", "wind_on", "wind_off", "coal", "gas", "nuclear", "oil")
energy_jobs_intensity_iso3_completed <- energy_jobs_intensity_iso3_completed %>% mutate(value = ifelse(category %in% c("manufacturing"), ifelse(fuel %in% manufacturing_sectors, value, 0), value))
# geothermal
energy_jobs_intensity_iso3_completed <- energy_jobs_intensity_iso3_completed %>% mutate(value = ifelse(fuel %in% c("geothermal"), 0, value))
#now remove geothermal and biogas which are for now used
energy_jobs_intensity_iso3_completed <- energy_jobs_intensity_iso3_completed %>% filter(!(fuel %in% c("geothermal", "biogas")))

############### Save data for modeling input, now taking completed ISO3 dataset and overwrite csv file ##########
energy_jobs_intensity_iso3_completed <- energy_jobs_intensity_iso3_completed %>% ungroup() %>% select_at(names(energy_jobs_intensity)) %>% as.data.frame()
write.csv(energy_jobs_intensity_iso3_completed, file.path(resdir, "energy_jobs_intensity_iso3_completed.csv"), row.names = F)






