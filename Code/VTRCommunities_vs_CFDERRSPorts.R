##### 
## A short bit of code to look at matches and mismatches between communities identified from VTR data and the ports listed in the CFDERRS data. 
#####

### Preliminaries!!!
# Helper function to install any needed missing packages
library_check<- function(libraries) {
  lapply(libraries, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}

# Run it to install needed libraries
library_check(c("raster", "tidyverse"))

# Data pathways
vtr.path<- "/Volumes/Shared/Research/COCA-conf/"
cfderrs.path<- "/Volumes/Shared/Research/COCA-conf/"
out.path<- "./Results/"
### End preliminaries

### Start code
## Read in Footprint data 
vtr.dat<- readRDS(paste(vtr.path, "VTR fishing footprints by community and gear type 2011-2015.rds", sep = ""))

# get the community names
comm.names<- vtr.dat$JGS.COMMUNITY

## Read in CFDERRS ports
cfderrs.dat<- read_csv(paste(cfderrs.path, "port_name_list.csv", sep = "")) %>%
  dplyr::select(., -X1)

## Preliminary inspection -- what are going to be some known issues? what column of CFDERRS is most like the VTR community names
head(comm.names)
head(cfderrs.dat)

# Well, immediate issue is that the VTR communities have multiple "_". Let's split the names by the LAST instance of the "_". This should give us two columns, one for community, one for state. Then we can remove any special characters or spaces from the community column in the VTR data as well as the PORT_NAME column for the cfderrs data. Finally, combining the stripped commuinity column with the second state column for VTR and then matching up to a combined stripped PORT_NAME column and PORT_STATE from the CFDERRS should give us the best chance to match. 
# VTR data first
comm.names.split<- strsplit(comm.names, "_\\s*(?=[^_]+$)", perl=TRUE)

# First item in each list element will have community, second has the state...
comm.dat<- data.frame("JGS" = comm.names, "CommunityOnly" = unlist(lapply(comm.names.split, "[", 1)), "StateOnly" = unlist(lapply(comm.names.split, "[", 2)))

# Did that make sense?
unique(comm.dat$StateOnly)
unique(comm.dat$CommunityOnly)

# Seems good...lets clean up the Community only column to remove all spaces and special characters
comm.dat$CommunityOnlyStripped<- str_replace_all(comm.dat$CommunityOnly, "[^[:alnum:]]", "")

# Make the community merge column
comm.dat$MatchColumn<- paste(comm.dat$CommunityOnlyStripped, comm.dat$StateOnly, sep = "_")

# Now CFDERRS
cfderrs.dat$PortNameStripped<- str_replace_all(cfderrs.dat$PORT_NAME, "[^[:alnum:]]", "")
cfderrs.dat$MatchColumn<- paste(cfderrs.dat$PortNameStripped, cfderrs.dat$PORT_STATE, sep = "_")

## Merge them together and save the result
comm.cfderrs.dat<- comm.dat %>%
  left_join(., cfderrs.dat)

## Unique only
names.matched<- comm.cfderrs.dat[unique(comm.cfderrs.dat$JGS),]
write_csv(names.matched, paste(out.path, "VTR_CFDERRS_Matched.csv", sep = ""))
