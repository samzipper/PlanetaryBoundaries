## Carlisle-Flows+Biology_01_QA-QC.R
#' This script is intended to open the raw data received from Daren Carlisle
#' with monthly flows and bug/fish status and take an initial look.

# load paths + packages
source(file.path("src", "paths+packages.R"))

## load raw data
df.bio.in <- 
  file.path(dir.carlisle.raw, "OutTom_Bio.csv") %>% 
  read.csv(stringsAsFactors=F)

df.str.in <- 
  file.path(dir.carlisle.raw, "OutTom.csv") %>% 
  read.csv(stringsAsFactors=F)

# relevant columns descriptions from Daren email: 
#   ID=USGS gage ID, with leading "T" so that leading zeros are not inadvertently lost
#   df.bio$YEAR=year of biological sampling
#   df.bio$COND=continuous (O/E) measure of biological condition for invertebrates (note "NA" for fish)
#   df.bio$BIO.COND=categorical biological condition indicator for invertebrates and fish
#   df.bio$BUGS.FISH=indicates invertebrates (B) or fish (F)
#
#   df.str$YearMonth=Year and month concatenated
#   df.str$Observed_Monthly_Q=OBSERVED monthly flow, in cfs
#   df.str$Pred_Nat_Monthly_Q=PREDICTED NATURAL monthly flow, in cfs

## get lat/lon of USGS sites
df.sites <-
  df.str.in$ID %>% 
  unique() %>% 
  sub(".", "", .) %>%  # get rid of leading "T"
  readNWISsite() %>%   # get site data
  transform(ID = paste0("T", site_no)) %>%   # add ID column
  select(ID, dec_lat_va, dec_long_va, drain_area_va) %>%    # only keep useful columns
  set_colnames(c("ID", "lat", "long", "drain_area_mi2"))

## mess with bio data
# find sites that have COND (O/E) data
df.bio.cond <- 
  df.bio.in %>% 
  subset(is.finite(COND))

length(unique(df.bio.cond$ID))  # some sites have samples in multiple years

# take mean of COND for sites with multiple readings
df.bio.cond.mean <-
  df.bio.cond %>% 
  group_by(ID) %>% 
  summarize(COND.bio.mean = mean(COND))

## mess with streamflow data
df.str <-
  df.str.in %>% 
  left_join(., df.sites, by="ID") %>% 
  transform(year = as.numeric(substr(YearMonth, 1, 4)),
            month = as.numeric(substr(YearMonth, 5, 6)),
            Pred_Q_mm.d = 1000*Pred_Nat_MonthlyQ*86400/(3.281*drain_area_mi2*5280*5280), # convert cfs to mm/d
            Obs_Q_mm.d = 1000*Observed_MonthlyQ*86400/(3.281*drain_area_mi2*5280*5280)) 

# calculate some summary statistics about the sites
df.sites <-
  df.str %>% 
  group_by(ID) %>% 
  summarize(year_start = min(year),
            year_end = max(year),
            rec_length_mo = sum(is.finite(Obs_Q_mm.d)),
            Obs_Q_mm.yr = 12*mean(Obs_Q_mm.d*days_in_month(month)),
            Pred_Q_mm.yr = 12*mean(Pred_Q_mm.d*days_in_month(month)),
            COND = Obs_Q_mm.yr/Pred_Q_mm.yr) %>% 
  left_join(df.sites, ., by="ID") %>% 
  left_join(., df.bio.cond.mean, by="ID")

# one of the sites has weird predictions
df.str %>% 
  subset(ID=="T12334550") %>% 
  head()

df.sites %>% 
  subset(COND > 5)

# list of sites to exclude and why
bad.sites <-
  df.sites$ID[df.sites$COND > 2] # weird cond values

good.sites <- df.sites$ID[!(df.sites$ID %in% bad.sites)]


# start/end years
df.sites %>% 
  subset(ID %in% good.sites) %>% 
  summary()

sum(is.finite(df.sites$COND) & is.finite(df.sites$COND.bio.mean) & df.sites$COND < 2)
sum(is.finite(df.sites$COND))
sum(is.finite(df.sites$COND.bio.mean))

# plotting ----------------------------------------------------------------

# USA outline for maps
df.USA <- map_data("state")

## map of sites
df.sites %>% 
  subset(ID %in% good.sites) %>% 
  ggplot(aes(x=long, y=lat)) +
  geom_polygon(data=df.USA, aes(x=long, y=lat, group=group), fill=NA, color=col.gray) +
  geom_point() +
  scale_x_continuous(name="Long") +
  scale_y_continuous(name="Lat") +
  labs(subtitle=paste0("Excluding ", length(bad.sites), " sites with Q O/E > 2 - streamflow model problem?")) +
  coord_quickmap() +
  ggsave(file.path("plots", "Carlise-Flows+Biology_01_QA-QC_MapSites.png"),
         width=6, height=4, units="in")

df.sites %>% 
  subset(ID %in% good.sites) %>% 
  ggplot(aes(x=long, y=lat, fill=COND.bio.mean)) +
  geom_polygon(data=df.USA, aes(x=long, y=lat, group=group), fill=NA, color=col.gray) +
  geom_point(shape=21, color="black") +
  scale_x_continuous(name="Long") +
  scale_y_continuous(name="Lat") +
  scale_fill_gradient2(name="Bio O/E", midpoint=1, limits=c(0,2)) +
  labs(subtitle=paste0("Excluding ", length(bad.sites), " sites with Q O/E > 2 - streamflow model problem?")) +
  coord_quickmap() +
  theme(legend.position="bottom") +
  ggsave(file.path("plots", "Carlise-Flows+Biology_01_QA-QC_MapBioO-E.png"),
         width=6, height=4, units="in")

df.sites %>% 
  subset(ID %in% good.sites) %>% 
  ggplot(aes(x=long, y=lat, fill=COND)) +
  geom_polygon(data=df.USA, aes(x=long, y=lat, group=group), fill=NA, color=col.gray) +
  geom_point(shape=21, color="black") +
  scale_x_continuous(name="Long") +
  scale_y_continuous(name="Lat") +
  scale_fill_gradient2(name="Q O/E", midpoint=1, limits=c(0,2)) +
  labs(subtitle=paste0("Excluding ", length(bad.sites), " sites with Q O/E > 2 - streamflow model problem?")) +
  coord_quickmap() +
  theme(legend.position="bottom") +
  ggsave(file.path("plots", "Carlise-Flows+Biology_01_QA-QC_MapQO-E.png"),
         width=6, height=4, units="in")

df.sites %>% 
  subset(ID %in% good.sites) %>% 
  ggplot(aes(x=long, y=lat, color=log10(Obs_Q_mm.yr))) +
  geom_polygon(data=df.USA, aes(x=long, y=lat, group=group), fill=NA, color=col.gray) +
  geom_point() +
  scale_x_continuous(name="Long") +
  scale_y_continuous(name="Lat") +
  scale_color_gradient(name="log(Q) [mm/yr]") +
  labs(subtitle=paste0("Excluding ", length(bad.sites), " sites with Q O/E > 2 - streamflow model problem?")) +
  coord_quickmap() +
  theme(legend.position="bottom") +
  ggsave(file.path("plots", "Carlise-Flows+Biology_01_QA-QC_MapQMeanAnnual.png"),
         width=6, height=4, units="in")

## year of bio data
df.bio.cond %>% 
  subset(ID %in% good.sites) %>% 
  ggplot(aes(x=YEAR)) +
  geom_histogram(binwidth=1) +
  scale_x_continuous(name="Sample Year") +
  scale_y_continuous(name="Number of Sites") +
  ggsave(file.path("plots", "Carlise-Flows+Biology_01_QA-QC_HistBioYear.png"),
         width=4, height=4, units="in")
  
## summary stats of site info
df.sites %>% 
  subset(ID %in% good.sites) %>% 
  ggplot(aes(x=rec_length_mo)) +
  geom_histogram(binwidth=5) +
  scale_x_continuous(name="Streamflow Record Length [mo]") +
  scale_y_continuous(name="Number of Sites") +
  ggsave(file.path("plots", "Carlise-Flows+Biology_01_QA-QC_HistQRecLength.png"),
         width=4, height=4, units="in")

df.sites %>% 
  subset(ID %in% good.sites) %>% 
  ggplot(aes(x=Obs_Q_mm.yr)) +
  geom_histogram(binwidth=50) +
  scale_x_continuous(name="Observed Mean Annual Discharge [mm]") +
  scale_y_continuous(name="Number of Sites") +
  ggsave(file.path("plots", "Carlise-Flows+Biology_01_QA-QC_HistQMeanAnnual.png"),
         width=4, height=4, units="in")

df.sites %>% 
  subset(ID %in% good.sites) %>%   # there are 8 sites with a COND > 2 (5 with COND > 5)
  ggplot(aes(x=COND)) +
  geom_histogram(binwidth=0.05) +
  geom_vline(xintercept=1, color=col.gray) +
  scale_x_continuous(name="Observed/Expected Mean Annual Discharge", limits=c(0,2)) +
  scale_y_continuous(name="Number of Sites") +
  labs(subtitle=paste0("Excluding ", length(bad.sites), " sites with Q O/E > 2 - streamflow model problem?")) +
  ggsave(file.path("plots", "Carlise-Flows+Biology_01_QA-QC_HistQO-E.png"),
         width=4, height=4, units="in")

df.sites %>% 
  subset(ID %in% good.sites) %>%
  ggplot(aes(x=COND.bio.mean)) +
  geom_histogram(binwidth=0.05) +
  geom_vline(xintercept=1, color=col.gray) +
  scale_x_continuous(name="Observed/Expected Biological Condition", limits=c(0,2)) +
  scale_y_continuous(name="Number of Sites") +
  ggsave(file.path("plots", "Carlise-Flows+Biology_01_QA-QC_HistBioO-E.png"),
         width=4, height=4, units="in")

## comparison: bio vs Q O/E
df.sites %>% 
  subset(ID %in% good.sites) %>%
  ggplot(aes(x=COND.bio.mean, y=COND)) +
  geom_abline(intercept=0, slope=1, color=col.gray) +
  geom_point() +
  stat_smooth(method="lm") +
  scale_x_continuous(name="Observed/Expected Biological Condition", limits=c(0,2)) +
  scale_y_continuous(name="Observed/Expected Mean Annual Discharge", limits=c(0,2)) +
  labs(subtitle="Excluding 8 sites with O/E > 2 - streamflow model problem?") +
  ggsave(file.path("plots", "Carlise-Flows+Biology_01_QA-QC_QvsBioO-E.png"),
         width=4, height=4, units="in")

df.sites %>% 
  subset(ID %in% good.sites) %>%
  ggplot(aes(x=abs(COND.bio.mean-1), y=abs(COND-1))) +
  geom_abline(intercept=0, slope=1, color=col.gray) +
  geom_point() +
  stat_smooth(method="lm") +
  scale_x_continuous(name="abs(O/E) - 1 for Biological Condition", limits=c(0,1)) +
  scale_y_continuous(name="abs(O/E) - 1 for Mean Annual Discharge", limits=c(0,1)) +
  labs(subtitle="Excluding 8 sites with O/E > 2 - streamflow model problem?") +
  ggsave(file.path("plots", "Carlise-Flows+Biology_01_QA-QC_QvsBioO-Eabs.png"),
         width=4, height=4, units="in")

df.sites %>% 
  subset(ID %in% good.sites) %>%
  lm(COND ~ COND.bio.mean, data=.) %>% 
  summary()
