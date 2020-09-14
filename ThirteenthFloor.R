#### 0) INTRO ####
## SET WORKING DIRECTORY (DOWNLOADS)
path <- paste0("C:/Users/", Sys.info()[["user"]], "/Downloads")
setwd(path)

# !!! (before executing the code, manually download 
# "000-daily-trade-estimates.zip" to this directory)
# source: https://drive.google.com/drive/folders/1vs08OwD0QmFoUznrE3QkV_KoHxyBmrDf

## LOAD PACKAGES
packs <- c("openxlsx","reshape2","data.table","ggplot2","maps","Cairo",
           "tabulizer","gdata","stringr","countrycode","XML","stlplus")
newpacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(newpacks)) install.packages(newpacks)
lapply(packs, library, character.only=T)

## ADD TWO CUSTOM FUNCTIONS
`%!in%` = Negate(`%in%`); NAZ <- function(x) { x[is.na(x)] <- 0; x }


#### 1) CPB ####
## DOWNLOAD CPB TRADE DATA
# (fine aggregation level is present in Excel only...)
cpb <- read.xlsx("https://www.cpb.nl/sites/default/files/wtmonitor/cpb-data-wtm.xlsx")

## PROCESSING FROM EXCEL FORMAT TO LONG DATA FORMAT
cpb[3,4:ncol(cpb)] <- cpb[2,4:ncol(cpb)]; cpb <- cpb[3:28,]; row.names(cpb) <- c()
cpb[1,2] <- "Code"; cpb[1,1] <- "Region"; colnames(cpb) <- cpb[1,]; cpb <- cpb[-1, ]
cpb$Flow <- substring(cpb$Code,1,1); cpb$Flow[cpb$Flow=="t"] <- "Trade";
cpb$Flow[cpb$Flow=="x"] <- "Export"; cpb$Flow[cpb$Flow=="m"] <- "Import"; 
cpb$Region[cpb$Region%in%c("World exports", "World imports", "World trade")] <- "World"
# melt to long data format
cpb <- melt(cpb, id.vars=c("Region","Flow","Code","Values 2010, usd bln"))
names(cpb) <- c("Region","Flow","Code","Val_2010","Period","Index_2010")
cpb$Val_2010 <- as.numeric(cpb$Val_2010); cpb$Index_2010 <- as.numeric(cpb$Index_2010) 
# change date format
cpb$Period <- gsub("m", "-", cpb$Period); cpb$Period <- as.Date(paste0(cpb$Period,"-01"))
# construct factors for sorting regions
cpb$Region <- as.factor(cpb$Region)
cpb$Region <- factor(cpb$Region, levels=cpb$Region[2:13])
# construct even more aggregated groups...
cpb$Group[cpb$Region%in%cpb$Region[c(2,3,8)]] <- "Aggregated"
cpb$Group[cpb$Region%in%cpb$Region[c(4:7)]] <- "Advanced"
cpb$Group[cpb$Region%in%cpb$Region[c(9:13)]] <- "Emerging"
# ...and prepare them for sorting
cpb$Group <- as.factor(cpb$Group)
cpb$Group <- factor(cpb$Group, levels=c("Aggregated","Advanced","Emerging"))

## RECALC TO THE NEW BASE YEAR
new_base_year <- 2017
setDT(cpb)[, Index_Upd := Index_2010/mean(Index_2010[substr(Period,1,4)==new_base_year])*100, by = .(Code)]


pic1 <- ggplot(cpb[cpb$Flow!="Trade" & cpb$Period>"2017-12-31",], aes(x=Period, y=Index_Upd)) + 
  geom_line(aes(group=interaction(Region, Flow), col=Region, size=Region), alpha=0.5) + 
  facet_grid(Flow~Group) + theme_minimal(base_size=17) + xlab(NULL) + ylab(NULL) +
  theme(legend.position="right", axis.text.x = element_text(angle = 90),
        panel.spacing.y = unit(1, "lines"), legend.title = element_blank()) +
  scale_size_manual(values=c(1,0.5,1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5)) +
  scale_color_manual(values=c("black","blue","blue","purple","orange","forestgreen",
                              "firebrick","red","grey40","green","darkgoldenrod","cyan4")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
# save in PDF
cairo_pdf("pic1.pdf", width=9, height=5); pic1; invisible(dev.off())
# save in PNG
png('pic1.png', width = dev.size("in")[1]*1.5, height = dev.size("in")[1]*0.8, units = 'in', res = 300)
pic1; dev.off()
###


#### 2) COUNTRY GROUPS ON WORLD MAP ####
## GET COUNTRY GROUPS FROM PDF
addr <- "https://www.cpb.nl/sites/default/files/omnidownload/CPB-Background-Document-April2020-The-CPB-World-Trade-Monitor-technical-description-update_3.pdf"
GRP <- extract_tables(addr, page=11) # tabulizer (may be Java ERROR!!!)
GRP <- as.data.frame(GRP[1], stringsAsFactors=F) # convert from list to data frame
GRP <- GRP[!apply(GRP == "", 1, all),] # delete empty rows
GRP <- subset(GRP, substr(X1,1,1)!="(" & substr(X1,3,3)==" ") # delete endnotes
GRP[] <- lapply(GRP, function(x) gsub("\\s*\\([^\\)]+\\)","",x)) #delete comments in cells
GRP[] <- lapply(GRP, function(x) gsub('\\b\\w{3,}\\b','',x)) # delete country names
GRP[] <- trim(lapply(GRP, function(x) gsub("\\s+",' ',x))) # delete multiple spaces

# split first col of GRP into three cols
intrm <- as.data.frame(do.call(rbind, str_split(GRP[,1], ' ')), stringsAsFactors=F)
intrm[2][intrm[2]==intrm[1]] <- ""; intrm[3][intrm[3]==intrm[1]] <- "" # delete repeated values
# construct GRP file with 5 cols and country groups from PDF
GRP <- cbind(intrm, GRP[,2], GRP[,3]); names(GRP) <- c( # (setting col names)
  "Advanced economies", "Asia excl. Japan", "Eastern Europe / CIS", "Latin America", "Africa and Middle East")
grp_list <- names(GRP)

## RESHAPE TABLES IN LONG FORMAT, ADD CODES AND STANDARD COUNTRY NAMES
GRP <- melt(GRP, id.vars=c()); names(GRP) <- c("Aggregate","iso2")
GRP[, ] <- lapply(GRP[, ], as.character) # all columns as text
GRP <- subset(GRP, iso2!="") # drop empty rows
GRP$iso3 <- countrycode(GRP$iso2,'iso2c','iso3c')
GRP$Name <- countrycode(GRP$iso2,'iso2c','country.name')

## ADD CPB FINE AGGREGATION GROUPINGS
GRP$AggrFine <- GRP$Aggregate
GRP$AggrFine[GRP$Aggregate=="Advanced economies"] <- "Other advanced economies"
GRP$AggrFine[GRP$iso3=="USA"] <- "United States"
GRP$AggrFine[GRP$iso3=="JPN"] <- "Japan"
GRP$AggrFine[GRP$Aggregate=="Asia excl. Japan"] <- "Emerging Asia (excl China)"
GRP$AggrFine[GRP$iso3=="CHN"] <- "China"
## ADD EURO AREA GROUPING (COUNTRY LIST FROM EUROSTAT)
eu_site <- readLines("https://ec.europa.eu/info/business-economy-euro/euro-area/what-euro-area_en")
ea_members <- readHTMLTable(htmlParse(eu_site), stringsAsFactors=F)
ea_members <- ea_members[[1]][2:dim(ea_members[[1]])[1],1]
GRP$AggrFine[GRP$Name%in%ea_members] <- "Euro Area"
#

## PREPARE DATA FOR THE MAP
world <- map_data("world")
world <- world[world$region != "Antarctica",] # exclude antarctica
world$iso3 <- countrycode(world$region,"country.name","iso3c")
world$iso3[world$subregion=="Hong Kong"] <- "HKG"; world$iso3[world$subregion=="Macao"] <- "MAC"
world$iso2 <- GRP$iso2[match(world$iso3, GRP$iso3)]
world$cntr_grp <- GRP$Aggregate[match(world$iso3, GRP$iso3)]
world$cntr_grp_fine <- GRP$AggrFine[match(world$iso3, GRP$iso3)]
world$cntr_grp <- factor(world$cntr_grp, grp_list); world$cntr_grp_fine <- factor(
  world$cntr_grp_fine, levels=trim(c(as.character(cpb$Region[4:7]), as.character(cpb$Region[9:13]))))

## MAP FOR CPB FINE AGGREGATION GROUPINGS 
pic2 <- ggplot(world, aes(long, lat, group=group, fill=cntr_grp_fine)) + geom_polygon(show.legend = F) + 
  theme_void(base_size=17) + geom_map(map=world, aes(map_id=region), color="white") + 
  theme(plot.margin = margin(0, 0, 0.5, 0, "cm"), legend.position="bottom", legend.title = element_blank(), 
        legend.text=element_text(size=12), legend.spacing.x = unit(0.2, "cm")) +
  guides(fill=guide_legend(nrow=3, byrow=T)) + scale_fill_discrete(breaks = levels(world$cntr_grp_fine))
# save in PDF
cairo_pdf("pic2.pdf", width=9, height=5); pic2; invisible(dev.off())
# save in PNG
png('pic2.png', width = dev.size("in")[1]*1.5, height = dev.size("in")[1]*0.8, units = 'in', res = 300)
pic2; dev.off()
###


#### 3) AIS DAILY TRADE ESTIMATES (RAW DATA IS OBTAINED FROM IMF WP) ####
## CREATE EMPTY FILE FOR DAILY TRADE ESTIMATES
g <- unzip(paste0(path,"/000-daily-trade-estimates.zip"), exdir=tempdir())
g <- sort(g); options(scipen=999); tr20 <- fread(g[6])
TRADE <- as.data.frame(matrix(ncol=ncol(tr20), nrow=0)); names(TRADE) <- names(tr20)

tab1 <- as.data.frame(cbind(lengths(sapply(tr20, unique)), sapply(tr20, class)))
names(tab1) <- c("unique", "class"); tab1 <- tab1[-grep("dummy", row.names(tab1)),]
tab1 <- tab1[-grep("_ma", row.names(tab1)),]; tab1 <- tab1[-grep("pid", row.names(tab1)),]
rm(tr20)

# BIND ALL CSV FILES INTO THIS EMPTY FILE
i <- 1
while (i < 7) {
  tr <- fread(g[i])
  TRADE <- rbind(TRADE, tr)
  i = i+1
}
rm(tr)

## GET STANDARD DATE FORMAT AND ADD MONTH
TRADE$date <- paste0(substr(TRADE$date,1,2),"-",substr(TRADE$date,3,5),"-",substr(TRADE$date,6,9))
TRADE$date <- paste0(substr(TRADE$date,1,2),"-",
                     match(substr(TRADE$date,4,6), tolower(month.abb)),"-",substr(TRADE$date,8,11))
TRADE$date <- as.Date(TRADE$date, format="%d-%m-%Y")
TRADE$month <- as.Date(paste0(substr(TRADE$date,1,8),"01"))
# check if data is full for the last several months
dcast(TRADE, month~"", length, value.var="date")[(length(unique(TRADE$month))-3):length(unique(TRADE$month))]
TRADE <- subset(TRADE, month!="2020-08-01") # delete data for august (not complete data)

## AGGREGATE DATA TO MONTHLY PERIODICITY
TRADE_M <- dcast(TRADE, country_name+month+VESSEL_TYPE_COARSE~"", sum, value.var=c("imp_mtc","exp_mtc"))
rm(TRADE)

pic3 <- ggplot(TRADE_M[TRADE_M$country_name=="WORLD" & TRADE_M$VESSEL_TYPE_COARSE!="Total",], aes(
  month, exp_mtc/1000000)) + geom_line(aes(group=interaction(country_name, VESSEL_TYPE_COARSE)), col="darkorange") + 
  facet_wrap(~VESSEL_TYPE_COARSE, scales="free_y") + xlab(NULL) + ylab(NULL) + theme_minimal(base_size = 17) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# save in PDF
cairo_pdf("pic3.pdf", width=9, height=5); pic3; invisible(dev.off())
# save in PNG
png('pic3.png', width = dev.size("in")[1]*1.5, height = dev.size("in")[1]*0.8, units = 'in', res = 300)
pic3; dev.off()
###

## GET BACI TRADE DATA (FOR WEIGHTING)
url_hs17 <- "http://www.cepii.fr/DATA_DOWNLOAD/baci/trade_flows/BACI_HS17_V202001.zip"
temp <- tempfile(); download.file(url_hs17,temp); f <- unzip(temp, list=T)
BACI <- read.csv(unz(temp, f[1,1])); unlink(temp)
BACI$k[nchar(BACI$k)==5] <- paste0("0",BACI$k)[nchar(BACI$k)==5] # add leading zeros where necessary

## PREPARE DATA FOR BINDING (col names should fit)
BACI_M <- dcast(BACI, j+substr(k,1,4)~"v", sum, value.var="v")
BACI_X <- dcast(BACI, i+substr(k,1,4)~"v", sum, value.var="v")
names(BACI_M) <- c("rep","cmd","val"); names(BACI_X) <- c("rep","cmd","val")
BACI_M$flow <- "Imp"; BACI_X$flow <- "Exp"

## BIND EXPORTS & IMPORTS INTO ONE FILE
BACI_XM <- rbind(BACI_X, BACI_M); rm(BACI, BACI_X, BACI_M)
BACI_XM$cmd2 <- substr(BACI_XM$cmd,1,2) # add 2-digit codes

## CONSTRUCT HS GROUPS BY VESSEL TYPES (FROM IMF PAPER)
# https://www.imf.org/~/media/Files/Publications/WP/2020/English/wpiea2020057-print-pdf.ashx
unique(TRADE_M$VESSEL_TYPE_COARSE) # look at vessel types
BACI_XM$vessel[BACI_XM$cmd2%in%c("01","02","03","04","05","07","08",15:16)] <- "Foodstuff"
BACI_XM$vessel[BACI_XM$cmd%in%c(2711)] <- "LPG/LNG"
BACI_XM$vessel[BACI_XM$cmd%in%c(8701:8705)] <- "Vehicles"
BACI_XM$vessel[BACI_XM$cmd%in%c(2709:2710)] <- "Oil/chemicals"
BACI_XM$vessel[BACI_XM$cmd2%in%c(28:29)] <- "Oil/chemicals"
BACI_XM$vessel[BACI_XM$cmd2%in%c("06",18:24,30:67,69:71,82:86,90:97)] <- "Container_gcargo"
BACI_XM$vessel[BACI_XM$cmd%in%c(sort(
  unique(BACI_XM$cmd[BACI_XM$cmd2%in%c(87) & BACI_XM$cmd%!in%c(8701:8705)])))] <- "Container_gcargo"
BACI_XM$vessel[BACI_XM$cmd2%in%c("09",10:14,17,25:26,68,72:81)] <- "Bulk"
BACI_XM$vessel[BACI_XM$cmd%in%c(sort(
  unique(BACI_XM$cmd[BACI_XM$cmd2%in%c(27) & BACI_XM$cmd%!in%c(2709:2711)])))] <- "Bulk"
BACI_XM$vessel[BACI_XM$cmd2%in%c(88:89)] <- "Unassigned"
BACI_XM <- subset(BACI_XM, vessel!="Unassigned")

## AGGREGATE DATA BY VESSEL TYPES
BACI_XM <- dcast(BACI_XM, rep+vessel~flow, sum, value.var="val")
BACI_XM$cntr <- countrycode(BACI_XM$rep, 'un', 'iso3c')
# default codes in countrycode function are outdated for the fillowing countries:
BACI_XM$cntr[BACI_XM$rep==251] <- "FRA"; BACI_XM$cntr[BACI_XM$rep==381] <- "ITA"
BACI_XM$cntr[BACI_XM$rep==842] <- "USA"; BACI_XM$cntr[BACI_XM$rep==579] <- "NOR"
BACI_XM$cntr[BACI_XM$rep==699] <- "IND"; BACI_XM$cntr[BACI_XM$rep==757] <- "CHE"
# specific correction of codes (Belgium-Luxembourg & South African Customs Union)
BACI_XM$cntr[BACI_XM$rep==58] <- "BEL"; BACI_XM$cntr[BACI_XM$rep==711] <- "ZAF"
# calculate export and import structure
setDT(BACI_XM)[, Exp_Str := Exp/sum(Exp), by = .(rep)]
setDT(BACI_XM)[, Imp_Str := Imp/sum(Imp), by = .(rep)]


## AGGREGATE OVER VESSEL TYPES
TRADE_M$cntr <- countrycode(TRADE_M$country_name, 'country.name', 'iso3c')
TRADE_Mw <- subset(TRADE_M, VESSEL_TYPE_COARSE=="Total" & country_name=="WORLD")
TRADE_M$imp_str <- BACI_XM$Imp_Str[match(interaction(TRADE_M$cntr,TRADE_M$VESSEL_TYPE_COARSE),
                                   interaction(BACI_XM$cntr, BACI_XM$vessel))]
TRADE_M$exp_str <- BACI_XM$Exp_Str[match(interaction(TRADE_M$cntr,TRADE_M$VESSEL_TYPE_COARSE),
                                         interaction(BACI_XM$cntr, BACI_XM$vessel))]
TRADE_M <- subset(TRADE_M, is.na(cntr)==F); TRADE_M <- subset(TRADE_M, VESSEL_TYPE_COARSE!="Total")
TRADE_M$imp_mtc <- NAZ(TRADE_M$imp_mtc); TRADE_M$exp_mtc <- NAZ(TRADE_M$exp_mtc) # NAs to zeros
# metric tons of cargo discounted by trade value structure
setDT(TRADE_M)[, imp_T := sum(imp_mtc*imp_str), by = .(month, cntr)]
setDT(TRADE_M)[, exp_T := sum(exp_mtc*exp_str), by = .(month, cntr)]
TRADE_M <- dcast(TRADE_M, cntr+country_name+month~"", mean, value.var=c("imp_T","exp_T"))

## SUBSET TO CPB GROUPS
TRADE_M$AggrFine <- GRP$AggrFine[match(TRADE_M$cntr, GRP$iso3)]
TRADE_M <- subset(TRADE_M, is.na(AggrFine)==F)
TRADE_M <- subset(TRADE_M, is.na(imp_T)==F)
cntr_check <- dcast(TRADE_M, cntr+country_name~"", sum, value.var=c("imp_T","exp_T"))

## AGGREGATE TRADE VALUE BY COUNTRIES
cntr_str <- dcast(BACI_XM, cntr~"", sum, value.var=c("Imp","Exp"))
cntr_str <- subset(cntr_str, cntr%in%cntr_check$cntr)

## COMPARE RANKS BY COUNTRIES BETWEEN CPB AND AIS DATA
cntr_check$impV_17 <- cntr_str$Imp[match(cntr_check$cntr, cntr_str$cntr)]
cntr_check$expV_17 <- cntr_str$Exp[match(cntr_check$cntr, cntr_str$cntr)]
cntr_check$imp_rdiff <- rank(-cntr_check$impV_17)-rank(-cntr_check$imp_T)
cntr_check$exp_rdiff <- rank(-cntr_check$expV_17)-rank(-cntr_check$exp_T)

pic4 <- ggplot(cntr_check, aes(imp_rdiff, exp_rdiff)) + theme_minimal(base_size=17) +
  geom_text(aes(label=cntr, col=pmax(abs(exp_rdiff),abs(imp_rdiff)))) +
  theme(legend.position="none") + scale_colour_gradient(low="grey", high="red") +
  xlab("Rank difference for imports") + ylab("Rank difference for exports") +
  geom_hline(yintercept=0, col="grey", linetype="dashed") + 
  geom_vline(xintercept=0, col="grey", linetype="dashed")
# save in PDF
cairo_pdf("pic4.pdf", width=9, height=5); pic4; invisible(dev.off())
# save in PNG
png('pic4.png', width = dev.size("in")[1]*1.5, height = dev.size("in")[1]*0.8, units = 'in', res = 300)
pic4; dev.off()
###

## DROP COUNTRIES WITH MULTIPLE NULLS IN DATA (CHE & PAR)
setDT(TRADE_M)[, nulls := sum(imp_T==0 | exp_T==0), by = .(cntr)]
TRADE_M <- subset(TRADE_M, nulls==0) # only countries without nulls in data

## ADD BACI TRADE VALUE DATA FOR WEIGHTING
TRADE_M$impV_17 <- cntr_check$impV_17[match(TRADE_M$cntr, cntr_check$cntr)]
TRADE_M$expV_17 <- cntr_check$expV_17[match(TRADE_M$cntr, cntr_check$cntr)]
# weights for country groups
setDT(TRADE_M)[, imp_S := impV_17/sum(impV_17), by = .(month, AggrFine)]
setDT(TRADE_M)[, exp_S := expV_17/sum(expV_17), by = .(month, AggrFine)]
# weights for the world aggregate
setDT(TRADE_M)[, imp_Sw := impV_17/sum(impV_17), by = .(month)]
setDT(TRADE_M)[, exp_Sw := expV_17/sum(expV_17), by = .(month)]

## NORMALIZE AIS DATA AS INDEX (2017 = 100)
base_year <- 2017
setDT(TRADE_M)[, imp_T := imp_T/mean(imp_T[substr(month,1,4)==base_year])*100, by = .(cntr)]
setDT(TRADE_M)[, exp_T := exp_T/mean(exp_T[substr(month,1,4)==base_year])*100, by = .(cntr)]

## PREPARE AIS DATA FOR WEIGHTING (country groups and world aggregate separately)
TRADE_M$imp_parts <- TRADE_M$imp_T*TRADE_M$imp_S
TRADE_M$exp_parts <- TRADE_M$exp_T*TRADE_M$exp_S
TRADE_M$imp_partsW <- TRADE_M$imp_T*TRADE_M$imp_Sw
TRADE_M$exp_partsW <- TRADE_M$exp_T*TRADE_M$exp_Sw

## FINAL GROUPING
TRADE_GRP <- dcast(TRADE_M, AggrFine+month~"", sum, value.var=c("imp_parts","exp_parts"))
names(TRADE_GRP) <- c("region", "month", "imp_ais","exp_ais")
TRADE_W <- dcast(TRADE_M, month~"", sum, value.var=c("imp_partsW","exp_partsW"))
TRADE_W$region <- "World"; names(TRADE_W)[2:3] <- c("imp_ais", "exp_ais")
TRADE_W <- subset(TRADE_W, select=c("region", "month", "imp_ais","exp_ais"))
TRADE_GRP <- rbind(TRADE_W, TRADE_GRP)

## ADD CPB DATA
TRADE_GRP$imp_cpb <- cpb$Index_Upd[match(interaction(TRADE_GRP$region, TRADE_GRP$month, "Import"),
                                         interaction(trim(as.character(cpb$Region)), cpb$Period, cpb$Flow))]
TRADE_GRP$exp_cpb <- cpb$Index_Upd[match(interaction(TRADE_GRP$region, TRADE_GRP$month, "Export"),
                                         interaction(trim(as.character(cpb$Region)), cpb$Period, cpb$Flow))]

## ADJUST AIS DATA FOR SEASONALITY (CPB DATA IS ALREADY ADJUSTED)
# trend + half of the remainder (for gliding)
setDT(TRADE_GRP)[, imp_ais := stlplus(ts(
  imp_ais, frequency = 12, start = 2015+3/12), s.window="periodic")$data$trend + 0.5*stlplus(ts(
    imp_ais, frequency = 12, start = 2015+3/12), s.window="periodic")$data$remainder, by = .(region)]
setDT(TRADE_GRP)[, exp_ais := stlplus(ts(
  exp_ais, frequency = 12, start = 2015+3/12), s.window="periodic")$data$trend + 0.5*stlplus(ts(
    exp_ais, frequency = 12, start = 2015+3/12), s.window="periodic")$data$remainder, by = .(region)]

## COMPARE OUR WORLD AGGREGATE WITH THE DEFAULT
TRADE_Mw$imp_ais <- TRADE_GRP$imp_ais[match(interaction("WORLD", TRADE_Mw$month),
                                            interaction(toupper(TRADE_GRP$region), TRADE_GRP$month))]
TRADE_Mw$exp_ais <- TRADE_GRP$exp_ais[match(interaction("WORLD", TRADE_Mw$month),
                                            interaction(toupper(TRADE_GRP$region), TRADE_GRP$month))]
setDT(TRADE_Mw)[, imp_mtc := imp_mtc/mean(imp_mtc[substr(month,1,4)==base_year])*100, by = .(country_name)]
setDT(TRADE_Mw)[, exp_mtc := exp_mtc/mean(exp_mtc[substr(month,1,4)==base_year])*100, by = .(country_name)]
setDT(TRADE_Mw)[, imp_mtc := stlplus(ts(
  imp_mtc, frequency = 12, start = 2015+3/12), s.window="periodic")$data$trend + 0.5*stlplus(ts(
    imp_mtc, frequency = 12, start = 2015+3/12), s.window="periodic")$data$remainder, by = .(country_name)]
setDT(TRADE_Mw)[, exp_mtc := stlplus(ts(
  exp_mtc, frequency = 12, start = 2015+3/12), s.window="periodic")$data$trend + 0.5*stlplus(ts(
    exp_mtc, frequency = 12, start = 2015+3/12), s.window="periodic")$data$remainder, by = .(country_name)]
TRADE_Mw <- subset(TRADE_Mw, select=-c(cntr, country_name, VESSEL_TYPE_COARSE))

TRADE_Mw$imp_cpb <- TRADE_GRP$imp_cpb[match(interaction("WORLD", TRADE_Mw$month),
                                            interaction(toupper(TRADE_GRP$region), TRADE_GRP$month))]
TRADE_Mw$exp_cpb <- TRADE_GRP$exp_cpb[match(interaction("WORLD", TRADE_Mw$month),
                                            interaction(toupper(TRADE_GRP$region), TRADE_GRP$month))]
TRADE_Mw <- melt(TRADE_Mw, id.vars="month")
TRADE_Mw$flow <- substr(TRADE_Mw$variable,1,3)
TRADE_Mw$variable <- substr(TRADE_Mw$variable,5,7)
TRADE_Mw$variable[TRADE_Mw$variable=="mtc"] <- "ais_def"
TRADE_Mw$variable[TRADE_Mw$variable=="ais"] <- "ais_team"
TRADE_Mw$variable[TRADE_Mw$variable=="cpb"] <- "cpb_data"
names(TRADE_Mw)[2] <- "source"

pic5 <- ggplot(TRADE_Mw, aes(month, value)) + 
  geom_line(aes(group=interaction(source, flow), col=source, size=source)) +
  facet_wrap(~flow) + theme_minimal(base_size=17) + theme(legend.position='bottom') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_size_manual(values=c(1,1,2)) + xlab(NULL) + ylab(NULL) +
  scale_color_manual(values=c("darkorange","darkgreen",alpha("blue", 0.4))) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
# save in PDF
cairo_pdf("pic5.pdf", width=9, height=5); pic5; invisible(dev.off())
# save in PNG
png('pic5.png', width = dev.size("in")[1]*1.5, height = dev.size("in")[1]*0.8, units = 'in', res = 300)
pic5; dev.off()
###
#

## RESHAPE TO LONG FORMAT
TRADE_GRP <- melt(TRADE_GRP, id.vars=c("region","month"))
TRADE_GRP$flow <- substr(TRADE_GRP$variable,1,3)
TRADE_GRP$variable <- substr(TRADE_GRP$variable,5,7)
names(TRADE_GRP)[3] <- "source"

fact_list <- trim(as.character(levels(cpb$Region)))
fact_list <- fact_list[fact_list%!in%c("Advanced economies", "Emerging economies")]
TRADE_GRP$region <- as.factor(TRADE_GRP$region)
TRADE_GRP$region <- factor(TRADE_GRP$region, levels=fact_list)

g_imp <- ggplot(TRADE_GRP[TRADE_GRP$flow=="imp",], aes(month, value)) + 
  geom_line(aes(group=interaction(region, source), col=source, size=source)) +
  facet_wrap(~region, ncol=5, scales="free_y", labeller = label_wrap_gen(width=14)) + 
  theme_minimal(base_size=17) + xlab(NULL) + ylab(NULL) +
  scale_color_manual(values=c("darkgreen",alpha("blue", 0.4))) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  scale_size_manual(values=c(1,2)) + theme(legend.position='bottom') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g_exp <- ggplot(TRADE_GRP[TRADE_GRP$flow=="exp",], aes(month, value)) + 
  geom_line(aes(group=interaction(region, source), col=source, size=source)) +
  facet_wrap(~region, ncol=5, scales="free_y", labeller = label_wrap_gen(width=14)) + 
  theme_minimal(base_size=17) + xlab(NULL) + ylab(NULL) +
  scale_color_manual(values=c("darkgreen",alpha("blue", 0.4))) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  scale_size_manual(values=c(1,2)) + theme(legend.position='bottom') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# save in PDF
cairo_pdf("pic6.pdf", width=9, height=5); g_exp; invisible(dev.off())
# save in PNG
png('pic6.png', width = dev.size("in")[1]*1.5, height = dev.size("in")[1]*0.8, units = 'in', res = 300)
g_exp; dev.off()
#
# save in PDF
cairo_pdf("pic7.pdf", width=9, height=5); g_imp; invisible(dev.off())
# save in PNG
png('pic7.png', width = dev.size("in")[1]*1.5, height = dev.size("in")[1]*0.8, units = 'in', res = 300)
g_imp; dev.off()
#
## FINISH ##