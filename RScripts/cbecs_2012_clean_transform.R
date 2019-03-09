#John Grando
#Process CBECS 2012 data file and return two dataframes
#1. A cleaned data frame, e.g. NA's recomputed or encoded
#2. A transformed data frame, e.g. encoders separated to individual columns

#Libraries
library(dplyr)
library(tidyr)
library(caret)

#make column categorical and replace na with -1
make_cat_w_repl <- function(x){
  as.factor(ifelse(is.na(x), as.character(-1), as.character(x)))
}

#replace nas with a specified number in a numerical column
na_repl_num <- function(x, num){
  ifelse(is.na(x), num, x)
}

clean_encode_cbecs <- function(data, pba_filter=NA) {
  #Identify Weight columns
  weight_list <- names(data[grepl('^FINAL.*', names(data), ignore.case = TRUE)])
  #Identify imputation flag columns
  impute_list <- names(data[grepl('^Z.*', names(data), ignore.case = TRUE)])
  
  #clean data frame
  trim_df <- data %>% 
    ###Remove outliers
    #Extremely large electricity use points influencing plots
    filter(ELBTU < 5E8) %>% 
    #reduce scale of response variables
    mutate_at(vars(ELBTU, NGBTU, DHBTU, FKBTU, MFBTU), funs(./1000)) %>% 
    #Remove weight and imputation columns as well as IDs
    select(-one_of(weight_list), -one_of(impute_list), -PUBID) %>% 
    #Remove MFUSED, no variance %>% 
    select(-MFUSED) %>% 
    #Remove small buildings - not of interest and contain NAs - also remove NAs from this column implicitly
    filter(SQFT > 1000) %>% 
    #Remove buildings open for less than a year
    filter(YRCON != 2012) %>% 
    #Drop month ready for occupancy in 2012, not useful
    select(-MONCON) %>% 
    #YRCON identifies buildings older than 1946 as 995, which makes the numeric feature not accurate.  
    #Remove and rely on categories in YRCONC
    select(-YRCON) %>% 
    #If buidling was indicated to be not cooled, then percent cooled is zero
    mutate(COOLP = ifelse(COOL==2, 0, COOL)) %>% 
    #If one of the preceding non-electrical use questions is answered as yes, then zero for percent-lit
    mutate(LTOHRP = ifelse(ELUSED==2|WKHRS==0, 0, LTOHRP)) %>% 
    #If lights are always on, then they are never of
    mutate(LTNHRP = ifelse(WKHRS==168|OPEN24==1|LTNR24==2, 0, LTNHRP)) %>% 
    ### REMOVE FOR NOW, TOO MANY NULLS ###
    select(-LTNHRP) %>% 
    #0 percent heated, heated to less than 50, No useful information.  all one response
    select(-HTLS50) %>% 
    #Update BASEMNT to have value of 0 for one-floor buildings
    mutate(BASEMNT = ifelse(NFLOOR==1,0,BASEMNT)) %>%
    #make a has-baseament encoder and make a factor
    mutate(BASEMNT_HAS = as.factor(ifelse(is.na(BASEMNT)|BASEMNT==0,0,1))) %>% 
    #if responded with no elevators or escalators then number of elevators is 0
    mutate(NELVTR = ifelse(ELEVTR==2, 0, NELVTR)) %>% 
    mutate(NESLTR = ifelse(ESCLTR==2, 0, NESLTR)) %>% 
    #if don't know or refuse then assume zero?
    mutate_at(vars(NELVTR, NESLTR), funs(na_repl_num(.,0))) %>% 
    #If only indicated that electricity is bought from local utility, then assume it is 100%
    mutate(ELLUPCT = ifelse(ELLOCUT==1 & ELNONLU==2 & ELOTSRC==2 & ELCPLT==2, 100, ELLUPCT)) %>% 
    mutate(ELLUPCT = ifelse(ELLOCUT==2 & ELNONLU==1 & ELOTSRC==2 & ELCPLT==2, 0, ELLUPCT)) %>% 
    mutate(ELLUPCT = ifelse(ELLOCUT==2 & ELNONLU==2 & ELOTSRC==1 & ELCPLT==2, 0, ELLUPCT)) %>% 
    mutate(ELLUPCT = ifelse(ELLOCUT==2 & ELNONLU==2 & ELOTSRC==2 & ELCPLT==1, 0, ELLUPCT)) %>% 
    #fix 999 flag which means 'dont know' - NA
    mutate(ELLUPCT = ifelse(ELLUPCT==999, NA, ELLUPCT)) %>% 
    ##### Remove ELLUPCT FOR NOW ####
    select(-ELLUPCT) %>% 
    #Not interested in cost
    select(-WOEXP, -WOEXPC) %>%
    #if respondent did not indicate any residential refrigerators, then it is zero
    #also, if the question didn't apply then assume zero
    mutate(RFGRSN = ifelse(is.na(RFGEQP)|RFGEQP==2|RFGRES==2, 0, RFGRSN)) %>% 
    #same for compact refrigerators
    mutate(RFGCOMPN = ifelse(is.na(RFGEQP)|RFGEQP==2|RFGCOMP==2, 0, RFGCOMPN)) %>% 
    #same for walkins
    mutate(RFGWIN = ifelse(is.na(RFGEQP)|RFGEQP==2|RFGWI==2, 0, RFGWIN)) %>% 
    #same for open cases
    mutate(RFGOPN = ifelse(is.na(RFGEQP)|RFGEQP==2|RFGOP==2, 0, RFGOPN)) %>% 
    #same for closed cases
    mutate(RFGCLN = ifelse(is.na(RFGEQP)|RFGEQP==2|RFGCL==2, 0, RFGCLN)) %>% 
    #same for vending
    mutate(RFGVNN = ifelse(is.na(RFGEQP)|RFGEQP==2|RFGVEN==2, 0, RFGVNN)) %>%
    #same for ice makers
    mutate(RFGICN = ifelse(is.na(RFGEQP)|RFGEQP==2|RFGICE==2, 0, RFGICN)) %>%
    #same for large cold storage
    mutate(RFGSTP = ifelse(is.na(RFGEQP)|RFGEQP==2|RFGSTO==2, 0, RFGSTP)) %>%
    #if tv/video response was 'no', then it is zero
    mutate(TVVIDEON = ifelse(TVVIDEO==2, 0, TVVIDEON)) %>% 
    #if cash register response was 'no', then it is zero
    mutate(RGSTRN = ifelse(RGSTR==2, 0, RGSTRN)) %>% 
    #if copoer response was 'no', then it is zero
    mutate(COPIERN = ifelse(COPIER==2, 0, COPIERN)) %>% 
    #NFLOOR has flags, 994 and 995 which are groupings, convert to a binned predictor and remove NFLOOR
    mutate(NFLOOR_bin = as.factor(
      ifelse(NFLOOR == 1, 1, 
             ifelse(NFLOOR <= 5, 2,
                    ifelse(NFLOOR <= 10, 3,
                           ifelse(NFLOOR <= 14, 4,
                                  ifelse(NFLOOR == 994, 5, 
                                         ifelse(NFLOOR == 995, 6, NA)))))))) %>% 
    select(-NFLOOR) %>% 
    #FLCEILHT has flags, 995
    mutate(FLCEILHT_bin = as.factor(
      ifelse(FLCEILHT <= 10, 1, 
             ifelse(FLCEILHT <= 15, 2, 
                    ifelse(FLCEILHT <= 20, 3, 
                           ifelse(FLCEILHT <= 35, 4,
                                  ifelse(FLCEILHT <= 50, 5,
                                         ifelse(FLCEILHT == 995, 6, NA)))))))) %>% 
    select(-FLCEILHT) %>% 
    #NELVTR has flags, 995
    mutate(NELVTR_bin = as.factor(
      ifelse(NELVTR == 0, 1,
             ifelse(NELVTR <= 5, 2, 
                    ifelse(NELVTR <= 10, 3, 
                           ifelse(NELVTR <= 20, 4, 
                                  ifelse(NELVTR <= 30, 5,
                                         ifelse(NELVTR <= 40, 6,
                                                ifelse(NELVTR <= 50, 7,
                                                       ifelse(NELVTR == 995, 8, NA)))))))))) %>% 
    select(-NELVTR) %>%
    #NESLTR has flags, 995
    mutate(NESLTR_bin = as.factor(
      ifelse(NESLTR == 0, 1, 
             ifelse(NESLTR <= 5, 2, 
                    ifelse(NESLTR <= 15, 3, 
                           ifelse(NESLTR <= 20, 4,
                                  ifelse(NESLTR == 995, 7, NA))))))) %>% 
    select(-NESLTR) %>%
    #RWSEAT has flags, 99995
    mutate(RWSEAT_bin = as.factor(
      ifelse(RWSEAT == 0, 1, 
             ifelse(RWSEAT <= 250, 2, 
                    ifelse(RWSEAT <= 500, 3, 
                           ifelse(RWSEAT <= 1000, 4,
                                  ifelse(RWSEAT <= 1500, 5,
                                          ifelse(RWSEAT == 99995, 6, NA)))))))) %>% 
    select(-RWSEAT) %>%
    #PBSEAT has flags, 999995
    mutate(PBSEAT_bin = as.factor(
      ifelse(PBSEAT == 0, 1, 
             ifelse(PBSEAT <= 1250, 2, 
                    ifelse(PBSEAT <= 2500, 3, 
                           ifelse(PBSEAT <= 5000, 4,
                                  ifelse(PBSEAT <= 15000, 5,
                                         ifelse(PBSEAT == 999995, 6, NA)))))))) %>% 
    select(-PBSEAT) %>%
    #HCBED has flags, 9995
    mutate(HCBED_bin = as.factor(
      ifelse(HCBED == 0, 1, 
             ifelse(HCBED <= 50, 2, 
                    ifelse(HCBED <= 100, 3, 
                           ifelse(HCBED <= 150, 4,
                                  ifelse(HCBED <= 250, 5,
                                         ifelse(HCBED == 9995, 6, NA)))))))) %>% 
    select(-HCBED) %>%
    #NRSBED has flags, 9995
    mutate(NRSBED_bin = as.factor(
      ifelse(NRSBED == 0, 1, 
             ifelse(NRSBED <= 50, 2, 
                    ifelse(NRSBED <= 100, 3, 
                           ifelse(NRSBED <= 150, 4,
                                  ifelse(NRSBED <= 250, 5,
                                         ifelse(NRSBED == 9995, 6, NA)))))))) %>% 
    select(-NRSBED) %>%
    #LODGRM has flags, 99995
    mutate(LODGRM_bin = as.factor(
      ifelse(LODGRM == 0, 1, 
             ifelse(LODGRM <= 50, 2, 
                    ifelse(LODGRM <= 100, 3, 
                           ifelse(LODGRM <= 200, 4,
                                  ifelse(LODGRM <= 350, 5,
                                         ifelse(LODGRM <= 500, 6,
                                                ifelse(LODGRM <= 750, 7,
                                                       ifelse(LODGRM == 99995, 8, NA)))))))))) %>% 
    select(-LODGRM) %>%
    #NOCC has flags, 995
    #mutate(NOCC_bin = as.factor(
    #  ifelse(NOCC == 0, 1, 
    #         ifelse(NOCC <= 5, 2, 
    #                ifelse(NOCC <= 50, 3, 
    #                       ifelse(NOCC <= 100, 4,
    #                              ifelse(NOCC <= 200, 5,
    #                                     ifelse(NOCC == 995, 6, NA)))))))) %>% 
    #NOCCAT ALREADY BINS THIS NUMERIC COLUMN
    select(-NOCC) %>%
    #XRAYN has flags, 995
    mutate(XRAYN_bin = as.factor(
      ifelse(XRAYN == 0, 1, 
             ifelse(XRAYN <= 5, 2, 
                    ifelse(XRAYN <= 10, 3, 
                           ifelse(XRAYN <= 20, 4,
                                  ifelse(XRAYN == 995, 5, NA))))))) %>% 
    select(-XRAYN) %>%
    #RFGCOMPN has flags, 995
    mutate(RFGCOMPN_bin = as.factor(
      ifelse(RFGCOMPN == 0, 1, 
             ifelse(RFGCOMPN <= 50, 2, 
                    ifelse(RFGCOMPN <= 250, 3, 
                           ifelse(RFGCOMPN <= 1000, 4,
                                  ifelse(RFGCOMPN == 99995, 5, NA))))))) %>% 
    select(-RFGCOMPN) %>%
    #SERVERN has flags, 9995
    mutate(SERVERN_bin = as.factor(
      ifelse(SERVERN == 0, 1, 
             ifelse(SERVERN <= 50, 2, 
                    ifelse(SERVERN <= 100, 3, 
                           ifelse(SERVERN <= 200, 4,
                                  ifelse(SERVERN <= 500, 5,
                                         ifelse(SERVERN == 9995, 6, NA)))))))) %>% 
    select(-SERVERN) %>%
    #TVVIDEON has flags, 995
    mutate(TVVIDEON_bin = as.factor(
      ifelse(TVVIDEON == 0, 1, 
             ifelse(TVVIDEON <= 50, 2, 
                    ifelse(TVVIDEON <= 100, 3, 
                           ifelse(TVVIDEON <= 200, 4,
                                  ifelse(TVVIDEON == 995, 5, NA))))))) %>% 
    select(-TVVIDEON) %>%
    #Categorical conversions with nas - If question not asked, then encode NA as -1
    mutate_at(vars(FREESTN, RENOV, OWNOCC, RDLTNF, EQGLSS, SUNGLS, ELEVTR, ESCLTR, ACT1, ACT2, ACT3, DATACNTR, DRYCL, VACANT, 
                   CUBE, CUBEC, CUBELOC, COURT, FEDFAC, FACACT, MANIND, PLANT, 
                   FACDST, FACDHW, FACDCW, FACELC, BLDPLT, GOVTYP, OWNPPR, 
                   NWNPPR, NWNOPR, WHOPPR, ANYEGY, FKTYPE, ELHT1, NGHT1, FKHT1,
                   PRHT1, STHT1, HWHT1, WOHT1, COHT1, SOHT1, OTHT1, ELHT2, NGHT2,
                   FKHT2, PRHT2, STHT2, HWHT2, WOHT2, COHT2, SOHT2, OTHT2, FURNAC,
                   PKGHT, BOILER, STHW, HTPMPH, SLFCON, OTHTEQ, BLRRAD, BLRFNCL, BLRINDC,
                   BLRAIR, BLRPKG, BLRDUCT, DHRAD, DHFNCL, DHINDC, DHWATR, DHAIR, DHPKG,
                   DHDUCT, OTSTRP, OTDUCT, OTPIU, PKGHTTYP, PKGFURN, PKGHTP, PKGCOIL, PKGPIU,
                   PKGDUCT, HPHPKG, HPHSPLT, HPHROOM, HPHMINI, HPHVRF, HPHAIR, HPHGRD, HPHDUAL, 
                   HPHWTR, HPHBKUP, SHRDNT, SHBBRD, SHPORT, SHWALL, SHFURN, SHUNIT, SHPTAC,
                   HTVCAV, HTVVAV, HTVFLR, HTVOAS, HTVDEM, HTVNON, MAINHT, NWMNHT, ELCOOL, NGCOOL,
                   FKCOOL, PRCOOL, STCOOL, HWCOOL, CWCOOL, OTCOOL, PKGCLTYP, CHLAIRCL, CHLWTRCL, CHLABSRP,
                   HTRCHLR, CHLAIR, CHLFNCL, CHLINDC, CHLBEAM, CHLPKG, CHLDUCT, DCWAIR, DCWFNCL, DCWINDC,
                   DCWBEAM, DCWPKG, DCWDUCT, HPCPKG, HPCSPLT, HPCROOM, HPCMINI, HPCVRF, HPCAIR, HPCGRD, HPCDUAL,
                   HPCWTR, CLVCAV, CLVVAV, CLVFLR, CLVOAS, CLVDEM, CLVNON, NWMNCL, RDHTNF, HWRDHT, HWRDCL, ECN, ECNTYPE,
                   MAINT, ELWATR, NGWATR, FKWATR, PRWATR, STWATR, HWWATR, WOWATR, COWATR, SOWATR, OTWATR, WTHTEQ, BOOSTWT,
                   INSTWT, ELCOOK, NGCOOK, FKCOOK, PRCOOK, STCOOK, HWCOOK, WOCOOK, COCOOK, SOCOOK, OTCOOK, ELMANU, NGMANU,
                   FKMANU, PRMANU, STMANU, HWMANU, WOMANU, COMANU, SOMANU, OTMANU, NGGENR, FKGENR, PRGENR, WOGENR, COGENR,
                   SOGENR, OTGENR, PVC, FUELCL, LRGTRB, MCROTB, ENGINE, GENUSE, COGEN, TOGRID, ELLOCUT, ELNONLU, ELOTSRC,
                   ELCPLT, NGSRC, PRAMTC, PRUNIT, WOAMT, WOSRC, SNACK, FASTFD, CAF, FDPREP, KITCHN, BREAKRM, OTFDRM, HWTRM,
                   LAUNDR, CONFSP, MEDEQP, CTSCAN, MRI, LINACC, OUTSURG, LABEQP, MCHEQP, POOL, HTPOOL, POOLSRC, STRLZR,
                   RFGRES, RFGCOMP, RFGWI, RFGOP, RFGCL, RFGVEN, RFGICE, RFGSTO, WHRECOV, WHHT2, WHWT, WHOT, 
                   MLTMON, MLTMNC, FLATC, TRNGRM, STDNRM, DCNTRSFC, WBOARDS, PRNTYP, LTNR24, FLUOR, CFLR, BULB, HALO, 
                   HID, LED, OTLT, EMCSLT, SCHED, OCSN, DIM, DAYHARV, TRIM, PLGCTRL, DRLGHT, TINT, REFL, AWN, SKYLT, 
                   COOTH, WOOTH, OTOTH, HWOTH, SOOTH, DCWLOOP, CWOTH, STOTH, DHLOOP, DHHT1, DHHT2, DHCOOL, DHCOOK, 
                   DHMANU, DHOTH, PROTH, CHLLOOP, FKOTH, BLRLOOP, NGOTH, SQFTC, WLCNS, RFCNS, RFCOOL, RFTILT, BLDSHP, 
                   GLSSPC, EQGLSS, SUNGLS, 
                   ATTIC, YRCONC, RENADD, RENRDC, RENCOS, RENINT, RENRFF, RENWLL, RENWIN, 
                   RENHVC, RENLGT, RENPLB, RENELC, RENINS, RENSAF, RENSTR, RENOTH, ONEACT, 
                   PBAPLUS, PBA, FACIL, GOVOWN, OWNTYPE, NOCCAT, OWNOCC, OWNOPR, OPEN24, 
                   OPEN24, OPNMF, OPNWE, NWKERC, HT1, HT2, COOL, WATR, COOK, MANU, CAPGEN, ELUSED,
                   NGUSED, FKUSED, PRUSED, STUSED, HWUSED, CWUSED, OTUSED, WOUSED, COUSED, SOUSED, 
                   RCAC, PKGCL, CHILLR, CHWT, HTPMPC, ACWNWL, EVAPCL, OTCLEQ, MAINCL, EMCS, RDCLNF, GENR, AMIMETER, 
                   ENRGYPLN, RFGEQP, PCTERM, PCTRMC, LAPTPC, SERVER, SERVERC, TVVIDEO, RGSTR, COPIER, FAX, LOHRPC,
                   LNHRPC, LTEXPC, PKLT, WINTYP, REGION, CENDIV, RENOV, WKHRSC, PUBCLIM,
                   NFLOOR_bin, FLCEILHT_bin, NELVTR_bin, NESLTR_bin, RWSEAT_bin, PBSEAT_bin, HCBED_bin, NRSBED_bin,
                   LODGRM_bin, XRAYN_bin, RFGCOMPN_bin, SERVERN_bin, TVVIDEON_bin, MONUSE #NOCC_bin
                   ),
              funs(make_cat_w_repl(.))) %>%
    #Move down the order of activities and when there is a null, fill it in with the difference of 100 - sum
    mutate(ACT1PCT = ifelse(ACT1==-1, 100, ACT1PCT)) %>% 
    mutate(ACT2PCT = ifelse(ACT2==-1, 100-ACT1PCT, ACT2PCT)) %>% 
    mutate(ACT3PCT = ifelse(ACT3==-1, 100-ACT1PCT-ACT2PCT, ACT3PCT)) %>% 
    #create additional column that catches difference
    mutate(OTHPCT = 100 - ACT1PCT - ACT2PCT - ACT3PCT) %>% 
    #per sf values to track: electricity
    #mutate(ELCNSPerSf = ELCNS / SQFT) %>%
    #electricity btu
    #mutate(ELBTUPerSf = ELBTU / SQFT) %>%
    #electricity heating btu
    #mutate(ELHTBTUPerSf = ELHTBTU / SQFT) %>%
    #electricity cooling btu
    #mutate(ELCLBTUPerSf = ELCLBTU / SQFT) %>%
    #electricity ventilation btu
    #mutate(ELVNBTUPerSf = ELVNBTU / SQFT) %>%
    #electricity water heating btu
    #mutate(ELWTBTUPerSf = ELWTBTU / SQFT) %>%
    #electricity lighting btu
    #mutate(ELLTBTUPerSf = ELLTBTU / SQFT) %>%
    #electricity cooking btu
    #mutate(ELCKBTUPerSf = ELCKBTU / SQFT) %>%
    #electricity refrigeration btu
    #mutate(ELRFBTUPerSf = ELRFBTU / SQFT) %>%
    #electricity office equipment btu
    #mutate(ELOFBTUPerSf = ELOFBTU / SQFT) %>%
    #electricity computing btu
    #mutate(ELPCBTUPerSf = ELPCBTU / SQFT) %>%
    #electricity miscellaneous btu
    #mutate(ELOTBTUPerSf = ELOTBTU / SQFT) %>%
    #natural gas ccf
    #mutate(NGCNSPerSf = NGCNS / SQFT) %>%
    #natural gas BTU
    #mutate(NGBTUPerSf = NGBTU / SQFT) %>%
    #natural gas heating BTU
    #mutate(NGHTBTUPerSf = NGHTBTU / SQFT) %>%
    #natural gas cooling BTU
    #mutate(NGCLBTUPerSf = NGCLBTU / SQFT) %>%
    #natural gas water heating BTU
    #mutate(NGWTBTUPerSf = NGWTBTU / SQFT) %>%
    #natural gas cooking BTU
    #mutate(NGCKBTUPerSf = NGCKBTU / SQFT) %>%
    #natural gas misc BTU
    #mutate(NGOTBTUPerSf = NGOTBTU / SQFT) %>%
    #District heat thousand pounds
    #mutate(DHCNSPerSf = DHCNS / SQFT) %>%
    #District heat thousand btu
    #mutate(DHBTUPerSf = DHBTU / SQFT) %>% 
    #District heat heating thousand btu
    #mutate(DHHTBTUPerSf = DHHTBTU / SQFT) %>% 
    #District heat cooling thousand btu
    #mutate(DHCLBTUPerSf = DHCLBTU / SQFT) %>% 
    #District heat water heating thousand btu
    #mutate(DHWTBTUPerSf = DHWTBTU / SQFT) %>% 
    #District heat cooking thousand btu
    #mutate(DHCKBTUPerSf = DHCKBTU / SQFT) %>%
    #District heat misc thousand btu
    #mutate(DHOTBTUPerSf = DHOTBTU / SQFT) %>%
    #fuel oil
    #mutate(FKCNSPerSf = FKCNS / SQFT) %>%
    #Fuel oil thousand btu
    #mutate(FKBTUPerSf = FKBTU / SQFT) %>%
    #Fuel oil heating thousand btu
    #mutate(FKHTBTUPerSf = FKHTBTU / SQFT) %>%
    #Fuel oil cooling thousand btu
    #mutate(FKCLBTUPerSf = FKCLBTU / SQFT) %>%
    #Fuel oil water heating thousand btu
    #mutate(FKWTBTUPerSf = FKWTBTU / SQFT) %>%
    #Fuel oil cooking thousand btu
    #mutate(FKCKBTUPerSf = FKCKBTU / SQFT) %>%
    #Fuel oil misc thousand btu
    #mutate(FKOTBTUPerSf = FKOTBTU / SQFT) %>%
    #Major fuel btu
    #mutate(MFBTUPerSf = MFBTU / SQFT) %>%
    #Major fuel heating btu
    #mutate(MFHTBTUPerSf = MFHTBTU / SQFT) %>%
    #Major fuel cooling btu
    #mutate(MFCLBTUPerSf = MFCLBTU / SQFT) %>%
    #Major fuel ventilation btu
    #mutate(MFVNBTUPerSf = MFVNBTU / SQFT) %>%
    #Major fuel water heating btu
    #mutate(MFWTBTUPerSf = MFWTBTU / SQFT) %>%
    #Major fuel lighting btu
    #mutate(MFLTBTUPerSf = MFLTBTU / SQFT) %>%
    #Major fuel cooking btu
    #mutate(MFCKBTUPerSf = MFCKBTU / SQFT) %>%
    #Major fuel refrigeration btu
    #mutate(MFRFBTUPerSf = MFRFBTU / SQFT) %>%
    #Major fuel office equipment use btu
    #mutate(MFOFBTUPerSf = MFOFBTU / SQFT) %>%
    #Major fuel computing use btu
    #mutate(MFPCBTUPerSf = MFPCBTU / SQFT) %>%
    #Major fuel other btu
    #mutate(MFOTBTUPerSf = MFOTBTU / SQFT) %>%
    #columns not interested in tracking for this study (e.g. expenditures, total usage, etc.)
    #select(-ELBTU, -NGBTU, -DHBTU, -FKBTU, -MFBTU) %>% 
    select(-ELCNS, -ELHTBTU, -ELCLBTU, -ELCLBTU, -ELVNBTU, -ELWTBTU, -ELLTBTU, -ELCKBTU,
           -ELRFBTU, -ELOFBTU, -ELPCBTU, -ELOTBTU,
           -NGCNS, -NGHTBTU, -NGCLBTU, -NGWTBTU, -NGCKBTU, -NGOTBTU, -FKCNS, 
           -DHCNS, -DHHTBTU, -DHCLBTU, -DHWTBTU, -DHCKBTU, -DHOTBTU, 
           -FKHTBTU, -FKCLBTU, -FKWTBTU, -FKCKBTU, -FKOTBTU,
           -MFHTBTU, -MFCLBTU, -MFVNBTU, -MFWTBTU, -MFLTBTU, -MFCKBTU, 
           -MFRFBTU, -MFOFBTU, -MFPCBTU, -MFOTBTU,
           -ELEXP, -NGEXP, -FKEXP, -DHEXP, -MFEXP) %>% 
    #replace nas with zeroes - if question not asked or applicable then assume zero as input
    #Removed and binned RWSEAT, PBSEAT,   HCBED, NRSBED, LODGRM, XRAYN, SERVERN, TVVIDEON
    mutate_at(vars(OCCUPYP, LODOCCP, HEATP, FURNP, PKGHP, 
                 BOILP, STHWP, HTPHP, SLFCNP, OTHTP, RCACP, PKGCP, CHILP, CHWTP, HTPCP, ACWNWP, EVAPP, OTCLP, CONFSPP,
                 FDSEAT, EDSEAT, PRNTRN, FLUORP, CFLRP, BULBP, HALOP, HIDP, LEDP, FLUORP, CFLRP, BULBP, HALOP, HIDP, LEDP, 
                 OTLTP, DAYLTP, PCTERMN, LAPTPN, RGSTRN, COPIERN, DHBTU, FKBTU, NGBTU#,
                 #ELCNSPerSf, ELBTUPerSf, ELHTBTUPerSf, ELCLBTUPerSf, ELVNBTUPerSf, ELWTBTUPerSf, ELLTBTUPerSf,
                 #ELCKBTUPerSf, ELRFBTUPerSf, ELOFBTUPerSf, ELPCBTUPerSf, ELOTBTUPerSf, NGCNSPerSf, NGBTUPerSf, 
                 #NGHTBTUPerSf, NGCLBTUPerSf, NGWTBTUPerSf, 
                 #NGCKBTUPerSf, NGOTBTUPerSf, FKCNSPerSf, DHCNSPerSf, DHBTUPerSf, DHHTBTUPerSf, DHCLBTUPerSf,
                 #DHWTBTUPerSf, DHCKBTUPerSf, DHOTBTUPerSf, FKBTUPerSf, FKHTBTUPerSf, FKCLBTUPerSf, FKWTBTUPerSf, 
                 #FKCKBTUPerSf, FKOTBTUPerSf, MFBTUPerSf, MFHTBTUPerSf, MFCLBTUPerSf, MFVNBTUPerSf, MFWTBTUPerSf,
                 #MFLTBTUPerSf, MFCKBTUPerSf, MFRFBTUPerSf, MFOFBTUPerSf, MFPCBTUPerSf, MFOTBTUPerSf
                 ), 
            funs(na_repl_num(., 0)))
  #Get appropriate numeric columns and divide to get PerSf metrics
  PerSfVector <- c('NFLOOR', 'BASEMNT', 'NELVTR', 'NESLTR', 'RWSEAT', 'PBSEAT',
                   'EDSEAT', 'FDSEAT', 'HCBED', 'NRSBED', 'LODGRM', 'NOCC', 'NWKER',
                   'XRAYN', 'RFGRSN', 'RFGCOMPN', 'RFGWIN', 'RFGOPN', 'RFGCLN', 'RFGVNN',
                   'RFGICN', 'PCTERMN', 'LAPTPN', 'PRNTRN', 'SERVERN', 'TVVIDEON', 'RGSTRN')
  #per_sf_df <- trim_df %>% select_(.dots = PerSfVector)
  #per_sf_df <- per_sf_df / trim_df[,c('SQFT')]
  #colnames(per_sf_df) <- paste(colnames(per_sf_df), "PerSf", sep="")
  
  #remove columns with only one factor
  print('removed columns with one factor or value')
  print(colnames(trim_df[, sapply(trim_df, function(col) length(unique(col)))<=1]))
  clean_df <- trim_df[, sapply(trim_df, function(col) length(unique(col)))>1]
  #%>% 
  #  select(names(trim_df[!(colnames(trim_df) %in% PerSfVector)])) %>% 
  #  bind_cols(per_sf_df)
  #Make list of numeric column groups
  removed_response_list <- colnames(clean_df[,!grepl('EL.*PerSf|NG.*PerSf|DH.*PerSf|FK.*PerSf|MF.*PerSf', colnames(clean_df), ignore.case = TRUE)])
  #response_cols <- c('ELBTUPerSf', 'NGBTUPerSf', 'DHBTUPerSf', 'FKBTUPerSf', 'MFBTUPerSf')
  response_cols <- c('ELBTU', 'NGBTU', 'DHBTU', 'FKBTU', 'MFBTU')
  tmp_column_list <- append(removed_response_list, response_cols)
  clean_numeric_cols <- names(clean_df[, unlist(lapply(clean_df, is.numeric)) & colnames(clean_df) %in% tmp_column_list])
  clean_non_numeric_cols <- names(clean_df[, !(colnames(clean_df) %in% clean_numeric_cols) & colnames(clean_df) %in% tmp_column_list])
  clean_full_column_list <- append(clean_numeric_cols, clean_non_numeric_cols)
  clean_df <- clean_df[,clean_full_column_list]
  
  if(!is.na(pba_filter)) {clean_df <- clean_df %>% filter(PBAPLUS==pba_filter)}
  #Convert factor columns to one-hot encoders
  dummy_f <- dummyVars(" ~ .", data = clean_df)
  encoded_df <- data.frame(predict(dummy_f, newdata=clean_df))
  encoded_numeric_cols <- clean_numeric_cols
  encoded_non_numeric_cols <- names(encoded_df[, !(colnames(encoded_df) %in% encoded_numeric_cols)])
  encoded_full_column_list <- append(encoded_numeric_cols, encoded_non_numeric_cols)
  return(
    list(clean_df=clean_df,
         encoded_df=encoded_df,
         clean_numeric_cols=clean_numeric_cols,
         clean_non_numeric_cols=clean_non_numeric_cols,
         encoded_numeric_cols=encoded_numeric_cols,
         encoded_non_numeric_cols=encoded_non_numeric_cols,
         response_cols = response_cols
    )
  )
}