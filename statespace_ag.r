


{
  #set it up for district for now
  
  dist.id <- read_csv(file = "../data/cen2021/dist.id.csv") 
  # View(dist.id)
  # dist.id[dist.id$dname=="Kathmandu",]
  #converting dataframe to datatable
  setDT(dist.id)
  
  
  regions.nm = c(dist.id$dname,"absentee")
  regions = paste0("reg",c(dist.id$dist,78))
  names(regions)=regions.nm
  
  nreg = length(regions)#77 dist + absentee
  
  ages = seq(-1,100,by=1)#for (-1) newborns "alive" at the beginning of the time interval 
  nage = length(ages) #
  agecodes = ages
  
  sexnames = c("f","m") 
  nsex = length(sexnames)
  sexcodes = 1:2
  
  initime = 2021 #will change later 
  fintime = 2061
  ts = 1 #timestep
  nper = (fintime-initime)/ts
  pers = seq(initime,fintime,by=ts)
  
  
  fertages = seq(15,45,by=1)#during the next five years
  nfertage = length(fertages) #Later for cohort - need 8
  nfertsex = 1
  
  #how many id vars
  # grep(pattern = "^n",ls())
  
  #input data
  ntrans = 7 #sx, asfr,srb, edu, inmigdom, outmigdom, immigration, emigration, reclassificaion (two additional database)
  input.vars <-  c("pop",#population in '000
                   "sx", # survival ratio
                   "asfr", #births per 1000 years of exposure
                   "idmr", #in-domestic migration
                   "odmr", #out-domestic migration
                   "imr", #immigrants
                   "emr", #emigration
                   "srb" #male/female 
  )# transition probabilities
  
  output.vars <- c("pop",
                   "dx",
                   "emi",#"emires",
                   "imm",#"immres",
                   "idmig",
                   "odmig",
                   "births",
                   "birfem",
                   "birmal"
    )#
  idvar_order <- c("var","tob","sex","Time","region")
  #tob = Time of birth (beginning year of the period)
  #pop_1990_f_2015_REG (this will be used for REGEX)
  
}#definitions


{#create folders
  cc.dir.val = "../data"
  if(length(grepl(pattern = "data",x = dir("../")))==0){
    dir.create(path=cc.dir.val)
  }
  
  #create an results folder to save indicators graphs
  cc.dir.val = "../results"
  if(length(grep(pattern = "results",x = dir("../")))==0){
    dir.create(path=cc.dir.val)
  }
  
  #create an output folder - model output
  cc.dir.val = "../data/output"
  if(length(grep(pattern = "output$",x = dir("../data")))==0){
    dir.create(path=cc.dir.val)
  }
  
  
  #each scenario will be saved in separate folder
  #create an output/SCEN folder
  cc.dir.scen = paste("../data/output",iscen_fullname,sep="/")
  if(length(grep(pattern = iscen_fullname,x = dir("../data/output")))==0){
    dir.create(path=cc.dir.scen)
  }
  
  path_scen = paste(cc.dir.scen,"/",sep="")
  
}#create folders  



# if(any(sapply(list(fert, mort, int.mig, recl, edu), is.not.null))) SSP <- "SSP6"


#create empty files or use from others
#for alternative scenarios
if(iscen!="baseline"){
  stop("..")
  #load the old xxxdts and the results
  print("Load Baseline Files")
  
  #to copy from the following folder
  cc.dir.usescen = paste("../data/output",usescen,sep="/")
  dir(cc.dir.usescen)
  # dttosave #same
  
  load(file=dir(paste("datatosave.*RData",sep=""),path = "../data/output/baseline_emort0_efert0_0mig/",full.names = T))
  
  dttosave
  # [1] "anchordt" "asfrdt"   "emrdt"    "imrdt"    "popdt"    "propdt"  
  # [7] "srbdt"    "sxdt"   
  
  ifiles <- grep(pattern="dt.RData",dir(cc.dir.usescen,full.names = T),value=T)
  #check #single files only
  print(ifiles)  
  # [1] "../data/output/baseline_emort1_efert1_emig0/anchordt.RData"
  # [2] "../data/output/baseline_emort1_efert1_emig0/asfrdt.RData"  
  # [3] "../data/output/baseline_emort1_efert1_emig0/emrdt.RData"   
  # [4] "../data/output/baseline_emort1_efert1_emig0/imrdt.RData"   
  # [5] "../data/output/baseline_emort1_efert1_emig0/popdt.RData"   
  # [6] "../data/output/baseline_emort1_efert1_emig0/propdt.RData"  
  # [7] "../data/output/baseline_emort1_efert1_emig0/srbdt.RData"   
  # [8] "../data/output/baseline_emort1_efert1_emig0/sxdt.RData"  
  
  #change some files
  if(exists("usescen_fert") && usescen_fert != usescen){
    ifiles[grep("asfrdt",ifiles)] <- gsub(pattern = usescen,usescen_fert,ifiles[grep("asfrdt",ifiles)])
  }
  
  if(exists("usescen_mort") && usescen_mort != usescen){
    ifiles[grep("sxdt",ifiles)] <- gsub(pattern = usescen,usescen_mort,ifiles[grep("sxdt",ifiles)])
  }
  
  
  
  #8 files loaded
  # rm(list=ls(pattern = ".RData"))
  for(ifile in ifiles) {
    ifile.nm = gsub(".RData","",strsplit(ifile,"/")[[1]][5])
    assign(ifile.nm,get(load(file=ifile)))
  }
  id.cols <- c(names(popdt)[1:5],"tob")
  
  
  
  #check the input
  popdt[agest==5]
  asfrdt[agest==15]
  #in fillstatespace.r let us fill the data!!
  
  
  if(iscen == "ssp1"){
    
    #fert = low - 20% lower by 2020-2040 and 25% lower by 2040-2060 {??} [done]
    #low fert
    #fertfact.high = c(seq(1,1.2,length.out=5),seq(1.2,1.25,length.out=5)[-1],rep(1.25,8))
    
    #mort = low [done]
    #updated above (usescen_mort)
    
    #mig= medium [done]
    #No  change
    
    #edu = sdg [done]
    
  } else if(iscen == "ssp3"){#ssp1
    
    #fertfact.high = c(seq(1,1.2,length.out=5),seq(1.2,1.25,length.out=5)[-1],rep(1.25,8))
    
  } else if (iscen == "ssp4"){
    #update HiFert countries with High mortality
    usescen_mort = "baselineHmort_emort1high_efert_emig0"##statespace  
    ifile <- gsub(pattern = usescen,usescen_mort,ifiles[grep("sxdt",ifiles)])
    mortH <- get(load(file=ifile))
    sxdt[mortH[region%in%reghf],on = setdiff(id.cols,"tob"), sx:=i.sx]
  }
  
} else if (iscen=="baseline"){
  
  #can also make columns as scenarios
  #make individual data.table for each variable
  popdt <- expand.grid(region = regions,
                       Time=pers,
                       sex=sexnames,
                       agest=ages,
                       stringsAsFactors=F)
  setDT(popdt)
  
  for(i in c("sx","imr","emr","idmr","odmr")){ #sx imr emr
    #sx/imr/emr
    xxxdt <- expand.grid(region = regions,
                         Time=pers[-length(pers)],
                         sex=sexnames,
                         agest=ages,
                         stringsAsFactors=F)
    setDT(xxxdt)
    xxdtname <- paste(i,"dt",sep="")
    assign(xxdtname,xxxdt);rm(xxxdt)
  }
  
  for(i in "asfr"){ #asfrdt
    xxxdt <- expand.grid(region = regions,
                         Time=pers[-length(pers)],sex=sexnames[1],
                         agest=fertages,stringsAsFactors=F)
    setDT(xxxdt)
    xxdtname <- paste(i,"dt",sep="")
    assign(xxdtname,xxxdt);rm(xxxdt)
  }
  
  
  for(i in "srb"){ #srbdt
    # stop()
    xxxdt <- expand.grid(region=regions,Time=pers[-length(pers)],stringsAsFactors=F)
    setDT(xxxdt)
    xxdtname <- paste(i,"dt",sep="")
    assign(xxdtname,xxxdt);rm(xxxdt)
  }
  dttosave <-grep("dt$",ls(),value = T)#regex
  # original_input =  c("popdt","sxdt","propdt","emrdt", "imrdt","asfrdt","srbdt")

  for(ifile in dttosave) {
    xx <- get(ifile)
    xx[,value:=NA]
    write.csv(xx[,value:=NA],file = paste0("../data/popproj nso 2021/",ifile,".csv"))
   # write.csv(xx[,value:=NA],file = paste0("/output/baselineruban_edu_mort_fert_mig/fill_",ifile,".csv"))
  }
  
}# fertmys or baseline
