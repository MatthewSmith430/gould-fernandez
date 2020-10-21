#' @title Gould-Fernandez Two-Mode Brokerage
#'
#' @description This function extracts the two mode brokerage chains from a two-mode network. Only works for interlocking directorates
#' @param g igraph object.
#' @param event_attribute This needs to be a dataframe with the firm name and the sector attribute
#' @param actor_attribute This needs to a dataframe with the person name and the gender attribute
#' @export
#' @return List - 1.)Brokerage path list 2.) Individual role dataframe list
#' 3.) Total two-mode GF results dataframe.
#' @references Jasny, L. and Lubell, M., 2015. Two-mode brokerage in policy networks. Social Networks, 41, pp.36-47.
GF_twomode<-function(g,event_attribute,actor_attribute){
  NET<-g
  SECTOR_ID<-event_attribute
  colnames(SECTOR_ID)<-c("COMPANY ID","SECTOR")
  GENDER_ID<-actor_attribute
  colnames(GENDER_ID)<-c("name","gender")
  sub<-igraph::make_graph( ~ 1-2-3-4-5)
  NET<-igraph::simplify(NET)
  H<-igraph::subgraph_isomorphisms(sub, NET)
  EL<-list()
  for (i in 1:length(H)){
    R<-H[[i]]
    R2<-as.vector(R)
    EL[[i]]<-R2
  }
  EL2<-do.call(rbind.data.frame, EL)
  colnames(EL2)<-c("1","2","3","4","5")

  ISO<-list()
  for (i in 1:length(EL)){
    VEC<-EL[[i]]
    subCheck<-igraph::induced_subgraph(NET, VEC)
    ISO[[i]]<-igraph::ecount(subCheck)
  }
  DFedges<-plyr::ldply(ISO,data.frame)
  colnames(DFedges)<-"Number of Edges"
  DATA<-cbind(EL2,DFedges)

  dat<-DATA
  dat.sort<-t(apply(dat, 1, sort))
  DATA2<- dat[!duplicated(dat.sort),]

  #DATA2<-DATA2[DATA2$`Number of Edges` == 2,]

  igraph::V(NET)$id<-1:length(igraph::V(NET)$name)
  Vert<-igraph::get.data.frame(NET, what = "vertices")

  DATA2$`1`<-Vert$name[match(DATA2$`1`,Vert$id)]
  DATA2$`2`<-Vert$name[match(DATA2$`2`,Vert$id)]
  DATA2$`3`<-Vert$name[match(DATA2$`3`,Vert$id)]
  DATA2$`4`<-Vert$name[match(DATA2$`4`,Vert$id)]
  DATA2$`5`<-Vert$name[match(DATA2$`5`,Vert$id)]
  colnames(DATA2)<-c("Firm1","Director1","Firm2BROKER",
                     "Director2","Firm3","NoEdges")

  DATA2<-as.data.frame(DATA2,stringsAsFactors=FALSE)
  DATA2$FIRM1.check<-DATA2$Firm1
  DATA2$DIR1.check<-DATA2$Director1
  DATA2$FIRM2.check<-DATA2$Firm2BROKER
  DATA2$DIR2.check<-DATA2$Director2
  DATA2$FIRM3.check<-DATA2$Firm3

  DATA2$FIRM1.check<-Vert$type[match(DATA2$FIRM1.check,Vert$name)]
  DATA2$DIR1.check<-Vert$type[match(DATA2$DIR1.check,Vert$name)]
  DATA2$FIRM2.check<-Vert$type[match(DATA2$FIRM2.check,Vert$name)]
  DATA2$DIR2.check<-Vert$type[match(DATA2$DIR2.check,Vert$name)]
  DATA2$FIRM3.check<-Vert$type[match(DATA2$FIRM3.check,Vert$name)]

  NEWDF<-dplyr::filter(DATA2,DATA2$FIRM1.check==FALSE)
  NEWDF<-dplyr::filter(NEWDF,NEWDF$FIRM2.check==FALSE)
  NEWDF<-dplyr::filter(NEWDF,NEWDF$FIRM3.check==FALSE)

  NEWDF2<-NEWDF[,1:6]

  UNF<-unique(unique(NEWDF2$Firm1))
  WH_LIST<-list()
  for (j in 1:length(UNF)){
    FIL<-dplyr::filter(NEWDF2,NEWDF2$Firm1==UNF[[j]])
    FIL2<-FIL[FIL$Firm3%in%FIL$Firm2BROKER,]

    #FIL2<-dplyr::filter(FIL,FIL$Firm2BROKER==NEWDF2$Firm3[[j]])
    #FIL3<-dplyr::filter(FIL,NEWDF2$Firm1==NEWDF2$Firm1[[j]])
    #NEWDF3<-anti_join(NEWDF2,FIL2,by="NoEdges")
    WH<-which(NEWDF2$Firm1 %in% FIL2$Firm1 &
                NEWDF2$Firm2BROKER %in% FIL2$Firm2BROKER &
                NEWDF2$Firm3 %in% FIL2$Firm3)
    WH_LIST[[j]]<-WH
  }
  WH_B<-unlist(WH_LIST)
  WH_B<-unique(WH_B)
  NEWDF2<-NEWDF2[-WH_B,]
  NEWDF2$SECTOR1<-NEWDF2$Firm1
  NEWDF2$SECTOR2<-NEWDF2$Firm2BROKER
  NEWDF2$SECTOR3<-NEWDF2$Firm3
  NEWDF2$GENDER1<-NEWDF2$Director1
  NEWDF2$GENDER2<-NEWDF2$Director2

  NEWDF2$SECTOR1<-SECTOR_ID$SECTOR[match(NEWDF2$Firm1,
                                         SECTOR_ID$`COMPANY ID`)]
  NEWDF2$GENDER1<-GENDER_ID$gender[match(NEWDF2$Director1,
                                         GENDER_ID$name)]
  NEWDF2$SECTOR2<-SECTOR_ID$SECTOR[match(NEWDF2$Firm2,
                                         SECTOR_ID$`COMPANY ID`)]
  NEWDF2$GENDER2<-GENDER_ID$gender[match(NEWDF2$Director2,
                                         GENDER_ID$name)]
  NEWDF2$SECTOR3<-SECTOR_ID$SECTOR[match(NEWDF2$Firm3,
                                         SECTOR_ID$`COMPANY ID`)]
  #Female - 1, Male - 2
  COOR_DF<-dplyr::filter(NEWDF2,NEWDF2$SECTOR1==
                           NEWDF2$SECTOR2&
                           NEWDF2$SECTOR2==NEWDF2$SECTOR3)
  if (length(COOR_DF$Firm1)>0){
    COOR_DF$GENDER_COMBO<-rep(0,length(COOR_DF$Firm1))
    for (i in 1:length(COOR_DF$GENDER_COMBO)){
      CH<-COOR_DF[i,]
      Cg1<-CH$GENDER1
      Cg2<-CH$GENDER2
      if (Cg1==Cg2&&Cg1=="female"){
        COOR_DF$GENDER_COMBO[[i]]<-"FF"
      }else if(Cg1==Cg2&&Cg1=="male"){
        COOR_DF$GENDER_COMBO[[i]]<-"MM"
      } else if (Cg1!=Cg2&&Cg1=="male"){
        COOR_DF$GENDER_COMBO[[i]]<-"MF"
      }else {COOR_DF$GENDER_COMBO[[i]]<-"FM"}
    }
  }else{
    COOR_DF<-data.frame(Firm1=NA,
                        Director1=NA,
                        Firm2BROKER=NA,
                        Firm3=NA,NoEdges=NA,
                        SECTOR1=NA,SECTOR2=NA,SECTOR3=NA,
                        GENDER1=NA,GENDER2=NA,
                        GENDER_COMBO=NA)
  }

  REP_DF<-dplyr::filter(NEWDF2,NEWDF2$SECTOR1==NEWDF2$SECTOR2&
                          NEWDF2$SECTOR2!=NEWDF2$SECTOR3|NEWDF2$SECTOR2==NEWDF2$SECTOR3&
                          NEWDF2$SECTOR2!=NEWDF2$SECTOR1)
  if (length(REP_DF$Firm1)>0){

    REP_DF$GENDER_COMBO<-rep(0,length(REP_DF$Firm1))
    for (i in 1:length(REP_DF$GENDER_COMBO)){
      RH<-REP_DF[i,]
      Rg1<-RH$GENDER1
      Rg2<-RH$GENDER2
      if (Rg1==Rg2&&Rg1=="female"){
        REP_DF$GENDER_COMBO[[i]]<-"FF"
      }else if(Rg1==Rg2&&Rg1=="male"){
        REP_DF$GENDER_COMBO[[i]]<-"MM"
      } else if (Rg1!=Rg2&&Rg1=="male"){
        REP_DF$GENDER_COMBO[[i]]<-"MF"
      }else {REP_DF$GENDER_COMBO[[i]]<-"FM"}
    }
  }else{
    REP_DF<-data.frame(Firm1=NA,
                       Director1=NA,
                       Firm2BROKER=NA,
                       Firm3=NA,NoEdges=NA,
                       SECTOR1=NA,SECTOR2=NA,SECTOR3=NA,
                       GENDER1=NA,GENDER2=NA,
                       GENDER_COMBO=NA)
  }

  ITIN_DF<-dplyr::filter(NEWDF2,NEWDF2$SECTOR1==NEWDF2$SECTOR3&
                           NEWDF2$SECTOR1!=NEWDF2$SECTOR2)
  if (length(ITIN_DF$Firm1)>0){
    ITIN_DF$GENDER_COMBO<-rep(0,length(ITIN_DF$Firm1))
    for (i in 1:length(ITIN_DF$GENDER_COMBO)){
      IH<-ITIN_DF[i,]
      Ig1<-IH$GENDER1
      Ig2<-IH$GENDER2
      if (Ig1==Ig2&&Ig1=="female"){
        ITIN_DF$GENDER_COMBO[[i]]<-"FF"
      }else if(Ig1==Ig2&&Ig1=="male"){
        ITIN_DF$GENDER_COMBO[[i]]<-"MM"
      } else if (Ig1!=Ig2&&Ig1=="male"){
        ITIN_DF$GENDER_COMBO[[i]]<-"MF"
      }else {ITIN_DF$GENDER_COMBO[[i]]<-"FM"}
    }
  }else{
    ITIN_DF<-data.frame(Firm1=NA,
                        Director1=NA,
                        Firm2BROKER=NA,
                        Firm3=NA,NoEdges=NA,
                        SECTOR1=NA,SECTOR2=NA,SECTOR3=NA,
                        GENDER1=NA,GENDER2=NA,
                        GENDER_COMBO=NA)
  }

  LIA_DF<-dplyr::filter(NEWDF2,NEWDF2$SECTOR1!=NEWDF2$SECTOR2&
                          NEWDF2$SECTOR2!=NEWDF2$SECTOR3&NEWDF2$SECTOR1!=NEWDF2$SECTOR3)
  if (length(LIA_DF$Firm1)>0){
    LIA_DF$GENDER_COMBO<-rep(0,length(LIA_DF$Firm1))
    for (i in 1:length(LIA_DF$GENDER_COMBO)){
      LH<-LIA_DF[i,]
      Lg1<-LH$GENDER1
      Lg2<-LH$GENDER2
      if (Lg1==Lg2&&Lg1=="female"){
        LIA_DF$GENDER_COMBO[[i]]<-"FF"
      }else if(Lg1==Lg2&&Lg1=="male"){
        LIA_DF$GENDER_COMBO[[i]]<-"MM"
      } else if (Lg1!=Lg2&&Lg1=="male"){
        LIA_DF$GENDER_COMBO[[i]]<-"MF"
      }else {LIA_DF$GENDER_COMBO[[i]]<-"FM"}
    }


  }else{
    LIA_DF<-data.frame(Firm1=NA,
                       Director1=NA,
                       Firm2BROKER=NA,
                       Firm3=NA,NoEdges=NA,
                       SECTOR1=NA,SECTOR2=NA,SECTOR3=NA,
                       GENDER1=NA,GENDER2=NA,
                       GENDER_COMBO=NA)
  }
  BROKERAGE_PATH_LIST<-list(COOR_DF,REP_DF,ITIN_DF,LIA_DF)
  GCL<-c("MM","FF","FM","MF")
  ##############################################################################
  ##I need to add an if statement to check whether the DF is empty and alter
  if (is.na(REP_DF$Firm1[1])){
    REP_RES2<-data.frame(Broker=NA,
                         GenderCombo=NA,
                         Freq=NA,
                         Role="RepGate")
    REP_RES3<-data.frame(Broker=NA,
                         RepGate.MM=NA,RepGate.FF=NA,RepGate.FM=NA,
                         RepGate.MF=NA)
    REP_RES_G2<-data.frame(Broker=NA,
                           GenderCombo=NA,
                           BrokerSector=NA,
                           Freq=NA,
                           Role="RepGate")
    REP_RES_G3<-data.frame(Broker=NA,BrokerSector=NA,RepGate.MM=NA,
                           RepGate.FF=NA,RepGate.FM=NA,RepGate.MF=NA)
  }else {
    REP_RES1<-cbind(REP_DF$Firm2BROKER,REP_DF$GENDER_COMBO
    )
    REP_RES1<-tibble::as_tibble(REP_RES1)
    REP_RES_G1<-cbind(REP_DF$Firm2BROKER,
                      REP_DF$GENDER_COMBO,REP_DF$SECTOR2
    )
    REP_RES_G1<-tibble::as_tibble(REP_RES_G1)

    REP_RES_G2<-stats::ftable(REP_RES_G1)
    REP_RES_G2<-tibble::as_tibble(REP_RES_G2)
    colnames(REP_RES_G2)<-c("Broker","GenderCombo","BrokerSector","Freq")
    REP_RES_G2<-dplyr::filter(REP_RES_G2,Freq!=0)
    #REP_RES_G2$Role<-rep("RepGate",length(REP_RES_G2$Freq))
    REP_RES_G3<-tidyr::spread(REP_RES_G2,GenderCombo,Freq)

    REP_RES2<-table(REP_RES1)
    REP_RES2<-tibble::as_tibble(REP_RES2)
    colnames(REP_RES2)<-c("Broker","GenderCombo","Freq")
    REP_RES2<-dplyr::filter(REP_RES2,Freq!=0)
    #REP_RES2$Role<-rep("RepGate",length(REP_RES2$Freq))
    REP_RES3<-tidyr::spread(REP_RES2,GenderCombo,Freq)#melt(REP_RES2,id.vars = "Broker")
    NR<-GCL[!(GCL%in%unique(REP_RES2$GenderCombo))]
    if (length(NR)>0){
      for (i in 1:length(NR)){
        y<-NR[i]
        REP_RES3[,y]<-rep(0,length(REP_RES3$Broker))
        REP_RES_G3[,y]<-rep(0,length(REP_RES_G3$Broker))
      }
    }else{
      REP_RES3<-REP_RES3
      REP_RES_G3<-REP_RES_G3
    }
    colnames(REP_RES3)[2:5]<-paste0("RepGate.",colnames(REP_RES3)[2:5])
    colnames(REP_RES_G3)[3:6]<-paste0("RepGate.",colnames(REP_RES_G3)[3:6])
  }

  #############################################################################

  if (is.na(COOR_DF$Firm1[1])){
    COOR_RES2<-data.frame(Broker=NA,
                          GenderCombo=NA,
                          Freq=NA,
                          Role="Coordinator")
    COOR_RES_G2<-data.frame(Broker=NA,
                            GenderCombo=NA,
                            BrokerSector=NA,
                            Freq=NA,
                            Role="Coordinator")

    COOR_RES3<-data.frame(Broker=NA,
                          Coordinator.MM=NA,Coordinator.FF=NA,Coordinator.FM=NA,
                          Coordinator.MF=NA)
    COOR_RES_G3<-data.frame(Broker=NA,BrokerSector=NA,Coordinator.MM=NA,
                            Coordinator.FF=NA,Coordinator.FM=NA,Coordinator.MF=NA)

  }else{
    COOR_RES1<-cbind(COOR_DF$Firm2BROKER,COOR_DF$GENDER_COMBO
    )
    COOR_RES1<-tibble::as_tibble(COOR_RES1)
    COOR_RES_G1<-cbind(COOR_DF$Firm2BROKER,
                       COOR_DF$GENDER_COMBO,COOR_DF$SECTOR2
    )
    COOR_RES_G1<-tibble::as_tibble(COOR_RES_G1)

    COOR_RES_G2<-stats::ftable(COOR_RES_G1)
    COOR_RES_G2<-tibble::as_tibble(COOR_RES_G2)
    colnames(COOR_RES_G2)<-c("Broker","GenderCombo","BrokerSector","Freq")
    COOR_RES_G2<-dplyr::filter(COOR_RES_G2,Freq!=0)
    #COOR_RES_G2$Role<-rep("Coordinator",length(COOR_RES_G2$Freq))

    COOR_RES2<-table(COOR_RES1)
    COOR_RES2<-tibble::as_tibble(COOR_RES2)
    colnames(COOR_RES2)<-c("Broker","GenderCombo","Freq")
    COOR_RES2<-dplyr::filter(COOR_RES2,Freq!=0)
    #COOR_RES2$Role<-rep("Coordinator",length(COOR_RES2$Freq))

    COOR_RES3<-tidyr::spread(COOR_RES2,GenderCombo,Freq)
    COOR_RES_G3<-tidyr::spread(COOR_RES_G2,GenderCombo,Freq)

    NR<-GCL[!(GCL%in%unique(COOR_RES2$GenderCombo))]
    if (length(NR)>0){
      for (i in 1:length(NR)){
        y<-NR[i]
        COOR_RES3[,y]<-rep(0,length(COOR_RES3$Broker))
        COOR_RES_G3[,y]<-rep(0,length(COOR_RES_G3$Broker))
      }
    }else{
      COOR_RES3<-COOR_RES3
      COOR_RES_G3<-COOR_RES_G3
    }
    colnames(COOR_RES3)[2:5]<-paste0("Coordinator.",colnames(COOR_RES3)[2:5])
    colnames(COOR_RES_G3)[3:6]<-paste0("Coordinator.",colnames(COOR_RES_G3)[3:6])
  }

  #############################################################################
  if (is.na(LIA_DF$Firm1[1])){
    LIA_RES2<-data.frame(Broker=NA,
                         GenderCombo=NA,
                         Freq=NA,
                         Role="Liaison")
    LIA_RES_G2<-data.frame(Broker=NA,
                           GenderCombo=NA,
                           BrokerSector=NA,
                           Freq=NA,
                           Role="Liaison")

    LIA_RES3<-data.frame(Broker=NA,
                         Liaison.MM=NA,Liaison.FF=NA,Liaison.FM=NA,
                         Liaison.MF=NA)

    LIA_RES_G3<-data.frame(Broker=NA,BrokerSector=NA,Liaison.MM=NA,
                           Liaison.FF=NA,Liaison.FM=NA,Liaison.MF=NA)
  }else{
    LIA_RES1<-cbind(LIA_DF$Firm2BROKER,LIA_DF$GENDER_COMBO
    )
    LIA_RES1<-tibble::as_tibble(LIA_RES1)
    LIA_RES_G1<-cbind(LIA_DF$Firm2BROKER,
                      LIA_DF$GENDER_COMBO,LIA_DF$SECTOR2
    )
    LIA_RES_G1<-tibble::as_tibble(LIA_RES_G1)

    LIA_RES_G2<-stats::ftable(LIA_RES_G1)
    LIA_RES_G2<-tibble::as_tibble(LIA_RES_G2)
    colnames(LIA_RES_G2)<-c("Broker","GenderCombo","BrokerSector","Freq")
    LIA_RES_G2<-dplyr::filter(LIA_RES_G2,Freq!=0)
    #LIA_RES_G2$Role<-rep("Liaison",length(LIA_RES_G2$Freq))

    LIA_RES2<-table(LIA_RES1)
    LIA_RES2<-tibble::as_tibble(LIA_RES2)
    colnames(LIA_RES2)<-c("Broker","GenderCombo","Freq")
    LIA_RES2<-dplyr::filter(LIA_RES2,Freq!=0)
    #LIA_RES2$Role<-rep("Liaison",length(LIA_RES2$Freq))
    LIA_RES_G3<-tidyr::spread(LIA_RES_G2,GenderCombo,Freq)
    LIA_RES3<-tidyr::spread(LIA_RES2,GenderCombo,Freq)

    NR<-GCL[!(GCL%in%unique(LIA_RES2$GenderCombo))]
    if (length(NR)>0){
      for (i in 1:length(NR)){
        y<-NR[i]
        LIA_RES3[,y]<-rep(0,length(LIA_RES3$Broker))
        LIA_RES_G3[,y]<-rep(0,length(LIA_RES_G3$Broker))
      }
    }else{
      LIA_RES3<-LIA_RES3
      LIA_RES_G3<-LIA_RES_G3
    }
    colnames(LIA_RES3)[2:5]<-paste0("Liaison.",colnames(LIA_RES3)[2:5])
    colnames(LIA_RES_G3)[3:6]<-paste0("Liaison.",colnames(LIA_RES_G3)[3:6])

  }
  ############################################################################
  if (is.na(ITIN_DF$Firm1[1])){
    ITIN_RES2<-data.frame(Broker=NA,
                          GenderCombo=NA,
                          Freq=NA,
                          Role="Itinerant")
    ITIN_RES_G2<-data.frame(Broker=NA,
                            GenderCombo=NA,
                            BrokerSector=NA,
                            Freq=NA,
                            Role="Itinerant")
    ITIN_RES3<-data.frame(Broker=NA,
                          Liaison.MM=NA,Liaison.FF=NA,Liaison.FM=NA,
                          Liaison.MF=NA)
    ITIN_RES_G3<-data.frame(Broker=NA,BrokerSector=NA,Liaison.MM=NA,
                            Liaison.FF=NA,Liaison.FM=NA,Liaison.MF=NA)
  }else{
    ITIN_RES1<-cbind(ITIN_DF$Firm2BROKER,ITIN_DF$GENDER_COMBO
    )
    ITIN_RES1<-tibble::as_tibble(ITIN_RES1)
    ITIN_RES_G1<-cbind(ITIN_DF$Firm2BROKER,
                       ITIN_DF$GENDER_COMBO,ITIN_DF$SECTOR2
    )
    ITIN_RES_G1<-tibble::as_tibble(ITIN_RES_G1)

    ITIN_RES_G2<-stats::ftable(ITIN_RES_G1)
    ITIN_RES_G2<-tibble::as_tibble(ITIN_RES_G2)
    colnames(ITIN_RES_G2)<-c("Broker","GenderCombo","BrokerSector","Freq")
    ITIN_RES_G2<-dplyr::filter(ITIN_RES_G2,Freq!=0)
    #ITIN_RES_G2$Role<-rep("Itinerant",length(ITIN_RES_G2$Freq))

    ITIN_RES2<-table(ITIN_RES1)
    ITIN_RES2<-tibble::as_tibble(ITIN_RES2)
    colnames(ITIN_RES2)<-c("Broker","GenderCombo","Freq")
    ITIN_RES2<-dplyr::filter(ITIN_RES2,Freq!=0)
    #ITIN_RES2$Role<-rep("Itinerant",length(ITIN_RES2$Freq))

    ITIN_RES_G3<-tidyr::spread(ITIN_RES_G2,GenderCombo,Freq)
    ITIN_RES3<-tidyr::spread(ITIN_RES2,GenderCombo,Freq)

    NR<-GCL[!(GCL%in%unique(ITIN_RES2$GenderCombo))]
    if (length(NR)>0){
      for (i in 1:length(NR)){
        y<-NR[i]
        ITIN_RES3[,y]<-rep(0,length(ITIN_RES3$Broker))
        ITIN_RES_G3[,y]<-rep(0,length(ITIN_RES_G3$Broker))
      }
    }else{
      ITIN_RES3<-ITIN_RES3
      ITIN_RES_G3<-ITIN_RES_G3
    }
    colnames(ITIN_RES3)[2:5]<-paste0("Itinerant.",colnames(ITIN_RES3)[2:5])
    colnames(ITIN_RES_G3)[3:6]<-paste0("Itinerant.",colnames(ITIN_RES_G3)[3:6])
  }

  ################################################################
  COM_ID<-dplyr::filter(Vert,Vert$type==FALSE)
  COM_ID<-dplyr::select(COM_ID,"name")

  CRep<-merge(COM_ID,REP_RES3,by.x="name",
              by.y="Broker",all.x=TRUE)
  CLia<-merge(COM_ID,LIA_RES3,by.x="name",
              by.y="Broker",all.x=TRUE)
  CIter<-merge(COM_ID,ITIN_RES3,by.x="name",
               by.y="Broker",all.x=TRUE)
  CCoor<-merge(COM_ID,COOR_RES3,by.x="name",
               by.y="Broker",all.x=TRUE)

  TOTAL_DF<-merge(CRep,CLia,
                  by.x = "name",by.y="name",all.x=TRUE)
  TOTAL_DF<-merge(TOTAL_DF,CIter,
                  by.x = "name",by.y="name",all.x=TRUE)
  TOTAL_DF<-merge(TOTAL_DF,CCoor,
                  by.x = "name",by.y="name",all.x=TRUE)

  TOTAL_DF[is.na(TOTAL_DF)]<-0

  TOTAL_DF$SECTOR<-TOTAL_DF$name

  TOTAL_DF$SECTOR<-SECTOR_ID$SECTOR[match(TOTAL_DF$name,SECTOR_ID$`COMPANY ID`)]

  RESULTS_LIST<-list(COOR_RES_G3,REP_RES_G3,ITIN_RES_G3,LIA_RES_G3)
  GF_LIST<-list(BROKERAGE_PATH_LIST,RESULTS_LIST,TOTAL_DF)
  return(GF_LIST)
}
