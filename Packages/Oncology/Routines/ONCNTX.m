ONCNTX ;Hines OIFO/GWB; No treatment stuffing ;08/27/97
 ;;2.11;ONCOLOGY;**13,15,16,19,22,25,26,27,32,33,34,36,37,38,39,41,42,43,44,45,46,47,49**;Mar 07, 1995;Build 38
 ;
NTX ;No treatment stuffing
 ;ROADS
 N SITE
 S SITE=$P(^ONCO(165.5,DA,0),U,1)
 I DATEDX<3030000 D
 .D HDR,SURR,SURATFR^ONCNTX1,NODER
 .I (SITE=35)!(SITE=58)!(SITE=63)!(SITE=65)!($$LYMPHOMA^ONCFUNC(DA)=1) D NODATFR^ONCUTX1
 .E  D NODATFR^ONCNTX1
 .D SOSNR,SOSATFR^ONCNTX1
 .W ! D PAUSE^ONCOPA2A
 ;
 ;FORDS
 D HDR
 S TXDT=$P($G(^ONCO(165.5,DA,3.1)),U,38)_"S0"
 K ^ONCO(165.5,"ATX",DA,TXDT)
 S $P(^ONCO(165.5,DA,3.1),U,38)="0000000"
 S ^ONCO(165.5,"ATX",DA,"0000000S0")=""
 D SUR,SURATF^ONCNTX1,SM,NODE
 ;Code 9 (FORDS 138-139)
 N MO S MO=$$HIST^ONCFUNC(DA)
 I ($E(TP,1,4)=6770)!($E(TP,1,4)=6771)!($E(TP,1,4)=6772)!($E(TP,1,4)=6776)!(($$LYMPHOMA^ONCFUNC(DA)=1)&($E(TP,1,4)=6777))!($E(TP,1,4)=6776)!(TP=67809)!(TP=67420)!(TP=67421)!(TP=67423)!(TP=67424)!((MO'<97310)&(MO'>99899)) D NODEATF^ONCUTX1
 E  D NODEATF^ONCNTX1
 D SOSN,SOSNATF^ONCNTX1,RR,DSD
 D RFNS W !
 K ^ONCO(165.5,DA,14)
 K DIR S DIR(0)="E" D ^DIR
 S $P(^ONCO(165.5,DA,3),U,6)=0
 S $P(^ONCO(165.5,DA,3.1),U,12)=0
 S $P(^ONCO(165.5,DA,3),U,35)=""
 D HDR,RAD1,RADATF^ONCNTX1,RAD2,RSSQ^ONCNTX1,RFNR W !
 K DIR S DIR(0)="E" D ^DIR
 S $P(^ONCO(165.5,DA,3),U,13)="00"
 S $P(^ONCO(165.5,DA,3.1),U,14)="00"
 S $P(^ONCO(165.5,DA,3),U,16)="00"
 S $P(^ONCO(165.5,DA,3.1),U,16)="00"
 S $P(^ONCO(165.5,DA,3),U,19)="00"
 S $P(^ONCO(165.5,DA,3.1),U,18)="00"
 S $P(^ONCO(165.5,DA,3.1),U,36)=1
 S $P(^ONCO(165.5,DA,3),U,25)=0
 S $P(^ONCO(165.5,DA,3.1),U,20)=0
 D HDR,CHE,CHEMATF^ONCNTX1,HOR^ONCNTX1,HTATF^ONCNTX1,IMM^ONCNTX1,IMMATF^ONCNTX1,HTEP^ONCNTX1,SSS^ONCNTX1,OTH^ONCNTX1,OTHATF^ONCNTX1
 K DIR S DIR(0)="E" D ^DIR I Y=0 S Y="@0" G EXIT
 W ! S Y="@425" G EXIT
 ;
SURR ;SURGERY OF PRIMARY (R) (165.5,58.2)
 S $P(^ONCO(165.5,DA,3),U,38)=$S(DATEDX>2971231:1,1:"00")
 S $P(^ONCO(165.5,DA,3),U,34)=1
 S DR="58.2;74" D DIQ1
 I $D(NTX) D
 .W !,"SURGICAL PROCEDURES (R)=ROADS"
 .W !,"-----------------------------"
 .W !,"SURGERY OF PRIMARY..........(R): ",ONC(165.5,DA,58.2,"E")
 W !,"SURGICAL APPROACH...........(R): ",ONC(165.5,DA,74,"E")
 K ONC
 Q
 ;
SUR ;SURGERY OF PRIMARY (F) (165.5,58.6)
 S TOPX=$P($G(^ONCO(165.5,DA,2)),U,1)
 I (TOPX=67420)!(TOPX=67421)!(TOPX=67423)!(TOPX=67424)!($E(TOPX,3,4)=76)!(TOPX=67809) D  G SUR1
 .S $P(^ONCO(165.5,DA,3.1),U,29)=1
 S $P(^ONCO(165.5,DA,3.1),U,29)=$S(DATEDX>2971231:1,1:"00")
SUR1 S TXDT=$P($G(^ONCO(165.5,DA,3)),U,1)_"S1"
 K ^ONCO(165.5,"ATX",DA,TXDT)
 S $P(^ONCO(165.5,DA,3),U,1)="0000000" D SPSDT^ONCATF
 S ^ONCO(165.5,"ATX",DA,"0000000S1")=""
 S $P(^ONCO(165.5,DA,3),U,28)=8
 S $P(^ONCO(165.5,DA,0),U,11)="00000000"
 S $P(^ONCO(165.5,DA,"THY1"),U,36)="0000000"
 S $P(^ONCO(165.5,DA,3.1),U,28)=0
 S DR="58.6;50;74;59;435;14;170" D DIQ1
 I $D(NTX) D
 .W !,"SURGICAL PROCEDURES (F)=FORDS"
 .W !,"-----------------------------"
 .W !,"DATE FIRST SURGICAL PROCEDURE..: ",ONC(165.5,DA,170,"E")
 .W !,"SURGERY OF PRIMARY..........(F): ",ONC(165.5,DA,58.6,"E")
 W !,"MOST DEFINITIVE SURG DATE......: ",ONC(165.5,DA,50,"E")
 K ONC,TXDT,TOPX
 Q
 ;
SM ;SURGICAL MARGINS (165.5,59)
 S $P(^ONCO(165.5,DA,3),U,28)=8
 S TPG=$P($G(^ONCO(165.5,DA,2)),U,1)
 I ($E(TPG,3,4)=76)!(TPG=67809)!(TPG=67420)!(TPG=67421)!(TPG=67423)!(TPG=67424) S $P(^ONCO(165.5,DA,3),U,28)=9
 N MO S MO=$$HIST^ONCFUNC(DA)
 S HST14=$E(MO,1,4)
 I (HST14=9750)!((HST14>9759)&(HST14<9765))!((HST14>9799)&(HST14<9821))!(HST14=9826)!((HST14>9830)&(HST14<9921))!((HST14>9930)&(HST14<9965))!((HST14>9979)&(HST14<9990)) S $P(^ONCO(165.5,DA,3),U,28)=9
 I $$LYMPHOMA^ONCFUNC(DA),($E(TPG,3,4)=77) S $P(^ONCO(165.5,DA,3),U,28)=9
 S DR="59" D DIQ1
 W !,"SURGICAL MARGINS...............: ",ONC(165.5,DA,59,"E")
 K HST14,ONC
 Q
 ;
NODER ;SCOPE OF LN SURGERY (R) (165.5,138)
 ;For unknown primary, leukemia, lymphoma, & brain, code 9
 N LAST,SC,SGRP,SITE
 S SITE=$P(^ONCO(165.5,DA,0),U,1)
 I (SITE=35)!(SITE=58)!(SITE=63)!(SITE=65)!($$LYMPHOMA^ONCFUNC(DA)=1) D  D NUMND^ONCATF Q
 .D SGRP^ONCUTX1
 .I ($E(TPG,3,4)=76)!(TPG=67809)!(TPG=67420)!(TPG=67421)!(TPG=67423)!(TPG=67424) S SGRP=67141
 .F SC=0:0 S SC=$O(^ONCO(164,SGRP,"SC5",SC)) Q:SC="B"  S LAST=SC
 .S $P(^ONCO(165.5,DA,3),U,40)=LAST
 .W !,"SCOPE OF LN SURGERY.........(R): ",$P(^ONCO(164,SGRP,"SC5",LAST,0),U,1)
 .D NODER^ONCUTX
 S $P(^ONCO(165.5,DA,3),U,40)=1
 S $P(^ONCO(165.5,DA,3),U,42)="00"
 D NUMND^ONCATF
 S DR="138;140" D DIQ1
 W:$D(NTX) !,"SCOPE OF LN SURGERY.........(R): ",ONC(165.5,DA,138,"E")
 W !,"NUMBER OF LN REMOVED........(R): ",ONC(165.5,DA,140,"E")
 Q
 ;
NODE ;SCOPE OF LN SURGERY (F) (165.5,138.4)
 ;Code 9 (FORDS 138-139)
 S TP=$P($G(^ONCO(165.5,DA,2)),U,1)
 N MO S MO=$$HIST^ONCFUNC(DA)
 I ($E(TP,1,4)=6770)!($E(TP,1,4)=6771)!($E(TP,1,4)=6772)!($E(TP,1,4)=6776)!(($$LYMPHOMA^ONCFUNC(DA)=1)&($E(TP,1,4)=6777))!($E(TP,1,4)=6776)!(TP=67809)!(TP=67420)!(TP=67421)!(TP=67423)!(TP=67424)!((MO'<97310)&(MO'>99899)) D  D SCPDT^ONCATF Q
 .S $P(^ONCO(165.5,DA,3.1),U,31)=9
 .W !,"SCOPE OF LN SURGERY.........(F): Unknown/NA"
 .D NODE^ONCUTX
 S $P(^ONCO(165.5,DA,3.1),U,31)=0
 S TXDT=$P($G(^ONCO(165.5,DA,3.1)),U,22)_"S2"
 K ^ONCO(165.5,"ATX",DA,TXDT)
 S $P(^ONCO(165.5,D0,3.1),U,22)="0000000" D SCPDT^ONCATF
 S ^ONCO(165.5,"ATX",DA,"0000000S2")=""
 S DR="138.4;138.2" D DIQ1
 W:$D(NTX) !,"SCOPE OF LN SURGERY.........(F): ",ONC(165.5,DA,138.4,"E")
 W !,"SCOPE OF LN SURGERY DATE.......: ",ONC(165.5,DA,138.2,"E")
 Q
 ;
SOSNR ;SURG PROC/OTHER SITE (R) (165.5,139)
 S $P(^ONCO(165.5,DA,3),U,41)=1
 S DR=139 D DIQ1
 W:$D(NTX) !,"SURG PROC/OTHER SITE........(R): ",ONC(165.5,DA,139,"E")
 Q
 ;
SOSN ;SURG PROC/OTHER SITE (F) (165.5,139.4)
 S $P(^ONCO(165.5,DA,3.1),U,33)=0
 S TXDT=$P($G(^ONCO(165.5,DA,3.1)),U,24)_"S3"
 K ^ONCO(165.5,"ATX",DA,TXDT)
 S $P(^ONCO(165.5,D0,3.1),U,24)="0000000" D SOSNDT^ONCATF
 S ^ONCO(165.5,"ATX",DA,"0000000S3")=""
 S DR="139.4;139.2" D DIQ1
 W:$D(NTX) !,"SURG PROC/OTHER SITE........(F): ",ONC(165.5,DA,139.4,"E")
 W !,"SURG PROC/OTHER SITE DATE......: ",ONC(165.5,DA,139.2,"E")
 Q
 ;
RR ;RECONSTRUCTION/RESTORATION (165.5,23)
 I DATEDX>3021231 Q
 I DATEDX>2951231 D
 .S $P(^ONCO(165.5,DA,3),U,33)=$S(DATEDX>2971231:1,1:9)
 .S DR=23 D DIQ1
 .W !,"RECONSTRUCTION/RESTORATION.....: ",ONC(165.5,DA,23,"E")
 Q
 ;
DSD ;DATE OF SURGICAL DISCHARGE (165.5,435)
 ;READMISSION W/I 30 DAYS/SURG (165.5,14)
 S $P(^ONCO(165.5,DA,"THY1"),U,36)="0000000"
 S $P(^ONCO(165.5,DA,3.1),U,28)=0
 S $P(^ONCO(165.5,DA,7),U,19)=9
 S $P(^ONCO(165.5,DA,7),U,20)=""
 S DR="435;14;46" D DIQ1
 W !,"DATE OF SURGICAL DISCHARGE.....: ",ONC(165.5,DA,435,"E")
 W !,"READMISSION W/I 30 DAYS/SURG...: ",ONC(165.5,DA,14,"E")
 W !,"CAP PROTOCOL REVIEW............: ",ONC(165.5,DA,46,"E")
 K ONC
 Q
 ;
RFNS ;REASON NO SURGERY OF PRIMARY (165.5,58)
 N RFNS
 S RFNS=$$GET1^DIQ(165.5,DA,1.2)
 I (RFNS="Autopsy only")!(RFNS="Death certificate only") D  Q
 .S $P(^ONCO(165.5,DA,3),U,26)=9
 .W !,"REASON NO SURGERY OF PRIMARY...: Unknown"
 S RFNSDD=$P(^DD(165.5,58,0),U,3)
 W ! K DIR S DIR(0)="SA^"_RFNSDD
 S DIR("A")="REASON NO SURGERY OF PRIMARY: "
 S DIR("B")="Not part of 1st course"
 S DIR("??")="^D RFNSHLP^ONCNTX1"
 D ^DIR
 I Y[U Q
 S $P(^ONCO(165.5,DA,3),U,26)=Y
 K RFNSDD Q
 ;
RAD1 ;RADIATION (165.5,51.2)
 N RFNR
 S RFNR=$P($G(^ONCO(165.5,DA,3)),U,35)
 D ^ONCRFNR
 S DR="51.2;51" D DIQ1
 W:$D(NTX) !,"RADIATION:.....................: ",ONC(165.5,DA,51.2,"E")
 W !,"DATE RADIATION STARTED.........: ",ONC(165.5,DA,51,"E")
 K ONC,TXDT Q
 ;
RAD2 ;RADIATION (cont)
 S $P(^ONCO(165.5,DA,3),U,22)=0
 S $P(^ONCO(165.5,DA,3),U,21)=1
 S $P(^ONCO(165.5,DA,"BLA2"),U,18)=1
 S $P(^ONCO(165.5,DA,"THY1"),U,43)=0
 S $P(^ONCO(165.5,DA,24),U,9)=1
 S $P(^ONCO(165.5,DA,"THY1"),U,44)=0
 S $P(^ONCO(165.5,DA,3),U,20)=0
 S $P(^ONCO(165.5,DA,"BLA2"),U,16)="0000000"
 K ^ONCO(165.5,DA,15)
 K ^ONCO(165.5,DA,16)
 S DR="126;125;363;442;363.1;443;56;361" D DIQ1
 W !,"LOCATION OF RADIATION..........: ",ONC(165.5,DA,126,"E")
 W !,"RADIATION TREATMENT VOLUME.....: ",ONC(165.5,DA,125,"E")
 W !,"REGIONAL TREATMENT MODALITY....: ",ONC(165.5,DA,363,"E")
 W !,"REGIONAL DOSE:cGy..............: ",ONC(165.5,DA,442,"E")
 W !,"BOOST TREATMENT MODALITY.......: ",ONC(165.5,DA,363.1,"E")
 W !,"BOOST DOSE:cGy.................: ",ONC(165.5,DA,443,"E")
 W !,"NUMBER OF TREATMENTS...........: ",ONC(165.5,DA,56,"E")
 W !,"DATE RADIATION ENDED...........: ",ONC(165.5,DA,361,"E")
 K ONC,TXDT Q
 ;
RFNR ;REASON FOR NO RADIATION (165.5,75)
 S RFNRDD=$P(^DD(165.5,75,0),U,3)
 W ! K DIR S DIR(0)="SA^"_RFNRDD
 S DIR("A")="REASON FOR NO RADIATION: "
 S DIR("B")="Not part of 1st course"
 S DIR("??")="^D RFNRHLP^ONCNTX1"
 D ^DIR
 I Y[U Q
 S $P(^ONCO(165.5,DA,3),U,35)=Y
 I X=8 S RFNR=8 D ^ONCRFNR D
 .W !!,"DATE RADIATION STARTED changed to 88/88/8888"
 K RFNRDD Q
 ;
CHE ;CHEMOTHERAPY (165.5,53.2)
 I $D(NTX) D
 .N DIE,DR,DP,DL
 .S DIE="^ONCO(165.5,",DR=53.2 D ^DIE
 S TXDT=$P(^ONCO(165.5,DA,3),U,11)_"C"
 K ^ONCO(165.5,"ATX",DA,TXDT)
 S $P(^ONCO(165.5,DA,3),U,11)="0000000" D CHEMDT^ONCATF1
 S ^ONCO(165.5,"ATX",DA,"0000000C")=""
 K ^ONCO(165.5,DA,17)
 F CMX=28,29,30,44,45 S $P(^ONCO(165.5,DA,"LUN2"),U,CMX)="" K CMX
 S DR="53.2;53" D DIQ1
 W !,"CHEMOTHERAPY DATE.............: ",ONC(165.5,DA,53,"E")
 K ONC Q
 ;
NCDS ;SURGICAL DX/STAGING PROC (165.5,58.1)
 S $P(^ONCO(165.5,D0,3),U,31)="0000000" D NCDSDT^ONCATF
 S DR=58.3 D DIQ1
 W !,"SURGICAL DX/STAGING PROC DATE: ",ONC(165.5,DA,58.3,"E")
 Q
 ;
HDR K DASH S $P(DASH,"-",80)="-"
 W @IOF,DASH,!,?1,PATNAM,?TAB,"First Course of Treatment",?SITTAB,SITEGP,!,?1,SSN,?TOPTAB,TOPNAM," ",TOPCOD,!,DASH
 K DASH
 Q
 ;
DIQ1 N DIC,DIQ K ONC
 S DIC="^ONCO(165.5,",DIQ="ONC(",DIQ(0)="E" D EN^DIQ1
 Q
 ;
EXIT W !
 K TP,TPG
 Q
