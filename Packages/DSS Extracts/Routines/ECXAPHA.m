ECXAPHA ;ALB/TMD-Pharmacy Extracts Unusual Volumes Report ;8/23/12  00:16
 ;;3.0;DSS EXTRACTS;**40,49,66,104,109,113,136**;Dec 22, 1997;Build 28
 ;
EN ; entry point
 N X,Y,DATE,ECRUN,ECXOPT,ECXDESC,ECXSAVE,ECXTL,ECTHLD,ECSD
 N ECSD1,ECSTART,ECED,ECEND,ECXERR,QFLG,ECXISIG
 S QFLG=0
 ; get today's date
 D NOW^%DTC S DATE=X,Y=$E(%,1,12) D DD^%DT S ECRUN=$P(Y,"@") K %DT
 D BEGIN Q:QFLG
 D SELECT Q:QFLG
 S ECXDESC=ECXTL_" Extract Unusual Volume Report"
 S ECXSAVE("EC*")=""
 W !!,"This report requires 132-column format."
 D EN^XUTMDEVQ("PROCESS^ECXAPHA",ECXDESC,.ECXSAVE)
 I POP W !!,"No device selected...exiting.",! Q
 I IO'=IO(0) D ^%ZISC
 D HOME^%ZIS
 D AUDIT^ECXKILL
 Q
 ;
BEGIN ; display report description
 W @IOF
 W !,"This report prints a listing of unusual volumes that would be"
 W !,"generated by the pharmacy extracts (PRE, IVP and UDP) as"
 W !,"determined by a user defined threshold value.  It shoud be run"
 W !,"prior to the generation of the actual extract(s) to identify and"
 W !,"fix as necessary any volumes determined to be erroneous."
 W !!,"Unusual volumes are defined as follows:"
 W !!,"PRE Extract:  Quantity field greater than the threshold value."
 W !,"IVP Extract:  Total Doses Per Day field greater than the threshold"
 W !,?14,"or less than the negative of the threshold value."
 W !,"UDP Extract:  Quantity field greater than threshold value."
 W !!,"Note: The threshold can be set after a report is selected."
 W !!,"Run times for this report will vary depending upon the size of"
 W !,"the extract and could take as long as 30 minutes or more to"
 W !,"complete.  This report has no effect on the actual extracts and"
 W !,"can be run as needed."
 W !!,"The report is sorted by Feeder Key, descending Volume, and SSN."
 S DIR(0)="E" W ! D ^DIR K DIR I 'Y S QFLG=1 Q
 W:$Y!($E(IOST)="C") @IOF,!!
 Q
 ;
SELECT ; user inputs for report option, threshold volume and date range
 N DONE,OUT
 S ECXISIG=0
 ; allow user to select report option (PRE,IVP or UDP)
 W "Choose the report you would like to run."
 S DIR(0)="S^1:PRE;2:IVP;3:UDP",DIR("A")="Selection",DIR("B")=1 D ^DIR K DIR S ECXOPT=Y I X["^" S QFLG=1 Q
 S ECXTL=$S(ECXOPT=1:"Prescription",ECXOPT=2:"IV Detail",ECXOPT=3:"Unit Dose Local",1:"")
 ; allow user to set threshold volume
 S ECTHLD=$S(ECXOPT=2:1000,1:500)
 W !!,"The default threshold volume for the ",ECXTL," extract is ",ECTHLD,"."
 S DIR(0)="Y",DIR("A")="Would you like to change the threshold",DIR("B")="NO" D ^DIR K DIR I X["^" S QFLG=1 Q
 I Y D
 .W !!,$S(ECXOPT=2:"threshold > Total Doses Per Day < -threshold",1:"Quantity > threshold")
 .S DIR(0)="N^0:100000:0",DIR("A")="Enter the new threshold volume" D ^DIR K DIR S ECTHLD=Y I X["^" S QFLG=1 Q
 ; check to see if SIG should be place on the sec line of rpt cvw - *136 
 I ECXOPT=3 S DIR(0)="Y",DIR("A")="Include SIG/Order Direction on line 2 of report",DIR("B")="NO" D ^DIR K DIR S:Y ECXISIG=1 I X["^" S QFLG=1 Q
 ; get date range from user
 W !!,"Enter the date range for which you would like to scan the ",ECXTL,!,"Extract records."
 S DONE=0 F  S (ECED,ECSD)="" D  Q:QFLG!DONE
 .K %DT S %DT="AEX",%DT("A")="Starting with Date: ",%DT(0)=-DATE D ^%DT
 .I Y<0 S QFLG=1 Q
 .S ECSD=Y,ECSD1=ECSD-.1
 .D DD^%DT S ECSTART=Y
 .K %DT S %DT="AEX",%DT("A")="Ending with Date: ",%DT(0)=-DATE D ^%DT
 .I Y<0 S QFLG=1 Q
 .I Y<ECSD D  Q
 ..W !!,"The ending date cannot be earlier than the starting date."
 ..W !,"Please try again.",!!
 .I $E(Y,1,5)'=$E(ECSD,1,5) D  Q
 ..W !!,"Beginning and ending dates must be in the same month and year."
 ..W !,"Please try again.",!!
 .S ECED=Y
 .D DD^%DT S ECEND=Y
 .S DONE=1
 Q
 ;
PROCESS ; entry point for queued report
 S ZTREQ="@"
 S ECXERR=0 D EN^ECXAPHA2 Q:ECXERR
 S QFLG=0 D PRINT
 Q
 ;
PRINT ; process temp file and print report
 N PG,QFLG,GTOT,LN,COUNT,FKEY,QTY,SSN,REC,EDAY,ECXCOUNT
 U IO
 I $D(ZTQUEUED),$$S^%ZTLOAD S ZTSTOP=1 K ZTREQ Q
 S (PG,QFLG,GTOT)=0,$P(LN,"-",132)=""
 D HEADER Q:QFLG
 S COUNT=0,FKEY="" F  S FKEY=$O(^TMP($J,FKEY)) Q:FKEY=""!QFLG  D
 .S QTY="" F  S QTY=$O(^TMP($J,FKEY,QTY)) Q:QTY=""!QFLG  D
 ..S EDAY="" F  S EDAY=$O(^TMP($J,FKEY,QTY,EDAY)) Q:EDAY=""!QFLG  D
 ...S ECXCOUNT="" F  S ECXCOUNT=$O(^TMP($J,FKEY,QTY,EDAY,ECXCOUNT)) Q:ECXCOUNT=""!QFLG  D
 ....S SSN=""
 ....F  S SSN=$O(^TMP($J,FKEY,QTY,EDAY,ECXCOUNT,SSN)) Q:SSN=""!QFLG  S REC=^(SSN)  D
 .....S COUNT=COUNT+1
 .....I $Y+3>IOSL D HEADER Q:QFLG
 .....W !,$P(REC,U),?8,$P(REC,U,2),?20,$P(REC,U,3),?29,$E($P(REC,U,4),1,40)
 .....W ?71,$P(REC,U,5),?89,$$RJ^XLFSTR($P(REC,U,6),9)_" "_$E($P(REC,U,7),1,7)
 .....I ECXOPT=1 D
 ......W ?108,$$RJ^XLFSTR($P(REC,U,8),12),?125,$$RJ^XLFSTR($P(REC,U,9),3)
 .....I ECXOPT'=1 D
 ......W ?116,$$RJ^XLFSTR($P(REC,U,8),14)
 .....I ECXOPT=3&($G(ECXISIG)) D
 ......W !,?5,"SIG: ",$S($P(REC,U,10)="":"N/A",1:$P(REC,U,10)),! ;136
 Q:QFLG
 I COUNT=0 W !!,?8,"No unusual volumes to report for this extract"
CLOSE ;
 I $E(IOST)="C",'QFLG D
 .S SS=22-$Y F JJ=1:1:SS W !
 .S DIR(0)="E" W ! D ^DIR K DIR
 Q
 ;
HEADER ;header and page control
 N SS,JJ
 I $E(IOST)="C" D
 .S SS=22-$Y F JJ=1:1:SS W !
 .I PG>0 S DIR(0)="E" W ! D ^DIR K DIR S:'Y QFLG=1
 Q:QFLG
 W:$Y!($E(IOST)="C") @IOF S PG=PG+1
 W !,ECXTL_" Extract Unusual Volume Report",?124,"Page: "_PG
 W !,"Start Date: ",ECSTART,?97,"Report Run Date/Time:  "_ECRUN
 W !,"End Date:   ",ECEND,?97,"Threshold Value = ",ECTHLD
 W !!,"Name",?11,"SSN",?21,"Day",?29,"Generic Name",?71,"Feeder Key"
 I ECXOPT=1 D
 .W ?95,"Quantity",?109,"Total Cost",?120,"Days Supply"
 E  D
 .I ECXOPT=2 W ?93,"Total Doses",?121,"Total Cost",!,?95,"Per Day"
 .I ECXOPT'=2 W ?96,"Quantity",?121,"Total Cost"
 W !,LN,!
 Q
 ;
SIG(ORDNO,PATNO) ;Get ordering instructions for unit dose order.  API added in patch 136
 N DATA,RECNO,I,SIG
 S SIG=""
 I ORDNO=""!(PATNO="") Q SIG
 S RECNO=ORDNO_","_PATNO_","
 D GETS^DIQ(55.06,RECNO,"26;120;121","E","DATA")
 F I=120,121,26 S SIG=$G(SIG)_$S($L(SIG)>0:" ",1:"")_$G(DATA(55.06,RECNO,I,"E"))
 Q SIG
