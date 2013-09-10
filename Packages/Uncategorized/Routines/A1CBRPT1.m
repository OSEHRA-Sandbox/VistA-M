A1CBRPT1 ; isc1/jas;routine to report task data;[ 05/07/93  3:24 PM ]
 ;;V1.0;; 
 ;
 S IOP="HOME" D ^%ZIS W @IOF
 W !,?20,"This report requires a 132 column printer."
 W !,?20,"Please don't attempt to queue this report."
 W !,?20,"Thanks...................................."
 W !!
 S %DT="AER",%DT("A")="Please Enter Beginning DATE/TIME: "
 D ^%DT G:Y=-1 QUIT S BVAL=Y,SVAL=Y
 S M=$O(^%A1CB("START",Y-.0001))
 S N=$N(^%A1CB("START",M,""))
 D ^%ZIS I POP Q
 U IO W @IOF
 S LCNT=2 D HEAD,LINE
 F I=0:0 S N=$O(^%A1CB(N)) Q:N=""  Q:N="START"  D NODE D:FLG=0 TIME S:FLG=0 LCNT=LCNT+1 D:LCNT>IOSL HEAD,LINE S:LCNT>IOSL LCNT=5
 D ^%ZISC
 K A1CBK,LCNT,FLG,X,Y,NODE,STIME,DEST,ZDUZ,INF,N,PTIME,BTIME,ETIME,ZTRTN,I,BFLG,%H
 Q
NODE ;PROCESS EACH NODE IN THE %A1CB GLOBAL
 S FLG=0
 S:'$D(^%A1CB(N)) A1CBK=1 Q:'$D(^%A1CB(N))  S NODE=^%A1CB(N),A1CBK=0
 I $P(NODE,"^",1)="" S FLG=1 D CLEAN Q
 ;PTIME=POST TIME FROM %ZTLOAD1
 S PTIME=$P(NODE,"^",5)
 ;BTIME=BEGINNING TIME OF ZTRTN FROM %ZTMS3
 S BTIME=$P(NODE,"^",3)
 ;ETIME=ENDING TIME OF ZTRTN FROM %ZTMS3
 S ETIME=$P(NODE,"^",4)
 ;ZTRTN=TASK NAME FROM %ZTSK
 S ZTRTN=$P(NODE,"^",1,2)
 ;STIME=SCHEDULE TIME FROM %ZTSK
 S STIME=$P(NODE,"^",6)
 ;DEST=DESCRIPTION TAKEN FROM ZTDESC IN TASKMAN
 S DEST=$P(NODE,"^",7)
 ;ZDUZ=USER REQUESTING REPORT
 S ZDUZ=$P(NODE,"^",8) I ZDUZ="" S ZDUZ=.5
 ;INF=NUMBER OF JOBS SCHEDULE PRIOR TO THIS JOB WHEN THIS JOB WAS SCHEDULED
 S INF=$P(NODE,"^",9)
 ;A1CBERR=JOB HAS FAILED AND WAS TRAPPED
 S A1CBERR=$P(NODE,"^",10)
 Q
TIME ;PROCESS TIME DATA
 U IO W !,ZTRTN
 S %H=STIME D YX^%DTC U IO W ?15,$E(X,4,5),"/",$E(X,6,7),"@",$P(Y,"@",2)
 S %H=PTIME D YX^%DTC U IO W ?31,$E(X,4,5),"/",$E(X,6,7),"@",$P(Y,"@",2)
 S BFLG=0 D EVAL
 I BFLG=1 W ?48,"IN START QUEUE",?64,"IN START QUEUE"
 I BFLG=0 S %H=BTIME D YX^%DTC U IO W ?48,$E(X,4,5),"/",$E(X,6,7),"@",$P(Y,"@",2)
 I BFLG=0,ETIME'="" S %H=ETIME D YX^%DTC U IO W ?64,$E(X,4,5),"/",$E(X,6,7),"@",$P(Y,"@",2)
 I BFLG=0,ETIME="" W ?64,"CHECK TASK"
 W ?80,$E(DEST,1,23),?105,$E($S($D(^VA(200,ZDUZ,0)):$E($P(^(0),"^",1),1,15),1:"No User Defined"),1,15),?122,$J(INF,2),?125,$J(N,7)
 Q
EVAL ;EVALUATE IF THE NODE IS GOOD OR BAD BASED ON IF ZTRTN DATA EXISTS
 I $L(BTIME)<1 S BFLG=1
 Q
HEAD ;HEADING FOR THE REPORT
 S LCNT=LCNT+3
 U IO W #,!," Label ",?16,"Scheduled",?32,"Posted To",?49," TaskMan",?65," TaskMan",?122,"TM"
 U IO W !,"  and  ",?16," To Run",?32," TaskMan",?49,"  Start",?65,"   End",?82,"  TaskMan",?110,"User",?122,"Jb"
 U IO W !,"Routine",?16,"Date/Time",?32,"Date/Time",?49,"Date/Time",?65,"Date/Time",?82,"Description",?110,"Name",?122,"Ad",?125,"  Task"
 Q
LINE ;PRINT A DASHED LINE THE LENGTH OF THE MARGIN OF THE DEVICE SELECTED
 S LCNT=LCNT+1
 W !
 F I=1:1:IOM W "-"
 Q
CLEAN ;CLEAN OUT %A1CB WHEN NODES EXISTS IN START BUT NO ZTRTN DATA
 S %H=$P(NODE,"^",3) 
 D YMD^%DTC
 S DATETIME=X_%
 K ^%A1CB(N)
 K ^%A1CB("START",DATETIME,N)
 Q
