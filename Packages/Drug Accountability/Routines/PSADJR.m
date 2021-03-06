PSADJR ;BIR/LTL,JMB-Balance Adjustments History ;8/21/97
 ;;3.0; DRUG ACCOUNTABILITY/INVENTORY INTERFACE;**15**; 10/24/97
 ;This routine reviews adjustment transactions for drugs.
 ;
 ;References to ^PSDRUG( are covered by IA #2095
 ;
LOOK ;Get locations to display
 S (PSACNT,PSAOUT)=0 D ^PSAUTL3 G:PSAOUT EXIT
 S PSACNT=0,PSACHK=$O(PSALOC(""))
 I PSACHK="",'PSALOC W !,"There are no active pharmacy locations." G EXIT
 I '$O(^PSD(58.8,PSALOC,1,0)) W !!,"There are no drugs in ",PSALOCN S PSAOUT=1 G EXIT
 S DIC="^PSD(58.8,PSALOC,1,",DIC(0)="AEMQZ",DIC("A")="Select drug for history: ",DA(1)=PSALOC,DIC("W")="I $S($P($G(^(0)),U,14):$P($G(^(0)),U,14)'>DT,1:0) W $C(7),""   ***INACTIVE ***""" W !
 D ^DIC K DIC S PSA=+Y I PSA<0 S PSAOUT=1 G EXIT
 I '$O(^PSD(58.81,"F",+Y,"")) W !!,"There have been no adjustments for this drug.",!! S PSAOUT=1 G EXIT
 W ! S DIR(0)="D:AEP",DIR("A")="How far back in time do you want to go: ",DIR("B")="T-6M" D ^DIR K DIR I $D(DIRUT) S PSAOUT=1 G EXIT
 S PSAT=Y
DEV ;asks device and queueing info
 K IO("Q") N %ZIS,IOP,POP S %ZIS="Q" D ^%ZIS I POP W !,"NO DEVICE SELECTED OR OUTPUT PRINTED!" S PSAOUT=1 G EXIT
 I $D(IO("Q")) N ZTDESC,ZTIO,ZTRTN,ZTSAVE,ZTDTH,ZTSK S ZTRTN="START^PSADJR",ZTDESC="Drug Acct. - Drug adjustment review",ZTSAVE("PSA*")="" D ^%ZTLOAD,HOME^%ZIS S PSAOUT=1 G EXIT
START ;compiles and prints output
 D SITES^PSAUTL1 S PSALOCN=$P(^PSD(58.8,PSALOC,0),"^")_PSACOMB
 S PSADRG=$P($G(^PSDRUG(PSA,0)),"^"),PSATAB=(80-$L(PSADRG))/2
 N %DT,PSASLN,PSAPG,PSAOUT,PSARPDT S (PSAPG,PSAOUT)=0,$P(PSASLN,"-",81)="",Y=DT D DD^%DT S PSARPDT=Y,PSAR=0 D HEADER
LOOP F  S PSAR=+$O(^PSD(58.81,"F",PSA,PSAR)) Q:'PSAR  I $P($G(^PSD(58.81,PSAR,0)),"^",4)'<PSAT,$P($G(^(0)),"^",2)=9!($P($G(^(0)),"^",2)=24),$P($G(^(0)),"^",3)=PSALOC D  Q:PSAOUT
 .I $P($G(^PSD(58.81,PSAR,0)),"^",2)=9,$Y+4>IOSL D HEADER Q:PSAOUT
 .I $P($G(^PSD(58.81,PSAR,0)),"^",2)=24,$Y+6>IOSL D HEADER Q:PSAOUT
 .S Y=$P($G(^PSD(58.81,PSAR,0)),"^",4) X ^DD("DD") W !,$E(Y,1,17)
 .W:$P($G(^PSD(58.81,PSAR,0)),"^",2)=9 ?22 W:$P($G(^PSD(58.81,PSAR,0)),"^",2)=24 ?30 W $J($P($G(^PSD(58.81,PSAR,0)),"^",6),5,0)
 .W ?37,$E($P($G(^VA(200,+$P($G(^PSD(58.81,PSAR,0)),"^",7),0)),"^"),1,20)
 .I $P($G(^PSD(58.81,PSAR,0)),"^",2)=9 W !?37,$P($G(^PSD(58.81,PSAR,0)),"^",16),! Q
 .S PSATRANL=$P($G(^PSD(58.81,+$P($G(^PSD(58.81,PSAR,0)),"^",17),0)),"^",3),PSAHOLD=PSALOC,PSAHOLDN=PSALOCN,PSALOC=PSATRANL
 .I PSALOC="" W !?37,"TRANSFER DATA MISSING",! S PSALOC=PSAHOLD,PSALOCN=PSAHOLDN Q
 .D SITES^PSAUTL1 S PSALOCN=$P(^PSD(58.8,PSALOC,0),"^")_PSACOMB
 .I $P($G(^PSD(58.81,PSAR,0)),"^",6)<0 W:$L(PSALOCN)<42 !?37,"TO "_PSALOCN,! I $L(PSALOCN)>43 S PSATF="T" W !?37,"TO " D TRAN
 .I $P($G(^PSD(58.81,PSAR,0)),"^",6)>0 W:$L(PSALOCN)<42 !?37,"FROM "_PSALOCN,! I $L(PSALOCN)>43 S PSATF="F" W !?37,"FROM " D TRAN
 .S PSALOC=PSAHOLD,PSALOCN=PSAHOLDN
EXIT W:$E(IOST)'="C" @IOF
 I $E(IOST,1,2)="C-",'$G(PSAOUT) D
 .S PSASS=21-$Y F PSAKK=1:1:PSASS W !
 .S DIR(0)="EA",DIR("A")="END OF REPORT!  Press <RET> to return to the menu." D ^DIR W @IOF
 D ^%ZISC S:$D(ZTQUEUED) ZTREQ="@" K IO("Q")
 K DA,DIC,DIR,DIRUT,DTOUT,DUOUT,PSA,PSACHK,PSACNT,PSACOMB,PSADRG,PSAHOLD,PSAHOLDN,PSAKK,PSALOC,PSALOCA,PSALOCN,PSAOUT,PSAR,PSASEL,PSASS,PSAT,PSATAB,PSATF,PSATRAN,PSATRANL,X,Y
 Q
HEADER ;prints header info
 I $E(IOST,1,2)="C-",PSAPG D  Q:PSAOUT
 .S PSASS=21-$Y F PSAKK=1:1:PSASS W !
 .S DIR(0)="E" D ^DIR K DIR I 'Y S PSAOUT=1 Q
 I $$S^%ZTLOAD W !!,"Task #",$G(ZTSK),", ",$G(ZTDESC)," was stopped by ",$P($G(^VA(200,+$G(DUZ),0)),"^"),"." S PSAOUT=1
 W:$Y @IOF S PSAPG=PSAPG+1
 W:$E(IOST)'="C" !,PSARPDT,?20,"DRUG ACCOUNTABILITY/INVENTORY INTERFACE"
 W !?22,"HISTORY OF ADJUSTMENTS AND TRANSFERS",?70,"PAGE: ",PSAPG,!
 W:$L(PSALOCN)>76 ?2,$P(PSALOCN,"(IP)",1)_"(IP)",!?19,$P(PSALOCN,"(IP)",2),! W:$L(PSALOCN)<77 ?((80-$L(PSALOCN))/2),PSALOCN,!?PSATAB,PSADRG,!
 I $P($G(^PSD(58.8,PSALOC,1,PSA,0)),"^",14),$P($G(^(0)),"^",14)'>DT W ?20,"** INACTIVE DRUG IN PHARMACY LOCATION **",!
 W !,"DATE",?22,"ADJUST",?30,"TRANS",?37,"TRANSACTOR & REASON",!,PSASLN
 Q
TRAN ;
 I $E(PSALOCN)="I" W $P($P(PSALOCN,":",2),"(IP)"),!
 I $E(PSALOCN)="O" W $P($P(PSALOCN,":",2),"(OP)"),!
 I $E(PSALOCN)="C" W "COMBINED:"_$P($P(PSALOCN,":",2),"(IP)",1)_"(IP)",! W:PSATF="T" ?49 W:PSATF="F" ?51 W $P($P(PSALOCN,":",2),"(IP)",2),!
 Q
