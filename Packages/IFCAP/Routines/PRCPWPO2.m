PRCPWPO2 ;WISC/RFJ-check on items for errors ;17 Jul 91
 ;;4.0;IFCAP;;9/23/93
 Q
 ;
 ;
GETITEMS ;     |-> exception or selection of items
 N %,DA,DIC,LINEDA,PRCPFINL,PRCPITEM,PRCPLIDA,PRCPOUT,PRCPQOH,PRCPQTY,PRCPQTYD,PRCPX,X,Y
 I PRCPOPT=1 G START ;all items selected
 ;
ITEM S PRCPFLAG=0,DIC("W")="S PRCPNODE=12 D DICW^PRCPWPO1 K PRCPNODE",DIC="^PRCS(410,"_PRCPDA_",""IT"",",DA(1)=PRCPDA,DIC(0)="QEAMZ",DIC("A")="Select LINE ITEM Number: " W !! D ^DIC K DIC,DA I +Y<0 G START
 S PRCPLIDA=+Y,PRCPX=Y(0),PRCPITEM=$P(PRCPX,"^",5)
 I '$D(^PRCP(445,PRCP("I"),1,PRCPITEM,0)) W !,"THIS ITEM IS NOT IN THE WAREHOUSE INVENTORY AND WILL NOT BE POSTED." S ^TMP($J,"NOPOST",PRCPLIDA)="" G ITEM
 I $D(^TMP($J,"POST",PRCPLIDA)) S ^TMP($J,"ONHAND",PRCPITEM)=^TMP($J,"ONHAND",PRCPITEM)+^TMP($J,"POST",PRCPLIDA) K ^TMP($J,"POST",PRCPLIDA)
 S (PRCPOUT,PRCPQTY)=$P(PRCPX,"^",2)-$P(PRCPX,"^",12) S:PRCPQTY<0 (PRCPQTY,PRCPOUT)=0 S PRCPQOH=^TMP($J,"ONHAND",PRCPITEM) S:PRCPQTY>PRCPQOH PRCPQTY=PRCPQOH D QTYD^PRCPWPOQ I PRCPFLAG G ITEM
 S ^TMP($J,"POST",PRCPLIDA)=PRCPQTYD,^TMP($J,"ONHAND",PRCPITEM)=^TMP($J,"ONHAND",PRCPITEM)-PRCPQTYD G ITEM
 ;
 ;
START ;     |-> loop items for errors
 W !!,"Please wait while I check the quantities and units of the items to distribute..."
 I PRCPOPT'=1 K ^TMP($J,"ONHAND") D RETRIEVE
 S (LINEDA,PRCPFLAG)=0 F  S LINEDA=$O(^PRCS(410,PRCPDA,"IT",LINEDA)) Q:'LINEDA  S PRCPX=$G(^(LINEDA,0)) I PRCPX'="",'$D(^TMP($J,"NOPOST",LINEDA)) D CHECK
 W !,"Finished Checking Items." S PRCPREPT(1)="BEFORE POSTING" I PRCPREPT=2!(PRCPREPT=4) D:'$D(IO("Q"))&(IO=IO(0)) R^PRCPU4 D QUE^PRCPWPOR
 I $O(^TMP($J,"POST",0))="" S PRCPREPT=1 W !,"***** NO INVENTORY ITEMS TO POST *****" D FINALASK^PRCPWPO4 D:PRCPFINL CHECKFIN^PRCPWPO4 Q  ;set prcprept=1 to prevent printing the report
 S XP="READY TO START POSTING",XH="ENTER 'YES' TO START POSTING, 'NO' OR '^' TO EXIT.",%=1 W ! D YN^PRCPU4 I %=1 G POST^PRCPWPO3
 Q
 ;
 ;
CHECK I PRCPOPT=3,'$D(^TMP($J,"POST",LINEDA)) Q
 Q:$G(^TMP($J,"POST",LINEDA))=0  S PRCPITEM=+$P(PRCPX,"^",5),(PRCPQTY,PRCPOUT)=$P(PRCPX,"^",2)-$P(PRCPX,"^",12) S:PRCPQTY<0 (PRCPQTY,PRCPOUT)=0
 S:$P(PRCPX,"^",14)'="" PRCPOUT=0 I PRCPOUT=0 S ^TMP($J,"POST",LINEDA)=0 Q
 W !,"LN ",LINEDA,?7,$E($$NSN^PRCPUX1(PRCPITEM),1,20)," (#",PRCPITEM,")",?35,"QTY.ORD: ",$P(PRCPX,"^",2),?50,"QTY.OUT: ",PRCPOUT
 I '$D(^PRCP(445,PRCP("I"),1,PRCPITEM,0)) W !?5,"*** ITEM NOT FOUND IN WAREHOUSE INVENTORY.  ITEM WILL NOT BE POSTED. ***" K ^TMP($J,"POST",LINEDA) Q
 I '$D(^PRC(441,PRCPITEM,0)) W !?5,"*** ITEM NOT FOUND IN ITEM MASTER FILE.  ITEM WILL NOT BE POSTED. ***" K ^TMP($J,"POST",LINEDA) Q
 I PRCPOPT'=3,'$D(^TMP($J,"POST",LINEDA)) S ^TMP($J,"POST",LINEDA)=PRCPOUT
 ;
QTY S (PRCPQTY,PRCPQTYD)=^TMP($J,"POST",LINEDA),PRCPQOH=^TMP($J,"ONHAND",PRCPITEM) I PRCPQTY>PRCPQOH S PRCPLIDA=LINEDA D  S PRCPFLAG=0 D QTYD^PRCPWPOQ S:'PRCPFLAG ^TMP($J,"POST",PRCPLIDA)=PRCPQTYD G QTY
 .   W !,"*** ERROR: QUANTITY TO POST (",PRCPQTY,") IS GREATER THAN QUANTITY ON HAND (",PRCPQOH,")" S PRCPQTY=PRCPQOH
 S ^TMP($J,"ONHAND",PRCPITEM)=^TMP($J,"ONHAND",PRCPITEM)-PRCPQTYD W ?65,"POST: ",+$G(^TMP($J,"POST",LINEDA)) Q
 ;
 ;
RETRIEVE ;     |-> retrieve current on-hand quantities
 N %,X,Y
 W !!,"Retrieving current on hand quantities..."
 F %=0:0 S %=$O(^PRCS(410,PRCPDA,"IT",%)) Q:'%  S Y=+$P($G(^(%,0)),"^",5),X=$G(^PRCP(445,PRCP("I"),1,Y,0)),^TMP($J,"ONHAND",Y)=$P(X,"^",7)
 Q
