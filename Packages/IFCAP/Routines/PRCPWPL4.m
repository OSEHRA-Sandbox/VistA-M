PRCPWPL4 ;WISC/RFJ-whse post issue book (post cont)                 ;13 Jan 94
 ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 Q
 ;
 ;
POST ;  post issue book
 S TOTLINES=0
 S (TOTLINES,TOTALINV,TOTALSAL)=0
 S LINEDA=0 F  S LINEDA=$O(^TMP($J,"PRCPWPLMPOST",LINEDA)) Q:'LINEDA  S QTYPOST=^(LINEDA) I QTYPOST D
 .   S IBDATA=$G(^PRCS(410,PRCPDA,"IT",LINEDA,0)) I IBDATA="" Q
 .   S ITEMDA=+$P(IBDATA,"^",5)
 .   S ITEMDATA=$G(^PRCP(445,PRCPINPT,1,ITEMDA,0)) I ITEMDATA="" Q
 .   ;  do not post qty < 0
 .   I QTYPOST>$P(ITEMDATA,"^",7) S QTYPOST=+$P(ITEMDATA,"^",7)
 .   I 'QTYPOST Q
 .   S UNITCOST=$P(ITEMDATA,"^",22) S:$P(ITEMDATA,"^",15)>UNITCOST UNITCOST=$P(ITEMDATA,"^",15) S:$P(IBDATA,"^",7)>UNITCOST UNITCOST=$P(IBDATA,"^",7)
 .   S TOTCOST=$J(QTYPOST*UNITCOST,0,2),INVCOST=$J(QTYPOST*$P(ITEMDATA,"^",22),0,2)
 .   S TOTALSAL=TOTALSAL+TOTCOST,TOTALINV=TOTALINV+INVCOST
 .   S TOTLINES=TOTLINES+1
 .   ;
 .   ;  *** whse ***
 .   S $P(^PRCS(410,PRCPDA,"IT",LINEDA,0),"^",12)=$P(^PRCS(410,PRCPDA,"IT",LINEDA,0),"^",12)+QTYPOST
 .   S $P(^PRCS(410,PRCPDA,"IT",LINEDA,0),"^",7)=UNITCOST
 .   ;  update totals posted
 .   S %=$G(^PRCS(410,PRCPDA,"IT",LINEDA,445))
 .   S $P(%,"^",3)=$P(%,"^",3)+QTYPOST,$P(%,"^",4)=$P(%,"^",4)+INVCOST,$P(%,"^",5)=$P(%,"^",5)+TOTCOST
 .   S $P(^PRCS(410,PRCPDA,"IT",LINEDA,445),"^",3,5)=$P(%,"^",3,5)
 .   ;  update beginning balance
 .   I '$D(^PRCP(445.1,PRCPINPT,1,ITEMDA,1,$E(DT,1,5),0)) D BALANCE^PRCPUBAL(PRCPINPT,ITEMDA,$E(DT,1,5))
 .   ;  update whse invpt
 .   S $P(ITEMDATA,"^",7)=$P(ITEMDATA,"^",7)-QTYPOST
 .   S $P(ITEMDATA,"^",27)=$P(ITEMDATA,"^",27)-INVCOST
 .   ;  update average cost
 .   S $P(ITEMDATA,"^",22)=0,QUANTITY=$P(ITEMDATA,"^",7)+$P(ITEMDATA,"^",19)
 .   I QUANTITY>0 S $P(ITEMDATA,"^",22)=$J($P(ITEMDATA,"^",27)/QUANTITY,0,3) I $P(ITEMDATA,"^",22)'>0 S $P(ITEMDATA,"^",22)=0
 .   S ^PRCP(445,PRCPINPT,1,ITEMDA,0)=ITEMDATA
 .   D SETOUT^PRCPUDUE(PRCPINPT,ITEMDA,-QTYPOST)
 .   ;  usage
 .   D ADDUSAG^PRCPUSAG(PRCPINPT,ITEMDA,QTYPOST,INVCOST)
 .   ;  transaction register
 .   K PRCPWPL3,Y
 .   S PRCPWPL3("QTY")=-QTYPOST,PRCPWPL3("INVVAL")=-INVCOST,PRCPWPL3("SELVAL")=-TOTCOST,PRCPWPL3("2237PO")=PRCPIBNM,PRCPWPL3("REF")=PRCPORD,PRCPWPL3("OTHERPT")=PRCPPRIM
 .   I $G(CANTEEN) S PRCPWPL3("REASON")="0:2:ISSUE to CANTEEN"
 .   D ADDTRAN^PRCPUTRX(PRCPINPT,ITEMDA,"R",PRCPWORD,.PRCPWPL3)
 .   ;  set line number in transaction register
 .   I $D(^PRCP(445.2,+$G(Y),0)) S $P(^(0),"^",24)=LINEDA
 .   ;
 .   ;
 .   ;  *** primary ***
 .   I 'PRCPFPRI Q
 .   S $P(^PRCS(410,PRCPDA,"IT",LINEDA,0),"^",13)=$P(^PRCS(410,PRCPDA,"IT",LINEDA,0),"^",13)+QTYPOST
 .   S ITEMDATA=$G(^PRCP(445,PRCPPRIM,1,ITEMDA,0)) I ITEMDATA="" D  Q
 .   .   S COSTCNTR=$P($G(^PRCP(445,PRCPPRIM,0)),"^",7) I COSTCNTR D COSTCNTR^PRCPUCC(PRCPPRIM,PRCPINPT,COSTCNTR,TOTCOST)
 .   S QTYPOST=QTYPOST*$P($$GETVEN^PRCPUVEN(PRCPPRIM,ITEMDA,PRCPPVNO,1),"^",4)
 .   ;  update beginning balance
 .   I '$D(^PRCP(445.1,PRCPPRIM,1,ITEMDA,1,$E(DT,1,5),0)) D BALANCE^PRCPUBAL(PRCPPRIM,ITEMDA,$E(DT,1,5))
 .   ;  update primary invpt
 .   S $P(ITEMDATA,"^",7)=$P(ITEMDATA,"^",7)+QTYPOST
 .   S $P(ITEMDATA,"^",27)=$P(ITEMDATA,"^",27)+TOTCOST
 .   ;  update average cost
 .   S $P(ITEMDATA,"^",22)=0,QUANTITY=$P(ITEMDATA,"^",7)
 .   I QUANTITY S $P(ITEMDATA,"^",22)=$J($P(ITEMDATA,"^",27)/QUANTITY,0,3) I $P(ITEMDATA,"^",22)'>0 S $P(ITEMDATA,"^",22)=0
 .   ;  update last cost
 .   S $P(ITEMDATA,"^",15)=$J(TOTCOST/QTYPOST,0,3),$P(ITEMDATA,"^",3)=DT
 .   S ^PRCP(445,PRCPPRIM,1,ITEMDA,0)=ITEMDATA
 .   ;  remove due-in
 .   D OUTST^PRCPUTRA(PRCPPRIM,ITEMDA,PRCPDA,-QTYPOST)
 .   ;  receipt history
 .   D RECEIPTS^PRCPUSAG(PRCPPRIM,ITEMDA,QTYPOST)
 .   ;  distribution costs
 .   S COSTCNTR=$P(^PRCP(445,PRCPPRIM,0),"^",7) I COSTCNTR D COSTCNTR^PRCPUCC(PRCPPRIM,PRCPINPT,COSTCNTR,TOTCOST)
 .   ;  drug accountability
 .   I $G(DRUGACCT) S %=+$P(ITEMDATA,"^",29) S:'% %=1 D EN^PSAGIP(PRCPPRIM,ITEMDA,QTYPOST*%,PRCPDA,PRCPIBNM,"RC"_PRCPPORD,TOTCOST)
 .   ;  transaction register
 .   I PRCPPORD D
 .   .   K PRCPWPL3
 .   .   S PRCPWPL3("QTY")=QTYPOST,(PRCPWPL3("INVVAL"),PRCPWPL3("SELVAL"))=TOTCOST,PRCPWPL3("2237PO")=PRCPIBNM,PRCPWPL3("REF")=PRCPORD,PRCPWPL3("OTHERPT")=PRCPINPT
 .   .   D ADDTRAN^PRCPUTRX(PRCPPRIM,ITEMDA,"RC",PRCPPORD,.PRCPWPL3)
 ;
 D ENDPOST^PRCPWPL5
 Q
