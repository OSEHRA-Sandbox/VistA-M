PSDRDR1 ;BIR/BJW-Narc Disp/Rec (reprint VA FORM 10-2321) (cont'd) ; 3 Mar 98
 ;;3.0; CONTROLLED SUBSTANCES ;**8**;13 Feb 97
 ;**Y2K compliance** display 4 digit year on va forms
 ;;This routine modified on 2-9-95 for E3R# 3311.
START ;compile data
 K ^TMP("PSDRDR",$J)
 I $D(PSDA),$D(^PSD(58.81,PSDA,0)) D SET,^PSDRDR2 G END
 F PSD=PSDSD:0 S PSD=$O(^PSD(58.81,"AC",PSD)) Q:'PSD!(PSD>PSDED)  F PSDA=0:0 S PSDA=$O(^PSD(58.81,"AC",PSD,PSDA)) Q:'PSDA  I $D(^PSD(58.81,PSDA,0)),$D(NAOU(+$P(^PSD(58.81,PSDA,0),"^",18))),$P(^PSD(58.81,PSDA,0),"^",3)=+PSDS D SET
 D ^PSDRDR2
END K %,%I,%H,%ZIS,C,COMM,COPY,D,DA,DIC,DIR,DIROUT,DIRUT,DIWF,DIWL,DIWR,DRUG,DRUGN,DTOUT,DUOUT,EXP,EXPD,EXP1,FLAG,LN,LOOP,LOT,MFG,NAOU,NAOUN,NEWBAL,NODE,NUM,OK,ORD,ORDN
 K FNOTE,PG,PHARM,PHARMN,PSD,PSDA,PSDATE,PSDCPY,PSDDT,PSDED,PSDEV,PSDN,PSDNA,PSDOUT,PSDS,PSDSD,PSDSN,PSDST,PSDYR,QTY,REC,RECN,REQD,REQDT,RPDT,SEL,SUM,TEXT,X,Y,ZTDESC,ZTDTH,ZTIO,ZTRTN,ZTSAVE,ZTSK
 K ^TMP("PSDRDR",$J) D ^%ZISC S:$D(ZTQUEUED) ZTREQ="@"
 Q
SET ;sets data for printing
 Q:'$D(^PSD(58.81,PSDA,0))  S NODE=^PSD(58.81,PSDA,0),PSDN=+$P(NODE,"^",18)
 S PSDNA=$S($P($G(^PSD(58.8,PSDN,0)),"^")]"":$P(^(0),"^"),1:"ZZ/"_PSDN)
 S DRUG=+$P(NODE,"^",5),DRUGN=$S($P($G(^PSDRUG(DRUG,0)),"^")]"":$P(^(0),"^"),1:"ZZ/"_DRUG)
 S QTY=$P(NODE,"^",6) I $D(^PSD(58.81,PSDA,4)),+$P(^(4),"^",3) S QTY=$P(^(4),"^",3)
 S COMM=$S($D(^PSD(58.81,PSDA,2,0)):1,1:0),MFG=$P(NODE,"^",13),LOT=$P(NODE,"^",14),EXP=$P(NODE,"^",15),EXPD=""
 ;;The next 2 lines were inserted for E3R# 3311,to print a drug balance.
 S:$D(^PSD(58.81,PSDA,4)) NEWBAL=$P(^(4),"^",7)+$P(^(4),"^",4),FNOTE="*"
 S:'$D(^PSD(58.81,PSDA,4)) NEWBAL=$P(NODE,"^",10)-QTY,FNOTE=""
 I EXP S (EXP1,EXPD)=$$FMTE^XLFDT(EXP,"5D") S:'$P(EXP1,"/",2) EXPD=$P(EXP1,"/")_"/"_$P(EXP1,"/",3) S EXP=EXPD
 S NUM=$S($P(NODE,"^",17)]"":$P(NODE,"^",17),1:"UNKNOWN")
 S ORD=+$P($G(^PSD(58.81,PSDA,1)),"^",7),ORDN=$S($P($G(^VA(200,ORD,0)),"^")]"":$P(^(0),"^"),1:"UNKNOWN")
 S REQD=$P($G(^PSD(58.81,PSDA,1)),"^",6),REQDT="" I REQD S Y=REQD
 X ^DD("DD") S PSDYR=$P(Y,",",2),PSDYR=$E(PSDYR,1,4)
 S REQDT=$E(REQD,4,5)_"/"_$E(REQD,6,7)_"/"_PSDYR
 S PSDST=$P(NODE,"^",4),PSDDT="" I PSDST S Y=PSDST X ^DD("DD")
 S PSDYR=$P(Y,",",2),PSDYR=$E(PSDYR,1,4) S PSDDT=$E(PSDST,4,5)_"/"_$E(PSDST,6,7)_"/"_PSDYR
 S (REC,PHARM,RECN,PHARMN)="" I $D(^PSD(58.81,PSDA,1)),+$P(^PSD(58.81,PSDA,1),"^",4) S PHARM=$P(^(1),"^"),REC=$P(^(1),"^",3)
 I PHARM S PHARMN=$P($G(^VA(200,PHARM,0)),"^")
 I REC S RECN=$P($G(^VA(200,REC,0)),"^")
 ;;The next line modified for E3R# 3311,newbal and fnote added.
 S ^TMP("PSDRDR",$J,PSDNA,NUM)=DRUGN_"^"_QTY_FNOTE_"^"_PSDDT_"^"_REQDT_"^"_ORDN_"^"_MFG_"^"_LOT_"^"_EXPD_"^"_COMM_"^"_PSDA_"^"_PHARMN_"^"_RECN_"^"_NEWBAL_"^"_FNOTE
 Q
