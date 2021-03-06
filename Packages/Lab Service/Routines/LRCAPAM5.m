LRCAPAM5 ;DALISC/FHS - RCS 14-4 REPORT PART 1
 ;;5.2;LAB SERVICE;;Sep 27, 1994
EN ;
INST ;
 K LRDA,LRRPTM S LRDA(1)=$$INSN^LRU I 'LRDA(1) W !!?10,"I am sorry,  there is no primary institution defined in ^XMB(1,1,""XUS"") ",!," Aborted " G EXIT
DIV ;
 K DIC
 S DIC("A")="Select Division: "
 S DIC("B")=$P($G(^DIC(4,+DUZ(2),0)),U)
 S DIC=4,DIC(0)="AQENMZ"
 D ^DIC G:Y<1 EXIT S LRDA=Y
MONTHS ;
 K DA,DIC
 S DA(1)=LRDA(1),DA=+LRDA,DIC(0)="AQEN"
 S DIC="^LRO(67.9,"_DA(1)_",1,"_DA_",1,"
 S DIC("A")="Select Month: "
 F  D ^DIC Q:Y<1  S LRRPTM(Y)=""
 I '$O(LRRPTM(0)) W !!?5,"Nothing Selected " G EXIT
DATTYP ;
 K DIR
 S DIR(0)="S^1:All workload;2:LMIP reportable workload;3:Non-LMIP workload"
 S DIR("A")="Enter the number for the workload data to report"
 S DIR("B")=1
 S DIR("?")="    reportable for LMIP."
 S DIR("?",1)="1 - will include all workload data in the file, period."
 S DIR("?",2)=" "
 S DIR("?",3)="2 - will include only workload which is associated with a"
 S DIR("?",4)="    WKLD code that is marked as reportable for LMIP uses."
 S DIR("?",5)=" "
 S DIR("?",6)="3 - will include any workload which is not marked as"
 D ^DIR G:($D(DTOUT))!($D(DUOUT)) EXIT
 S LRDTYP=Y,LRHD0=$S(LRDTYP=1:"ALL WORKLOAD DATA FOR: ",LRDTYP=2:"LMIP WORKLOAD DATA FOR: ",1:"Non-LMIP WORKLOAD DATA FOR: ")
REPTYP ;
 K DIR S DIR(0)="S^1:CDR report"
 S:LRDTYP=2 DIR(0)=DIR(0)_";2:LMIP report;3:CDR and LMIP reports"
 S DIR("A")="Enter the number for the report(s) you want printed"
 S DIR("B")=1
 D ^DIR G:($D(DTOUT))!($D(DUOUT)) EXIT S LRRTYP=Y
DETSUM ;
 I (LRRTYP=1)!(LRRTYP=3) D  G:$G(LREND) EXIT
 .W !!,"CDR format selection: "
 .K DIR,X,Y S DIR(0)="S^1:Detailed report;2:Summary report"
 .D ^DIR
 .I ($D(DTOUT))!($D(DUOUT)) S LREND=1 Q
 .S LRRPT=+X
DEVICE ;
 S %ZIS="Q" D ^%ZIS G:POP EXIT I $D(IO("Q")) G ZTLOAD
 D WAIT^DICD
QUE ;
 U IO K ^TMP($J,"RCS14-4"),^TMP($J,"LMIP")
 S (LRERR,LRMT)="" S:$D(ZTQUEUED) ZTREQ="@"
 F  S LRMT=$O(LRRPTM(LRMT)) Q:LRMT=""  S (LRCAP,LRTSTOT)=0  D
 .D INITSUM^LRCAPAM7
 .S:'$D(^TMP($J,"RCS14-4",$P(LRMT,U,2),0)) ^(0)=0 S LRTOT1=^(0)
 .F  S LRCAP=$O(^LRO(67.9,+LRDA(1),1,+LRDA,1,+LRMT,1,LRCAP)) Q:LRCAP<1  I $D(^(LRCAP,0))#2 S LRTREAT=0 D  S ^TMP($J,"RCS14-4",$P(LRMT,U,2),0)=LRTOT1
 ..S LRN=$G(^LRO(67.9,+LRDA(1),1,+LRDA,1,+LRMT,1,LRCAP,0)),LRN2=+$G(^(2))
 ..I '$O(^LRO(67.9,+LRDA(1),1,+LRDA,1,+LRMT,1,LRCAP,1,0)) S LRCAPIFN=+$O(^LAM("C",$P(LRN,U)_" ",0)) D:LRCAPIFN BMPSUM^LRCAPAM7 Q
 ..S LRCC=$P(LRN,U) S LRCCN=$E($$WKLDNAME^LRCAPU(LRCC),1,40)
 ..S:LRCCN["*ERR" LRERR=LRERR+1
 ..Q:((LRDTYP=2)&('LRN2))!((LRDTYP=3)&(LRN2))
 ..D BMPSUM^LRCAPAM7
 ..S:'$D(^TMP($J,"RCS14-4",$P(LRMT,U,2),1,LRCCN,0))#2 ^(0)=$P(LRN,U,1,8)_U_$P(LRN,U,12) F I=2,3,4 S N=$P(LRN,U,I) I N S $P(LRTOT1,U,I)=($P(LRTOT1,U,I)+N)
 ..S LRTREAT=0 F  S LRTREAT=$O(^LRO(67.9,+LRDA(1),1,+LRDA,1,+LRMT,1,LRCAP,1,LRTREAT)) Q:LRTREAT<1  S LRN1=^(LRTREAT,0) D T1
 D ^LRCAPAM6
EXIT ;
 D ^%ZISC
 K %ZIS,DA,DIC,I,LRBS,LRCAP,LRCC,LRCCN,LRDA,LRLINE,LR,LRMT,LRN,LRRPTM
 K LRSV,LRTREAT,LRTRN,N,NODE,LRTOT1,Y,LRCAPT,LRCAPTOT,LRTRN,N0,LRGTOT,LRTOT,LRN1
 K LRCAPNAM,LRCAPNUM,LRPG,LRTRE1,LRTRE1T,LRTRET,LRBS,LRCAPIFN,LRMTP,LRTRE
 K LRCDR,LRDTYP,DIR,DUOUT,DTOUT,DIRUT,ZTRTN,ZTSAVE,ZTIO,ZTDESC,LRFIRST
 K LRCAPFLG,LRN2,LRRTYP,LRHD0,LRTSTOT,LRCAPAM5
 K ^TMP($J,"RCS14-4"),^TMP($J,"LMIP"),LRERR
 Q
T1 ;
 D LKUP S:'$D(^TMP($J,"RCS14-4",$P(LRMT,U,2),1,LRCCN,LRTRN)) ^(LRTRN)=0 S ^(LRTRN)=(^(LRTRN)+$P(LRN1,U,2))
 S:'$D(^TMP($J,"RCS14-4",$P(LRMT,U,2),LRTRN)) ^(LRTRN)=0 S ^(LRTRN)=(^(LRTRN)+$P(LRN1,U,2))
 S LRTSTOT=LRTSTOT+$P(LRN1,U,2)
 Q
LKUP ;
 S NODE=$G(^DIC(42.4,+LRN1,0)),LRCDR=$S($P(NODE,U,6):$P(NODE,U,6),$P(LRN1,U)="XY ":2100,1:2000)
 S LRTRN="[ "_LRCDR_" ] "_$S($L($P(NODE,U)):$P(NODE,U),LRCDR=2100:"BLOOD BANK",1:"AMBULATORY CARE")
 S LRSV=$S($L($P(NODE,U,3)):$P(NODE,U,3),1:LRTRN)
 I $L(LRSV)<4 S LRSV=$S(LRSV="M":"MEDICINE",LRSV="S":"SURGERY",LRSV="P":"PSYCHIATRY",LRSV="NH":"NHCU",LRSV="NE":"NEUROLOGY",LRSV="I":"INTERMEDIATE MED",LRSV="R":"REHAB MEDICINE",1:LRSV)
 I $L(LRSV)<4 S LRSB=$S(LRSV="SCI":"SPINAL CORD INJURY",LRSV="D":"DOMICILIARY",LRSV="B":"BLIND REHAB",1:"RESPITE CARE")
 S LRBS=$S($L($P(NODE,U,5)):$P(NODE,U,5),1:LRTRN)
 S:'$D(^TMP($J,"RCS14-4",$P(LRMT,U,2),3))#2 ^(3)=0 S ^(3)=(^(3)+$P(LRN1,U,2))
 S:'$D(^TMP($J,"RCS14-4",$P(LRMT,U,2),3,LRSV)) ^(LRSV)=0 S ^(LRSV)=(^(LRSV)+$P(LRN1,U,2))
 S:'$D(^TMP($J,"RCS14-4",$P(LRMT,U,2),5))#2 ^(5)=0 S ^(5)=(^(5)+$P(LRN1,U,2))
 S:'$D(^TMP($J,"RCS14-4",$P(LRMT,U,2),5,LRBS)) ^(LRBS)=0 S ^(LRBS)=(^(LRBS)+$P(LRN1,U,2))
 Q
ZTLOAD ;
 S ZTIO=ION,ZTRTN="QUE^LRCAPAM5",ZTDESC="LR RCS/CDR REPORT"
 S ZTSAVE("LR*")="",ZTSAVE("LRDA*")=""
 D ^%ZTLOAD K ZTSK G EXIT
