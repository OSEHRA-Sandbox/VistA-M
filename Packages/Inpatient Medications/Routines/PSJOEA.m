PSJOEA ;BIR/MLM-INPATIENT ORDER ENTRY ;23 Jun 98 / 1:46 PM
 ;;5.0; INPATIENT MEDICATIONS ;**110,127,133,167,171**;16 DEC 97
 ;
 ; Reference to ^PS(55 is supported by DBIA #2191.
 ; Reference to EN^VALM is supported by DBIA #10118.
 ; Reference to ^PSSLOCK is supported by DBIA #2789
 ; Reference to ^DPT is supported by DBIA #10035.
 ; Reference to SDIMO^SDAMA203 is supported by DBIA #4133.
 ;
LOCK(DFN,PSJORD) ; Check to see if the order is already locked
 N Q S Q=0
 I PSJORD=+PSJORD N PSJO S PSJO=0 F  S PSJO=$O(^PS(53.1,"ACX",PSJORD,PSJO)) Q:'PSJO  S Q=$$LS^PSSLOCK(DFN,PSJO_"P") I 'Q Q
 I Q Q 1
 Q 0
 ;
SELECT ;
 N PSJCLIN,O
 Q:PSJORD=""!($G(Y)<0)  Q:('$$LOCK^PSJOEA(PSGP,PSJORD))
 N PSJO S PSJO=0 F  S PSJO=$O(^PS(53.1,"ACX",PSJORD,PSJO)) Q:'PSJO  D
 .S PSGORD="" S ON=PSJO_"P"
 .D DISACTIO(PSGP,PSJO_"P",$G(PSJPNV)) S:$G(PSJO)["V" O=ON
 I $D(^TMP("PSJCOM",$J)) D CHK^PSJOEA1
 S:'$G(PSGP) PSGP=$G(DFN)
 N PSJO S PSJO=0 F  S PSJO=$O(^PS(53.1,"ACX",PSJORD,PSJO)) Q:'PSJO  D
 .D UNL^PSSLOCK(PSGP,PSJO_"P") Q:$G(Y)<0
 D DONE
 Q
 ;
DISACTIO(DFN,PSJORD,PSJPNV)       ; Display UD order and allow actions.
 ; DFN    - Patient IEN
 ; PSJORD - Order #_location Code (P:53.1,V:55.01,U:55.06)
 ; PSJPNV - Invoked from Pending/NV option; (gets different hidden menu)
 N PSGP,PSJIVFLG,PSGSDX,PSGFDX,PSJXX1,ON55,PSJAPPT
 Q:PSJORD'["P"
 Q:$G(PSJCLIN)=-2
 S PSGP=DFN D ENIV^PSJAC
 D GETUD^PSJLMGUD(DFN,PSJORD)
 S PSGOEAV=$P(PSJSYSP0,"^",9)&PSJSYSU
 S:$G(PSJTUD) PSGPD=$G(PSJCOI),PSGPDN=$$OINAME^PSJLMUTL(+PSGPD)
 K PSGOENG I '$D(PSGPRF) D  Q:$G(PSGOENG)
 . I PSJORD["P" L +^PS(53.1,+PSJORD):1 E  S PSGOENG=1
 . I $G(PSGOENG) W !,"This order is being edited by another terminal.",! S PSGOENG=1 K DIR S DIR(0)="E" D ^DIR K DIR Q
 S PSGACT=$$ENACTION^PSGOE1(PSGP,PSJORD)
 I PSJORD["P" S PSJXX1=$G(^PS(53.1,+PSJORD,0)) I PSGP'=$P(PSJXX1,U,15)!(DFN'=$P(PSJXX1,U,15)) Q  ;L -^PS(53.1,+PSJORD) Q
 I PSJORD["P" D  S PSJXX1=$P($G(^PS(53.1,+PSJORD,0)),U,9) I $S($G(PSJIVFLG):1,$G(Y)<0:1,"PADE"[PSJXX1:1,1:0) Q  ;L -^PS(53.1,+PSJORD) Q
 .I $P(PSJXX1,U,9)="N",($P(PSJXX1,U,4)'="U") D  Q
 .. S P("PON")=PSJORD,PSIVFLG=1
 .. D GT531^PSIVORFA(+PSGP,PSJORD),VF^PSIVORC2
 .I $P(PSJXX1,U,9)="P" D  Q
 ..S:$G(PSJTUD) $P(PSJXX1,U,4)="U"
 ..N VAIP S PSJCLIN=$G(^PS(53.1,+PSJORD,"DSS")),PSJAPPT=$P(PSJCLIN,"^",2),PSJCLIN=$P(PSJCLIN,"^")
 ..I $P(PSJXX1,U,4)="U",(+PSJPDD) D  Q:(PSJCLIN=-2)
 ...I $$PATCH^XPDUTL("SD*5.3*285"),($$SDIMO^SDAMA203(PSJCLIN,DFN)>-1) Q
 ...W !!,"Cannot process an Out-patient Unit Dose order for ",$P($G(^DPT(+PSGP,0)),U) D PAUSE^VALM1 S PSJIVFLG=1,PSJCLIN=-2
 ..NEW PSGRSD,PSGRSDN,PSGRFD,PSGRFDN
 ..D REQDT^PSJLIVMD(PSJORD)
 ..I $P(PSJXX1,U,4)="U",($G(PSGSCH)="") W !!,"Invalid schedule, can't finish this order" D PAUSE^VALM1 Q
 ..I $P(PSJXX1,U,4)="U" N PSJLM S PSJLM=1,PSGORD=PSJORD D START^PSGOEF,ENSFE^PSGOEE0(PSGP,PSGORD),@$S($G(PSJTUD):"FINISH^PSGOEF",1:"EN^VALM(""PSJ LM PENDING EDIT"")") Q
 ..I $P(PSJXX1,U,4)'="U",PSGP=$P(PSJXX1,U,15),DFN=$P(PSJXX1,U,15) S PSJLYN=PSJORD D EN^PSJLIFN S PSJIVFLG=1 K PSJLYN,PSJMAI
 I $G(PSIVFLG) K PSIVFLG Q
 S PSGACT=$$ENACTION^PSGOE1(PSGP,PSJORD),PSGOEEF=0 D GETUD^PSJLMGUD(PSGP,PSJORD),ENSFE^PSGOEE0(PSGP,PSJORD),EN^VALM("PSJ LM UD ACTION")
 ;I PSJORD["P" L -^PS(53.1,+PSJORD)
 ;I PSJORD["U" L -^PS(55,PSGP,5,+PSJORD)
 ;Send SN to CPRS if autoverify OFF and Order Set Entry and no 21st piece
 I $D(PSGOES),'PSGOEAV,$D(PSGORD),PSGORD["P",$P($G(^PS(53.1,+PSGORD,0)),"^",21)']"" D ORSET^PSGOETO1
 ;D UNL^PSSLOCK(PSGP,PSJORD)
 Q
 ;
ACTLOG(PSGORDP,DFN,PSGORD)  ;Store 53.1 activity log in local array to be moved to 55
 ;PSGORDP: IEN from 53.1
 ;PSGORD : IEN from 55
 NEW PSGX,PSGXDA,PSGAL531,Q,QQ
 F PSGX=0:0 S PSGX=$O(^PS(53.1,+PSGORDP,"A",PSGX)) Q:'PSGX  D
 . S PSGAL531=$G(^PS(53.1,+PSGORDP,"A",PSGX,0))
 . S QQ=$G(^PS(55,DFN,5,+PSGORD,9,0)) S:QQ="" QQ="^55.09D" F Q=$P(QQ,U,3)+1:1 I '$D(^(Q)) S $P(QQ,U,3,4)=Q_U_Q,^(0)=QQ,PSGXDA=Q Q
 . S ^PS(55,DFN,5,+PSGORD,9,PSGXDA,0)=PSGAL531
 Q
 ;
UD ;
 N DA,DR,DIE,PSJCMPDA D ENGNA^PSGOETO S $P(^TMP("PSJCOM",$J,PSJO,0),"^",26)=DA_"U",$P(^TMP("PSJCOM2",$J,PSJO,0),"^")=DA,$P(^(0),"^",18)=DA S PSJCMPDA=DA
 M ^PS(55,PSGP,5,DA)=^TMP("PSJCOM2",$J,+PSJO) M ^PS(53.1,+PSJO)=^TMP("PSJCOM",$J,+PSJO) D EN1^PSJHL2(PSGP,"OD",+PSJO_"P") S PSJNOO=$P(^TMP("PSJCOM2",$J,+PSJO,.2),U,3) D EN1^PSJHL2(PSGP,"SN",+PSJCMPDA_"U")
 N PSGPDRG,PSGST,PSGNESD,PSGNEFD,ND2
 S PSGPDRG=$P($G(^PS(55,PSGP,5,PSJCMPDA,.2)),"^"),PSGST=$P($G(^PS(55,PSGP,5,PSJCMPDA,0)),"^",7)
 S ND2=$G(^PS(55,PSGP,5,PSJCMPDA,2)),PSGNESD=$P(ND2,"^",2),PSGNEFD=$P(ND2,"^",4) D CRA^PSGOETO
 K ^PS(53.1,"ACX",PSJORD,PSJO) K PSJPREX
 I $G(PSJCMPDA) D CMPLX2^PSJCOM1(PSGP,PSJORD,+PSJCMPDA_"U") I $G(PSGPXN) S PSJPREX=1
 Q
IV ; 
 K ON55 I $P($G(^PS(53.1,+PSJO,0)),"^",24)="R",$P($G(^PS(53.1,+PSJO,0)),"^",25) S ON55=$P(^PS(53.1,+PSJO,0),"^",25)
 I '$G(ON55) D NEW55^PSIVORFB
 S $P(^TMP("PSJCOM",$J,PSJO,0),"^",26)=ON55,$P(^TMP("PSJCOM2",$J,PSJO,0),"^")=+ON55
 S $P(^TMP("PSJCOM2",$J,PSJO,2),U,5)=PSJO_"P",$P(^TMP("PSJCOM",$J,PSJO,0),U,26)=ON55
 M ^PS(55,DFN,"IV",+ON55)=^TMP("PSJCOM2",$J,+PSJO) M ^PS(53.1,+PSJO)=^TMP("PSJCOM",$J,+PSJO) D EN1^PSJHL2(PSGP,"OD",+PSJO_"P") S P("NAT")=$P(^TMP("PSJCOM2",$J,+PSJO,.2),U,5) D EN1^PSJHL2(PSGP,"SN",ON55)
 K DA,DIK S DA(1)=DFN,DA=+ON55,DIK="^PS(55,"_DA(1)_",""IV"",",PSIVACT=1 D IX^DIK K DA,DIK
 K ^PS(53.1,"ACX",PSJORD,PSJO)
 Q
 ;
DONE ; Clean up
 K PSGPD,PSGPDN,PSGSCH,PSIVACT,PSJNOO
 Q
