RMPR21C ;PHX/JLT,RVD-CONTINUATION OF RMPR21B ;8/29/1994
 ;;3.0;PROSTHETICS;**12,28,41,62,129**;Feb 09, 1996;Build 2
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
 ;RVD patch #62 - PCE interface
 ;
R19 ;PASS RMPRA AS IEN OF 644, AND B2 AS ITEM MULTIPLE
 S:$D(RMPRCONT) $P(^RMPR(664,RMPRA,1,B2,0),U,14)=RMPRCONT
 S RMPRI=$P(^RMPR(664,RMPRA,1,B2,0),U,1),RMPRCT=$P(^(0),U,3),RMPRQT=$P(^(0),U,4),RMPRDES=$P(^(0),U,2)
 S RMPRIS=$P(^(0),U,5),SR=$S($D(RMPRSR(RMPRI)):RMPRSR(RMPRI),1:"")
 S RMPRT=$P(^RMPR(664,RMPRA,1,B2,0),U,9),RMPRR=$P(^(0),U,8),RMPRDIS=$P(^(0),U,10),RMPRS=$P(^(0),U,12),RMPRSC=$P(^(0),U,11),UOI=$P(^(0),U,5)
 S RMPRHCPC=$P(^RMPR(664,RMPRA,1,B2,0),U,16)
 S RMCPT=$P($G(^RMPR(664,RMPRA,1,B2,4)),U,2)
 K DD,DO S DIC="^RMPR(660,",DIC(0)="QL",X=DT,DLAYGO=660
 D FILE^DICN K DLAYGO
 S (RMPR660,DA)=+Y,^RMPR(660,RMPR660,0)=DT_U_RMPRDFN_U_DT_U_RMPRT_U_U_RMPRI_U_RMPRQT_U_UOI_U_RMPRV_U_RMPR("STA")_U_SR_U_U_RMPRF_U_RMPRS_U_U_$J(RMPRQT*RMPRCT,0,2)_"^^^^^^"
 ;HCPCS code
 S $P(^RMPR(660,RMPR660,0),U,22)=$P($G(^RMPR(661.1,RMPRHCPC,0)),U,4)
 S ^RMPR(660,RMPR660,"AMS")=RMPRG,^RMPR(660,RMPR660,"AM")=U_U_RMPRDIS_U_RMPRSC I $D(RMPRWO),RMPRWO S $P(^("AM"),U,2)=1,$P(^RMPR(660,RMPR660,"LB"),U,5)=RMPRWO
 S:$D(RMPRR) $P(^RMPR(660,RMPR660,0),U,18)=RMPRR
 S $P(^RMPR(660,RMPR660,0),U,27)=DUZ,^(1)=RMPRTRN_U_RMPRDES_"^^"_RMPRHCPC_"^^"_RMCPT
 S $P(^RMPR(664,RMPRA,1,B2,0),U,13)=RMPR660
 S (D1,RD)=0 F  S RD=$O(^RMPR(664,RMPRA,1,B2,1,RD)) Q:RD'>0  S ^RMPR(660,RMPR660,"DES",RD,0)=^RMPR(664,RMPRA,1,B2,1,RD,0),D1=RD
 S ^RMPR(660,RMPR660,"DES",0)="^660.028^"_D1_U_D1
 S ^RMPR(660,RMPR660,3)=$P($G(^RMPR(664,RMPRA,3)),U,4)
 ;modified by #62
 S ^TMP($J,"RMPRPCE",660,RMPR660)=RMPRG_"^"_$G(RMPRDFN)
 ;
 S DIK="^RMPR(660,",DA=RMPR660 D IX1^DIK
 I $D(RMPRWO),$D(^RMPR(664.2,+RMPRWO,0)) S DIC="^RMPR(664.2,"_RMPRWO_",1,",DIC("P")="664.22PA",DA(1)=RMPRWO,DIC(0)="LZ",R660=^RMPR(660,RMPR660,0),X=$P(R660,U,6) D FILE^DICN I +Y>0 D
 .S $P(^RMPR(664.2,RMPRWO,1,+Y,0),U,2)=$P(R660,U,7),$P(^(0),U,3)=$J($P(R660,U,16)/$P(R660,U,7),0,2)
 .S $P(^RMPR(664.2,RMPRWO,1,+Y,0),U,6)=$P(R660,U,9),$P(^(0),U,7)=$P(R660,U,8),$P(^(0),U,10)=RMPRTRN,$P(^(0),U,4)=$P(R660,U,14),$P(^(0),U,9)="PD",$P(^(0),U,11)=RMPRA,$P(^(0),U,12)=RMPR660
 .S DIK=DIC,DA(1)=RMPRWO,DA=+Y D IX1^DIK S ^RMPR(664.2,RMPRWO,1,"AC",RMPRA,DA)=""
 Q
