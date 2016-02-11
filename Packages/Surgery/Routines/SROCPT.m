SROCPT ;BIR/MAM,ADM - PRINT DESCRIPTION OF CPT CODE ON LOOKUP ; [ 05/14/99  11:28 AM ]
 ;;3.0;Surgery;**3,31,88,127,184**;24 Jun 93;Build 35
1 N SRCODE,SRDA,SRDATE,SRDES,SRI,SRX
 S SRDATE=DT
 S SRDA=$S($G(SRTN):SRTN,$D(DA(1)):DA(1),$D(DA):DA,1:"")
 I $G(SRDA) S SRDATE=$P($G(^SRF(SRDA,0)),"^",9)
 S SRDATE=$S($G(ICPTVDT):ICPTVDT,1:SRDATE)
 S SRCODE=Y,SRX=$$CPTD^ICPTCOD(SRCODE,"SRDES",,SRDATE)
 F SRI=1:1:SRX D:$TR(SRDES(SRI)," ")'="" EN^DDIOL(SRDES(SRI),"","!,?1")
 Q
DISPLAY ; output principal CPT
 I $D(Y),Y="" Q
 N SRCODE,SRCPT,SRDA,SRDES,SRI,SRK,SRP,SRW,SRX,SRY,SRZ
 S Y=$P($$CPT^ICPTCOD(Y),"^",2),SRDA=$S($D(SRTN):SRTN,1:"") Q:SRDA=""
 I $D(QPQPQ) D SSPRIN Q
 D DES I '$O(^SRF(SRDA,"OPMOD",0)) Q
 S SRCPT="Principal CPT Code: "_SRW D EN^DDIOL(SRCPT,"","!,?6")
 S SRX="Modifiers: -"
 S SRI=0 F  S SRI=$O(^SRF(SRDA,"OPMOD",SRI)) Q:'SRI  D
 .S SRZ=$P(^SRF(SRDA,"OPMOD",SRI,0),"^"),SRY=$$MOD^ICPTMOD(SRZ,"I",$P($G(^SRF(SRDA,0)),"^",9)),SRX=SRX_$P(SRY,"^",2)_" "_$E($P(SRY,"^",3),1,57) D EN^DDIOL(SRX,"","!,?7") S SRX="           -"
 Q
OTHDISP ; output other procedure CPT
 I $D(Y),Y="" Q
 N SRCODE,SRCPT,SRDA,SRDES,SRI,SRK,SRP,SRW,SRX,SRY,SRZ
 S SRDA(1)=$S($D(SRTN):SRTN,1:""),SRDA=$S($D(DA):DA,1:"") Q:SRDA(1)=""!(SRDA="")
 I $D(QPQPQ) D SSOTH Q
 D DES I '$O(^SRF(SRDA(1),13,SRDA,"MOD",0)) Q
 S SRCPT="Other CPT Code: "_SRW D EN^DDIOL(SRCPT,"","!,?6")
 S SRX="Modifiers: -"
 S SRI=0 F  S SRI=$O(^SRF(SRDA(1),13,SRDA,"MOD",SRI)) Q:'SRI  D
 .S SRZ=$P(^SRF(SRDA(1),13,SRDA,"MOD",SRI,0),"^"),SRY=$$MOD^ICPTMOD(SRZ,"I",$P($G(^SRF(SRDA(1),0)),"^",9)),SRX=SRX_$P(SRY,"^",2)_" "_$E($P(SRY,"^",3),1,57) D EN^DDIOL(SRX,"","!,?7") S SRX="           -"
 Q
DES ; get short name and description
 N X,Z,SRDAA,SRDD S (SRCODE,SRK)=Y,SRDAA=$S($D(SRTN):SRTN,$D(SRDA(1)):SRDA(1),$D(SRDA):SRDA,1:"")
 S SRDD=DT I $G(SRDAA) S SRDD=$E($P(^SRF(SRDAA,0),"^",9),1,7)
 S SRY=$$CPT^ICPTCOD(SRCODE,SRDD),SRW=$P(SRY,"^",2)_"  "_$P(SRY,"^",3)
 S SRY=$$CPTD^ICPTCOD(SRCODE,"SRDES",,SRDD),SRK=SRK_" " F SRI=1:1:SRY D  Q:$L(SRK_" "_X)>245  S SRK=SRK_" "_X
 .S X=SRDES(SRI) F  S Z=$F(X,"  ") Q:'Z  S X=$E(X,1,Z-2)_$E(X,Z,255)
 S Y=SRK
 Q
ACTIV(SRTN,SRCODE) ; screen for active CPT codes
 K ICPTVDT
 N SROK,SRSDATE S SROK=1,SRSDATE=DT
 I $G(SRTN) S SRSDATE=$E($P(^SRF(SRTN,0),"^",9),1,7)
 S SROK=$P($$CPT^ICPTCOD(SRCODE,SRSDATE),"^",7),ICPTVDT=SRSDATE
 Q SROK
IN ; check CPT input
 N SRX,SRCPT K SRCMOD S SRX=X,SRCPT=$P(SRX,"-"),SRCMOD=$P(SRX,"-",2) I SRCMOD="" K SRCMOD
 S X=SRCPT
 Q
SSPRIN ; append CPT modifiers to principal CPT code
 N SRCMOD,SRCOMMA,SRCPT,SRI,SRM,X I $O(^SRF(SRTN,"OPMOD",0)) D
 .S (SRCOMMA,SRI)=0,SRCMOD="",SRCPT=Y_"-" F  S SRI=$O(^SRF(SRTN,"OPMOD",SRI)) Q:'SRI  D
 ..S SRM=$P(^SRF(SRTN,"OPMOD",SRI,0),"^"),SRCMOD=$P($$MOD^ICPTMOD(SRM,"I"),"^",2)
 ..S SRCPT=SRCPT_$S(SRCOMMA:",",1:"")_SRCMOD,SRCOMMA=1
 .S Y=SRCPT
 Q
SSOTH ; append CPT modifiers to other CPT code
 N SRCMOD,SRCOMMA,SRCPT,SRI,SRM,X I $O(^SRF(SRTN,13,SRDA,"MOD",0)) D
 .S (SRCOMMA,SRI)=0,SRCMOD="",SRCPT=Y_"-" F  S SRI=$O(^SRF(SRTN,13,SRDA,"MOD",SRI)) Q:'SRI  D
 ..S SRM=$P(^SRF(SRTN,13,SRDA,"MOD",SRI,0),"^"),SRCMOD=$P($$MOD^ICPTMOD(SRM,"I"),"^",2)
 ..S SRCPT=SRCPT_$S(SRCOMMA:",",1:"")_SRCMOD,SRCOMMA=1
 .S Y=SRCPT
 Q
CHK(SRCPT) ; check entered CPT code against Surgery Codes
 Q:$G(SRCPT)="" 0
 N SRF,SRI,SRJ,SRX,SRLIST,II S SRF=0
 I $E(SRCPT,1)="D" F II=0:1:9999 I $E(SRCPT,2,99)=II S SRF=1 Q  ;D0000-D9999
 I 'SRF F II=00100:1:01999,10000:1:79999 I +SRCPT=II S SRF=1 Q
 I 'SRF F SRJ=1:1 S SRLIST=$P($T(LST9K+SRJ)," ;;",2) Q:SRLIST=""!(SRF=1)  F SRI=1:1 S II=$P(SRLIST,",",SRI) Q:II=""  I +SRCPT=II S SRF=1 Q
 I 'SRF I SRCPT?.N1"T" S SRF=1
 I 'SRF D
 . D EN^DDIOL("Planned Principal Procedure Code for Surgical procedure must be within the ",,"!,?3")
 . D EN^DDIOL("range of: 00100-01999;  10000-69999;  70000-79999;  allowed 90000 codes; ",,"!,?3")
 . D EN^DDIOL("D0000-D9999;  xxxxT.",,"!,?3")
 Q SRF
 ;
LST9K ; allowed 90K codes
 ;;90865,90870,91040,91120,91122,92502,92504,92511,92611,92612,92613,92614,92615 
 ;;92616,92617,92960,92961,92970,92986,92987,92990,93312,93313,93314,93315,93316
 ;;93317,93318,93355,93505,93580,93581,93582,93583,93631,93650,95940,95955,95958
 ;;95961,95990,95991,96420,96422,96425,96440,96450,96521,96522,97597,97598,97602
 ;;97605,97606,97607,97608,99144,99149
