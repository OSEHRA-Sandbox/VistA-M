ONCOSUR2 ;Hines OIFO/RTK - ONCOSUR continued ;12/19/00
 ;;2.11;ONCOLOGY;**15,19,22,27,33,34,36,40,42**;Mar 07, 1995
 ;
 D TMK,EXT,LYM,TNM,CS,SUR,SCT W !
 Q
 ;
TMK ;TUMOR MARKER 1 (165.5,25.1)
 ;TUMOR MARKER 2 (165.5,25.2)
 ;TUMOR MARKER 3 (165.5,25.3)
 F SITESUB=67500:1:67509 S SITE(SITESUB)="BREAST"
 F SITESUB=67180:1:67189 S SITE(SITESUB)="COLORECTAL"
 S SITE(67199)="COLORECTAL"
 S SITE(67220)="LIVER"
 S SITE(67221)="LIVER"
 S SITE(67569)="OVARY"
 S SITE(67619)="PROSTATE"
 S SITE(67620)="TESTIS"
 S SITE(67621)="TESTIS"
 S SITE(67629)="TESTIS"
 I $G(SITE(OLDTOP))=$G(SITE(X)) Q
 D MESSAGE
 W !?5,"TUMOR MARKERS"
 S $P(^ONCO(165.5,D0,24),U,2)=""
 S $P(^ONCO(165.5,D0,24),U,3)=""
 S $P(^ONCO(165.5,D0,24),U,7)=""
 Q
 ;
EXT ;EXTENSION (165.5,30)
 S ONCOX="E",ONCOT=OLDTOP S OLDEXT=$$GETLIST^ONCODEL(D0,ONCOX,ONCOT)
 S ONCOX="E",ONCOT=X S NEWEXT=$$GETLIST^ONCODEL(D0,ONCOX,ONCOT)
 I OLDEXT=NEWEXT Q
 D MESSAGE
 W !?5,"EXTENSION"
 S $P(^ONCO(165.5,D0,2),U,10)=""
 Q
 ;
LYM ;LYMPH NODES (165.5,31)
 S ONCOX="L",ONCOT=OLDTOP S OLDLYM=$$GETLIST^ONCODEL(D0,ONCOX,ONCOT)
 S ONCOX="L",ONCOT=X S NEWLYM=$$GETLIST^ONCODEL(D0,ONCOX,ONCOT)
 I OLDLYM=NEWLYM Q
 D MESSAGE
 W !?5,"LYMPH NODES"
 S $P(^ONCO(165.5,D0,2),U,11)=""
 Q
 ;
TNM ;CLINICAL STAGING
 ;PATHOLOGIC STAGING
 S OLDTNM=$P($G(^ONCO(164,OLDTOP,0)),U,11) I OLDTNM="" Q
 S NEWTNM=$P($G(^ONCO(164,X,0)),U,11) I NEWTNM="" Q
 I OLDTNM=NEWTNM Q
 D MESSAGE
 W !?5,"CLINICAL STAGING",?36,"(all fields)"
 W !?5,"PATHOLOGIC STAGING",?36,"(all fields)"
 S $P(^ONCO(165.5,D0,2),U,25)=""   ;37.1 ;CLINICAL T
 S $P(^ONCO(165.5,D0,2),U,26)=""   ;37.2 ;CLINICAL N
 S $P(^ONCO(165.5,D0,2),U,27)=""   ;37.3 ;CLINICAL M
 S $P(^ONCO(165.5,D0,2),U,20)=""   ;38   ;CLINICAL STAGE GROUP
 S $P(^ONCO(165.5,D0,3),U,32)=""   ;19   ;STAGED BY (CLINICAL STAGE)
 S $P(^ONCO(165.5,D0,2.1),U,12)="" ;134  ;CLINICAL RISK FACTORS
 S $P(^ONCO(165.5,D0,24),U,8)=""   ;136  ;SERUM TUMOR MARKERS
 S $P(^ONCO(165.5,D0,2.1),U,1)=""  ;85   ;PATHOLOGIC T
 S $P(^ONCO(165.5,D0,2.1),U,2)=""  ;86   ;PATHOLOGIC N
 S $P(^ONCO(165.5,D0,2.1),U,3)=""  ;87   ;PATHOLOGIC M
 S $P(^ONCO(165.5,D0,2.1),U,4)=""  ;88   ;PATHOLOGIC STAGE GROUP
 S $P(^ONCO(165.5,D0,2.1),U,5)=""  ;89   ;STAGED BY (PATHOLOGIC STAGE)
 S $P(^ONCO(165.5,D0,2.1),U,13)="" ;135  ;PATHOLOGIC RISK FACTORS
 Q
 ;
SUR ;SURGICAL PROCEDURES
 S OLDSCG=$P($G(^ONCO(164,OLDTOP,0)),U,16) I OLDSCG="" Q
 S NEWSCG=$P($G(^ONCO(164,X,0)),U,16) I NEWSCG="" Q
 I OLDSCG=NEWSCG Q
 D MESSAGE
 W !?5,"SURGICAL PROCEDURES",?36,"(all fields)"
 ;
 S $P(^ONCO(165.5,D0,3),U,34)=""      ;74   ;SURGICAL APPROACH (R) 
 S $P(^ONCO(165.5,D0,3),U,38)=""      ;58.2 ;SURGERY OF PRIMARY (R) 
 S $P(^ONCO(165.5,D0,3.1),U,7)=""     ;50.2 ;SURGERY OF PRIMARY @FAC (R)
 S $P(^ONCO(165.5,DA,3),U,40)=""      ;138  ;SCOPE OF LN SURGERY (R)
 S $P(^ONCO(165.5,DA,3.1),U,9)=""     ;138.1;SCOPE OF LN SURGERY @FAC (R)
 S $P(^ONCO(165.5,DA,3),U,42)=""      ;140  ;NUMBER OF LN REMOVED (R)
 S $P(^ONCO(165.5,DA,3.1),U,11)=""    ;140.1;NUMBER OF LN REMOVED @FAC(R)
 S $P(^ONCO(165.5,DA,3),U,41)=""      ;139  ;SURG PROC/OTHER SITE (R)
 S $P(^ONCO(165.5,DA,3.1),U,10)=""    ;139.1;SURG PROC/OTHER SITE @FAC(R)
 ;
 S $P(^ONCO(165.5,D0,3.1),U,29)=""    ;58.6 ;SURGERY OF PRIMARY (F) 
 S TXDT=$P(^ONCO(165.5,DA,3),U,1)_"S1"
 S $P(^ONCO(165.5,DA,3),U,1)=""       ;50   ;MOST DEFINITIVE SURG DATE
 K ^ONCO(165.5,"ATX",DA,TXDT)
 S $P(^ONCO(165.5,DA,3),U,28)=""      ;59   ;SURGICAL MARGINS
 S $P(^ONCO(165.5,D0,3.1),U,30)=""    ;58.7 ;SURGERY OF PRIMARY @FAC (F)
 S $P(^ONCO(165.5,DA,3.1),U,8)=""     ;50.3 ;MOST DEFINITIVE SURG @FAC DATE
 ;
 S $P(^ONCO(165.5,DA,3.1),U,31)=""    ;138.4;SCOPE OF LN SURGERY (F)
 S TXDT=$P($G(^ONCO(165.5,DA,3.1)),U,22)_"S2"
 S $P(^ONCO(165.5,DA,3.1),U,22)=""    ;138.2;SCOPE OF LN SURGERY DATE
 K ^ONCO(165.5,"ATX",DA,TXDT)
 S $P(^ONCO(165.5,DA,3.1),U,32)=""    ;138.5;SCOPE OF LN SURGERY @FAC (F)
 S $P(^ONCO(165.5,DA,3.1),U,23)=""    ;138.2;SCOPE OF LN SURGERY @FAC DT
 ;
 S $P(^ONCO(165.5,DA,3.1),U,33)=""    ;139.4;SURG PROC/OTHER SITE (F)
 S TXDT=$P(^ONCO(165.5,DA,3.1),U,24)_"S3"
 S $P(^ONCO(165.5,DA,3.1),U,24)=""    ;139.2;SURG PROC/OTHER SITE DATE
 K ^ONCO(165.5,"ATX",DA,TXDT)
 S $P(^ONCO(165.5,DA,3.1),U,34)=""    ;139.5;SURG PROC/OTHER SITE @FAC(F)
 S $P(^ONCO(165.5,DA,3.1),U,25)=""    ;139.3;SURG PROC/OTHER SITE @FAC DT
 S $P(^ONCO(165.5,DA,3),U,33)=""      ;23   ;RECONSTRUCTION/RESTORATION
 S $P(^ONCO(165.5,DA,"THY1"),U,36)="" ;435  ;DATE OF SURGICAL DISCHARGE
 S $P(^ONCO(165.5,DA,3.1),U,28)=""    ;14   ;READMISSION W/I 30 DAYS/SURG
 K ^ONCO(165.5,DA,14)                 ;108  ;TEXT-RX-SURGERY
 S $P(^ONCO(165.5,DA,3),U,26)=""      ;58   ;REASON NO SURGERY OF PRIMARY
 Q
SCT ;
 S OLDSCG=$P($G(^ONCO(164,OLDTOP,0)),U,16) I OLDSCG="" Q
 S NEWSCG=$P($G(^ONCO(164,X,0)),U,16) I NEWSCG="" Q
 I OLDSCG=NEWSCG Q
 D MESSAGE
 W !?5,"SUBSEQUENT COURSE OF TREATMENT",?36,"(all surgery fields)"
 F SCT=0:0 S SCT=$O(^ONCO(165.5,DA,4,SCT)) Q:SCT'>0  D
 .S $P(^ONCO(165.5,DA,4,SCT,0),U,4)=""   ;.04 ;SURGERY OF PRIMARY
 .S $P(^ONCO(165.5,DA,4,SCT,0),U,11)=""  ;.041;SURGERY OF PRIMARY DATE
 .S $P(^ONCO(165.5,DA,4,SCT,2),U,32)=""  ;35  ;SCOPE OF LN SURGERY
 .S $P(^ONCO(165.5,DA,4,SCT,2),U,34)=""  ;37  ;NUMBER OF LN REMOVED
 .S $P(^ONCO(165.5,DA,4,SCT,2),U,33)=""  ;36  ;SURG PROC/OTHER SITE
 .S $P(^ONCO(165.5,DA,4,SCT,2),U,30)=""  ;33  ;RECON/RESTORE DELAYED
 .S $P(^ONCO(165.5,DA,4,SCT,2),U,31)=""  ;34  ;RECON/RESTORE DELAY DATE
 W !
 Q
 ;
CS ;COLLOBORATIVE STAGING
 S OLDSCG=$P($G(^ONCO(164,OLDTOP,0)),U,16) I OLDSCG="" Q
 S NEWSCG=$P($G(^ONCO(164,X,0)),U,16) I NEWSCG="" Q
 I OLDSCG=NEWSCG Q
 D MESSAGE
 W !?5,"COLLABORATIVE STAGING",?36,"(all fields)"
 F PIECE=1:1:12 S $P(^ONCO(165.5,D0,"CS"),U,PIECE)=""
 F PIECE=1:1:11 S $P(^ONCO(165.5,D0,"CS1"),U,PIECE)=""
 Q
 ;
MESSAGE ;
 I MSSG=1 Q
 W !!?3,"You have changed the PRIMARY SITE.  The new topography is"
 W !?3,"outside of the old topography's site group.  This change"
 W !?3,"may affect the validity of all site-specific fields.  Therefore,"
 W !?3,"these fields have been initialized and need to be re-entered:"
 W ! S MSSG=1 Q
 Q
