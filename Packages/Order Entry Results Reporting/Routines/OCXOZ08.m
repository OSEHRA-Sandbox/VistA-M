OCXOZ08 ;SLC/RJS,CLA - Order Check Scan ;MAY 26,2016 at 15:49
 ;;3.0;ORDER ENTRY/RESULTS REPORTING;**32,221,243**;Dec 17,1997;Build 242
 ;;  ;;ORDER CHECK EXPERT version 1.01 released OCT 29,1998
 ;
 ; ***************************************************************
 ; ** Warning: This routine is automatically generated by the   **
 ; ** Rule Compiler (^OCXOCMP) and ANY changes to this routine  **
 ; ** will be lost the next time the rule compiler executes.    **
 ; ***************************************************************
 ;
 Q
 ;
CHK164 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK163+17^OCXOZ07.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK164 Variables
 ; OCXDF(2) ----> Data Field: FILLER (FREE TEXT)
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(59) ---> Data Field: CHOLECYSTOGRAM PROCEDURE FLAG (BOOLEAN)
 ; OCXDF(62) ---> Data Field: PATIENT AGE (NUMERIC)
 ; OCXDF(73) ---> Data Field: ORDERABLE ITEM IEN (NUMERIC)
 ;
 ;      Local Extrinsic Functions
 ; CH( --------------> IS THIS A CHOLECYSTOGRAM RADIOLOGY PROCEDURE
 ;
 S OCXDF(73)=$P($G(OCXPSD),"|",1) I $L(OCXDF(73)) S OCXDF(59)=$P($$CH(OCXDF(73)),"^",1) I $L(OCXDF(59)),(OCXDF(59)) S OCXDF(37)=$G(DFN) I $L(OCXDF(37)) D CHK171
 S OCXDF(2)=$P($G(OCXPSD),"|",2) I $L(OCXDF(2)),($E(OCXDF(2),1,2)="PS") S OCXDF(37)=$G(DFN) I $L(OCXDF(37)) S OCXDF(62)=$$AGE^ORQPTQ4(OCXDF(37)) I $L(OCXDF(62)) D CHK426^OCXOZ0D
 Q
 ;
CHK171 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK164+15.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK171 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(60) ---> Data Field: RECENT CHOLECYSTOGRAM FLAG (BOOLEAN)
 ; OCXDF(61) ---> Data Field: RECENT CHOLECYSTOGRAM TEXT (FREE TEXT)
 ; OCXDF(122) --> Data Field: RECENT CHOLECYSTOGRAM ORDER STATUS (FREE TEXT)
 ;
 ;      Local Extrinsic Functions
 ; RECCH( -----------> RECENT CHOLECYSTOGRAM PREOCEDURE
 ; RECCHST( ---------> RECENT CHOLECYSTOGRAM ORDER STATUS
 ;
 S OCXDF(60)=$P($$RECCH(OCXDF(37),7),"^",1) I $L(OCXDF(60)),(OCXDF(60)) S OCXDF(61)=$P($$RECCH(OCXDF(37),7),"^",3),OCXDF(122)=$P($$RECCHST(OCXDF(37),7),"^",2) D CHK176
 Q
 ;
CHK176 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK171+15.
 ;
 Q:$G(OCXOERR)
 ;
 ;      Local Extrinsic Functions
 ; FILE(DFN,63, -----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: PATIENT HAS RECENT CHOLECYSTOGRAM)
 ;
 S OCXOERR=$$FILE(DFN,63,"61,122") Q:OCXOERR 
 Q
 ;
CHK182 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK163+18^OCXOZ07.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK182 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(62) ---> Data Field: PATIENT AGE (NUMERIC)
 ; OCXDF(64) ---> Data Field: FORMATTED RENAL LAB RESULTS (FREE TEXT)
 ; OCXDF(76) ---> Data Field: CREATININE CLEARANCE (ESTIM) VALUE (NUMERIC)
 ; OCXDF(109) --> Data Field: NUMBER OF MEDS (NUMERIC)
 ; OCXDF(123) --> Data Field: POLYPHARMACY (BOOLEAN)
 ;
 ;      Local Extrinsic Functions
 ; CRCL( ------------> CREATININE CLEARANCE (ESTIMATED/CALCULATED)
 ; FILE(DFN,95, -----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: POLYPHARMACY)
 ; FLAB( ------------> FORMATTED LAB RESULTS
 ;
 S OCXDF(62)=$$AGE^ORQPTQ4(OCXDF(37)) I $L(OCXDF(62)),(OCXDF(62)>65) S OCXDF(64)=$$FLAB(OCXDF(37),"SERUM CREATININE^SERUM UREA NITROGEN","SERUM SPECIMEN") D CHK186
 S OCXDF(76)=$P($$CRCL(OCXDF(37)),"^",2) I $L(OCXDF(76)),(OCXDF(76)<50),(OCXDF(76)>0) D CHK247^OCXOZ0A
 S OCXDF(123)=$P($$POLYRX^ORKPS(OCXDF(37)),"^",1) I $L(OCXDF(123)),(OCXDF(123)) S OCXDF(109)=$P($$NUMRX^ORKPS(OCXDF(37)),"^",1),OCXOERR=$$FILE(DFN,95,"109") Q:OCXOERR 
 Q
 ;
CHK186 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK182+18.
 ;
 Q:$G(OCXOERR)
 ;
 ;      Local Extrinsic Functions
 ; FILE(DFN,64, -----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: PHARMACY PATIENT OVER 65)
 ;
 S OCXOERR=$$FILE(DFN,64,"64") Q:OCXOERR 
 Q
 ;
CH(OCXOI) ;  Compiler Function: IS THIS A CHOLECYSTOGRAM RADIOLOGY PROCEDURE
 ;
 N OCXVAL S OCXVAL=$$CM^ORQQRA(OCXOI) Q:(OCXVAL["C") 1_U_OCXVAL Q 0
 ;
CRCL(DFN) ;  Compiler Function: CREATININE CLEARANCE (ESTIMATED/CALCULATED)
 ;
 N HT,AGE,SEX,SCR,SCRD,CRCL,LRWKLD,RSLT,ORW,ORH,PSCR
 N HTGT60,ABW,IBW,BWRATIO,BWDIFF,LOWBW,ADJBW
 S RSLT="0^<Unavailable>"
 S PSCR="^^^^^^0"
 D VITAL^ORQQVI("WEIGHT","WT",DFN,.ORW,0,"",$$NOW^XLFDT)
 Q:'$D(ORW) RSLT
 S ABW=$P(ORW(1),U,3) Q:+$G(ABW)<1 RSLT
 S ABW=ABW/2.2  ;ABW (actual body weight) in kg
 D VITAL^ORQQVI("HEIGHT","HT",DFN,.ORH,0,"",$$NOW^XLFDT)
 Q:'$D(ORH) RSLT
 S HT=$P(ORH(1),U,3) Q:+$G(HT)<1 RSLT
 S AGE=$$AGE^ORQPTQ4(DFN) Q:'AGE RSLT
 S SEX=$P($$SEX^ORQPTQ4(DFN),U,1) Q:'$L(SEX) RSLT
 S OCXTL="" Q:'$$TERMLKUP^ORB31(.OCXTL,"SERUM CREATININE") RSLT
 S OCXTLS="" Q:'$$TERMLKUP^ORB31(.OCXTLS,"SERUM SPECIMEN") RSLT
 S SCR="",OCXT=0 F  S OCXT=$O(OCXTL(OCXT)) Q:'OCXT  D
 .S OCXTS=0 F  S OCXTS=$O(OCXTLS(OCXTS)) Q:'OCXTS  D
 ..S SCR=$$LOCL^ORQQLR1(DFN,$P(OCXTL(OCXT),U),$P(OCXTLS(OCXTS),U))
 ..I $P(SCR,U,7)>$P(PSCR,U,7) S PSCR=SCR
 S SCR=PSCR,SCRV=$P(SCR,U,3) Q:+$G(SCRV)<.01 RSLT
 S SCRD=$P(SCR,U,7) Q:'$L(SCRD) RSLT
 ;
 S HTGT60=$S(HT>60:(HT-60)*2.3,1:0)  ;if ht > 60 inches
 I HTGT60>0 D
 .S IBW=$S(SEX="M":50+HTGT60,1:45.5+HTGT60)  ;Ideal Body Weight
 .S BWRATIO=(ABW/IBW)  ;body weight ratio
 .S BWDIFF=$S(ABW>IBW:ABW-IBW,1:0)
 .S LOWBW=$S(IBW<ABW:IBW,1:ABW)
 .I BWRATIO>1.3,(BWDIFF>0) S ADJBW=((0.3*BWDIFF)+IBW)
 .E  S ADJBW=LOWBW
 I +$G(ADJBW)<1 D
 .S ADJBW=ABW
 S CRCL=(((140-AGE)*ADJBW)/(SCRV*72))
 ;
 S:SEX="M" RSLT=SCRD_U_$J(CRCL,1,1)
 S:SEX="F" RSLT=SCRD_U_$J((CRCL*.85),1,1)
 Q RSLT
 ;
FILE(DFN,OCXELE,OCXDFL) ;     This Local Extrinsic Function logs a validated event/element.
 ;
 N OCXTIMN,OCXTIML,OCXTIMT1,OCXTIMT2,OCXDATA,OCXPC,OCXPC,OCXVAL,OCXSUB,OCXDFI
 S DFN=+$G(DFN),OCXELE=+$G(OCXELE)
 ;
 Q:'DFN 1 Q:'OCXELE 1 K OCXDATA
 ;
 S OCXDATA(DFN,OCXELE)=1
 F OCXPC=1:1:$L(OCXDFL,",") S OCXDFI=$P(OCXDFL,",",OCXPC) I OCXDFI D
 .S OCXVAL=$G(OCXDF(+OCXDFI)),OCXDATA(DFN,OCXELE,+OCXDFI)=OCXVAL
 ;
 M ^TMP("OCXCHK",$J,DFN)=OCXDATA(DFN)
 ;
 Q 0
 ;
FLAB(DFN,OCXLIST,OCXSPEC) ;  Compiler Function: FORMATTED LAB RESULTS
 ;
 Q:'$G(DFN) "<Patient Not Specified>"
 Q:'$L($G(OCXLIST)) "<Lab Tests Not Specified>"
 N OCXLAB,OCXOUT,OCXPC,OCXSL,SPEC S OCXOUT="",SPEC=""
 I $L($G(OCXSPEC)) S OCXSL=$$TERMLKUP(OCXSPEC,.OCXSL)
 F OCXPC=1:1:$L(OCXLIST,U) S OCXLAB=$P(OCXLIST,U,OCXPC) I $L(OCXLAB) D
 .N OCXX,OCXY,X,Y,DIC,TEST,SPEC,OCXTL,OCXA,OCXR
 .S OCXTL="" Q:'$$TERMLKUP(OCXLAB,.OCXTL)
 .S OCXX="",TEST=0 F  S TEST=$O(OCXTL(TEST)) Q:'TEST  D
 ..I $L($G(OCXSL)) D
 ...S SPEC=0 F  S SPEC=$O(OCXSL(SPEC)) Q:'SPEC  D
 ....S OCXX=$$LOCL^ORQQLR1(DFN,TEST,SPEC) I $L(OCXX) D
 .....S OCXA($P(OCXX,U,7))=OCXX
 ..I '$L($G(OCXSL)) S OCXX=$$LOCL^ORQQLR1(DFN,TEST,"")
 ..Q:'$L(OCXX)
 .I $D(OCXA) S OCXR="",OCXR=$O(OCXA(OCXR),-1),OCXX=OCXA(OCXR)
 .I $L(OCXX) D
 ..S OCXY=$P(OCXX,U,2)_": "_$P(OCXX,U,3)_" "_$P(OCXX,U,4)
 ..S OCXY=OCXY_" "_$S($L($P(OCXX,U,5)):"["_$P(OCXX,U,5)_"]",1:"")
 ..I $L($P(OCXX,U,7)) S OCXY=OCXY_" "_$$FMTE^XLFDT($P(OCXX,U,7),"2P")
 .S:$L(OCXOUT) OCXOUT=OCXOUT_"   " S OCXOUT=OCXOUT_$G(OCXY)
 Q:'$L(OCXOUT) "<Results Not Found>" Q OCXOUT
 ;
RECCH(DFN,DAYS) ;  Compiler Function: RECENT CHOLECYSTOGRAM PREOCEDURE
 ;
 Q:'$G(DFN) 0 Q:'$G(DAYS) 0 N OUT S OUT=$$RECENTCH^ORKRA(DFN,DAYS) Q:'$L(OUT) 0 Q 1_U_OUT
 ;
RECCHST(DFN,DAYS)     ;  Compiler Function: RECENT CHOLECYSTOGRAM ORDER STATUS
 ;
 Q:'$G(DFN) 0 Q:'$G(DAYS) 0
 N ORDER S ORDER=$P($$RECENTCH^ORKRA(DFN,DAYS),U) Q:'$L(ORDER) 0
 N STATUS S STATUS=$P($$STATUS^ORQOR2(ORDER),U,2) Q:'$L(STATUS) 0
 Q 1_U_STATUS
 ;
TERMLKUP(OCXTERM,OCXLIST) ;
 Q $$TERM^OCXOZ01(OCXTERM,.OCXLIST)
 ;
