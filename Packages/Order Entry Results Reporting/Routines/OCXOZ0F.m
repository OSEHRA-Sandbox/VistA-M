OCXOZ0F ;SLC/RJS,CLA - Order Check Scan ;AUG 27,2019 at 09:18
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
CHK468 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK343+15^OCXOZ0C.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK468 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(74) ---> Data Field: VA DRUG CLASS (FREE TEXT)
 ; OCXDF(158) --> Data Field: DUPLICATE OPIOID MEDICATIONS TEXT (FREE TEXT)
 ;
 ;      Local Extrinsic Functions
 ; LIST( ------------> IN LIST OPERATOR
 ; OPIOID( ----------> OPIOID MEDICATIONS
 ;
 I $$LIST(OCXDF(74),"OPIOID ANALGESICS,OPIOID ANTAGONIST ANALGESICS") S OCXDF(37)=$G(DFN) I $L(OCXDF(37)) S OCXDF(158)=$P($$OPIOID(OCXDF(37)),"^",2) D CHK472
 Q
 ;
CHK472 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK468+14.
 ;
 Q:$G(OCXOERR)
 ;
 ;      Local Extrinsic Functions
 ; FILE(DFN,139, ----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: OPIOID MED ORDER)
 ;
 S OCXOERR=$$FILE(DFN,139,"158") Q:OCXOERR 
 Q
 ;
CHK473 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK1+37^OCXOZ02.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK473 Variables
 ; OCXDF(2) ----> Data Field: FILLER (FREE TEXT)
 ; OCXDF(160) --> Data Field: CONTROL REASON (FREE TEXT)
 ;
 ;      Local Extrinsic Functions
 ; FILE(DFN,141, ----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: HL7 DEA CERT REVOKED)
 ; FILE(DFN,143, ----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: HL7 PHARMACY HASH MISMATCH)
 ;
 I (OCXDF(160)[17),$L(OCXDF(2)),(OCXDF(2)="PS") S OCXOERR=$$FILE(DFN,141,"") Q:OCXOERR 
 I (OCXDF(160)[16),$L(OCXDF(2)),(OCXDF(2)="PS") S OCXOERR=$$FILE(DFN,143,"") Q:OCXOERR 
 Q
 ;
CHK484 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK355+16^OCXOZ0C.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK484 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ; OCXDF(136) --> Data Field: CLOZAPINE ANC W/IN 7 FLAG (BOOLEAN)
 ; OCXDF(137) --> Data Field: CLOZAPINE ANC W/IN 7 RESULT (NUMERIC)
 ;
 ;      Local Extrinsic Functions
 ;
 I (OCXDF(137)<"1.0") S OCXDF(136)=$P($P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",3),";",1) I $L(OCXDF(136)),(OCXDF(136)) D CHK488
 I (OCXDF(137)>.999),(OCXDF(137)<1.5) S OCXDF(136)=$P($P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",3),";",1) I $L(OCXDF(136)),(OCXDF(136)) D CHK494
 Q
 ;
CHK488 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK484+13.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK488 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(130) --> Data Field: CLOZAPINE LAB RESULTS (FREE TEXT)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ;
 ;      Local Extrinsic Functions
 ; FILE(DFN,144, ----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: CLOZAPINE ANC < 1.0)
 ;
 S OCXDF(130)=$P($$CLOZLABS^ORKLR(OCXDF(37),"",OCXDF(131)),"^",4),OCXOERR=$$FILE(DFN,144,"130") Q:OCXOERR 
 Q
 ;
CHK494 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK484+14.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK494 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(130) --> Data Field: CLOZAPINE LAB RESULTS (FREE TEXT)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ;
 ;      Local Extrinsic Functions
 ; FILE(DFN,145, ----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: CLOZAPINE ANC >= 1.0 & < 1.5)
 ;
 S OCXDF(130)=$P($$CLOZLABS^ORKLR(OCXDF(37),"",OCXDF(131)),"^",4),OCXOERR=$$FILE(DFN,145,"130") Q:OCXOERR 
 Q
 ;
EL24 ; Examine every rule that involves Element #24 [HL7 LAB TEST RESULTS CRITICAL]
 ;  Called from SCAN+9^OCXOZ01.
 ;
 Q:$G(OCXOERR)
 ;
 D R3R1A^OCXOZ0H   ; Check Relation #1 in Rule #3 'CRITICAL LAB RESULTS'
 Q
 ;
EL105 ; Examine every rule that involves Element #105 [HL7 LAB ORDER RESULTS CRITICAL]
 ;  Called from SCAN+9^OCXOZ01.
 ;
 Q:$G(OCXOERR)
 ;
 D R3R2A^OCXOZ0I   ; Check Relation #2 in Rule #3 'CRITICAL LAB RESULTS'
 Q
 ;
EL44 ; Examine every rule that involves Element #44 [ORDER FLAGGED]
 ;  Called from SCAN+9^OCXOZ01.
 ;
 Q:$G(OCXOERR)
 ;
 D R5R1A^OCXOZ0I   ; Check Relation #1 in Rule #5 'ORDER FLAGGED FOR CLARIFICATION'
 Q
 ;
EL134 ; Examine every rule that involves Element #134 [ORDER UNFLAGGED]
 ;  Called from SCAN+9^OCXOZ01.
 ;
 Q:$G(OCXOERR)
 ;
 D R5R2A^OCXOZ0J   ; Check Relation #2 in Rule #5 'ORDER FLAGGED FOR CLARIFICATION'
 Q
 ;
EL45 ; Examine every rule that involves Element #45 [ORDER REQUIRES CHART SIGNATURE]
 ;  Called from SCAN+9^OCXOZ01.
 ;
 Q:$G(OCXOERR)
 ;
 D R6R1A^OCXOZ0J   ; Check Relation #1 in Rule #6 'ORDER REQUIRES CHART SIGNATURE'
 Q
 ;
EL21 ; Examine every rule that involves Element #21 [PATIENT ADMISSION]
 ;  Called from SCAN+9^OCXOZ01.
 ;
 Q:$G(OCXOERR)
 ;
 D R7R1A^OCXOZ0J   ; Check Relation #1 in Rule #7 'PATIENT ADMISSION'
 Q
 ;
EL31 ; Examine every rule that involves Element #31 [RADIOLOGY ORDER CANCELLED]
 ;  Called from SCAN+9^OCXOZ01.
 ;
 Q:$G(OCXOERR)
 ;
 D R11R1A^OCXOZ0K   ; Check Relation #1 in Rule #11 'IMAGING REQUEST CANCELLED/HELD'
 Q
 ;
EL100 ; Examine every rule that involves Element #100 [CANCELED BY NON-ORIG ORDERING PROVIDER]
 ;  Called from SCAN+9^OCXOZ01.
 ;
 Q:$G(OCXOERR)
 ;
 D R11R1A^OCXOZ0K   ; Check Relation #1 in Rule #11 'IMAGING REQUEST CANCELLED/HELD'
 D R11R2A^OCXOZ0K   ; Check Relation #2 in Rule #11 'IMAGING REQUEST CANCELLED/HELD'
 D R11R3A^OCXOZ0L   ; Check Relation #3 in Rule #11 'IMAGING REQUEST CANCELLED/HELD'
 D R35R1A^OCXOZ0P   ; Check Relation #1 in Rule #35 'LAB ORDER CANCELLED'
 Q
 ;
EL30 ; Examine every rule that involves Element #30 [RADIOLOGY ORDER PUT ON-HOLD]
 ;  Called from SCAN+9^OCXOZ01.
 ;
 Q:$G(OCXOERR)
 ;
 D R11R2A^OCXOZ0K   ; Check Relation #2 in Rule #11 'IMAGING REQUEST CANCELLED/HELD'
 Q
 ;
EL32 ; Examine every rule that involves Element #32 [RADIOLOGY ORDER DISCONTINUED]
 ;  Called from SCAN+9^OCXOZ01.
 ;
 Q:$G(OCXOERR)
 ;
 D R11R3A^OCXOZ0L   ; Check Relation #3 in Rule #11 'IMAGING REQUEST CANCELLED/HELD'
 Q
 ;
EL46 ; Examine every rule that involves Element #46 [SERVICE ORDER REQUIRES CHART SIGNATURE]
 ;  Called from SCAN+9^OCXOZ01.
 ;
 Q:$G(OCXOERR)
 ;
 D R16R1A^OCXOZ0L   ; Check Relation #1 in Rule #16 'SERVICE ORDER REQUIRES CHART SIGNATURE'
 Q
 ;
EL76 ; Examine every rule that involves Element #76 [STAT LAB RESULT]
 ;  Called from SCAN+9^OCXOZ01.
 ;
 Q:$G(OCXOERR)
 ;
 D R18R1A^OCXOZ0L   ; Check Relation #1 in Rule #18 'STAT RESULTS AVAILABLE'
 Q
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
LIST(DATA,LIST) ;   IS THE DATA FIELD IN THE LIST
 ;
 S:'($E(LIST,1)=",") LIST=","_LIST S:'($E(LIST,$L(LIST))=",") LIST=LIST_"," S DATA=","_DATA_","
 Q (LIST[DATA)
 ;
OPIOID(ORPT) ;determine if pat is receiving opioid med
 ; rtn 1^opioid drug 1, opioid drug 2, opioid drug3, ...
 N ORDG,ORTN,ORNUM,ORDI,ORDCLAS,ORDERS,ORTEXT,DUP,DUPI,DUPJ,DUPLEN
 S ORDG=0,ORTN=0,DUPI=0,DUPLEN=20
 K ^TMP("ORR",$J)
 S ORDG=$O(^ORD(100.98,"B","RX",ORDG))
 D EN^ORQ1(ORPT_";DPT(",ORDG,2,"","","",0,0)
 N J,HOR,SEQ,X S J=1,HOR=0,SEQ=0
 S HOR=$O(^TMP("ORR",$J,HOR)) Q:+HOR<1 ORTN
 F  S SEQ=$O(^TMP("ORR",$J,HOR,SEQ)) Q:+SEQ<1  D
 .S X=^TMP("ORR",$J,HOR,SEQ)
 .S ORNUM=+$P(X,";")
 .Q:ORNUM=+$G(ORIFN)  ;quit if dup med order # = current order #
 .S ORDI=$$VALUE^ORCSAVE2(ORNUM,"DRUG")
 .I +$G(ORDI)>0 D
 ..S ORDCLAS=$P(^PSDRUG(ORDI,0),U,2)  ;va drug class
 ..I ($G(ORDCLAS)="CN101")!($G(ORDCLAS)="CN102") D  ;opioid classes
 ...S ORTEXT=$$FULLTEXT^ORQOR1(ORNUM)
 ...S ORTEXT=$P(ORTEXT,U)_" ["_$P(ORTEXT,U,2)_"]"
 ...S DUPI=DUPI+1,DUP(DUPI)=" ["_DUPI_"] "_ORTEXT
 ...S ORTN=1
 I DUPI>0 D
 .;S DUPLEN=$P(215/DUPI,".")
 .S DUPLEN=500
 .F DUPJ=1:1:DUPI D
 ..I DUPJ=1 S ORDERS=$E(DUP(DUPJ),1,DUPLEN)
 ..E  S ORDERS=ORDERS_", "_$E(DUP(DUPJ),1,DUPLEN)
 K ^TMP("ORR",$J)
 Q ORTN_U_$G(ORDERS)
 ;
