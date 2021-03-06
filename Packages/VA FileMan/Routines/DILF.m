DILF ;SFISC/STAFF-LIBRARY OF FUNCTIONS ;7:08 AM  25 Apr 2006
 ;;22.0;VA FileMan;**147**;Mar 30, 1999;Build 1
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 Q
 ;
 ;
LOCK(REF) ;
 ; LOCK the REFerence.  $T must be checked upon return  **147
 I '$D(DILOCKTM) S DILOCKTM=$G(^DD("DILOCKTM"),1) I $D(@REF) ;TO GET NAKED BACK
 LOCK @("+"_REF_":DILOCKTM")
 Q
 ;
 ;
 ;
CREF(X) G ENCREF^DIQGU
 ;
OREF(X) G ENOREF^DIQGU
 ;
FDA(DIEFF,DIEFDAS,DIEFFLD,DIEFFLG,DIEFVAL,DIEFAR,DIEFOUT) ;
 G LOADX^DIEF1
 ;
CLEAN ;
 G CLEAN^DIEFU
 ;
IENS(DIEFDA) ;
 G IENX^DIEFU
 ;
DA(DAIEN,DATARG) ;
 G DAX^DIEFU
 ;
DT(DIEFDT,DIEFX,DIEFY,DIEFDT0,DIOUTAR) ;
 G DTX^DIEFU
 ;
VALUES(DILFILE,DILFLD,DILFDA,DILOUT) ;
 I $G(DILFILE)=""!($G(DILFLD)="")!($G(DILFDA)="") S DILOUT=0 Q
 K DILOUT
 N DILCNT,DILIEN
 S DILIEN=""
 D VALLOOP
 S DILOUT=DILCNT
 Q
 ;
VALLOOP ;
 S DILCNT=0
 F  S DILIEN=$O(@DILFDA@(DILFILE,DILIEN)) Q:DILIEN=""  D
 . I $D(@DILFDA@(DILFILE,DILIEN,DILFLD)) D
 . . S DILCNT=DILCNT+1
 . . S DILOUT(DILCNT)=@DILFDA@(DILFILE,DILIEN,DILFLD)
 . . S DILOUT(DILCNT,"IENS")=DILIEN
 Q
 ;
VALUE1(DILFILE,DILFLD,DILFDA) ;
 I $G(DILFILE)=""!($G(DILFLD)="")!($G(DILFDA)="") Q "^"
 N DILIEN
 S DILIEN=$O(@DILFDA@(DILFILE,""))
 I DILIEN="" Q "^"
 I $D(@DILFDA@(DILFILE,DILIEN,DILFLD)) Q @DILFDA@(DILFILE,DILIEN,DILFLD)
 N DILCNT,DILOUT
 D VALLOOP
 I DILCNT Q DILOUT(1)
 Q "^"
 ;
ROUSIZE() ;
 Q $G(^DD("ROU"))
 ;
HTML(DISTRING,DIRECTN) ;
 ;
 ; entry point: use HTML to encode or decode ^ and & characters ; TOAD
 ; extrinsic function: return encoded or decoded value
 ;
H1 N DILONG,DIRULE I $G(DIRECTN,1)=1 D  Q:$G(DILONG) ""
 . S DIRULE(1,"&")="&amp;",DIRULE(2,"^")="&#94;"
 . N DIL S DIL=$L(DISTRING,"^")+$L(DISTRING,"&")-2
 . I $L(DISTRING)-DIL+(DIL*5)>255 D ERR^DICU1(207,,,,DISTRING) S DILONG=1 Q
 E  S DIRULE(1,"&#94;")="^",DIRULE(2,"&amp;")="&"
 Q $$TRANSL8(DISTRING,.DIRULE)
 ;
TRANSL8(DISTRING,DIRULES) ;
 ;
 ; HTML: $TRANSLATE for substrings instead of characters ; TOAD
 ; extrinsic function: return translated value
 ;
T1 N DIFRENCE,DIFROM,DILENGTH,DITO
 N DI S DI="" F  S DI=$O(DIRULES(DI)) Q:DI=""  D
 . S DIFROM=$O(DIRULES(DI,"")) Q:DISTRING'[DIFROM
 . S DITO=DIRULES(DI,DIFROM)
 . S DILENGTH=$L(DIFROM)
 . S DIFRENCE=$L(DITO)-DILENGTH
 . S DIAT=0 F  D  Q:'DIAT
 . . S DIAT=$F(DISTRING,DIFROM,DIAT) Q:'DIAT
 . . S $E(DISTRING,DIAT-DILENGTH,DIAT-1)=DITO
 . . S DIAT=DIAT+DIFRENCE
 Q DISTRING
