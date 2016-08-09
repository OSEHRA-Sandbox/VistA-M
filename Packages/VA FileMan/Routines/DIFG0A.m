DIFG0A ;SFISC/DG(OHPRD)-CALLED FOR CONTEXT SWITCH ;8MAR2006
 ;;22.2;VA FileMan;;Jan 05, 2016;Build 42
 ;;Per VA Directive 6402, this routine should not be modified.
 ;;Submitted to OSEHRA 5 January 2015 by the VISTA Expertise Network.
 ;;Based on Medsphere Systems Corporation's MSC FileMan 1051.
 ;;Licensed under the terms of the Apache License, Version 2.0.
 ;
SWITCH ;CONTEXT SWITCH
 N DIC,DIFGM,DIFGNDC,DA,DIFGINCR,DIFGSKIP,DIFGDI,DIFGMO,DIFGPOIN
 S DIFG=DIFG+1,(DIFGNDC,DIFGLAGO)=0
 S DIFGTYPE="FILE"
 D BASE^DIFG0
 I DIFGER G X1
 D FUNC^DIFG0
 I '$D(DIFG("NOLKUP")) D BEGEND
 I DIFGER G X1
 D SET
 D KILLVAR0
 D FILE^DIFG
 S DIFG=DIFG-1
 D KILLVAR
X1 Q
 ;
BEGEND ;CALL DIFG3 TO PROCESS BEGIN-END BLOCK
 I "AL"[$P(DIFGMO(DIFGMULT),U) S DIFGSECP=$P(^DD(DIC,.01,0),U,2) S:DIFGSECP["P" DIFGPOIN="" I DIFGSECP'["'"!($D(DIFGENV("LAYGO",DIC,.01))) S DIFGLAGO=1
 D ^DIFG3
 Q
 ;
SET ;
 I '$D(DIFGSKIP(DIFGMULT)),$D(^UTILITY("DIFG",$J,DIFGINCR,DIC)),'$D(^(DIC,"DA")) S ^UTILITY("DIFG",$J,DIFGINCR,DIC,"DA")=+Y,^("DR")=""
 I $D(DIFGSKIP(DIFGMULT)) S ^UTILITY("DIFG",$J,DIFGINCR,DIC,"DA")=DIFGALNK S:'$D(DIFGFLUS) ^("X")=$S($E(X)="`":$E(X,2,245)_"^N",X[("^UTILITY(""DIFG@"","_$J):X_"^N",1:X)
 I $D(DIFGFLUS),$P(DIFGMO(DIFGMULT),U)="L" S $P(^UTILITY("DIFG",$J,DIFGINCR,DIC,"MODE"),U)="M"
 S ^UTILITY("DIFG",$J,DIFGINCR,DIC,"GL")=^DIC(DIC,0,"GL"),(DA,DIFGDA(0))=DIFGALNK I $D(^("DIC(""DR"")")) S ^("MODE")="A"_"^"_$P(^("MODE"),U,2)
X2 K DIFGFLUS Q
 ;
KILLVAR0 ;KILL VARIABLES AFTER LOOKUP FOR FILE ON THE WAY TO FIELDS
 K DIFGALNK,DIFGO(DIFGMULT),DIFGFLD,DIFGPC,DIFGVAL,DIFGDOL,DIFGNUMF,DIFGNOLK,DIFGLAGO,Y,DIFG("NOLKUP")
 Q
 ;
KILLVAR ;KILL VARIABLES AFTER EACH CONTEXT SWITCH
 K DIFGDA,DIFGDIC,DIFGDOL,DIFGFIND,DIFGFIRP,DIFGFLDN,DIFGHAT,DIFGMLND,DIFGNODE,DIFGNUM,DIFGNUMF,DIFGPC,DIFGPTER,DIFGSECP,DIFGSTRT,DIFGVAL,DIFGNDC,DIFGM,DIFGFLD,DIFGDIC,DIFGSAVE,DIFGSVVL
 K:$P($G(DIFGMO(DIFGMULT)),U,2)]"" DIFGMOLK($P(DIFGMO(DIFGMULT),U,2))
 K DIFGSKIP
 Q
 ;
