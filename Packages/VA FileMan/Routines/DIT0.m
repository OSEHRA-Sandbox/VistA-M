DIT0 ;SFISC/GFT,XAK-PREPARE TO XFR ;15FEB2013
 ;;22.2;VA FileMan;;Jan 05, 2016;Build 42
 ;;Per VA Directive 6402, this routine should not be modified.
 ;;Submitted to OSEHRA 5 January 2015 by the VISTA Expertise Network.
 ;;Based on Medsphere Systems Corporation's MSC FileMan 1051.
 ;;Licensed under the terms of the Apache License, Version 2.0.
 ;
 N Y,DIC,DIT0KILL S DIT=DDF(1),DIC=L,DIC(0)="EQLAM",X="DATA INTO WHICH " D LK
 G Q:Y<0 S DFR=+Y,DTO(1)=DIC_+Y_",",DIC(0)="EQAM",X="FROM ",DIC("S")="I Y-"_+Y D LK G Q:Y<0
S S (D0,DA)=+Y W ! D  G Q:%<0 S (DH,DIT0KILL)=2-% I '% D F^DIT G S
 .I $D(^DD(DIT,.01,"DEL",1,0)) X ^(0) I  S %=2 Q
 .S %=2 W "   WANT TO DELETE THIS ENTRY AFTER IT'S TRANSFERRED" D YN^DICN
 S ^UTILITY("DIT",$J,+Y)=DFR_";"_$E(DIC,2,999)
 S DTO=0,DIK=DIC,DFR(1)=DIC_DA_"," K DIC D WAIT^DICD
GO D GO^DITR
 S DIT=DH D KL^DIT,^DIK:$G(DIT0KILL) S DA=DFR K DFR D IX1^DIK ;DELETE OLD ENTRY, CONDITIONALLY
 S DH=DIT D ASK^DITP,PTS^DITP:%=1
Q G Q^DIT
 ;
LK S DIC("A")="TRANSFER "_X_DFL G ^DIC
 ;
EN ; PROGRAMMER CALL
 ; DIT("F") = GLOBAL ROOT OR FILE # OF FILE TO TRANSFER FROM
 ; DIT("T") = GLOBAL ROOT OR FILE # OF FILE TO TRANSFER TO
 ; DA("F")  = ENTRY # IN FILE TO TRANSFER FROM
 ; DA("T")  = ENTRY # IN FILE TO TRANSFER TO
 ;
 N DIT0KILL
 I '$D(DIT("F"))!'$D(DIT("T"))!'$D(DA("F"))!'$D(DA("T")) G FIN
 S DDF(1)=DIT("F"),DDT(0)=DIT("T")
 I 'DDF(1) S DDF(1)=$S($D(@(DDF(1)_"0)"))#2:+$P(^(0),U,2),1:0) G FIN:'DDF(1) S DFR(1)=DIT("F")
 I 'DDT(0) S DDT(0)=$S($D(@(DDT(0)_"0)"))#2:+$P(^(0),U,2),1:0) G FIN:'DDT(0) S DTO(1)=DIT("T") G C
 G FIN:'$D(^DIC(+DDF(1),0,"GL")) S DFR(1)=^("GL")
 G FIN:'$D(^DIC(+DDT(0),0,"GL")) S DTO(1)=^("GL")
C S DB=DA("F"),(DB1,DFR)=DA("T"),DIK=DTO(1)
 I $D(DA(1)) F I=1:1 G:'$D(DA(I)) SET S DRF(I)=$P(DA(I),",",1)_",1,",DOT(I)=$P(DA(I),",",2)_",1,"
DON K DRF,DOT S DFR(1)=DFR(1)_DB_",",DTO(1)=DTO(1)_DB1_",",DKP=1,DMRG=1,DTO=0,DH=0,DIT0KILL=0 G GO
SET F I=I-1:-1 G:I'>0 DON S DFR(1)=DFR(1)_DRF(I),DTO(1)=DTO(1)_DOT(I)
FIN ;
 K DDF,DFR,DDT,DTO
 Q
