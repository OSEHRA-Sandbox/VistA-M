DDS02 ;SFISC/MKO-OVERFLOW FROM ^DDS01 ;2015-01-02  4:45 PM
 ;;22.2;VA FileMan;;Jan 05, 2016;Build 42
 ;;Per VA Directive 6402, this routine should not be modified.
 ;;Submitted to OSEHRA 5 January 2015 by the VISTA Expertise Network.
 ;;Based on Medsphere Systems Corporation's MSC FileMan 1051.
 ;;Licensed under the terms of the Apache License, Version 2.0.
 ;
REFRESH(DDSPG) ;Refreshes the setup for page
 N B,D,I,DIE,DDSDA,DDP
 F B=0:0 S B=$O(@DDSREFT@(DDSPG,B)) Q:'B  D
 .I '$D(DDSDA) S DDSDA=^(B),DIE=^(B,DDSDA,"GL"),DDP=$P(@DDSREFS@(DDSPG,B),U,3) ;GET THE ORIGINAL PAGE DATA
 .S D="" F  S D=$O(@DDSREFT@(DDSPG,B,D)) Q:D=""  I +$G(^(D))=1 S $P(^(D),U)=0 ;REMEMBER TO RELOAD BLOCKS ON THIS PAGE!
 .S I="" F  S I=$O(@DDSREFT@("F0",I)) Q:I=""  F  S D=$O(@DDSREFT@("F0",I,D)) Q:D=""  I $P(D,",",2)=B,$G(^(D,"F"))=3 K @DDSREFT@("F0",I,D) ;KILL OLD FORM-ONLY VALUE
 I $D(D) D EN^DDS1(DDSPG)
 Q
 ;
 ;
 ;
SV ;Save
 S DDACT="N"
 I $G(DDSDN)=1,DDO D ERR3^DDS3 Q
 I DDSSC'>1,'$P(DDSSC(DDSSC),U,4) D S^DDS3 Q  ;INCLUDED '$G(DDSSEL)
 D MSG^DDSMSG($$EZBLD^DIALOG(3093),1) ;**CANNOT SAVE
 Q
 ;
EXT ;Process external form
 I '$P($G(DDSU("DD")),U,2),$P($G(DDSU("DD")),U,2)["P" D PT
 I $P($G(DDSO(0)),U,3)=2,$E($P($G(DDSO(20)),U))="P" D PTFO
 ;
 S:DDSOLD=Y DIR0N=1
 S DDSX=X,DDSY=Y
 I Y]"",$P($G(DDSU("DD")),U,2)["O",$G(^DD(DDP,DDSFLD,2))'?."^" K Y(0) X ^(2) S Y(0)=Y
 ;
 S DDSEXT=$G(Y(0,0),$G(Y(0),Y)),X=DDSY
 ;
 I $D(DDSO(14)) K DDSERROR X DDSO(14) I $D(DDSERROR)#2 D  Q
 . K DDSERROR,DDSY S DIR0("L")=DDSEXT,DDSCHKQ=1
 ;
 I DDSY="",DDSFLD'=.01 D  Q:'$D(DDSY)
 . N DDSREQ,DDSKEY
 . S DDSREQ=$P($G(DDSU("A")),U)
 . S:DDSREQ="" DDSREQ=$P($G(DDSO(4)),U)
 . S:DDSREQ="" DDSREQ=$P($G(DDSU("DD")),U,2)["R"
 . S DDSKEY=$D(^DD("KEY","F",DDP,DDSFLD))>0
 . I 'DDSREQ,'DDSKEY Q
 . K DDSY
 . S DDSCHKQ=1,DIR0("L")=DDSEXT
 . D MSG^DDSMSG($$EZBLD^DIALOG($S(DDSKEY:3092.2,1:3092.1)),1) ;'REQUIRED KEY FIELD'
 ;
 S DY=$P(DIR0,U),DX=$P(DIR0,U,2)
REPNT I DDSEXT'=DDSX!$G(DDSREPNT) D  K DDSREPNT ;WRITE OUT NEW VALUE, IF IT DIFFERS FROM WHAT WAS INPUT
 . X IOXY
 . S DDSX=$E(DDSEXT,1,$P(DIR0,U,3))
 . I '$P(DIR0,U,6) S DDSX=DDSX_$J("",$P(DIR0,U,3)-$L(DDSEXT))
 . E  S DDSX=$J("",$P(DIR0,U,3)-$L(DDSEXT))_DDSX
 . W $P(DDGLVID,DDGLDEL)_DDSX_$P(DDGLVID,DDGLDEL,10)
 ;
CHECKEY I $G(DDSU("K")),DDSY]""!(DDSFLD'=.01) D  Q:'$D(DDSY)  ;CHECK KEY
 . N DDSFXR,DDSUI,DDSUNIQ,DDSVSV,DIIENS
 . D LOADXREF^DIKC1(DDP,"","",DDSU("K"),$NA(@DDSREFT@("F"))_"_","DDSFXR")
 . S:$D(@DDSREFT@("F"_DDP,DDSDA,DDSFLD,"D"))#2 DDSVSV=^("D") S ^("D")=DDSY
 . S DDSUNIQ=1,DDSUI=0
 . F  S DDSUI=$O(DDSFXR(DDP,DDSUI)) Q:'DDSUI  D  Q:'DDSUNIQ
 .. S DIIENS=DDSDA
 .. D SETXARR^DIKC(DDP,DDSUI,"DDSFXR","","D")
 .. S DDSUNIQ=$$UNIQUE^DIKK2(DDP,DDSUI,.X,.DA,"DDSFXR")
 . I 'DDSUNIQ D
 .. K DDSY
 .. S DDSCHKQ=1,DIR0("L")=DDSEXT
 .. D MSG^DDSMSG($$EZBLD^DIALOG(3094),1) ;"Another Entry already exists with this KEY value."
 .. K @DDSREFT@("F"_DDP,DDSDA,DDSFLD,"D") S:$D(DDSVSV)#2 ^("D")=DDSVSV
 ;
 D:$G(DDSDA)!'$D(DDSREP)
 . S:$D(Y(0)) @DDSREFT@("F"_DDP,DDSDA,DDSFLD,"X")=DDSEXT
 . S @DDSREFT@("F"_DDP,DDSDA,DDSFLD,"D")=DDSY I DDSY="",$D(DDSU("X")) S ^("X")="" ;CHANGE THE DATA!
 K DDSY
 Q
 ;
DEC(FILE,FIELD,DEC) ;NOT USED (??)
 S DEC="S X=$G(@DDSREFT@(""F"_FILE_""",DIIENS,"_FIELD_",""D""),"_$E(DEC,5,999)_")"
 Q
 ;
PT ;Modify Y for pointer type fields
 I $P(Y,U,3)=1 D
 . S ^("ADD")=$G(@DDSREFT@("ADD"))+1,^("ADD",^("ADD"))=+Y_","_U_$P(DDSU("DD"),U,3)
 S Y=$P(Y,U)
 Q
 ;
PTFO ;Modify Y for pointer type form only fields
 I $P(Y,U,3)=1 D
 . N R,I S R=""
 . F I=1:1 Q:$D(DA(I))[0  S R=R_DA(I)_","
 . S ^("ADD")=$G(@DDSREFT@("ADD"))+1,@DDSREFT@("ADD",@DDSREFT@("ADD"))=+Y_","_R_$S($P(DDSO(20),U,3):^DIC(+$P(DDSO(20),U,3),0,"GL"),1:U_$P($P(DDSO(20),U,3),":"))
 S Y=$S(Y=-1:"",1:$P(Y,U))
 Q
