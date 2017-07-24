DDSZ1 ;SFISC/MKO-GET BLOCK INFO,SCREEN IMAGE ;20JAN2004
 ;;22.2;MSC Fileman;;Jan 05, 2015;
 ;;Submitted to OSEHRA 5 January 2015 by the VISTA Expertise Network.
 ;;Based on Medsphere Systems Corporation's MSC Fileman 1051.
 ;;Licensed under the terms of the Apache License, Version 2.0.
 ;;GFT;**999,1003,1004**
 ;
EN(DDSPG,DDSBK,DDP,DDSBY,DDSBX,DDSBO,DDSTP,DDSREP,DDSNDD,DDSPGRP,DDSSCR,DDSNAV,DDSORD,DDSRNAV) ;
 ;Input:
 ;  DDSREFS = Global ref
 ;Output:
 ;  DDSSCR
 ;  DDSNAV
 ;  DDSORD
 ;  DDSRNAV
 ;
 N Y
 S:$G(DDSTP)="" DDSTP="e"
 I DDSTP'="h",$G(DDSBO),$D(DDSORD(DDSBO))[0 D
 . S DDSORD(DDSBO)=DDSBK
 . S:$G(DDSREP)>1 $P(DDSORD(DDSBO),U,2)=$S($P(DDSREP,U,5)]"":$P($$GETFLD^DDSLIB($P(DDSREP,U,5),"","","","",DDSBK),","),1:"FIRST")
 ;
LOOP N DDSHITE S DDSHITE=$$HITE^DDSR(DDSBK),DDSF=0 ;DJW/GFT  HEIGHT OF MULTIPLES
 F  S DDSF=$O(^DIST(.404,DDSBK,40,DDSF)) Q:DDSF'=+DDSF  D FLD
 ;
KILL K DDSC1,DDSC2,DDSCAP,DDSCLN,DDSD1,DDSD2,DDSD3
 K DDSDDL0,DDSF,DDSFLD,DDSKEY,DDSL0,DDSL01,DDSL2,DDSL4,DDSN
 Q
 ;
FLD ;Set up
 ;  @DDSREFS@(pg,bk,ddo,
 ;    "D")       = data $Y^data $X^data $L^field#
 ;                  ^xcap $Y^xcap $X^xcap colon^xcap req
 ;                  ^1 if computed field^1 if right justified
 ;    "COMPE")   = M code that sets X
 ;    "COMPE",1) = array sets DDSE(n)
 ;
 ;  @DDSREFS@("Ffile#",field#,"L",pg,bk,ddo)=""
 ;
 ;  DDSSCR(row)     = captions on that row
 ;  DDSSCR(row,col) = final columns underlined
 ;  DDSNAV(row,col) = ddo,bk for editable fields
 ;  DDSORD(bo,fo)   = ddo for editable fields
 ;
 ;Get field properties
 S:'$P(^DIST(.404,DDSBK,40,DDSF,0),U,3) $P(^(0),U,3)=3
 S DDSL0=$G(^DIST(.404,DDSBK,40,DDSF,0)),DDSL01=$G(^(.1)),DDSFLD=$S($P(DDSL0,U,3)=2:DDSF_","_DDSBK,1:+$G(^(1))),DDSL2=$G(^(2)),DDSL4=$G(^(4))
 K:$P(DDSL0,U,3)=3!'$P(DDSL0,U,3) DDSNDD ;REMEMBER THAT AT LEAST ONE FIELD IS A DATA DICTIONARY
 S DDSDDL0=$G(^DD(DDP,DDSFLD,0)) Q:DDSL0?."^"!(DDSL2?."^")
 S DDSKEY=DDSFLD'[","&($D(^DD("KEY","F",DDP,DDSFLD))>1)
 S DDSD1=$P($P(DDSL2,U),",")+DDSBY-1
 S DDSD2=$P($P(DDSL2,U),",",2)+DDSBX-1
 S DDSD3=$P(DDSL2,U,2)
 S DDSC1=$P($P(DDSL2,U,3),",")+DDSBY-1
 S DDSC2=$P($P(DDSL2,U,3),",",2)+DDSBX-1
 S DDSCAP=$TR($P(DDSL0,U,2)," ",$C(0))
 S DDSCLN=$S(DDSCAP="":"",$P(DDSL0,U,3)=1:"",$P(DDSL2,U,4):"",1:":")
 ;
 I DDSC1'<0,DDSC2'<0,$L(DDSCAP)>0,DDSCAP'="!M" D
 . ;Set CAP xref for ^-jumping
 . I DDSTP="e","^2^3^"[(U_$P(DDSL0,U,3)_U)!'$P(DDSL0,U,3) D
 .. N C,I,L
 .. S I=0 F  S I=$O(DDSPGRP(I)) Q:'I  Q:U_DDSPGRP(I)_U[(U_DDSPG_U)
 .. Q:'I
 .. S C=$P(DDSL0,U,2)
 .. S:C?1"Select ".E C=$P(C,"Select ",2,999)
UP .. S C=$E($$UP^DILIBF(C),1,40)
 .. S L=$L(DDSREFS)+$L(C)+$L(DDSPGRP(I))+$L(DDSPG)+$L(DDSBK)+$L(DDSF)+30
 .. S:L>127 C=$E(C,1,$L(C)-(L-127))
 .. S:C]"" @DDSREFS@("CAP",C,DDSPGRP(I),DDSPG,DDSBK,DDSF)=""
 . ;
 . ;Set DDSSCR
 . I DDSC1'<0,DDSC2'<0,$L(DDSCAP)>0,DDSCAP'="!M" D
 .. N DDSI,DDSX
 .. S DDSX=DDSCAP_DDSCLN
 .. F DDSI=1:1:+DDSREP D
CAPS ... S $E(DDSSCR(DDSI-1*DDSHITE+1+DDSC1),DDSC2+1,DDSC2+$L(DDSX))=DDSX ;GFT
 ... S:$S($P(DDSL4,U)]"":+DDSL4,1:$P(DDSDDL0,U,2)["R")!DDSKEY DDSSCR(DDSI-1*DDSHITE+1+DDSC1,DDSC2+1)=DDSC2+$L(DDSCAP)
 ;
 ;Set "D", "L" nodes, DDSNAV, and DDSORD
 I DDSD1'<0,DDSD2'<0,DDSD3>0 D
 . S @DDSREFS@(DDSPG,DDSBK,DDSF,"D")=DDSD1_U_DDSD2_U_DDSD3_U_DDSFLD
 . S @DDSREFS@("F"_$S(DDSFLD[",":0,1:DDP),DDSFLD,"L",DDSPG,DDSBK,DDSF)=""
 I DDSCAP="!M",DDSC1'<0,DDSC2'<0 S $P(@DDSREFS@(DDSPG,DDSBK,DDSF,"D"),U,5,8)=DDSC1_U_DDSC2_U_DDSCLN_U_($P(DDSDDL0,U,2)["R"!+DDSL4!DDSKEY)
 S:$P(DDSL4,U,3) $P(@DDSREFS@(DDSPG,DDSBK,DDSF,"D"),U,10)=1
 ;
 ;Computed fields
 I $P(DDSL0,U,3)=4 D  K DDSCOMP,DDSAR,DDSEXP,DDSFD Q
 . S DDSCOMP=$G(^DIST(.404,DDSBK,40,DDSF,30)) Q:DDSCOMP?."^"
 . D PARSE^DDSCOMP(DDP,DDSCOMP,DDSBK,.DDSEXP,.DDSAR,.DDSFD)
 . Q:DDSEXP=""!$G(DIERR)
 . S @DDSREFS@("COMPE",DDSBK,DDSF)=DDSEXP
 . F DDSAR=1:1:DDSAR D
 .. S:DDSAR(DDSAR)["*DDSREFC*" DDSAR(DDSAR)=$P(DDSAR(DDSAR),"*DDSREFC*")_$E(DDSREFS,1,$L(DDSREFS)-1)_",""COMPE"","_DDSBK_","_DDSF_","_DDSAR_$P(DDSAR(DDSAR),"*DDSREFC*",2,999)
 .. S @DDSREFS@("COMPE",DDSBK,DDSF,DDSAR)=DDSAR(DDSAR)
 .. I $D(DDSAR(DDSAR))>9 N I F I=1:1 Q:$D(DDSAR(DDSAR,I))[0  D
 ... S @DDSREFS@("COMPE",DDSBK,DDSF,DDSAR,I)=DDSAR(DDSAR,I)
 . S $P(@DDSREFS@(DDSPG,DDSBK,DDSF,"D"),U,9)=1
 . I $G(DDSFD)]"" F DDSAR=1:1:$L(DDSFD,U) D
 .. N F S F=$P(DDSFD,U,DDSAR) Q:F=""
 .. S @DDSREFS@("COMP",$P(F,","),$P($P(F,",",2,99),";"),DDSPG,DDSBK,DDSF)=""
 ;
 Q:DDSD1<0!(DDSD2<0)!(DDSD3'>0)!(DDSL2?."^")
 Q:$P(DDSDDL0,U,4)=" ; "  Q:DDSTP="h"  Q:DDSFLD=.001
 I '$P(DDSDDL0,U,2),DDSTP'="e" Q
 ;
 S DDSORD(DDSBO,+DDSL0)=DDSF
 S DDSNAV(DDSD1,DDSD2)=DDSF_","_DDSBK
 S:$P(DDSDDL0,U,2) DDSMUL(DDSBK,DDSF)=""
 ;
 I $G(DDSREP)>1 D
 . S $P(DDSNAV(DDSD1,DDSD2),",",3)=DDSBO
 . S DDSRNAV(DDSBO,DDSD1)=DDSBK
 . S DDSRNAV(DDSBO,DDSD1,DDSD2)=DDSF
HITE . S DDSRNAV(DDSBO,DDSD1-.4,DDSD2)=DDSF_",-1" ;DJW/GFT??
 . S DDSRNAV(DDSBO,DDSD1+.4,DDSD2)=DDSF_",+1"
 Q
