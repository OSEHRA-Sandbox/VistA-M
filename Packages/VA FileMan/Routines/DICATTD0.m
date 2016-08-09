DICATTD0 ;GFT/GFT - CREATE WORD-PROCESSING ATTRIBUTES IN SCREENMAN ;01:06 PM  9 Jan 1999
 ;;22.2;VA FileMan;;Jan 05, 2016;Build 42
 ;;Per VA Directive 6402, this routine should not be modified.
 ;;Submitted to OSEHRA 5 January 2015 by the VISTA Expertise Network.
 ;;Based on Medsphere Systems Corporation's MSC FileMan 1051.
 ;;Licensed under the terms of the Apache License, Version 2.0.
 ;
WORD(DICT) ;DICT=21 or 23  for DESCRIPTION and TECHNICAL DESCRIPTION
 N DIC,DUOUT,DTOUT,DICR,DIWETXT
 D DICR
 I $D(@DICR)=0 M @DICR=^DD(DICATTA,DICATTF,DICT) S DICATTDW(DICT)=1 I $D(@DICR)=0 S @DICR@(0)=0
 S DIWETXT="Editing '"_$P(^DD(DICATTA,DICATTF,0),U)_"' "_$P(^DD(0,DICT,0),U)
 S DIC=$P(DICR,")")_"," D EN^DIWE
 I $D(DUOUT)!$D(DTOUT) K @DICR,DICATTDW(DICT) W $$EZBLD^DIALOG(8077)
 S DDSCHG=1 Q
 ;
DICR S DICR="^UTILITY(""DICATTD"",$J,DICT)" Q
 ;
 ;
FILEWORD ;when we're done
 N DICT,DICR
 D DICR
 F DICT=21,23 D
 .I $D(DICATTDW(DICT)) K ^DD(DICATTA,DICATTF,DICT) M ^DD(DICATTA,DICATTF,DICT)=@DICR
 Q
