DIFROMSY ;SFIRMFO/DCM/TKW-MOVE KEY FILE ENTRIES ;12:32 PM  31 Oct 2001
 ;;22.2;VA FileMan;;Jan 05, 2016;Build 42
 ;;Per VA Directive 6402, this routine should not be modified.
 ;;Submitted to OSEHRA 5 January 2015 by the VISTA Expertise Network.
 ;;Based on Medsphere Systems Corporation's MSC FileMan 1051.
 ;;Licensed under the terms of the Apache License, Version 2.0.
 ;
DDKEYOUT(DIFRFILE,DIFRF2,DIFRTA) ; retrieve KEY entries for file
 ; DIFRFILE=top level file number
 ; DIFRF2=current file/subfile number
 ; DIFRTA=Global reference of transport global
 N DINODE,DIFRNAME,DIFRDO,DIFRD1,DIFRF,DIFRFLD,DIOUT,X,Y,DICNT1,DICNT2
 S DIFRNAME="",DIOUT=0
 F  S DIFRNAME=$O(^DD("KEY","BB",DIFRF2,DIFRNAME)) Q:DIFRNAME=""  D  Q:DIOUT
 . S DIFRD0=$O(^DD("KEY","BB",DIFRF2,DIFRNAME,0)) Q:'DIFRD0
 . S (DIFRD1,DICNT1,DICNT2)=0
 . F  S DIFRD1=$O(^DD("KEY",DIFRD0,2,DIFRD1)) Q:'DIFRD1  D  Q:DIOUT
 . . S X=$G(^DD("KEY",DIFRD0,2,DIFRD1,0))
 . . S DIFRF=$P(X,U,2),DIFRFLD=$P(X,U)
 . . I 'DIFRF!('DIFRFLD) Q
 . . S DICNT1=DICNT1+1,X=$$FNO^DILIBF(DIFRF)
 . . I '$D(@DIFRTA@("^DD",X,DIFRF,DIFRFLD)) D  Q
 . . . Q:'DIFRFDD&($G(@DIFRTA@("FIA",X,DIFRF))'=0)
 . . . D ERR1^DIFROMSX(DIFRF,DIFRFLD,DIFRNAME,"KEY") Q
 . . S DICNT2=DICNT2+1
 . Q:DIOUT  I DICNT2=0,'DIFRFDD Q
 . ;I DICNT1'=DICNT2 D ERR2^DIFROMSX(DIFRF2,DIFRNAME,"KEY") Q
 . M @DIFRTA@("KEY",DIFRFILE,DIFRF2,DIFRNAME)=^DD("KEY",DIFRD0)
 . S X=$NA(@DIFRTA@("KEY",DIFRFILE,DIFRF2,DIFRNAME,2))
 . F Y="B","BB","S" K @X@(Y)
 . K @DIFRTA@("KEY",DIFRFILE,DIFRF2,DIFRNAME,DIFRD0,3.1,"B")
 . D IXPTR Q
 Q
IXPTR ; export index pointer
 N DIIXPTR S DIIXPTR=$P(^DD("KEY",DIFRD0,0),U,4)
 I 'DIIXPTR D ERR1(9546,DIFRF2,DIFRNAME) Q
 N X,Y S X=$G(^DD("IX",DIIXPTR,0)),Y=$P(X,U,2),X=$P(X,U)
 I (+$P(X,"E")'=X)!(Y="") D ERR1(9546,DIFRF2,DIFRNAME) Q
 S @DIFRTA@("KEYPTR",DIFRFILE,DIFRF2,DIFRNAME)=X_"^"_Y
 Q
 ;
DDKEYIN(DIFRFILE,DIFRF2,DIFRSA) ;
 ; DIFRFILE=top level file#
 ; DIFRF2=current file/subfile#
 ; DIFRSA=global reference of transport global
 I '$D(^DD(.31)) N DIFRER S DIFRER("FILE")=.31 D BLD^DIALOG(401,.DIFRER) Q
 N DIFRIN,DIFRNAME,DIFRD1,DIOUT,DIFRIN1,DIFRF,DIFRFLD,DIFRKPTR,X
 S DIFRIN=$NA(@DIFRSA@("KEY",DIFRFILE,DIFRF2))
 S DIFRNAME=""
 F  S DIFRNAME=$O(@DIFRIN@(DIFRNAME)) Q:DIFRNAME=""  D
 . S (DIFRD1,DIOUT)=0,DIFRIN1=$NA(@DIFRIN@(DIFRNAME))
 . F  S DIFRD1=$O(@DIFRIN1@(2,DIFRD1)) Q:'DIFRD1  D  Q:DIOUT
 . . S X=$G(@DIFRIN1@(2,DIFRD1,0))
 . . S DIFRF=$P(X,U,2),DIFRFLD=$P(X,U)
 . . I 'DIFRF!('DIFRFLD) Q
 . . I '$D(^DD(DIFRF,DIFRFLD,0)) D ERR3^DIFROMSX(DIFRF,DIFRFLD,DIFRNAME,"KEY")
 . . Q
 . Q:DIOUT
 . S X=$G(@DIFRSA@("KEYPTR",DIFRFILE,DIFRF2,DIFRNAME)) D  Q:DIOUT
 . . I X="" D ERR1(9547,DIFRF2,DIFRNAME) Q
 . . S DIFRKPTR=$O(^DD("IX","BB",$P(X,U),$P(X,U,2),0))
 . . I 'DIFRKPTR D ERR1(9547,DIFRF2,DIFRNAME) Q
 . . S $P(@DIFRIN1@(0),U,4)=DIFRKPTR Q
 . N DIEN,DIK,DA,DIC,DO
 . S DIEN=$O(^DD("KEY","BB",DIFRF2,DIFRNAME,0))
 . I DIEN D  N DINUM S DINUM=DIEN
 . . S DIK="^DD(""KEY"",",DA=DIEN N DIEN D ^DIK Q
 . S DIC="^DD(""KEY"",",DIC(0)="L",DIC("DR")=".02///^S X="_""""_DIFRNAME_"""",X=DIFRF2 D FILE^DICN S DIEN=+Y
 . I DIEN'>0 D ERR4^DIFROMSX(DIFRF2,DIFRNAME,"KEY") Q
 . M ^DD("KEY",DIEN)=@DIFRIN1
 . K DIK,DA S DIK="^DD(""KEY"",",DA=DIEN D IX1^DIK
 . Q
 Q
 ;
ERR1(DIER,DIFRF2,DIFRNAME) ;
 N DIFRER S DIFRER(1)=DIFRNAME
 S DIFRER(2)=DIFRF2
 D BLD^DIALOG(DIER,.DIFRER) S DIOUT=1 Q
 ;
 ;9543  Field |1| of file |2|, part of '|3|' |4| entry, is missing from the transport global...
 ;9545  |1| entry |2| is not installed.  The REFERENCE FIELD |3| in file |4| does not exist on the system.
 ;9546  KEY '|1|' for file |2| cannot be transported, problem with Uniqueness Index for the KEY.
 ;9547  Key '|1|' for file |2| not installed.  Pointer to Uniqueness Index cannot be resolved.
 ;9549  |1| "|2|" on file |3| not installed, FILE^DICN call failed.
 ;
