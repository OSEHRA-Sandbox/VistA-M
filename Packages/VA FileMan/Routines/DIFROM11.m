DIFROM11 ;SFISC/XAK-CREATES RTN ENDING IN INIT1 ;APR 13, 1995@14:31
 ;;22.2;VA FileMan;;Jan 05, 2016;Build 42
 ;;Per VA Directive 6402, this routine should not be modified.
 ;;Submitted to OSEHRA 5 January 2015 by the VISTA Expertise Network.
 ;;Based on Medsphere Systems Corporation's MSC FileMan 1051.
 ;;Licensed under the terms of the Apache License, Version 2.0.
 ;
 S %Y="^UTILITY(U,$J,D,Y,",E=0
 F D="DIE","DIPT","DIBT" S %X=U_D_"(Y,",Y=0 F  S @("Y=$O(^"_D_"(Y))") Q:'Y  I $D(^(Y,0))#2 S DSV=^(0),F=$P(DSV,U,4) I F,$P(DSV,U,8)<3,$D(F(F))!$D(Q(D,Y)) D 1
 S D="DIST(.403,",%X=U_D_"Y,",Y=0 F  S Y=$O(^DIST(.403,Y)) Q:'Y  I $D(^(Y,0))#2 S DSV=^(0),F=$P(DSV,U,8) I F,$D(F(F))!$D(Q("DIST",Y)) D 1
 S X="" F D=0:0 S X=$O(^UTILITY(U,$J,X)) Q:X=""  S %X="^UTILITY(U,$J,"_""""_X_"""," D %XY^DIFROM1
 K ^UTILITY(U,$J) D FILE^DIFROM3:DL K ^UTILITY($J) G:'$D(DRN) EQ
 D DIFROM2 G Q
1 ;
 I 'DIT F %=0:0 S %=$O(^DIC(9.4,DPK,"EX",%)) Q:%'>0  I $P($P(DSV,U),$P(^(%,0),U))="" G QQ
 I D["DIST" I DIT!($P($P(DSV,U),DTL)="")!$D(Q("DIST",Y)) S Q("DIST")=0 D %XY^%RCR S $P(DSV,U,4)="",$P(DSV,U,6)="" S:'DSEC $P(DSV,U,2,3)=U S ^UTILITY(U,$J,D,Y,0)=DSV D BLK G QQ
 I DIT!($P($P(DSV,U),DTL)="")!$D(Q(D,Y)) S Q(D)=0 D %XY^%RCR K ^UTILITY(U,$J,D,Y,"RD"),^("AB") K:'$D(DTL(F))&(D["DIBT") ^(1) S:'DSEC ^(0)=$P(DSV,U,1,2)_U_U_F_U_U_U_U_$P(DSV,U,8,9) W "."
QQ Q
BLK N D,%X S D="DIST(.404,",%X=U_D_"Y,"
 F I=0:0 S I=$O(^UTILITY(U,$J,"DIST(.403,",Y,40,I)) Q:'I  I $D(^(I,0)) S %=+$P(^(0),U,2) S:$D(^DIST(.404,%,0)) $P(^UTILITY(U,$J,"DIST(.403,",Y,40,I,0),U,2)=$P(^(0),U) S K=Y,Y=% D:$D(^DIST(.404,%,0)) %XY^%RCR S Y=K D B2
 Q
B2 F J=0:0 S J=$O(^UTILITY(U,$J,"DIST(.403,",Y,40,I,40,J)) Q:'J  I $D(^(J,0)) S %=+^(0) I $D(^DIST(.404,%,0)) S $P(^UTILITY(U,$J,"DIST(.403,",Y,40,I,40,J,0),U)=$P(^(0),U),K=Y,Y=% D %XY^%RCR S Y=K
 Q
 ;
DIFROM2 ;
 S DIFROM=5,Y=DRN-1,S=""
 S DH=" ; LOADS AND INDEXES DD'S",^UTILITY($J,.3,0)=" K DIF,DIK,D,DDF,DDT,DTO,D0,DLAYGO,DIC,DIDUZ,DIR,DA,DFR,DTN,DIX,DZ D DT^DICRW S %=1,U=""^"",DSEC=1"
 S X="",DD="A" F E=1:1 S DD=$O(Q(DD)) Q:DD=""  S X=X_","""_$E(DD,1,3)_""""
 S DL=0,^UTILITY($J,1.4,0)=" S NO=$P(""I 0^I $D(@X)#2,X[U"",U,%) I %<1 K DIFQ Q"
 S DIRS(1)=" I %<1 K DIFQ Q"
 S:E>1 ^UTILITY($J,2,0)=" F X="_$E(X,2,99)_" D W Q:'$D(DIFQ)"
 G ^DIFROM2
 ;
EQ W $C(7),!!,"PACKAGE TOO LARGE!  DIFROM CAN NOT BUILD ANY MORE INIT ROUTINES.",!!
Q K ^UTILITY($J),^("^",$J),^UTILITY("DIF",$J),DIFROM,DR,DD,DLAYGO,DIRS,DIMA,DWLW,DREF,D1
 K DI,DISYS,DIX,DIY,DO,DZ,DIK,DIDUZ,DIFQ,DDF,DDT,NO,DIF,DIG,DIH,DIU,DIV,DIW
 K %,%1,%2,%A,%B,%C,%DT,%V,%X,%Y,%Z,DDH,DG,D0,DA,DIFRM,DL,D,E,DIC,DIE,DN,DPK,DQ
 K DIFC,DRN,DIRUT,DIROUT,DTOUT,DUOUT,DIR,DIFQR,DNAME,DSEC,DTL
 K A,C,I,J,K,F,L,N,Q,R,S,X,Y,Z,DSV,DIDIU,DIFKEP,DIFR,DIFR1,DIFR2,DIT,DH,DILN2,DIFL,VERSION
 K DIFRDIFI,DIFRF,DIFRIR,DIFRRMAX,DIFRRN,DIFRRTN,DIFRRXT,DIFRS,DIFRTX
 K DIOVRD
 Q
