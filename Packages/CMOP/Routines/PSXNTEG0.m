PSXNTEG0 ;ISC/XTSUMBLD KERNEL - Package checksum checker ;MAY 15, 1995@13:05
 ;;1.0;CONSOLIDATED MAIL OUTPATIENT PHARMACY;;10 MAY 95
 ;;7.1;MAY 15, 1995@13:05
 S XT4="I 1",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
CONT F XT1=1:1 S XT2=$T(ROU+XT1) Q:XT2=""  S X=$P(XT2," ",1),XT3=$P(XT2,";",3) X XT4 I $T W !,X X ^%ZOSF("TEST") S:'$T XT3=0 X:XT3 ^%ZOSF("RSUM") W ?10,$S('XT3:"Routine not in UCI",XT3'=Y:"Calculated "_$C(7)_Y_", off by "_(Y-XT3),1:"ok")
 ;
 K %1,%2,%3,X,Y,XT1,XT2,XT3,XT4 Q
ONE S XT4="I $D(^UTILITY($J,X))",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
 W !,"Check a subset of routines:" K ^UTILITY($J) X ^%ZOSF("RSEL")
 W ! G CONT
ROU ;;
PSXSITE ;;12487144
PSXSMRY ;;10077978
PSXSND ;;9037198
PSXSTAT ;;14615147
PSXSTP ;;339758
PSXSTRT ;;2691875
PSXSUDCN ;;4280634
PSXSYS ;;12995963
PSXTNRPT ;;13906904
PSXUNHLD ;;7418557
PSXUNREL ;;5255923
PSXUTL ;;2242338
PSXVEND ;;8559375
PSXVIEW ;;4523915
PSXVND ;;10484306
PSXVPN ;;13440916
