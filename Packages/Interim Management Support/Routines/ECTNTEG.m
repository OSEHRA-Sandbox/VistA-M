ECTNTEG ;ISC/XTSUMBLD KERNEL - Package checksum checker ;01/29/91 08:00
 ;;1.05;INTERIM MANAGEMENT SUPPORT;;
 ;;6.5;JAN 28, 1991@08:43
 S XT4="I 1",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
CONT F XT1=1:1 S XT2=$T(ROU+XT1) Q:XT2=""  S X=$P(XT2," ",1),XT3=$P(XT2,";",3) X XT4 I $T W !,X X ^%ZOSF("TEST") S:'$T XT3=0 X:XT3 ^%ZOSF("RSUM") W ?10,$S('XT3:"Routine not in UCI",XT3'=Y:"Calculated "_Y_", off by "_(Y-XT3),1:"ok")
 ;
 K %1,%2,%3,X,Y,XT1,XT2,XT3,XT4 Q
ONE S XT4="I $D(^UTILITY($J,X))",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
 W !,"Check a subset of routines:" K ^UTILITY($J) X ^%ZOSF("RSEL")
 W ! G CONT
ROU ;;
ECTASPC ;;2568923
ECTCLP ;;1722838
ECTCPP ;;2907689
ECTDIR ;;4388728
ECTDLAB ;;3472842
ECTDSUR ;;8598387
ECTEIN ;;3030059
ECTEPR ;;5844833
ECTEQT ;;2021278
ECTEST ;;4267093
ECTFAR ;;2347765
ECTFBAL ;;5171582
ECTFCON ;;3589149
ECTFCPB ;;2925193
ECTFCPO ;;2169639
ECTFCS ;;4021177
ECTMENU ;;11458612
ECTP1S ;;11340637
ECTP1S0 ;;5137639
ECTP1S1 ;;8616332
ECTP1TL ;;9420659
ECTP1TL0 ;;5412004
ECTP1TL1 ;;10649970
ECTPAS ;;10675926
ECTPAS0 ;;5774510
ECTPAS1 ;;11524230
ECTPEMP ;;5549948
ECTPFY ;;10030085
ECTPFYR ;;8360245
ECTPIND ;;13842702
ECTPIND1 ;;7470068
ECTPLNK ;;19681611
ECTPNUR ;;2560244
ECTPP ;;12396839
ECTPRPT ;;4415415
ECTPSRV ;;12744444
ECTPTL ;;8857005
ECTQSCR ;;1533352
ECTS334 ;;4276382
ECTS345 ;;4276192
ECTSBED ;;5518800
ECTSCAT ;;2287865
ECTSIN ;;8481869
ECTSIN1 ;;3692668
ECTSNP ;;11945870
ECTSOP ;;10557450
