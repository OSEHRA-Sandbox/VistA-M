SR86NTEG ;ISC/XTSUMBLD KERNEL - Package checksum checker ; [ 01/28/99  8:44 AM ]
 ;;3.0; Surgery ;**86**;24 Jun 93
 ;;7.3;2990108.135809
 S XT4="I 1",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
CONT F XT1=1:1 S XT2=$T(ROU+XT1) Q:XT2=""  S X=$P(XT2," ",1),XT3=$P(XT2,";",3) X XT4 I $T W !,X X ^%ZOSF("TEST") S:'$T XT3=0 X:XT3 ^%ZOSF("RSUM") W ?10,$S('XT3:"Routine not in UCI",XT3'=Y:"Calculated "_$C(7)_Y_", off by "_(Y-XT3),1:"ok")
 ;
 K %1,%2,%3,X,Y,XT1,XT2,XT3,XT4 Q
ONE S XT4="I $D(^UTILITY($J,X))",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
 W !,"Check a subset of routines:" K ^UTILITY($J) X ^%ZOSF("RSEL")
 W ! G CONT
ROU ;;
SRO1L ;;9617830
SRO1L1 ;;10337872
SROAMIS ;;11542412
SROAOP ;;11517475
SROAPIMS ;;12921485
SROARPT0 ;;18181236
SROARPT1 ;;18517181
SROAWL ;;20171024
SROAWL1 ;;17244443
SROERR ;;15041667
SROERR0 ;;8266051
SRONIN4 ;;8287073
SRONITE ;;2589588
SRONON0 ;;19285689
SRONRN3 ;;8921818
SROPCE0A ;;21810842
SROPCE0B ;;13544512
SROPCEX ;;6617925
SROPR02 ;;10033560
SROPROC ;;15776957
SROPRPT1 ;;9181686
SROPRPT2 ;;9678585
SROVER ;;13775899
SROVER2 ;;12378810
SROVER3 ;;13759449
