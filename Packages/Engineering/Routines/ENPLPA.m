ENPLPA ; GENERATED FROM 'ENPLP008' PRINT TEMPLATE (#1441) ; 09/28/95 ; (FILE 6925, MARGIN=132)
 G BEGIN
N W !
T W:$X ! I '$D(DIOT(2)),DN,$D(IOSL),$S('$D(DIWF):1,$P(DIWF,"B",2):$P(DIWF,"B",2),1:1)+$Y'<IOSL,$D(^UTILITY($J,1))#2,^(1)?1U1P1E.E X ^(1)
 S DISTP=DISTP+1,DILCT=DILCT+1 D:'(DISTP#100) CSTP^DIO2
 Q
DT I $G(DUZ("LANG"))>1,Y W $$OUT^DIALOGU(Y,"DD") Q
 I Y W $P("JAN^FEB^MAR^APR^MAY^JUN^JUL^AUG^SEP^OCT^NOV^DEC",U,$E(Y,4,5))_" " W:Y#100 $J(Y#100\1,2)_"," W Y\10000+1700 W:Y#1 "  "_$E(Y_0,9,10)_":"_$E(Y_"000",11,12) Q
 W Y Q
M D @DIXX
 Q
BEGIN ;
 S:'$D(DN) DN=1 S DISTP=$G(DISTP),DILCT=$G(DILCT)
 I $D(DXS)<9 F X=0:0 S X=$O(^DIPT(1441,"DXS",X)) Q:'X  S Y=$O(^(X,"")) F X=X:0 Q:Y=""  S DXS(X,Y)=^(Y),Y=$O(^(Y))
 F Y=0:0 Q:$Y>-1  W !
 D N:$X>0 Q:'DN  W ?0 W "VHA"
 D N:$X>53 Q:'DN  W ?53 W "PROJECT APPLICATION"
 D N:$X>107 Q:'DN  W ?107 W "PROJECT NUMBER"
 D N:$X>54 Q:'DN  W ?54 W "EXECUTIVE SUMMARY"
 S X=$G(^ENG("PROJ",D0,0)) D N:$X>111 Q:'DN  W ?111,$E($P(X,U,1),1,11)
 D T Q:'DN  D N D N:$X>45 Q:'DN  W ?45 W "***********  COST DATA  ***********"
 D T Q:'DN  D N D N:$X>0 Q:'DN  W ?0 W "40. CONSTRUCTION METHOD PLANNED: "
 S X=$G(^ENG("PROJ",D0,15)) S Y=$P(X,U,7) W:Y]"" $S($D(DXS(8,Y)):DXS(8,Y),1:Y)
 D N:$X>49 Q:'DN  W ?49 W "41. AE $ REQUIRED IN FY: "
 S X=$G(^ENG("PROJ",D0,5)) W ?0,$E($P(X,U,7),1,4)
 D N:$X>89 Q:'DN  W ?89 W "42. CONST $ REQUIRED IN FY: "
 S X=$G(^ENG("PROJ",D0,0)) W ?0,$E($P(X,U,7),1,4)
 D T Q:'DN  D N D N:$X>0 Q:'DN  W ?0 W "43. NRM COSTS: "
 W "YES"
 D N:$X>59 Q:'DN  W ?59 W "44. MAJOR/MINOR/MINOR MISC. COSTS: "
 W "NO"
 D N:$X>0 Q:'DN  W ?0 W "45. TOTAL M&R COSTS: "
 S X=$G(^ENG("PROJ",D0,19)) D N:$X>44 Q:'DN  W ?44 S Y=$P(X,U,1) W:Y]"" $J(Y,11,0)
 D N:$X>0 Q:'DN  W ?0 W "46. TOTAL BSEA COSTS: "
 D N:$X>44 Q:'DN  W ?44 S Y=$P(X,U,2) W:Y]"" $J(Y,11,0)
 D N:$X>0 Q:'DN  W ?0 W "47. TOTAL BSER COSTS: "
 D N:$X>44 Q:'DN  W ?44 S Y=$P(X,U,3) W:Y]"" $J(Y,11,0)
 D N:$X>0 Q:'DN  W ?0 W "48. TOTAL MI COSTS: "
 D N:$X>44 Q:'DN  W ?44 S Y=$P(X,U,4) W:Y]"" $J(Y,11,0)
 D N:$X>0 Q:'DN  W ?0 W "49. TOTAL CONST. COST (LOW BID): "
 D N:$X>44 Q:'DN  W ?44 S Y(6925,217,1)=$S($D(^ENG("PROJ",D0,19)):^(19),1:"") S X=$P(Y(6925,217,1),U,1)+$P(Y(6925,217,1),U,2)+$P(Y(6925,217,1),U,3)+$P(Y(6925,217,1),U,4) W:X'?."*" $J(X,11,0) K Y(6925,217)
 D N:$X>59 Q:'DN  W ?59 W "54. TOTAL CONST. COST (LOW BID): "
 D N:$X>0 Q:'DN  W ?0 W "50. CONST CONTCY % AND $: "
 S X=$G(^ENG("PROJ",D0,19)) D N:$X>34 Q:'DN  W ?34 S Y=$P(X,U,5) W:Y]"" $J(Y,5,1)
 D N:$X>40 Q:'DN  W ?40 W "%"
 D N:$X>44 Q:'DN  W ?44 X ^DD(6925,218.1,9.4) S X=$S(Y(6925,218.1,2):Y(6925,218.1,5),Y(6925,218.1,6):X)/100+999.999\1000*1000 S X=$J(X,0,0) W:X'?."*" $J(X,11,0) K Y(6925,218.1)
 D N:$X>59 Q:'DN  W ?59 W "55. CONST CONTCY % AND $: "
 D N:$X>100 Q:'DN  W ?100 W "%"
 D N:$X>0 Q:'DN  W ?0 W "51. IMPACT COSTS: "
 S X=$G(^ENG("PROJ",D0,19)) D N:$X>44 Q:'DN  W ?44 S Y=$P(X,U,7) W:Y]"" $J(Y,11,0)
 D N:$X>59 Q:'DN  W ?59 W "56. IMPACT COSTS: "
 D N:$X>0 Q:'DN  W ?0 W "52. TECHNICAL SERVICES % AND $"
 D N:$X>34 Q:'DN  W ?34 X ^DD(6925,220,10) S X=X*100 W:X'?."*" $J(X,5,1) K Y(6925,220)
 D N:$X>40 Q:'DN  W ?40 W "%"
 S X=$G(^ENG("PROJ",D0,19)) D N:$X>44 Q:'DN  W ?44 S Y=$P(X,U,6) W:Y]"" $J(Y,11,0)
 D N:$X>59 Q:'DN  W ?59 W "57. TECHNICAL SERVICES % AND $"
 D N:$X>100 Q:'DN  W ?100 W "%"
 D T Q:'DN  D N D N:$X>0 Q:'DN  W ?0 W "53. TOTAL PROJECT COSTS: "
 D N:$X>44 Q:'DN  W ?44 X ^DD(6925,222,9.3) S Y(6925,222,5)=X S X=$P(Y(6925,222,6),U,8),X=$S(Y(6925,222,2):Y(6925,222,4),Y(6925,222,5):X)+Y(6925,222,7)+$P(Y(6925,222,6),U,7)+$P(Y(6925,222,6),U,6) S X=$J(X,0,0) W:X'?."*" $J(X,11,0) K Y(6925,222)
 D N:$X>59 Q:'DN  W ?59 W "58. TOTAL PROJECT COSTS: "
 D T Q:'DN  D N D N:$X>0 Q:'DN  W ?0 W "***********  ACTIVATION DATA  ***********"
 D T Q:'DN  D N D N:$X>0 Q:'DN  W ?0 W "59. ACTIVATION $ REQUIRED IN FY: "
 S X=$G(^ENG("PROJ",D0,24)) W ?0,$E($P(X,U,1),1,4)
 D N:$X>59 Q:'DN  W ?59 W "66. EQUIP OVER $250K  : "
 X DXS(1,9) K DIP K:DN Y W $J(X,13)
 D N:$X>63 Q:'DN  W ?63 W "EQPMT (OVER $250K)"
 D N:$X>0 Q:'DN  W ?0 W "60. ADDITIONAL FTEE: "
 S DIP(1)=$S($D(^ENG("PROJ",D0,24)):^(24),1:"") S X=$P(DIP(1),U,3)+0 K DIP K:DN Y W $E(X,1,4)
 D N:$X>59 Q:'DN  W ?59 W "67. VAMC SCORE: "
 X DXS(2,9.2) S DIP(4)=X S X=DIP(5),X=$S(DIP(2):DIP(3),DIP(4):X) K DIP K:DN Y W X
 D N:$X>0 Q:'DN  W ?0 W "61. RECURRING PS $ : "
 G ^ENPLPA1
