SDAMXOE ; GENERATED FROM 'SD ENCOUNTER ENTRY' INPUT TEMPLATE(#1430), FILE 409.68;01/04/99
 D DE G BEGIN
DE S DIE="^SCE(",DIC=DIE,DP=409.68,DL=1,DIEL=0,DU="" K DG,DE,DB Q:$O(^SCE(DA,""))=""
 I $D(^(0)) S %Z=^(0) S %=$P(%Z,U,2) S:%]"" DE(2)=% S %=$P(%Z,U,3) S:%]"" DE(3)=% S %=$P(%Z,U,4) S:%]"" DE(4)=% S %=$P(%Z,U,5) S:%]"" DE(5)=% S %=$P(%Z,U,6) S:%]"" DE(6)=% S %=$P(%Z,U,12) S:%]"" DE(1)=%
 K %Z Q
 ;
W W !?DL+DL-2,DLB_": "
 Q
O D W W Y W:$X>45 !?9
 I $L(Y)>19,'DV,DV'["I",(DV["F"!(DV["K")) G RW^DIR2
 W:Y]"" "// " I 'DV,DV["I",$D(DE(DQ))#2 S X="" W "  (No Editing)" Q
TR R X:DTIME E  S (DTOUT,X)=U W $C(7)
 Q
A K DQ(DQ) S DQ=DQ+1
B G @DQ
RE G PR:$D(DE(DQ)) D W,TR
N I X="" G A:DV'["R",X:'DV,X:D'>0,A
RD G QS:X?."?" I X["^" D D G ^DIE17
 I X="@" D D G Z^DIE2
 I X=" ",DV["d",DV'["P",$D(^DISV(DUZ,"DIE",DLB)) S X=^(DLB) I DV'["D",DV'["S" W "  "_X
T G M^DIE17:DV,^DIE3:DV["V",P:DV'["S" X:$D(^DD(DP,DIFLD,12.1)) ^(12.1) I X?.ANP D SET I 'DDER X:$D(DIC("S")) DIC("S") I  W:'$D(DB(DQ)) "  "_% G V
 K DDER G X
P I DV["P" S DIC=U_DU,DIC(0)=$E("EN",$D(DB(DQ))+1)_"M"_$E("L",DV'["'") S:DIC(0)["L" DLAYGO=+$P(DV,"P",2) I DV'["*" D ^DIC S X=+Y,DIC=DIE G X:X<0
 G V:DV'["N" D D I $L($P(X,"."))>24 K X G Z
 I $P(DQ(DQ),U,5)'["$",X?.1"-".N.1".".N,$P(DQ(DQ),U,5,99)["+X'=X" S X=+X
V D @("X"_DQ) K YS
Z K DIC("S"),DLAYGO I $D(X),X'=U S DG(DW)=X S:DV["d" ^DISV(DUZ,"DIE",DLB)=X G A
X W:'$D(ZTQUEUED) $C(7),"??" I $D(DB(DQ)) G Z^DIE17
 S X="?BAD"
QS S DZ=X D D,QQ^DIEQ G B
D S D=DIFLD,DQ(DQ)=DLB_U_DV_U_DU_U_DW_U_$P($T(@("X"_DQ))," ",2,99) Q
Y I '$D(DE(DQ)) D O G RD:"@"'[X,A:DV'["R"&(X="@"),X:X="@" S X=Y G N
PR S DG=DV,Y=DE(DQ),X=DU I $D(DQ(DQ,2)) X DQ(DQ,2) G RP
R I DG["P",@("$D(^"_X_"0))") S X=+$P(^(0),U,2) G RP:'$D(^(Y,0)) S Y=$P(^(0),U),X=$P(^DD(X,.01,0),U,3),DG=$P(^(0),U,2) G R
 I DG["V",+Y,$P(Y,";",2)["(",$D(@(U_$P(Y,";",2)_"0)")) S X=+$P(^(0),U,2) G RP:'$D(^(+Y,0)) S Y=$P(^(0),U) I $D(^DD(+X,.01,0)) S DG=$P(^(0),U,2),X=$P(^(0),U,3) G R
 X:DG["D" ^DD("DD") I DG["S" S %=$P($P(";"_X,";"_Y_":",2),";") S:%]"" Y=%
RP D O I X="" S X=DE(DQ) G A:'DV,A:DC<2,N^DIE17
I I DV'["I",DV'["#" G RD
 D E^DIE0 G RD:$D(X),PR
 Q
SET N DIR S DIR(0)="SV"_$E("o",$D(DB(DQ)))_U_DU,DIR("V")=1
 I $D(DB(DQ)),'$D(DIQUIET) N DIQUIET S DIQUIET=1
 D ^DIR I 'DDER S %=Y(0),X=Y
 Q
BEGIN S DNM="SDAMXOE",DQ=1
 S:$D(DTIME)[0 DTIME=300 S D0=DA,DIEZ=1430,U="^"
1 S DW="0;12",DV="P409.63'",DU="",DLB="STATUS",DIFLD=.12
 S DU="SD(409.63,"
 S X=$G(SDVSIT("STA"))
 S Y=X
 S X=Y,DB(DQ)=1 G:X="" N^DIE17:DV,A I $D(DE(DQ)),DV["I"!(DV["#") D E^DIE0 G A:'$D(X)
 G RD:X="@",Z
X1 Q
2 S DW="0;2",DV="RP2'I",DU="",DLB="PATIENT",DIFLD=.02
 S DE(DW)="C2^SDAMXOE"
 S DU="DPT("
 S X=$G(SDVSIT("DFN"))
 S Y=X
 S X=Y,DB(DQ)=1 G:X="" N^DIE17:DV,A I $D(DE(DQ)),DV["I"!(DV["#") D E^DIE0 G A:'$D(X)
 G RD:X="@",Z
C2 G C2S:$D(DE(2))[0 K DB S X=DE(2),DIC=DIE
 K ^SCE("C",$E(X,1,30),DA)
 S X=DE(2),DIC=DIE
 K:$D(^SCE(DA,0)) ^SCE("ADFN",X,+^SCE(DA,0),DA)
C2S S X="" Q:DG(DQ)=X  K DB S X=DG(DQ),DIC=DIE
 S ^SCE("C",$E(X,1,30),DA)=""
 S X=DG(DQ),DIC=DIE
 S:$D(^SCE(DA,0)) ^SCE("ADFN",X,+^SCE(DA,0),DA)=""
 Q
X2 Q
3 D:$D(DG)>9 F^DIE17,DE S DQ=3,DW="0;3",DV="RP40.7'I",DU="",DLB="CLINIC STOP CODE",DIFLD=.03
 S DU="DIC(40.7,"
 S X=$G(SDVSIT("CLN"))
 S Y=X
 S X=Y,DB(DQ)=1 G:X="" N^DIE17:DV,A I $D(DE(DQ)),DV["I"!(DV["#") D E^DIE0 G A:'$D(X)
 G RD:X="@",Z
X3 Q
4 S DW="0;4",DV="P44'I",DU="",DLB="LOCATION",DIFLD=.04
 S DU="SC("
 S X=$G(SDVSIT("LOC"))
 S Y=X
 S X=Y,DB(DQ)=1 G:X="" N^DIE17:DV,A I $D(DE(DQ)),DV["I"!(DV["#") D E^DIE0 G A:'$D(X)
 G RD:X="@",Z
X4 Q
5 S DW="0;5",DV="P9000010",DU="",DLB="VISIT FILE ENTRY",DIFLD=.05
 S DE(DW)="C5^SDAMXOE"
 S DU="AUPNVSIT("
 S X=$G(SDVSIT("VST"))
 S Y=X
 S X=Y,DB(DQ)=1 G:X="" N^DIE17:DV,A I $D(DE(DQ)),DV["I"!(DV["#") D E^DIE0 G A:'$D(X)
 G RD:X="@",Z
C5 G C5S:$D(DE(5))[0 K DB S X=DE(5),DIC=DIE
 K ^SCE("AVSIT",$E(X,1,30),DA)
 S X=DE(5),DIC=DIE
 N SDX S SDX=X,X="VSIT" X ^%ZOSF("TEST") I $T S X=SDX D SUB^VSIT
 S X=DE(5),DIC=DIE
 ;
C5S S X="" Q:DG(DQ)=X  K DB S X=DG(DQ),DIC=DIE
 S ^SCE("AVSIT",$E(X,1,30),DA)=""
 S X=DG(DQ),DIC=DIE
 N SDX S SDX=X,X="VSIT" X ^%ZOSF("TEST") I $T S X=SDX D ADD^VSIT
 S X=DG(DQ),DIC=DIE
 K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(1)=$S($D(^SCE(D0,0)):^(0),1:"") S X=$P(Y(1),U,20),X=X S DIU=X K Y S X=DIV S X=$$IEN2VID^VSIT(X) X ^DD(409.68,.05,1,3,1.4)
 Q
X5 Q
6 D:$D(DG)>9 F^DIE17,DE S DQ=6,DW="0;6",DV="P409.68'I",DU="",DLB="PARENT ENCOUNTER",DIFLD=.06
 S DE(DW)="C6^SDAMXOE"
 S DU="SCE("
 S X=$G(SDVSIT("PAR"))
 S Y=X
 S X=Y,DB(DQ)=1 G:X="" N^DIE17:DV,A I $D(DE(DQ)),DV["I"!(DV["#") D E^DIE0 G A:'$D(X)
 G RD:X="@",Z
C6 G C6S:$D(DE(6))[0 K DB S X=DE(6),DIC=DIE
 K ^SCE("APAR",$E(X,1,30),DA)
 S X=DE(6),DIC=DIE
 K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(1)=$S($D(^SCE(D0,0)):^(0),1:"") S X=$P(Y(1),U,7),X=X S DIU=X K Y S X="" S DIH=$S($D(^SCE(DIV(0),0)):^(0),1:""),DIV=X S $P(^(0),U,7)=DIV,DIH=409.68,DIG=.07 D ^DICR:$N(^DD(DIH,DIG,1,0))>0
C6S S X="" Q:DG(DQ)=X  K DB S X=DG(DQ),DIC=DIE
 S ^SCE("APAR",$E(X,1,30),DA)=""
 S X=DG(DQ),DIC=DIE
 K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(1)=$S($D(^SCE(D0,0)):^(0),1:"") S X=$P(Y(1),U,7),X=X S DIU=X K Y S X=DIV S X=$P($G(^SCE(+X,0)),U,7) X ^DD(409.68,.06,1,2,1.4)
 Q
X6 Q
7 D:$D(DG)>9 F^DIE17 G ^SDAMXOE1
