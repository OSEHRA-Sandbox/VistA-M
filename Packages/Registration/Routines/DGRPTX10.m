DGRPTX10 ; ;08/28/09
 D DE G BEGIN
DE S DIE="^DPT(",DIC=DIE,DP=2,DL=1,DIEL=0,DU="" K DG,DE,DB Q:$O(^DPT(DA,""))=""
 I $D(^(0)) S %Z=^(0) S %=$P(%Z,U,5) S:%]"" DE(5)=%
 I $D(^(.11)) S %Z=^(.11) S %=$P(%Z,U,7) S:%]"" DE(1)=% S %=$P(%Z,U,16) S:%]"" DE(4)=%
 I $D(^(.13)) S %Z=^(.13) S %=$P(%Z,U,1) S:%]"" DE(2)=% S %=$P(%Z,U,2) S:%]"" DE(3)=%
 I $D(^(.21)) S %Z=^(.21) S %=$P(%Z,U,1) S:%]"" DE(6)=%
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
N I X="" G NKEY:$D(^DD("KEY","F",DP,DIFLD)),A:DV'["R",X:'DV,X:D'>0,A
RD G QS:X?."?" I X["^" D D G ^DIE17
 I X="@" D D G Z^DIE2
 I X=" ",DV["d",DV'["P",$D(^DISV(DUZ,"DIE",DLB)) S X=^(DLB) I DV'["D",DV'["S" W "  "_X
T G M^DIE17:DV,^DIE3:DV["V",P:DV'["S" X:$D(^DD(DP,DIFLD,12.1)) ^(12.1) I X?.ANP D SET I 'DDER X:$D(DIC("S")) DIC("S") I  W:'$D(DB(DQ)) "  "_% G V
 K DDER G X
P I DV["P" S DIC=U_DU,DIC(0)=$E("EN",$D(DB(DQ))+1)_"M"_$E("L",DV'["'") S:DIC(0)["L" DLAYGO=+$P(DV,"P",2) G:DV["*" AST^DIED D NOSCR^DIED S X=+Y,DIC=DIE G X:X<0
 G V:DV'["N" D D I $L($P(X,"."))>24 K X G Z
 I $P(DQ(DQ),U,5)'["$",X?.1"-".N.1".".N,$P(DQ(DQ),U,5,99)["+X'=X" S X=+X
V D @("X"_DQ) K YS
Z K DIC("S"),DLAYGO I $D(X),X'=U D:$G(DE(DW,"INDEX")) SAVEVALS G:'$$KEYCHK UNIQFERR^DIE17 S DG(DW)=X S:DV["d" ^DISV(DUZ,"DIE",DLB)=X G A
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
SAVEVALS S @DIEZTMP@("V",DP,DIIENS,DIFLD,"O")=$G(DE(DQ)) S:$D(^("F"))[0 ^("F")=$G(DE(DQ))
 I $D(DE(DW,"4/")) S @DIEZTMP@("V",DP,DIIENS,DIFLD,"4/")=""
 E  K @DIEZTMP@("V",DP,DIIENS,DIFLD,"4/")
 Q
NKEY W:'$D(ZTQUEUED) "??  Required key field" S X="?BAD" G QS
KEYCHK() Q:$G(DE(DW,"KEY"))="" 1 Q @DE(DW,"KEY")
BEGIN S DNM="DGRPTX10",DQ=1
1 D:$D(DG)>9 F^DIE17,DE S DQ=1,DW=".11;7",DV="NJ3,0XOa",DU="",DLB="COUNTY",DIFLD=.117
 S DQ(1,2)="S Y(0)=Y Q:Y']""""  S Z0=$S($D(^DPT(D0,.11)):+$P(^(.11),""^"",5),1:"""") Q:'Z0  S Y=$P($S($D(^DIC(5,Z0,1,Y,0)):^(0),1:""""),""^"",3)"
 S DE(DW)="C1^DGRPTX10"
 G RE
C1 G C1S:$D(DE(1))[0 K DB
 S X=DE(1),DIC=DIE
 S A1B2TAG="PAT" D ^A1B2XFR
 S X=DE(1),DIC=DIE
 D EVENT^IVMPLOG(DA)
 S X=DE(1),DIC=DIE
 S IVMX=X,X="IVMPXFR" X ^%ZOSF("TEST") D:$T DPT^IVMPXFR S X=IVMX K IVMX
 S X=DE(1),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".117;" D AVAFC^VAFCDD01(DA)
 S X=DE(1),DIIX=2_U_DIFLD D AUDIT^DIET
C1S S X="" G:DG(DQ)=X C1F1 K DB
 S X=DG(DQ),DIC=DIE
 S A1B2TAG="PAT" D ^A1B2XFR
 S X=DG(DQ),DIC=DIE
 D EVENT^IVMPLOG(DA)
 S X=DG(DQ),DIC=DIE
 S IVMX=X,X="IVMPXFR" X ^%ZOSF("TEST") D:$T DPT^IVMPXFR S X=IVMX K IVMX
 S X=DG(DQ),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".117;" D AVAFC^VAFCDD01(DA)
 I $D(DE(1))'[0!(^DD(DP,DIFLD,"AUDIT")'="e") S X=DG(DQ),DIIX=3_U_DIFLD D AUDIT^DIET
C1F1 Q
X1 S Z0=$S($D(^DPT(D0,.11)):+$P(^(.11),"^",5),1:0) K:'Z0 X Q:'Z0!'$D(^DIC(5,Z0,1,0))  S DIC="^DIC(5,Z0,1,",DIC(0)="QEM" D ^DIC S X=+Y K:Y'>0 X K Z0,DIC
 Q
 ;
2 D:$D(DG)>9 F^DIE17,DE S DQ=2,DW=".13;1",DV="Fa",DU="",DLB="PHONE NUMBER [RESIDENCE]",DIFLD=.131
 S DE(DW)="C2^DGRPTX10"
 G RE
C2 G C2S:$D(DE(2))[0 K DB
 S X=DE(2),DIC=DIE
 D EVENT^IVMPLOG(DA)
 S X=DE(2),DIC=DIE
 S IVMX=X,X="IVMPXFR" X ^%ZOSF("TEST") D:$T DPT^IVMPXFR S X=IVMX K IVMX
 S X=DE(2),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".131;" D AVAFC^VAFCDD01(DA)
 S X=DE(2),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
 S X=DE(2),DIIX=2_U_DIFLD D AUDIT^DIET
C2S S X="" G:DG(DQ)=X C2F1 K DB
 S X=DG(DQ),DIC=DIE
 D EVENT^IVMPLOG(DA)
 S X=DG(DQ),DIC=DIE
 S IVMX=X,X="IVMPXFR" X ^%ZOSF("TEST") D:$T DPT^IVMPXFR S X=IVMX K IVMX
 S X=DG(DQ),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".131;" D AVAFC^VAFCDD01(DA)
 S X=DG(DQ),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
 I $D(DE(2))'[0!(^DD(DP,DIFLD,"AUDIT")'="e") S X=DG(DQ),DIIX=3_U_DIFLD D AUDIT^DIET
C2F1 Q
X2 K:$L(X)>20!($L(X)<4) X
 I $D(X),X'?.ANP K X
 Q
 ;
3 D:$D(DG)>9 F^DIE17,DE S DQ=3,DW=".13;2",DV="Fa",DU="",DLB="PHONE NUMBER [WORK]",DIFLD=.132
 S DE(DW)="C3^DGRPTX10"
 G RE
C3 G C3S:$D(DE(3))[0 K DB
 S X=DE(3),DIC=DIE
 D EVENT^IVMPLOG(DA)
 S X=DE(3),DIC=DIE
 S IVMX=X,X="IVMPXFR" X ^%ZOSF("TEST") D:$T DPT^IVMPXFR S X=IVMX K IVMX
 S X=DE(3),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".132;" D AVAFC^VAFCDD01(DA)
 S X=DE(3),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
 S X=DE(3),DIIX=2_U_DIFLD D AUDIT^DIET
C3S S X="" G:DG(DQ)=X C3F1 K DB
 S X=DG(DQ),DIC=DIE
 D EVENT^IVMPLOG(DA)
 S X=DG(DQ),DIC=DIE
 S IVMX=X,X="IVMPXFR" X ^%ZOSF("TEST") D:$T DPT^IVMPXFR S X=IVMX K IVMX
 S X=DG(DQ),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".132;" D AVAFC^VAFCDD01(DA)
 S X=DG(DQ),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
 I $D(DE(3))'[0!(^DD(DP,DIFLD,"AUDIT")'="e") S X=DG(DQ),DIIX=3_U_DIFLD D AUDIT^DIET
C3F1 Q
X3 K:$L(X)>20!($L(X)<4) X
 I $D(X),X'?.ANP K X
 Q
 ;
4 D:$D(DG)>9 F^DIE17,DE S DQ=4,DW=".11;16",DV="*S",DU="",DLB="BAD ADDRESS INDICATOR",DIFLD=.121
 S DE(DW)="C4^DGRPTX10",DE(DW,"INDEX")=1
 S DU="1:UNDELIVERABLE;2:HOMELESS;3:OTHER;4:ADDRESS NOT FOUND;"
 G RE
C4 G C4S:$D(DE(4))[0 K DB
 S X=DE(4),DIC=DIE
 D EVENT^IVMPLOG(DA)
 S X=DE(4),DIC=DIE
 K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(1)=$S($D(^DPT(D0,.11)):^(.11),1:"") S X=$P(Y(1),U,13),X=X S DIU=X K Y S X=DIV S X=$$NOW^XLFDT() S DIH=$G(^DPT(DIV(0),.11)),DIV=X S $P(^(.11),U,13)=DIV,DIH=2,DIG=.118 D ^DICR
C4S S X="" G:DG(DQ)=X C4F1 K DB
 S X=DG(DQ),DIC=DIE
 D EVENT^IVMPLOG(DA)
 S X=DG(DQ),DIC=DIE
 K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(1)=$S($D(^DPT(D0,.11)):^(.11),1:"") S X=$P(Y(1),U,13),X=X S DIU=X K Y S X=DIV S X=$$NOW^XLFDT() S DIH=$G(^DPT(DIV(0),.11)),DIV=X S $P(^(.11),U,13)=DIV,DIH=2,DIG=.118 D ^DICR
C4F1 N X,X1,X2 S DIXR=53 D C4X1(U) K X2 M X2=X D C4X1("O") K X1 M X1=X
 D
 . D FC^DGFCPROT(.DA,2,.121,"KILL",$H,$G(DUZ),.X,.X1,.X2,$G(XQY0)) Q
 K X M X=X2 D
 . D FC^DGFCPROT(.DA,2,.121,"SET",$H,$G(DUZ),.X,.X1,.X2,$G(XQY0)) Q
 G C4F2
C4X1(DION) K X
 S X(1)=$G(@DIEZTMP@("V",2,DIIENS,.121,DION),$P($G(^DPT(DA,.11)),U,16))
 S X=$G(X(1))
 Q
C4F2 Q
X4 Q
5 D:$D(DG)>9 F^DIE17,DE S DQ=5,DW="0;5",DV="RP11'a",DU="",DLB="MARITAL STATUS",DIFLD=.05
 S DE(DW)="C5^DGRPTX10"
 S DU="DIC(11,"
 G RE
C5 G C5S:$D(DE(5))[0 K DB
 S X=DE(5),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".05;" D AVAFC^VAFCDD01(DA)
 S X=DE(5),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
 S X=DE(5),DIC=DIE
 D EVENT^IVMPLOG(DA)
 S X=DE(5),DIIX=2_U_DIFLD D AUDIT^DIET
C5S S X="" G:DG(DQ)=X C5F1 K DB
 S X=DG(DQ),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".05;" D AVAFC^VAFCDD01(DA)
 S X=DG(DQ),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
 S X=DG(DQ),DIC=DIE
 D EVENT^IVMPLOG(DA)
 I $D(DE(5))'[0!(^DD(DP,DIFLD,"AUDIT")'="e") S X=DG(DQ),DIIX=3_U_DIFLD D AUDIT^DIET
C5F1 Q
X5 Q
6 D:$D(DG)>9 F^DIE17,DE S DQ=6,DW=".21;1",DV="Fa",DU="",DLB="K-NAME OF PRIMARY NOK",DIFLD=.211
 S DE(DW)="C6^DGRPTX10",DE(DW,"INDEX")=1
 G RE
C6 G C6S:$D(DE(6))[0 K DB
 D ^DGRPTX11
C6S S X="" G:DG(DQ)=X C6F1 K DB
 D ^DGRPTX12
C6F1 N X,X1,X2 S DIXR=594 D C6X1(U) K X2 M X2=X D C6X1("O") K X1 M X1=X
 I $G(X(1))]"" D
 . I '$G(XUNOTRIG) N XUNOTRIG S XUNOTRIG=1 D DELCOMP^XLFNAME2(2,.DA,.211,1.02) Q
 K X M X=X2 I $G(X(1))]"" D
 . I '$G(XUNOTRIG) N XUNOTRIG S XUNOTRIG=1,DG20NAME=X D NARY^XLFNAME7(.DG20NAME),UPDCOMP^XLFNAME2(2,.DA,.211,.DG20NAME,1.02,+$P($G(^DPT(DA,"NAME")),U,2),"CL35") K DG20NAME Q
 G C6F2
C6X1(DION) K X
 S X(1)=$G(@DIEZTMP@("V",2,DIIENS,.211,DION),$P($G(^DPT(DA,.21)),U,1))
 S X=$G(X(1))
 Q
C6F2 S DIEZRXR(2,DIIENS)=$$OREF^DILF($NA(@$$CREF^DILF(DIE)))
 F DIXR=11 S DIEZRXR(2,DIXR)=""
 Q
X6 K:$L(X)>35!($L(X)<3) X I $D(X) S DG20NAME=X,(X,DG20NAME)=$$FORMAT^XLFNAME7(.DG20NAME,3,35) K:'$L(X) X,DG20NAME
 I $D(X),X'?.ANP K X
 Q
 ;
7 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=7 D X7 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X7 S:X="" Y="@30"
 Q
8 D:$D(DG)>9 F^DIE17 G ^DGRPTX13
