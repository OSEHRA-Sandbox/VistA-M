IBXSA23 ; ;05/13/17
 D DE G BEGIN
DE S DIE="^IBA(355.93,",DIC=DIE,DP=355.93,DL=3,DIEL=0,DU="" K DG,DE,DB Q:$O(^IBA(355.93,DA,""))=""
 I $D(^(0)) S %Z=^(0) S %=$P(%Z,U,3) S:%]"" DE(4)=% S %=$P(%Z,U,4) S:%]"" DE(7)=% S %=$P(%Z,U,5) S:%]"" DE(11)=% S %=$P(%Z,U,6) S:%]"" DE(13)=% S %=$P(%Z,U,7) S:%]"" DE(14)=% S %=$P(%Z,U,8) S:%]"" DE(15)=% S %=$P(%Z,U,9) S:%]"" DE(18)=%
 I  S %=$P(%Z,U,10) S:%]"" DE(12)=% S %=$P(%Z,U,11) S:%]"" DE(21)=%
 K %Z Q
 ;
W W !?DL+DL-2,DLB_": "
 Q
O D W W Y W:$X>45 !?9
 I $L(Y)>19,'DV,DV'["I",(DV["F"!(DV["K")) G RW^DIR2
 W:Y]"" "// " I 'DV,DV["I",$D(DE(DQ))#2 K X S X("FIELD")=DIFLD,X("FILE")=DP W "  ("_$$EZBLD^DIALOG(710,.X)_")" K X S X="" Q  ;**
TR Q:DV["K"&(DUZ(0)'="@")  R X:DTIME E  S (DTOUT,X)=U W $C(7)
 Q
A K DQ(DQ) S DQ=DQ+1
B G @DQ
RE G A:DV["K"&(DUZ(0)'["@"),PR:$D(DE(DQ)) D W,TR
N I X="" G NKEY:$D(^DD("KEY","F",DP,DIFLD)),A:DV'["R",X:'DV,X:D'>0,A
RD G QS:X?."?" I X["^" D D G ^DIE17
 I X="@" D D G Z^DIE2
 I X=" ",DV["d",DV'["P",$D(^DISV(DUZ,"DIE",DLB)) S X=^(DLB) I DV'["D",DV'["S" W "  "_X
T G M^DIE17:DV,^DIE3:DV["V",P:DV'["S" I X?.ANP D SET^DIED I 'DDER G V
 K DDER G X
P I DV["P" S DIC=U_DU,DIC(0)=$E("EN",$D(DB(DQ))+1)_"M"_$E("L",DV'["'") S:DIC(0)["L" DLAYGO=+$P(DV,"P",2) G:DV["*" AST^DIED D NOSCR^DIED S X=+Y,DIC=DIE G X:X<0
 G V:DV'["N" D D I $L($P(X,"."))>24 K X G Z
 I $P(DQ(DQ),U,5)'["$",X?.1"-".N.1".".N,$P(DQ(DQ),U,5,99)["+X'=X" S X=+X
V D @("X"_DQ) K YS
UNIQ I DV["U",$D(X),DIFLD=.01 K % M %=@(DIE_"""B"",X)") K %(DA) K:$O(%(0)) X
Z K DIC("S"),DLAYGO I $D(X),X'=U D:$G(DE(DW,"INDEX")) SAVEVALS G:'$$KEYCHK UNIQFERR^DIE17 S DG(DW)=X S:DV["d" ^DISV(DUZ,"DIE",DLB)=X G A
X W:'$D(ZTQUEUED) $C(7),"??" I $D(DB(DQ)) G Z^DIE17
 S X="?BAD"
QS S DZ=X D D,QQ^DIEQ G B
D S D=DIFLD,DQ(DQ)=DLB_U_DV_U_DU_U_DW_U_$P($T(@("X"_DQ))," ",2,99) Q
Y I '$D(DE(DQ)) D O G RD:"@"'[X,A:DV'["R"&(X="@"),X:X="@" S X=Y G N
PR S DG=DV,Y=DE(DQ),X=DU I $D(DQ(DQ,2)) X DQ(DQ,2) G RP
R I DG["P",@("$D(^"_X_"0))") S X=+$P(^(0),U,2) G RP:'$D(^(Y,0)) S Y=$P(^(0),U),X=$P(^DD(X,.01,0),U,3),DG=$P(^(0),U,2) G R
 I DG["V",+Y,$P(Y,";",2)["(",$D(@(U_$P(Y,";",2)_"0)")) S X=+$P(^(0),U,2) G RP:'$D(^(+Y,0)) S Y=$P(^(0),U) I $D(^DD(+X,.01,0)) S DG=$P(^(0),U,2),X=$P(^(0),U,3) G R
 X:DG["D" ^DD("DD") I DG["S" S %=$P($P(";"_X,";"_Y_":",2),";") I %]"" S Y=$S($G(DUZ("LANG"))'>1:%,'DIFLD:%,1:$$SET^DIQ(DP,DIFLD,Y))
RP D O I X="" S X=DE(DQ) G A:'DV,A:DC<2,N^DIE17
I I DV'["I",DV'["#" G RD
 D E^DIE0 G RD:$D(X),PR
 Q
SAVEVALS S @DIEZTMP@("V",DP,DIIENS,DIFLD,"O")=$G(DE(DQ)) S:$D(^("F"))[0 ^("F")=$G(DE(DQ))
 I $D(DE(DW,"4/")) S @DIEZTMP@("V",DP,DIIENS,DIFLD,"4/")=""
 E  K @DIEZTMP@("V",DP,DIIENS,DIFLD,"4/")
 Q
NKEY W:'$D(ZTQUEUED) "??  Required key field" S X="?BAD" G QS
KEYCHK() Q:$G(DE(DW,"KEY"))="" 1 Q @DE(DW,"KEY")
BEGIN S DNM="IBXSA23",DQ=1
1 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=1 D X1 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X1 S DIPA("NVA_PRV-0")=$G(^IBA(355.93,DIPA("NVA_PRV"),0))
 Q
2 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=2 D X2 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X2 S:$P(DIPA("NVA_PRV-0"),U,2)=1 Y="@1032"
 Q
3 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=3 D X3 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X3 S:$P(DIPA("NVA_PRV-0"),U,3)'="" Y="@1031"
 Q
4 S DW="0;3",DV="FX",DU="",DIFLD=.03,DLB=$$LABEL^DIALOGZ(DP,DIFLD)
 G RE
X4 K:$L(X)>3!($L(X)<1) X I $D(X),$P($G(^IBA(355.93,DA,0)),U,2)'=2 K X
 I $D(X),X'?.ANP K X
 Q
 ;
5 S DQ=6 ;@1031
6 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=6 D X6 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X6 S:$P(DIPA("NVA_PRV-0"),U,3)'="" Y="@1035"
 Q
7 S DW="0;4",DV="FX",DU="",DIFLD=.04,DLB=$$LABEL^DIALOGZ(DP,DIFLD)
 G RE
X7 K:$L(X)>2!($L(X)<2) X I $D(X),$P($G(^IBA(355.93,DA,0)),U,2)'=2 K X
 I $D(X),X'?.ANP K X
 Q
 ;
8 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=8 D X8 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X8 S Y="@1035"
 Q
9 S DQ=10 ;@1032
10 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=10 D X10 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X10 S:$P(DIPA("NVA_PRV-0"),U,5)'=""&($P(DIPA("NVA_PRV-0"),U,6)'="")&($P(DIPA("NVA_PRV-0"),U,7)'="") Y="@1033"
 Q
11 S DW="0;5",DV="RFX",DU="",DIFLD=.05,DLB=$$LABEL^DIALOGZ(DP,DIFLD)
 G RE
X11 S X=$$UP^XLFSTR(X) K:$L(X)>30!($L(X)<1)!$$BADADD^IBCEP8B(X) X I $D(X),$P($G(^IBA(355.93,DA,0)),U,2)'=1 K X
 I $D(X),X'?.ANP K X
 Q
 ;
12 S DW="0;10",DV="FX",DU="",DIFLD=.1,DLB=$$LABEL^DIALOGZ(DP,DIFLD)
 G RE
X12 K:$L(X)>30!($L(X)<1) X I $D(X),$P($G(^IBA(355.93,DA,0)),U,2)'=1 K X
 I $D(X),X'?.ANP K X
 Q
 ;
13 S DW="0;6",DV="FX",DU="",DIFLD=.06,DLB=$$LABEL^DIALOGZ(DP,DIFLD)
 G RE
X13 K:$L(X)>20!($L(X)<1) X I $D(X),$P($G(^IBA(355.93,DA,0)),U,2)'=1 K X
 I $D(X),X'?.ANP K X
 Q
 ;
14 S DW="0;7",DV="P5'X",DU="",DIFLD=.07,DLB=$$LABEL^DIALOGZ(DP,DIFLD)
 S DU="DIC(5,"
 G RE
X14 I $D(X),$P($G(^IBA(355.93,DA,0)),U,2)'=1 K X
 Q
 ;
15 S DW="0;8",DV="RFX",DU="",DIFLD=.08,DLB=$$LABEL^DIALOGZ(DP,DIFLD)
 G RE
X15 K:$L(X)>10!($L(X)<9)!'((X?9N)!(X?5N1"-"4N))!($E(X,$L(X)-3,$L(X))="0000") X I $D(X),$P($G(^IBA(355.93,DA,0)),U,2)'=1 K X
 I $D(X),X'?.ANP K X
 Q
 ;
16 S DQ=17 ;@1033
17 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=17 D X17 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X17 S:$P(DIPA("NVA_PRV-0"),U,9)'="" Y="@1034"
 Q
18 S DW="0;9",DV="FX",DU="",DIFLD=.09,DLB="LAB OR FACILITY PRIMARY ID"
 S DE(DW)="C18^IBXSA23"
 G RE
C18 G C18S:$D(DE(18))[0 K DB
 S X=DE(18),DIC=DIE
 K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(1)=$S($D(^IBA(355.93,D0,0)):^(0),1:"") S X=$P(Y(1),U,13),X=X S DIU=X K Y S X="" S DIH=$G(^IBA(355.93,DIV(0),0)),DIV=X S $P(^(0),U,13)=DIV,DIH=355.93,DIG=.13 D ^DICR
C18S S X="" G:DG(DQ)=X C18F1 K DB
 S X=DG(DQ),DIC=DIE
 X ^DD(355.93,.09,1,1,1.3) I X S X=DIV S Y(1)=$S($D(^IBA(355.93,D0,0)):^(0),1:"") S X=$P(Y(1),U,13),X=X S DIU=X K Y S X=DIV S X="16" S DIH=$G(^IBA(355.93,DIV(0),0)),DIV=X S $P(^(0),U,13)=DIV,DIH=355.93,DIG=.13 D ^DICR
C18F1 Q
X18 K:$L(X)>10!($L(X)<1) X I $D(X),$P($G(^IBA(355.93,DA,0)),U,2)'=1 K X
 I $D(X),X'?.ANP K X
 Q
 ;
19 S DQ=20 ;@1034
20 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=20 D X20 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X20 S:$P(DIPA("NVA_PRV-0"),U,11)'="" Y="@1035"
 Q
21 D:$D(DG)>9 F^DIE17,DE S DQ=21,DW="0;11",DV="S",DU="",DIFLD=.11,DLB=$$LABEL^DIALOGZ(DP,DIFLD)
 S DU="77:SERVICE LOCATION;FA:FACILITY;LI:INDEPENDENT LAB;TL:TESTING LAB;"
 G RE
X21 Q
22 S DQ=23 ;@1035
23 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=23 D X23 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X23 S:$P(DIPA("NVA_PRV-0"),U,14)'="" Y="@1036"
 Q
24 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=24 D X24 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X24 D EN2^IBCEP82(DIPA("NVA_PRV"),4)
 Q
25 S DQ=26 ;@1036
26 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=26 D X26 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X26 S:$D(^IBA(355.93,DIPA("NVA_PRV"),"TAXONOMY"))>0 Y="@1037"
 Q
27 S D=0 K DE(1) ;42
 S DIFLD=42,DGO="^IBXSA210",DC="3^355.9342IP^TAXONOMY^",DV="355.9342M*P8932.1'",DW="0;1",DOW=$$LABEL^DIALOGZ(DP,DIFLD),DLB=$P($$EZBLD^DIALOG(8042,DOW),": ") S:D DC=DC_D
 S DU="USC(8932.1,"
 G RE:D I $D(DSC(355.9342))#2,$P(DSC(355.9342),"I $D(^UTILITY(",1)="" X DSC(355.9342) S D=$O(^(0)) S:D="" D=-1 G M27
 S D=$S($D(^IBA(355.93,DA,"TAXONOMY",0)):$P(^(0),U,3,4),$O(^(0))'="":$O(^(0)),1:-1)
M27 I D>0 S DC=DC_D I $D(^IBA(355.93,DA,"TAXONOMY",+D,0)) S DE(27)=$P(^(0),U,1)
 G RE
R27 D DE
 S D=$S($D(^IBA(355.93,DA,"TAXONOMY",0)):$P(^(0),U,3,4),1:1) G 27+1
 ;
28 S DQ=29 ;@1037
29 G 1^DIE17
