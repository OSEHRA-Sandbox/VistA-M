PRCHT373 ; ;01/09/98
 D DE G BEGIN
DE S DIE="^PRC(442,D0,2,",DIC=DIE,DP=442.01,DL=2,DIEL=1,DU="" K DG,DE,DB Q:$O(^PRC(442,D0,2,DA,""))=""
 I $D(^(0)) S %Z=^(0) S %=$P(%Z,U,4) S:%]"" DE(1)=%
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
BEGIN S DNM="PRCHT373",DQ=1
1 S DW="0;4",DV="RFX",DU="",DLB="BOC",DIFLD=3.5
 S DE(DW)="C1^PRCHT373"
 S X=$$SUPBOC^PRCHNPO7(-1)
 S Y=X
 G Y
C1 G C1S:$D(DE(1))[0 K DB S X=DE(1),DIC=DIE
 K ^PRC(442,DA(1),2,"D",+$P(X," ",1),DA)
 S X=DE(1),DIC=DIE
 S ALN=$P(^PRC(442,DA(1),2,DA,0),"^") K:ALN>0 ^PRC(442,DA(1),2,"AH",+X,ALN,DA) K ALN
C1S S X="" Q:DG(DQ)=X  K DB S X=DG(DQ),DIC=DIE
 S ^PRC(442,DA(1),2,"D",+$P(X," ",1),DA)=""
 S X=DG(DQ),DIC=DIE
 S ALN=$P(^PRC(442,DA(1),2,DA,0),"^") S:ALN>0 ^PRC(442,DA(1),2,"AH",+X,ALN,DA)="" K ALN
 Q
X1 N Z0 S Z0=$P(^PRC(442,DA(1),0),"^",5) K:'Z0 X I $D(X) K:'$D(^PRCD(420.1,Z0,0)) X I $D(X) S X=$$SUPBOC^PRCHNPO7 D EN8^PRCHNPO7
 I $D(X),X'?.ANP K X
 Q
 ;
2 S DQ=3 ;@87
3 S DQ=4 ;@6
4 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=4 D X4 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X4 S PRCHTOT=0,PRCHSCN="" F I=0:0 S PRCHSCN=$O(^PRC(442.8,"B",PRCHPONO,PRCHSCN)) Q:PRCHSCN=""  I $P(^PRC(442.8,PRCHSCN,0),U,2)=DA S PRCHTOT=PRCHTOT+$P(^(0),U,5)
 Q
5 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=5 D X5 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X5 S PRCHQTY=$P(^PRC(442,DA(1),2,DA,0),U,2)
 Q
6 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=6 D X6 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X6 I PRCHTOT>PRCHQTY S PRCHDA1=DA(1),PRCHDA=DA,PRCHLINO=$P(^PRC(442,DA(1),2,PRCHREC,0),U) W !,"Line Item # = ",PRCHLINO,!,"Quantity Ordered: "_$P(^PRC(442,DA(1),2,PRCHREC,0),U,2),! S Y="@555"
 Q
7 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=7 D X7 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X7 W !!,"Enter/Edit Delivery Schedule for this Item?  NO// " R X:DTIME S:'$T X="^" S:X="" X="N" S:X["?" Y="@6" S:"Yy?"'[$E(X) Y="@56" W "  "_$S("Yy"[$E(X):"(YES)","Nn"[$E(X):"(NO)",1:"")
 Q
8 S DQ=9 ;@16
9 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=9 D X9 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X9 S PRCHDA=DA,PRCHDA1=DA(1),PRCHLINO=$P(^PRC(442,DA(1),2,PRCHREC,0),U,1) W !,"Line Item # = ",PRCHLINO,!,"Quantity Ordered: "_$P(^(0),U,2),!
 Q
10 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=10 D X10 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X10 I PRCHTOT=PRCHQTY W !,"Delivery schedule quantity of ",PRCHTOT," equals the order quantity.",!,"You may edit delivery schedule(s), but cannot add a new schedule.",!!
 Q
11 S DQ=12 ;@555
12 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=12 D X12 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X12 I PRCHTOT>PRCHQTY W !,"Delivery schedule quantity of ",PRCHTOT," EXCEEDS order quantity of ",PRCHQTY,".",!,"You must edit one or more schedules so that the delivery total equals",!,"no more than ",PRCHQTY,".",!!
 Q
13 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=13 D X13 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X13 S:$D(PRCHSEEN) Y="@55"
 Q
14 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=14 D X14 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X14 S PRCHSEEN=1 W !!,"** To DELETE a schedule, zero out the quantity to be delivered. To add a new",!,"delivery schedule do the following:",!!
 Q
15 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=15 D X15 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X15 W "a. If there is no delivery schedule already in file answer 'Yes' when asked if     you are adding a new delivery schedule."
 Q
16 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=16 D X16 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X16 W !,"b. If there is only one delivery schedule already in the file you will see         'OK? YES//' answer 'No' and then answer 'Yes' when asked if you are adding a    new delivery schedule."
 Q
17 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=17 D X17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X17 W !,"c. If there is more than one delivery schedule in the file, hit <return> key at    'CHOOSE' prompt and answer 'Yes' when asked if you are adding a new delivery    schedule.",!!
 Q
18 S DQ=19 ;@55
19 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=19 X DR(99,1,9.2) S Y(101)=$S($D(^PRC(442.8,D0,0)):^(0),1:"") S X=$P(Y(101),U,1),Y(102)=X S X=$P(Y(101),U,1) S D0=I(0,0) S D1=I(1,0) S X=$S(D(0)>0:D(0),1:"")
 S DGO="^PRCHT374",DC="^442.8^PRC(442.8," G DIEZ^DIE0
R19 D DE G A
 ;
20 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=20 D X20 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X20 S PRCHQTY=$P(^PRC(442,PRCHIEN,2,DA,0),U,2)
 Q
21 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=21 D X21 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X21 I $G(PRCHFLG),PRCHTOT>$G(PRCHQTY) W !!,"Delivery schedule(s) quantity of ",PRCHTOT," is still greater than ordered quantity of ",$G(PRCHQTY),".",!! S Y="@16"
 Q
22 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=22 D X22 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X22 I $G(PRCHFLG) S PRCHFLG=0 W !!!!,"****Returning to Item Multiple editing session.",!! S Y="@44"
 Q
23 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=23 D X23 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X23 I PRCHTOT>PRCHQTY S Y="@555"
 Q
24 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=24 D X24 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X24 W !!,"Enter/Edit Delivery Schedule for this Item?  NO// " R X:DTIME S:'$T X="^" S:X="" X="N" S:X["?" Y="@6" S:"Yy?"[$E(X) Y="@16"
 Q
25 S DQ=26 ;@56
26 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=26 D X26 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X26 K DIE("NO^"),PRCHDA1,PRCHDA,PRCHSCN
 Q
27 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=27 D X27 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X27 K PRCHTOT,PRCHQTY,PRCHSEEN,PRCHMORE
 Q
28 G 1^DIE17
