PSREF ; XAK,MJK/ALBANY ; 12 DEC 84  8:55 AM
 ;4.02 ; 12/13/84
 S PSFROM="PSREF",PPSITE=%PSITE,%PSITE=$P(%PSITE,"^",1,3)_"^1^"_$P(%PSITE,"^",5,99)
2 K PSMP R !,"PATIENT: ",X:$S($D(DTIME):DTIME,1:99999) G Q:"^"[X!'$T,RX:X?1"RX".N
 S DIC="^DPT(",DIC(0)="QEM" D ^DIC G 2:Y<0 I '$D(^PS(55,+Y,"P")) W !?5,"NO PRESCRIPTION INFORMATION" G 2
 S DFN=+Y I $D(^DPT(DFN,.1)),^(.1)]"" W !?10,*7,"PATIENT IS AN INPATIENT IN WARD ",^(.1) R !!,"DO YOU WISH TO CONTINUE? (Y/N) N// ",ANS G:"N"[$E(ANS,1) 2
 S ADDR="" I $D(^(.11)),$P(^(.11),"^",1)]"" S ADDR=^(.11)
 I $D(^(.121)) S X=$S($P(^(.121),"^",8):$P(^(.121),"^",8),$P(^(.121),"^",7):9999999,1:0) I DT'<$P(^(.121),"^",7),DT'>X S ADDR=^(.121),ADDRFL=""
 I ADDR]"" S X=ADDR W:$D(ADDRFL) !!,*7,?2,">>TEMPORARY ADDRESS<<" W !?5,$P(X,"^",1),"  ",$P(X,"^",4),"  ",$S($D(^DIC(5,+$P(X,"^",5),0)):$P(^(0),"^",2),1:""),"  ",$P(X,"^",6) K ADDR,ADDRFL
 E  W !,"NO ADDRESS INFORMATION",!
 I $D(^PS(55,DFN,1)),^(1)]"" S X=^(1) W !!?5 F I=1:1 Q:$P(X," ",I,99)=""  W $P(X," ",I)," " W:$X>75 !
 W ! S DIE=DIC,DA=DFN,DR=$S($P(%PSITE,"^",25):"[PSREF]",1:"[PSREFA]") D ^DIE
 S PPL="" D IN G 2
IN S PNM=$P(^DPT(DFN,0),"^",1),LIM=10,NOW=$P($H,",",2)\60,NOW=NOW\60*100+(NOW#60)+1/10000+DT,DRUG="" D P2^PSRENEW K RX
C Q:'PC  W !!,"CHOOSE 1-",PC,"> " R PL G CQ:PPL=""&("^"[PL),CCC^PSREF1:"^"[PL,C:PL'?.ANP S X=PL,PSPOP="Refill" D PY^PSUTL G C:PSPOP
R K PSDUP,PSPOP,PSPR I $P(%PSITE,"^",7) S SPND="" G R3
R0 R !,"REFILL DATE: TODAY//",X G CQ:"^"=X S:X="" X="T" S %DT="EX" D ^%DT G R0:Y<0 S RFD=Y
R1 R !,"MAIL/WINDOW: MAIL//",X G CQ:"^"=X S:X="" X="M" G R1:"MW"'[$E(X,1) S MW=$E(X,1)
R3 S:$D(PSCLC) CC=PSCLC I '$D(PSCLC) R !,"CLERK CODE: ",X G CQ:"^"=X G R3:$L(X)<2!($L(X)>5) S CC=X
 G ^PSREF1
CQ K PC,PSMW,PSA,PPL,PL,PY,PI,PNM,DFN,NOW,PR,IOP,RCT,SPND,NEW1,NEW11,RX0,RX2 W ! Q
RX S X=$P(X,"RX",2),DIC="^PSRX(",DIC(0)="QEZ" D ^DIC G 2:Y<0 S (J,PY(1))=+Y,PL=1,PPL="",RX0=Y(0),RX2=$S($D(^PSRX(J,2)):^(2),1:"") D R G 2
Q S %PSITE=PPSITE K PPSITE Q
