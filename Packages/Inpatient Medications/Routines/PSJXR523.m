PSJXR523 ; COMPILED XREF FOR FILE #55.09 ; 11/04/94
 ; 
 S DA(1)=0 S DA=0
A1 ;
 I $D(DISET) K DIKLM S:$D(DA(2)) DIKLM=1 G:$D(DA(2)) 2 S DA(2)=DA(1) S DA(1)=DA,DA=0 G @DIKM1
A S DA(1)=$O(^PS(55,DA(2),5,DA(1))) I DA(1)'>0 S DA(1)=0 G END
1 ;
B S DA=$O(^PS(55,DA(2),5,DA(1),9,DA)) I DA'>0 S DA=0 Q:DIKM1=1  G A
2 ;
 S DIKZ(0)=$S($D(^PS(55,DA(2),5,DA(1),9,DA,0))#2:^(0),1:"")
 S X=$P(DIKZ(0),U,1)
 I X'="" I '$D(DIU(0)) D:$D(PSGAL(35))#2 KILL^PSGAL5:PSGAL(35)=X K PSGAL
 S X=$P(DIKZ(0),U,2)
 I X'="" I '$D(DIU(0)) D:$D(PSGAL(36))#2 KILL^PSGAL5:PSGAL(36)=X K PSGAL
 S X=$P(DIKZ(0),U,3)
 I X'="" I '$D(DIU(0)) D:$D(PSGAL(37))#2 KILL^PSGAL5:PSGAL(37)=X K PSGAL
 S X=$P(DIKZ(0),U,4)
 I X'="" I '$D(DIU(0)) D:$D(PSGAL(78)) KILL^PSGAL5:PSGAL(78)=X K PSGAL
 S X=$P(DIKZ(0),U,5)
 I X'="" I '$D(DIU(0)) D:$D(PSGAL(79)) KILL^PSGAL5:PSGAL(79)=X K PSGAL
 G:'$D(DIKLM) B Q:$D(DISET)
END G ^PSJXR524
