YSXRAK3 ; COMPILED XREF FOR FILE #615.23 ; 11/30/04
 ; 
 S DA=0
A1 ;
 I $D(DIKILL) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^YS(615.2,DA(1),15,DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^YS(615.2,DA(1),15,DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" K ^YS(615.2,DA(1),15,"B",$E(X,1,30),DA)
 G:'$D(DIKLM) A Q:$D(DIKILL)
END G ^YSXRAK4
