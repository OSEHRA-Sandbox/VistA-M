IBDX15 ; COMPILED XREF FOR FILE #357.14 ; 11/30/04
 ; 
 S DA=0
A1 ;
 I $D(DIKILL) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^IBE(357.1,DA(1),"H",DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^IBE(357.1,DA(1),"H",DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" K ^IBE(357.1,DA(1),"H","B",$E(X,1,30),DA)
 G:'$D(DIKLM) A Q:$D(DIKILL)
END Q
