IBDX9511 ; COMPILED XREF FOR FILE #357.952 ; 11/30/04
 ; 
 S DA=0
A1 ;
 I $D(DISET) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^IBD(357.95,DA(1),2,DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^IBD(357.95,DA(1),2,DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" S ^IBD(357.95,DA(1),2,"B",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,1)
 I X'="" S:$P($G(^IBD(357.95,DA(1),2,DA,0)),"^",2)]"" ^IBD(357.95,"AD",DA(1),X,$P(^IBD(357.95,DA(1),2,DA,0),"^",2),DA)=^IBD(357.95,DA(1),2,DA,0)
 S X=$P(DIKZ(0),U,2)
 I X'="" S:X]"" ^IBD(357.95,"AD",DA(1),$P($G(^IBD(357.95,DA(1),2,DA,0)),"^"),X,DA)=^IBD(357.95,DA(1),2,DA,0)
 G:'$D(DIKLM) A Q:$D(DISET)
END G ^IBDX9512
