YSXRAK11 ; COMPILED XREF FOR FILE #615.34 ; 10/03/13
 ; 
 S DA=0
A1 ;
 I $D(DISET) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^YS(615.2,DA(1),5,DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^YS(615.2,DA(1),5,DA,0))
 S X=$P($G(DIKZ(0)),U,1)
 I X'="" S ^YS(615.2,DA(1),5,"B",$E(X,1,30),DA)=""
 G:'$D(DIKLM) A Q:$D(DISET)
END G ^YSXRAK12
