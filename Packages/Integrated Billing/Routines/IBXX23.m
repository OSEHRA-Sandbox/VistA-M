IBXX23 ; COMPILED XREF FOR FILE #399.043 ; 09/01/09
 ; 
 S DA=0
A1 ;
 I $D(DISET) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^DGCR(399,DA(1),"OP",DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^DGCR(399,DA(1),"OP",DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" S ^DGCR(399,"AOPV",$P(^DGCR(399,DA(1),0),U,2),$E(X,1,30),DA(1))=""
 S X=$P(DIKZ(0),U,1)
 I X'="" S DGRVRCAL=1
 G:'$D(DIKLM) A Q:$D(DISET)
END G ^IBXX24
