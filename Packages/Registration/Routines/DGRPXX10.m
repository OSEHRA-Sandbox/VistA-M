DGRPXX10 ; COMPILED XREF FOR FILE #2.98 ; 06/24/93
 ; 
 S DA=0
A1 ;
 I $D(DIKILL) K DIKLM S:$D(DA(1)) DIKLM=1 G:$D(DA(1)) 1 S DA(1)=DA,DA=0 G @DIKM1
0 ;
A S DA=$O(^DPT(DA(1),"S",DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$S($D(^DPT(DA(1),"S",DA,0))#2:^(0),1:"")
 S X=$P(DIKZ(0),U,2)
 I X'="" K ^DPT("ASDCN",$P(^(0),"^"),DA,DA(1))
 G:'$D(DIKLM) A Q:$D(DIKILL)
END G ^DGRPXX11
