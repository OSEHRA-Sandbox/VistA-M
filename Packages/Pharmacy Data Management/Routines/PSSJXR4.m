PSSJXR4 ; COMPILED XREF FOR FILE #55.0108 ; 12/13/16
 ; 
 S DA=0
A1 ;
 I $D(DIKILL) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^PS(55,DA(1),"SCTALK",DA)) I DA'>0 S DA=0 G END
1 ;
 G:'$D(DIKLM) A Q:$D(DIKILL)
END G ^PSSJXR5
