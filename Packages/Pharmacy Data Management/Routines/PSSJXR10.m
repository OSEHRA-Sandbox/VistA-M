PSSJXR10 ; COMPILED XREF FOR FILE #55.02 ; 12/06/18
 ; 
 S DA(2)=DA(1) S DA(1)=0 S DA=0
A1 ;
 I $D(DIKILL) K DIKLM S:DIKM1=2 DIKLM=1 S:DIKM1'=2&'$G(DIKPUSH(2)) DIKPUSH(2)=1,DA(2)=DA(1),DA(1)=DA,DA=0 G @DIKM1
A S DA(1)=$O(^PS(55,DA(2),"IV",DA(1))) I DA(1)'>0 S DA(1)=0 G END
1 ;
B S DA=$O(^PS(55,DA(2),"IV",DA(1),"AD",DA)) I DA'>0 S DA=0 Q:DIKM1=1  G A
2 ;
 S DIKZ(0)=$G(^PS(55,DA(2),"IV",DA(1),"AD",DA,0))
 S X=$P($G(DIKZ(0)),U,2)
 I X'="" X ^DD(55.02,.02,1,1,2)
 S DIKZ(0)=$G(^PS(55,DA(2),"IV",DA(1),"AD",DA,0))
 S X=$P($G(DIKZ(0)),U,3)
 I X'="" X ^DD(55.02,.03,1,1,2)
 S DIKZ(0)=$G(^PS(55,DA(2),"IV",DA(1),"AD",DA,0))
 S X=$P($G(DIKZ(0)),U,1)
 I X'="" X ^DD(55.02,.01,1,1,2)
 G:'$D(DIKLM) B Q:$D(DIKILL)
END G ^PSSJXR11
