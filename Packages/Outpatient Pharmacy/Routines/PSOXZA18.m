PSOXZA18 ; COMPILED XREF FOR FILE #52.2551 ; 07/31/17
 ; 
 S DA(2)=DA(1) S DA(1)=0 S DA=0
A1 ;
 I $D(DISET) K DIKLM S:DIKM1=2 DIKLM=1 S:DIKM1'=2&'$G(DIKPUSH(2)) DIKPUSH(2)=1,DA(2)=DA(1),DA(1)=DA,DA=0 G @DIKM1
A S DA(1)=$O(^PSRX(DA(2),"REJ",DA(1))) I DA(1)'>0 S DA(1)=0 G END
1 ;
B S DA=$O(^PSRX(DA(2),"REJ",DA(1),"COM",DA)) I DA'>0 S DA=0 Q:DIKM1=1  G A
2 ;
 S DIKZ(0)=$G(^PSRX(DA(2),"REJ",DA(1),"COM",DA,0))
 S X=$P($G(DIKZ(0)),U,1)
 I X'="" S ^PSRX(DA(2),"REJ",DA(1),"COM","B",$E(X,1,30),DA)=""
 G:'$D(DIKLM) B Q:$D(DISET)
END Q
