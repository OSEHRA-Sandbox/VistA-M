PSOXZA13 ; COMPILED XREF FOR FILE #52.07 ; 12/06/18
 ; 
 S DA=0
A1 ;
 I $D(DISET) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^PSRX(DA(1),"RTS",DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^PSRX(DA(1),"RTS",DA,0))
 S X=$P($G(DIKZ(0)),U,1)
 I X'="" S ^PSRX(DA(1),"RTS","B",$E(X,1,30),DA)=""
CR1 S DIXR=758
 K X
 S X(1)=$P(DIKZ(0),U,1)
 S X=$G(X(1))
 I $G(X(1))]"" D
 . K X1,X2 M X1=X,X2=X
 . S ^PSRX("ARTS",X,DA(1),DA)=""
CR2 K X
 G:'$D(DIKLM) A Q:$D(DISET)
END G ^PSOXZA14
