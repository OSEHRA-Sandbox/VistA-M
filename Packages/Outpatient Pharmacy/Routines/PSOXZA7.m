PSOXZA7 ; COMPILED XREF FOR FILE #52.2 ; 02/21/19
 ; 
 S DA=0
A1 ;
 I $D(DIKILL) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^PSRX(DA(1),"P",DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^PSRX(DA(1),"P",DA,0))
 S X=$P($G(DIKZ(0)),U,9)
 I X'="" D KAS2^PSOSUTL
 S X=$P($G(DIKZ(0)),U,16)
 I X'="" K ^PSRX("AN",$E(X,1,30),DA(1),DA)
 S X=$P($G(DIKZ(0)),U,19)
 I X'="" K ^PSRX("AM",$E(X,1,30),DA(1),DA)
 S DIKZ("PF")=$G(^PSRX(DA(1),"P",DA,"PF"))
 S X=$P($G(DIKZ("PF")),U,1)
 I X'="" K ^PSRX("PFIL",$E(X,1,30),DA(1),DA)
 S X=$P($G(DIKZ(0)),U,1)
 I X'="" K ^PSRX("ADP",$E(X,1,7),DA(1),DA)
 S X=$P($G(DIKZ(0)),U,1)
 I X'="" D:'$G(PSOSUSPA) PARKILL^PSOUTLA
CR1 S DIXR=463
 K X
 S X(1)=$P(DIKZ(0),U,10)
 S X(2)=$P(DIKZ(0),U,19)
 S X=$G(X(1))
 I $G(X(1))]"",$G(X(2))]"" D
 . K X1,X2 M X1=X,X2=X
 . S:$D(DIKIL) (X2,X2(1),X2(2))=""
 . D SKIDX^PSOPXRMU(.X,.DA,"P","K")
CR2 K X
 G:'$D(DIKLM) A Q:$D(DIKILL)
END G ^PSOXZA8
