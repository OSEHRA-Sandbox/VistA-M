YSXRAC1 ; COMPILED XREF FOR FILE #601.2 ; 12/09/04
 ; 
 S DIKZK=2
 S DIKZ(0)=$G(^YTD(601.2,DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" K ^YTD(601.2,"B",$E(X,1,30),DA)
END G ^YSXRAC2
