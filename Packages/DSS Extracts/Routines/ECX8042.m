ECX8042 ; COMPILED XREF FOR FILE #727.804 ; 01/04/05
 ; 
 S DIKZK=1
 S DIKZ(0)=$G(^ECX(727.804,DA,0))
 S X=$P(DIKZ(0),U,3)
 I X'="" S ^ECX(727.804,"AC",$E(X,1,30),DA)=""
END Q
