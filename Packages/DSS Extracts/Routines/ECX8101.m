ECX8101 ; COMPILED XREF FOR FILE #727.81 ; 02/20/19
 ; 
 S DIKZK=2
 S DIKZ(0)=$G(^ECX(727.81,DA,0))
 S X=$P($G(DIKZ(0)),U,3)
 I X'="" K ^ECX(727.81,"AC",$E(X,1,30),DA)
 S X=$P($G(DIKZ(0)),U,17)
 I X'="" S DH=727.81,DV=16,DU=1 S DIIX=2 D:$G(DIK(0))'["A" AUDIT^DIK1
END Q
