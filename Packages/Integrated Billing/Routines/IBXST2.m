IBXST2 ; ;12/13/04
 S X=DG(DQ),DIC=DIE
 K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(0)=X S Y(1)=$S($D(^DGCR(399,D0,"S")):^("S"),1:"") S X=$P(Y(1),U,14)="" I X S X=DIV S Y(1)=$S($D(^DGCR(399,D0,"S")):^("S"),1:"") S X=$P(Y(1),U,14),X=X S DIU=X K Y S X=DIV S X=DT X ^DD(399,12,1,1,1.4)
 S X=DG(DQ),DIC=DIE
 X ^DD(399,12,1,2,1.3) I X S X=DIV S Y(1)=$S($D(^DGCR(399,D0,"S")):^("S"),1:"") S X=$P(Y(1),U,15),X=X S DIU=X K Y S X=DIV S X=DUZ S DIH=$G(^DGCR(399,DIV(0),"S")),DIV=X S $P(^("S"),U,15)=DIV,DIH=399,DIG=15 D ^DICR
 S X=DG(DQ),DIC=DIE
 X ^DD(399,12,1,3,1.3) I X S X=DIV S Y(1)=$S($D(^DGCR(399,D0,"S")):^("S"),1:"") S X=$P(Y(1),U,13),X=X S DIU=X K Y S X=DIV S X=DUZ S DIH=$G(^DGCR(399,DIV(0),"S")),DIV=X S $P(^("S"),U,13)=DIV,DIH=399,DIG=13 D ^DICR
 S X=DG(DQ),DIC=DIE
 S ^DGCR(399,"AP",$E(X,1,30),DA)=""
