TIUEPN6 ; ;07/28/17
 S X=DE(21),DIC=DIE
 I +$P($G(^TIU(8925,+DA,0)),U),+$P($G(^TIU(8925,+DA,13)),U),+$P($G(^TIU(8925,+DA,0)),U,5) K ^TIU(8925,"ALOC",+X,+$P($G(^TIU(8925,+DA,0)),U),+$P($G(^TIU(8925,+DA,0)),U,5),(9999999-$P($G(^TIU(8925,+DA,13)),U)),DA)
 S X=DE(21),DIC=DIE
 I +$P($G(^TIU(8925,+DA,15)),U) K ^TIU(8925,"ALOCP",+X,+$P($G(^TIU(8925,+DA,15)),U),+DA)
