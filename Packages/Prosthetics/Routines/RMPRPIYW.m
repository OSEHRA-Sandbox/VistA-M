RMPRPIYW ;HINCIO/ODJ - PIP Data Entry - Transfer;3/8/01
 ;;3.0;PROSTHETICS;**61**;Feb 09, 1996
 Q
 ;
 ;***** FLOCNM - Prompt for 'FROM' location
FLOCNM(RMPRSTN,RMPR5,RMPREXC) ;
 N RMPRERR,DIR,X,Y,DUOUT,DTOUT,DIROUT,RMPRYN,DA,RMPRTDT
 D NOW^%DTC S RMPRTDT=X
 S RMPREXC=""
 S RMPRERR=0
 S DIR(0)="FOA^1:30"
 S DIR("A")="From Location: "
 S DIR("?")="^D QM^RMPRPIYB"
 S DIR("??")="^D QM2^RMPRPIYB"
FLOCNM1 D ^DIR
 I $D(DTOUT) S RMPREXC="T" G FLOCNMX
 I $D(DIROUT) S RMPREXC="P" G FLOCNMX
 I X=""!(X["^")!($D(DUOUT)) S RMPREXC="^" G FLOCNMX
 K RMPR5
 S RMPR5("STATION")=RMPRSTN
 S RMPR5("NAME")=X
 D LIKE^RMPRPIYB(RMPRSTN,X,.RMPREXC,.RMPR5)
 I $G(RMPR5("IEN"))="" D  G FLOCNM1
 . W !,"Please enter a valid Location"
 . Q
FLOCNMX Q RMPRERR
 ;
 ;***** TLOCNM - Prompt for 'TO' Location
TLOCNM(RMPRSTN,RMPR5,RMPREXC) ;
 N RMPRERR,DIR,X,Y,DUOUT,DTOUT,DIROUT,RMPRYN,DA,RMPRTDT
 D NOW^%DTC S RMPRTDT=X
 S RMPREXC=""
 S RMPRERR=0
 S DIR(0)="FOA^1:30"
 S DIR("A")="Enter Receiving Location: "
 S DIR("?")="^D QM^RMPRPIYB"
 S DIR("??")="^D QM2^RMPRPIYB"
TLOCNM1 D ^DIR
 I $D(DTOUT) S RMPREXC="T" G TLOCNMX
 I $D(DIROUT) S RMPREXC="P" G TLOCNMX
 I X=""!(X["^")!($D(DUOUT)) S RMPREXC="^" G TLOCNMX
 K RMPR5
 S RMPR5("STATION")=RMPRSTN
 S RMPR5("NAME")=X
 D LIKE^RMPRPIYB(RMPRSTN,X,.RMPREXC,.RMPR5)
 I $G(RMPR5("IEN"))="" D  G TLOCNM1
 . W !,"Please enter a valid Location"
 . Q
TLOCNMX Q RMPRERR
