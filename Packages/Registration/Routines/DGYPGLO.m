DGYPGLO ;ALB/LM - TREATING SPECIALTY INPATIENT LISTING OUTPUT ;2-2-93
 ;;5.2;REGISTRATION;**27**;JUL 29,1992
 ;
 Q
 ;
START D KILL
 ;
 D NOW^%DTC S Y=$E(%,1,12) X ^DD("DD") S DGNOW=Y ; used to print date/time of report
 ;
 S (WCT,TCT,TSCT,END)=0
 ;
 I PTLWD D ^DGYPGLO1 G:END END
 I PTLTS D ^DGYPGLO2 G:END END
 I PTCTS D ^DGYPGLO3 G:END END
 ;
END K DGTSDT
 D END^DGYPGLO1
 D END^DGYPGLO2
 D END^DGYPGLO3
KILL K WCT,TCT,TSCT,PAGE,%,ADMDT,D,D1,DGNOW,END,I,L,N,N1,T,T1,TREAT,W,W1,X,Y,ID,DFN,DGT,DG1,DGA1,DGXFR0,X1
 Q
 ;
SUB Q:END  D:$Y+6>IOSL HEAD W !,"------------------",!,"SUBCOUNT = "
 Q
 ;
HEAD I PAGE,$E(IOST,1,2)="C-" W !,"Press RETURN to continue or '^' to exit: " R X:DTIME S:'$T!(X="^") END=1 Q:END
HEAD1 W:'($E(IOST,1,2)'="C-"&'$D(PAGE)) @IOF
HEAD2 S PAGE=PAGE+1
 W !!,"Treating Specialty Inpatient Information"
 W ?(IOM-38),"Date/Time Printed: ",DGNOW
 W !,"as of ",DGTSDT
 W ?(IOM-10),"PAGE ",$J(PAGE,3)
 W !?(IOM-$L(REPORT)\2),REPORT
 Q
