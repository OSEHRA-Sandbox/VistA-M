YSWZ ;ALB/ASF-MENTAL HEALTH WAIT LIST SUB MENU DRIVER ; 7/10/89  11:17 ;
 ;;5.01;MENTAL HEALTH;;Dec 30, 1994
 ;
 D DT^DICRW S DIK="^DOPT(""YSWAIT"","
 G:$D(^DOPT("YSWAIT",5)) A S ^DOPT("YSWAIT",0)="WAIT LIST OPTION^1N^" F I=1:1 S X=$T(@I) Q:X=""  S ^DOPT("YSWAIT",I,0)=$P(X,";",2,99)
 D IXALL^DIK
A K DIC W !! S DIC="^DOPT(""YSWAIT"",",DIC(0)="QEAM" D ^DIC Q:Y<0  D @+Y G A
 ;
1 ; Called from MENU option YSWAITP
 ; PRINT WAIT LISTS
 D 1^YSWX G:Y<1 END S %ZIS="Q" D ^%ZIS G:POP END K IOP I $D(IO("Q")) S ZTRTN="11^YSWZ",ZTSAVE("N")="",ZTDESC="YS WAIT LIST PT" D ^%ZTLOAD,END Q
11 D LP^YSWX,DSP^YSWX,KILL^%ZTLOAD,^%ZISC,END Q
 ;
2 ; Called from MENU option YSWAITE
 ; ADD PATIENT TO WAITING LIST
 S YSM=1 D 1^YSWX S DIC(0)="AELMQ",DLAYGO=617 D:Y>0 2^YSWX D END Q
 ;
3 ; Called from MENU option YSWAITEDI
 ; EDIT PATIENT LIST DATA
 D 1^YSWX S DIC(0)="AEQM" D:Y>0 2^YSWX D:Y>0 3^YSWX D END Q
 ;
4 ; Called from MENU option YSWAITSHUF
 ; MOVE PATIENT ON LIST
 D 1^YSWX S DIC(0)="AEQM" D:Y>0 2^YSWX D:Y>0 LP^YSWX,4^YSWX D END Q
 ;
5 ; Called from MENU option YSWAITREM
 ; TAKE PATIENT OFF LIST
 D 1^YSWX S DIC(0)="AEQM" D:Y>0 2^YSWX D:Y>0 5^YSWX
END ;
 K %Y,DA,DIC,DIE,DIK,DR,E1,H1,I,I1,I8,IO("Q"),N,N1,N2,N3,S1,S2,V,V1,V2,W2,W3,W4,X,X1,X2,X3,X8,X9,Y,YSAGE,YSBY,YSCAT,YSCOMM,YSCONT,YSDFN,YSDOB,YSELIG,YSLFT,YSM,YSNM,YSOND,YSPROM,YSSEX,YSSSN,ZTSK Q
