YSPP9 ;ALB/ASF-PROBLEM LIST ;10/4/89  14:09 ;
 ;;5.01;MENTAL HEALTH;;Dec 30, 1994
 ;
 S YSFHDR="Short Problem List <<section 10>>",YSLFT=0 D:'$D(YSNOFORM) ENHD^YSFORM
ENCE ; Called indirectly from YSCEN31
 ;
 S YST=$S(IOST?1"C-".E:0,1:1),YSSL=$S(YST:8,1:3),P4="PL",YSLFT=0
 D EN1^YSPROB5 K DX,DXS,N2,P4,YSDS,YSDXN,YSSL,Z Q:$D(YSNOFORM)!$D(YSCENN)  D WAIT1^YSUTL:'YST,ENFT^YSFORM:YST Q
