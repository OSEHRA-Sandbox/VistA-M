YS38POST ;DALIRMFO/MJD-PATCH YS*5.01*38 POST RTN.; 09/23/97
 ;;5.01;MENTAL HEALTH;**38**;Dec 30, 1994
 ;
 ; The zero node for file #604.68 ASI NARRATIVE should look like:
 ; ^YSTX(604.68,0) = ASI NARRATIVE^604.68^25^21
 S $P(^YSTX(604.68,0),U,3)=25
 S $P(^YSTX(604.68,0),U,4)=21
 ;
 QUIT
