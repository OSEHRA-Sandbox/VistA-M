YSUTL ;SLC/DKG,HIOFO/FT - UTILITIES FOR PT DATA, DATES, ETC. ;2/12/13 1:19pm
 ;;5.01;MENTAL HEALTH;**23,108**;Dec 30, 1994;Build 17
 ;Reference to ^DPT( supported by DBIA #10035
 ;Reference to ^VA(200, supported by DBIA #10060
 ;Reference to VADPT APIs supported by DBIA #10061
 ;Reference to XMB APIs supported by DBIA #10069
ENDD ;Formats date for time-stamping entry of patient data
 I '$D(Y) D 
 .S %=$H>21549+$H-.1,%Y=%\365.25+141,%=%#365.25\1,YSPTD=%+306#(%Y#4=0+365)#153#61#31+1,YSPTM=%-YSPTD\29+1,Y=%Y_"00"+YSPTM_"00"+YSPTD,YSDT(0)=$E(Y,4,5)_"/"_$E(Y,6,7)_"/"_$E(Y,2,3) K %,%Y,YSPTD,YSPTM
 S Y=$E("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",$E(Y,4,5)*3-2,$E(Y,4,5)*3)_" "_$S(Y#100:$J(Y#100\1,2)_", ",1:"")_(Y\10000+1700)_$S(Y#1:"  "_$E(Y_0,9,10)_":"_$E(Y_"000",11,12),1:""),YSDT(1)=Y
 Q
ENDTM ;Convert $H time into FM time
 S %=$P($H,",",2),%=%#3600\60/100+(%\3600)/100 S YSDTM=DT_% K % D ENDD,ENHM K A,M,X,Y,YSHM,YSHR,YSMN
 Q
ENHM ;Convert FM time to regular format
 D:'$D(YSDTM) ENDTM S:'$D(YSHM) YSHM=$P(YSDTM,".",2) S YSMN=$E(YSHM,3,4) S:$L(YSMN)=1 YSMN=YSMN_"0"
 S YSHR=$E(YSHM,1,2),A=$S(YSHR<12:YSHR,YSHR>12:YSHR-12,YSHR=12:12,1:"00"),M=$S(YSHR<12:"A",YSHR=12&(YSMN>0):"P",YSHR>12:"P",1:0) S:A?1"0".N A=" "_$E(A,2) S:$L(A)=1 A=" "_A S YSTM=A_":"_YSMN_" "_M_"M"
 K A,M
 Q
ENBUL ;Generate a bulletin
 S DIC=3.8,DIC(0)="MZ",X="YS PSYCHTEST" D ^DIC Q:Y'>0
 Q:'$D(YSORD)
 I '$D(YSDT(1)) K Y D ENDTM
 S XMB="YS PSYCHTEST",XMB(1)=$P(^DPT(YSDFN,0),U),XMB(2)=$P(^VA(200,DUZ,0),U),XMB(3)=YSDT(1) S XMB(4)="" S:YSORD]"" XMB(4)=$P(^VA(200,YSORD,0),U),XMY(YSORD)="" S XMDUZ=DUZ D EN^XMB
 Q
WAIT ;Press RETURN to continue prompt
 F I0=1:1:IOSL-$Y-4 W !
 N DTOUT,DUOUT,DIRUT
 S DIR(0)="E" D ^DIR K DIR S YSTOUT=$D(DTOUT),YSUOUT=$D(DUOUT),YSLFT=$D(DIRUT)
 W @IOF
 Q
 ;
WAIT1 ;Write blank lines
 F I0=1:1:IOSL-$Y-2 W !
WAIT11 ;Return, escape or branch
 S YSLFT=0
 R !,"Press return to continue or ""^"" to escape or ""^N"" for section ""N"" ",YSSKIP:DTIME
 S YSTOUT='$T,YSUOUT=YSSKIP="^" I YSTOUT!YSUOUT S YSLFT=1 Q
 I YSSKIP["?" W !!,"To select different ""areas of info"", enter ""^N"" where ""N"" is set to the following area you want. Ex. enter ""^3"" to go to the 3rd screen.",!! G WAIT11
 I YSSKIP?1"^"1N.N S X=$E(YSSKIP,2,3) I X>0,X<11 S YSA8="",YSI=0 F I=X:1:10 S YSA8=YSA8_I_","
 Q
AE ;Kill variables
 K %,%Y,D,D0,DIC,DA,DIE,DQ,DR,DZ,X,Y,Z
 Q
PSIG ;Get user's name
 S Y="" S:X Y=$P($G(^VA(200,X,0)),U)
 Q
ENPT ;Get patient name, sex. dob, age and ssn (full and last 4)
 D KVAR^VADPT S DFN=YSDFN D DEM^VADPT,PID^VADPT
 S YSNM=VADM(1),YSSEX=$P(VADM(5),U),YSDOB=$P(VADM(3),U,2),YSAGE=VADM(4),YSSSN=VA("PID"),YSBID=VA("BID") K X
 Q
