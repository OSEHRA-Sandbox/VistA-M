PRCHHI0 ;WISC/TGH-IFCAP CONTROL STRING SEGMENT ;5/19/92  3:17 PM
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 Q
CNTL(A,A1,A2,CNTR) N B,I,JD,X,Y
 S PRCHA=A,PRCHTC=A2
 D NOW^%DTC S X=$P(%,".") D JD^PRCFDLN S PRCHJD=$E(X,1,3)+1700_$E(Y,1,3)
 S TIME=$P(%,".",2)_"000000",TIME=$E(TIME,1,6)
 S PRCHTP(1,CNTR+1)="S X=PRCHSYS;500"
 S PRCHTP(1,CNTR+2)="S X=+$P(PRCHA,U);1"
 S PRCHTP(1,CNTR+3)="S X=200;63"
 S PRCHTP(1,CNTR+4)="S X=PRCHTC;430"
 S PRCHTP(1,CNTR+5)="S X=PRCHJD;501"
 S PRCHTP(1,CNTR+6)="S X=TIME;502"
 S PRCHPON=$P($P(PRCHA,U),""-"")_$P($P(PRCHA,U),""-"",2)
 S PRCHPON=PRCHPON_"           ",PRCHPON=$E(PRCHPON,1,11)
 S PRCHTP(1,CNTR+7)="S X=PRCHPON;503"
 S PRCHTP(1,CNTR+8)="S X=""001"";503.1"
 S PRCHTP(1,CNTR+9)="S X=""001"";503.2"
 S PRCHTP(1,CNTR+10)="S X=""001"";504"
 S CNTR=CNTR+10
 S PRCFA("STRING")=""
 S PRCFA("STRING")=PRCHSYS_"^"_+$P(PRCHA,U)_"^200^"_PRCHTC_"^"_PRCHJD_"^"_TIME_"^"_$P($P(PRCHA,U),"-")_$P($P(PRCHA,U),"-",2)_"  "_"^001^001^040^|"
 Q
