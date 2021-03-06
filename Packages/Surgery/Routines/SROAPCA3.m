SROAPCA3 ;BIR/MAM - CARDIAC OCCURRENCE DATA ;09/28/2011
 ;;3.0;Surgery;**38,71,95,101,125,160,164,166,174,175,176**;24 Jun 93;Build 8
 D EN^SROCCAT K SRA S SRA(205)=$G(^SRF(SRTN,205)),SRA(208)=$G(^SRF(SRTN,208)),SRA(206)=$G(^SRF(SRTN,206)),SRA(209)=$G(^SRF(SRTN,209))
 S NYUK=$P(SRA(208),"^") D YN S SRAO(1)=SHEMP_"^384"
 S Y=$P($G(^DPT(DFN,.35)),"^") D DT^SROAPCA1 S SRAO(2)=X
 S NYUK=$P(SRA(205),"^",27) D YN S SRAO(3)=SHEMP_"^258",NYUK=$P(SRA(208),"^",3) D YN S SRAO(4)=SHEMP_"^386",NYUK=$P(SRA(205),"^",17) D YN S SRAO(5)=SHEMP_"^254",NYUK=$P(SRA(209),"^",12) D YN S SRAO(6)=SHEMP_"^490"
 S NYUK=$P(SRA(208),"^",5) D YN S SRAO(7)=SHEMP_"^388",NYUK=$P(SRA(208),"^",6) D YN S SRAO(8)=SHEMP_"^389",NYUK=$P(SRA(205),"^",13) D YN S SRAO(9)=SHEMP_"^285"
 S NYUK=$P(SRA(208),"^",7) D YN S SRAO(10)=SHEMP_"^391",NYUK=$P(SRA(205),"^",22) D YN S SRAO(11)=SHEMP_"^410"
 S NYUK=$P(SRA(205),"^",21) D YN S SRAO(12)=SHEMP_"^256" D DUR
 S NYUK=$P(SRA(205),"^",26) D YN S SRAO(13)=SHEMP_"^411"
 S NYUK=$P(SRA(206),"^",39) D YN S SRAO(14)=SHEMP_"^466"
 S NYUK=$P(SRA(206),"^",40) D YN S SRAO(15)=SHEMP_"^467",NYUK=$P(SRA(205),"^",6) D YN S SRAO(18)=SHEMP_"^248"
 S NYUK=$P(SRA(205),"^",40) D YN S SRAO(16)=SHEMP_"^448",NYUK=$P(SRA(205),"^",8) D YN S SRAO(17)=SHEMP_"^404"
  I $Y+5>IOSL D PAGE^SROAPCA I SRSOUT Q
 W !!,"VII. OUTCOMES"
 W !,"Operative Death:",?18,$P(SRAO(1),"^"),?43,"Date of Death:",?58,$P(SRAO(2),"^")
 W !!,"Perioperative (30 day) Occurrences:"
 W !,?2,"Perioperative MI:",?35,$P(SRAO(3),"^"),?41,"Tracheostomy:",?75,$P(SRAO(14),"^")
 W !,?2,"Endocarditis:",?35,$P(SRAO(4),"^"),?41,"Ventilator supp within 30 days:",?75,$P(SRAO(6),"^")
 W !,?2,"Superficial Incisional SSI:",?35,$P(SRAO(18),"^"),?41,"Stroke/CVA:",?68,$J($P(SRAO(12),"^"),11)
 W !,?2,"Mediastinitis:",?35,$P(SRAO(7),"^"),?41,"Coma > or = 24 Hours:",?75,$P(SRAO(11),"^")
 W !,?2,"Cardiac Arrest Requiring CPR:",?35,$P(SRAO(13),"^"),?41,"New Mech Circulatory Support:",?75,$P(SRAO(15),"^")
 W !,?2,"Reoperation for Bleeding:",?35,$P(SRAO(8),"^"),?41,"Postop Atrial Fibrillation:",?75,$P(SRAO(16),"^")
 W !,?2,"On ventilator > or = 48 hr:",?35,$P(SRAO(9),"^"),?41,"Wound Disruption:",?75,$P(SRAO(17),"^")
 W !,?2,"Repeat cardiac Surg procedure:",?35,$P(SRAO(10),"^"),?41,"Renal Failure Requiring Dialysis:",?75,$P(SRAO(5),"^")
 D RES
 Q
DUR ; get stroke/cva duration
 N SROCC,SRDUR I $P(SRAO(12),"^")="NO" S X=1
 I X'=1 S SROCC=0 F  S SROCC=$O(^SRF(SRTN,16,SROCC)) Q:'SROCC  I $P(^SRF(SRTN,16,SROCC,0),"^",2)=12 S X=$P(^SRF(SRTN,16,SROCC,0),"^",8)
 S SRDUR=$S(X=2:"<24 HOURS",X=3:"24-72 HOURS",X=4:">72 HOURS",1:"NO SYMPTOMS")
 S SRAO(12)=SRDUR_"^256"
 Q
YN ; store answer
 S SHEMP=$S(NYUK="NS":"NS",NYUK="N":"NO",NYUK="Y":"YES",1:"")
 Q
 ;
RES I $Y+12>IOSL D PAGE^SROAPCA I SRSOUT Q
 S SRA(208)=$G(^SRF(SRTN,208))
 S SRA(.2)=$G(^SRF(SRTN,.2))
 W !!,"VIII. RESOURCE DATA"
 S Y=$P(SRA(208),"^",14) D DT^SROAPCA1 W !,"Hospital Admission Date:",?25,X
 S Y=$P(SRA(208),"^",15) D DT^SROAPCA1 W !,"Hospital Discharge Date:",?25,X
 S Y=$P(SRA(.2),"^",10) D DT^SROAPCA1 W !,"Time Patient In  OR: ",?25,X
 S Y=$P(SRA(.2),"^",2) D DT^SROAPCA1 W ?45,"Operation Began: "_X
 S Y=$P(SRA(.2),"^",3) D DT^SROAPCA1 W !,"Operation Ended: ",?25,X
 S Y=$P(SRA(.2),"^",12) D DT^SROAPCA1 W ?45,"Time Patient Out OR: ",X
 S Y=$P(SRA(208),"^",22) I Y>1 D DT^SROAPCA1 S Y=X
 S Y=$S(Y="NS":"Unable to determine",Y="RI":"Remains intubated at 30 days",1:Y) W !,"Date and Time Patient Extubated: ",?47,Y
 I $P(SRA(208),"^",22)>1,$P(SRA(.2),"^",12) D
 .S X=$$FMDIFF^XLFDT($P(SRA(208),"^",22),$P(SRA(.2),"^",12),2) W !,?5,"Postop Intubation Hrs: "_$FN((X/3600),"+",1)
 S Y=$P(SRA(208),"^",23) I Y>1 D DT^SROAPCA1 S Y=X
 S Y=$S(Y="NS":"Unable to determine",Y="RI":"Remains in ICU at 30 days",1:Y) W !,"Date and Time Patient Discharged from ICU: ",?47,Y
 S Y=$P(SRA(209),"^") W !,"Patient is Homeless: ",?47,$S(Y="Y":"YES",Y="N":"NO",Y="NS":"NS",1:"")
 S Y=$P(SRA(206),"^",41) W !,"Cardiac Surg Performed at Non-VA Facility: ",?47,$S(Y="Y":"YES",Y="N":"NO",Y="NS":"UNKNOWN",1:"")
 W !,"Resource Data Comments: "
 I $G(^SRF(SRTN,206.2))'="" S SRQ=0 S X=$G(^SRF(SRTN,206.2)) W:$L(X)<49 X,! I $L(X)>48 S Z=$L(X) D
 .I X'[" " W ?25,X Q
 .S I=0,LINE=1 F  S SRL=$S(LINE=1:48,1:74) D  Q:SRQ
 ..I $E(X,1,SRL)'[" " W X,! S SRQ=1 Q
 ..S J=SRL-I,Y=$E(X,J),I=I+1 I Y=" " W $E(X,1,J-1),!,?5 S X=$E(X,J+1,Z),Z=$L(X),I=0,LINE=LINE+1 I Z<SRL W X S SRQ=1 Q
 I $Y+7>IOSL D PAGE^SROAPCA I SRSOUT Q
 W ! F MOE=1:1:80 W "="
 W !,"IX. SOCIOECONOMIC, ETHNICITY, AND RACE"
 N SREMP S SREMP=$P(SRA(208),"^",18) S SREMP=$S(SREMP=1:"EMPLOYED FULL TIME",SREMP=2:"EMPLOYED PART TIME",SREMP=3:"NOT EMPLOYED",SREMP=4:"SELF EMPLOYED",SREMP=5:"RETIRED",SREMP=6:"ACTIVE MILITARY DUTY",SREMP=9:"UNKNOWN",1:" ")
 W !,?1,"Employment Status Preoperatively: ",?40,SREMP
 K SRA,SRAO
 ; Race/Ethnic
 D ENTH^SRORACE
 I $Y+7>IOSL D PAGE^SROAPCA I SRSOUT Q
 D ^SROAPCA4
 W !!," *** End of report for "_SRANM_" assessment #"_SRTN_" ***"
 I $E(IOST)'="P" W ! K DIR S DIR(0)="E" D ^DIR K DIR
 Q
