RMPG7C ;DDC/KAW-RMPF*2.0*17 - MICRO-TECH [ 06/06/01  1:43 PM ]
 ;;2.0;REMOTE ORDER/ENTRY SYSTEM;**17**;6/6/01
 W !!,"MICRO-TECH"
 S VN="MICRO-TECH"
 K RMPFMARR
 F IX=1:1:27 S SX=$T(MODEL+IX) K RMPFMARR D
 .S MD=$P(SX,";",3),CS=$P(SX,";",4) W !!?3,MD
 .S MP="" F  S MP=$O(^RMPF(791811,"B",MD,MP)) Q:'MP  Q:$P(^RMPF(791811,MP,0),"^",2)=VN
 .I 'MP S X=""_MD_"",DIC="^RMPF(791811,",DIC(0)="L",DLAYGO=791811 K DD,DO D FILE^DICN S MP=+Y K DIC I Y=-1 W ?$X+5,"*** UNABLE TO ADD ",MD," ***" Q
 .S DA=MP,DIE="^RMPF(791811,",DR=".02////MICRO-TECH;.03///CUSTOM HEARING AIDS;.08////1;999////0;.06////0;.04///"_CS D ^DIE
 .K RMPFCARR
 .F IY=1:1 S ST=$T(COMP+IY) Q:ST=""  D
 ..S CS=$P(ST,";",IX+4) Q:CS=""
 ..S CP=$E($P(ST,";",3),1,30),CD=$P(ST,";",4),(CA,CX)=""
 ..F  S CA=$O(^RMPF(791811.2,"B",CP,CA)) Q:'CA  I $P(^RMPF(791811.2,CA,0),"^",3)=CD S CX=CA Q
 ..I 'CX S X=CP,DIC="^RMPF(791811.2,",DIC(0)="L",DLAYGO=791811.2,DIC("DR")=".03///"_CD K DD,DO D FILE^DICN S CX=+Y K DIC I Y=-1 W ?$X+5,"*** UNABLE TO ADD ",CP," ***" Q
 ..D SET1^RMPG7 W "."
 ..S RMPFCARR("OLD",CP)=CX_"^"_CS
 .F OM=5:1 Q:$P(SX,";",OM)=""  S RMPFMARR("OLD",$P(SX,";",OM))=""
 .S MD=0
 .F IY=1:1 S MD=$O(RMPFMARR("OLD",MD))  Q:MD=""  D
 ..I '$D(^RMPF(791811,"B",MD)) W !,"MISSING MODEL: ",MD Q
 ..S MP=$O(^RMPF(791811,"B",MD,0))
 ..S CP="" F IZ=1:1 S CP=$O(RMPFCARR("OLD",CP)) Q:CP=""  D
 ...S CX=$P(RMPFCARR("OLD",CP),"^",1)
 ...S CS=$P(RMPFCARR("OLD",CP),"^",2)
 ...I CX D SET3^RMPG7
 K CA,CD,CP,CS,CX,DA,DD,DIC,DIE,DLAYGO,DO,DR,IX,IY,IZ,MD,MP,OM,RMPFCARR,RMPFMARR
 K ST,SX,VN,X,Y,VN
 G ^RMPG7D
MODEL ;;Micro-TeCh Models
 ;;FULL SHELL ITE;118.85;ITE;LOW PROFILE ITE
 ;;FULL SHELL ITE OPT 1;138.67;ITE;LOW PROFILE ITE
 ;;FULL SHELL ITE OPT 2;212.96;ITE;LOW PROFILE ITE
 ;;FULL SHELL ITE OPT 3;346.67;ITE;LOW PROFILE ITE
 ;;HALF SHELL;148.56;1/2 SHELL
 ;;HALF SHELL OPT 1;168.38;1/2 SHELL
 ;;HALF SHELL OPT 2;242.67;1/2 SHELL
 ;;HALF SHELL OPT 3;376.39;1/2 SHELL
 ;;CANAL;175.81;CANAL;MINI CANAL
 ;;CANAL OPT 1;193.15;CANAL;MINI CANAL
 ;;CANAL OPT 2;264.46;CANAL;MINI CANAL
 ;;CANAL OPT 3;314.98;CANAL;MINI CANAL
 ;;TRU-CANAL CIC;302.10;TRU-CANAL II
 ;;TRU-CANAL CIC OPT 1;332.81;TRU-CANAL II
 ;;TRU-CANAL CIC OPT 2;353.61;TRU-CANAL II
 ;;TRU-CANAL CIC OPT 3;374.41;TRU-CANAL II
 ;;FULL SHELL ITE SS;208.00;ITE;LOW PROFILE ITE
 ;;FULL SHELL ITE MS;326.86;ITE;LOW PROFILE ITE
 ;;FULL SHELL ITE MM;459.59;ITE;LOW PROFILE ITE
 ;;HALF SHELL SS;239.20;1/2 SHELL
 ;;HALF SHELL MS;345.68;1/2 SHELL
 ;;HALF SHELL MM;469.00;1/2 SHELL
 ;;CANAL SS;260.01;CANAL;MINI CANAL
 ;;CANAL MS;368.46;CANAL;MINI CANAL
 ;;CANAL MM;403.87;CANAL;MINI CANAL
 ;;TRU-CANAL CIC SS;343.21;TRU-CANAL II
 ;;TRU-CANAL CIC MS;456.12;TRU-CANAL II
COMP ;;Components
 ;;COMPRESSION PRO D;AGCO;;;72.80;72.80;;;72.80;72.80;;;72.80;;;;72.80
 ;;EZ COMPRESSION D;AGCO/EZ;;;;;;;;;;;;;;39.61
 ;;OCTIVA;ASP1;;;;113.90;;;;113.90;;;;113.90
 ;;TRIM-PRO CLASS D;CLD2;;49.52;;;;49.52;;;;49.52
 ;;SUPER K-AMP;KAMP;;;59.42;;;;59.42;;;;59.42;;;;59.42
 ;;CLASS D POWER;POW1;;;;37.14;;;;;;;;;;;;;;37.14;37.14
 ;;PUSH PULL 3;POW2;;;37.33;;;;37.33;;;;37.33
 ;;NXN;WDRC;;;;123.81;;;;123.81;;;;123.81;;;;123.81
 ;;PRE-AMPLIFIED TELECOIL;TCA1;;;17.22;17.22;;;17.22;17.22;;;;;;;;;17.22;17.22;17.22;17.22
 ;;TOUCHLESS TELECOIL;TCA2;;;;59.43;;;;59.43;;;;;;;;;;;59.43;;;59.43;;;59.43
 ;;VARIABLE HIGH CUT;AHC;;11.48;11.48;11.48;;11.48;11.48;11.48;;11.48;11.48;11.48
 ;;ACTIVE LOWCUT FILTER POT;ALC;;11.48;11.48;11.48;;11.48;11.48;11.48;;11.48;11.48;;;;11.48
 ;;CROSSOVER FREQUENCY CONTROL;CFP;;;;11.48;;;;11.48;;;;11.48
 ;;COMPRESSION RATIO CONTROL;CPR;;;11.48;11.48;;;11.48;11.48;;;11.48;11.48;;;11.48
 ;;DIRX;DMIC1;;;;99.04;;;;99.04;;;;;;;;;;;99.04;;;99.04
 ;;FLEX TIP CANAL;FLEX1;;;20.77;20.77;;;20.77;20.77;;;20.77;20.77;;;20.77;20.77;20.77;20.77;20.77;20.77;20.77;20.77;20.77;20.77;20.77;20.77;20.77
 ;;LOW CUT TONE;LCP;;11.48;11.48;11.48;;11.48;11.48;11.48;;11.48;11.48;11.48
 ;;RESONANCE BOOSTER;PEAK;;;;11.48;;;;11.48;;;;11.48;;;;11.48
 ;;CANAL LOCK;RW;;;;;;;11.48;11.48;;;11.48;11.48;11.48;;11.48;11.48;;;;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48
 ;;SOFT SEAL;SOFT1;;;11.48;11.48;;;11.48;11.48;;;11.48;11.48;;;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48
 ;;SET SCREW VOLUME CONTROL;SSVC;;11.48;11.48;11.48;;11.48;11.48;11.48;;11.48;11.48;11.48;;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48;11.48
 ;;THRESHOLD KNEE CONTROL;TK;;;11.48;11.48;;;11.48;11.48;;;11.48;11.48;;11.48
 ;;ACCENT 1 MEMORY;WDRC1;;;;;;;;;;;;;;;;;108.95;;;108.95;;;108.95;;;108.95
 ;;NX2 (PROGRAMMABLE) 1 MEMORY;WDRC3;;;;;;;;;;;;;;;;;;199.08;;;199.08;;;199.08;;;199.08
 ;;NX2 (PROGRAMMABLE) 2 MEMORY;WDRC4;;;;;;;;;;;;;;;;;;;204.53;;;204.53;;;204.53
 ;;NX2 (PROGRAMMABLE) 3 MEMORY;WDRC5;;;;;;;;;;;;;;;;;;;204.53;;;204.53;;;204.53
 ;;2ND YEAR WARRANTY;WRNTY2YR;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67;34.67
