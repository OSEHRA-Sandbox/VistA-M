DINTEG2 ;SFISC/dizSUMB FILEMAN-FileMan checksum checker ;MAR 30, 1999  13:20
 ;;22.0;VA FileMan;;Mar 30, 1999;Build 1
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 S DIZ4="I 1" D DSP,INI
CONT F DIZ1=1:1 S DIZ2=$T(ROU+DIZ1) Q:DIZ2=""  S X=$P(DIZ2," ",1),DIZ3=$P(DIZ2,";",3) X DIZ4 I $T W !,X X DIZTEST W:'$T ?28,DIZ6 S:'$T DIZ3=0 X:DIZ3 DIZSUM W ?10,$S('DIZ3:"",DIZ3'=Y:$C(7)_"Calculated "_Y_", off by "_(Y-DIZ3),1:"ok")
 G CONT^DINTEG3
 S X="" F  S X=$O(^UTILITY($J,X)) Q:X=""  W !,X,?10,"not a routine in this INTEGRITY checker"
 K D,D1,D2,D3,X,Y,DIZ,DIZ1,DIZ2,DIZ3,DIZ4,DIZ5,DIZ6,DIZTEST,DIZSUM,DISYS,DIZSEL,^UTILITY($J) Q
ONE D INI S DIZSEL=$S($D(^%ZOSF("RSEL")):^("RSEL"),1:"F  S DIR(0)=""FO^1:8"",DIR(""A"")=""ROUTINE NAME"" D ^DIR Q:$D(DIRUT)  X DIZTEST W:'$T ?28,DIZ6 I $T S ^UTILITY($J,Y)=""""")
 S DIZ4="I $D(^UTILITY($J,X)) K ^(X)" D DSP
 W !,"Check a subset of routines:" K ^UTILITY($J) X DIZSEL
 W ! G CONT
DSP S X=$T(+2) W !!,"Checksum routine created on "_$P(X,";",6)_" by "_$P(X,";",4)_" V"_$P(X,";",3) Q
INI K ^UTILITY($J) D OS^DII S DIZTEST=$S($D(^DD("OS",DISYS,18)):^(18),1:"I $T(^@X)]"""""),DIZ5="",DIZ6=$C(7)_"Routine not in UCI"
 S DIZSUM="ZL @X S Y=0 F D=1,3:1 S D1=$T(+D),D3=$F(D1,"" "") Q:'D3  S D3=$S($E(D1,D3)'="";"":$L(D1),$E(D1,D3+1)="";"":$L(D1),1:D3-2) F D2=1:1:D3 S Y=$A(D1,D2)*D2+Y" Q
ROU ;;
DICUF ;;2175347
DICUIX ;;7050873
DICUIX1 ;;3761139
DICUIX2 ;;7134369
DID ;;11403295
DID1 ;;10810057
DID2 ;;10525120
DIDC ;;8218283
DIDG ;;5459532
DIDH ;;6566881
DIDH1 ;;9603613
DIDT ;;6481242
DIDTC ;;7099203
DIDU ;;7893426
DIDU1 ;;1818550
DIDU2 ;;3645214
DIDX ;;8570843
DIE ;;10522278
DIE0 ;;4913436
DIE1 ;;10102666
DIE17 ;;9556587
DIE2 ;;8785703
DIE3 ;;4915247
DIE9 ;;5169923
DIED ;;8319960
DIEF ;;11594187
DIEF1 ;;9138291
DIEFU ;;4610014
DIEFW ;;3026875
DIEH ;;6060388
DIEH1 ;;1201975
DIEKMSG ;;4110527
DIENV ;;1166929
DIENVSTP ;;1202956
DIENVWRN ;;1424931
DIEQ ;;5540099
DIEQ1 ;;1766980
DIET ;;5583630
DIETED ;;12174194
DIEV ;;10272887
DIEV1 ;;4308402
DIEVK ;;6784329
DIEVK1 ;;5034393
DIEVS ;;2284643
DIEZ ;;9351704
DIEZ0 ;;10216147
DIEZ1 ;;8970637
DIEZ2 ;;14215709
DIEZ3 ;;1035196
DIEZ4 ;;4969253
DIFG ;;9620802
DIFG0 ;;9271581
DIFG0A ;;5263645
DIFG0B ;;3277889
DIFG1 ;;6466432
DIFG2 ;;6268614
DIFG3 ;;11191749
DIFG3A ;;5426591
DIFG4 ;;11076453
DIFG4A ;;4158452
DIFG5 ;;11716060
DIFG6 ;;12531183
DIFG7 ;;3294917
DIFGA ;;10149588
DIFGA1 ;;1674663
DIFGB ;;7602021
DIFGG ;;5089070
DIFGG2 ;;9806486
DIFGG4 ;;5207113
DIFGGI ;;5710645
DIFGGSB ;;483886
DIFGGSB1 ;;8206690
DIFGGSB2 ;;5150555
DIFGGU ;;5525512
DIFGO ;;3849838
DIFGSRV ;;1145738
DIFROM ;;12725017
DIFROM0 ;;9269715
DIFROM1 ;;9679123
DIFROM11 ;;9004800
DIFROM12 ;;6412655
DIFROM2 ;;6844257
DIFROM3 ;;7863608
DIFROM4 ;;4141771
DIFROM41 ;;14320255
DIFROM42 ;;3818444
DIFROM5 ;;13318228
DIFROM6 ;;8014990
DIFROM7 ;;5693246
DIFROMH ;;8812360
DIFROMH1 ;;7701962
DIFROMS ;;1767005
DIFROMS1 ;;7066700
DIFROMS2 ;;7691714
DIFROMS3 ;;8999406
DIFROMS4 ;;4187300
DIFROMS5 ;;3339679
DIFROMSB ;;1316407
DIFROMSC ;;1542160
DIFROMSD ;;3803374
DIFROMSE ;;5059847
DIFROMSI ;;8387932
DIFROMSK ;;1421979
DIFROMSL ;;371524
DIFROMSO ;;1615788
DIFROMSP ;;7375587
DIFROMSR ;;4932510
DIFROMSS ;;3490849
DIFROMSU ;;5222720
DIFROMSV ;;89285
DIFROMSX ;;3683091
DIFROMSY ;;3656754
DIG ;;6293627
DIH ;;4688941
DII ;;6196367
DII1 ;;468832
DIIS ;;374782
DIISS ;;2408793
DIK ;;9360789
DIK1 ;;6155873
DIKC ;;11169535
DIKC1 ;;4416248
DIKC2 ;;5211266
DIKCDD ;;633139
DIKCFORM ;;10827227
DIKCP ;;4105817
DIKCP1 ;;7463319
DIKCP2 ;;2304021
DIKCP3 ;;4532184
DIKCU ;;3210281
DIKCU1 ;;3879444
DIKCU2 ;;4020096
DIKCUTL ;;2987602
DIKCUTL1 ;;6197088
DIKCUTL2 ;;6730540
DIKCUTL3 ;;3718932
