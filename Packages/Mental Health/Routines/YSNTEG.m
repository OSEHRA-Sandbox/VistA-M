YSNTEG ;ISC/XTSUMBLD KERNEL - Package checksum checker ;DEC 30, 1994@12:03:30
 ;;5.01;MENTAL HEALTH;;Dec 30, 1994
 ;;7.2;DEC 30, 1994@12:03:30
 S XT4="I 1",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
CONT F XT1=1:1 S XT2=$T(ROU+XT1) Q:XT2=""  S X=$P(XT2," ",1),XT3=$P(XT2,";",3) X XT4 I $T W !,X X ^%ZOSF("TEST") S:'$T XT3=0 X:XT3 ^%ZOSF("RSUM") W ?10,$S('XT3:"Routine not in UCI",XT3'=Y:"Calculated "_$C(7)_Y_", off by "_(Y-XT3),1:"ok")
 G CONT^YSNTEG0
 K %1,%2,%3,X,Y,XT1,XT2,XT3,XT4 Q
ONE S XT4="I $D(^UTILITY($J,X))",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
 W !,"Check a subset of routines:" K ^UTILITY($J) X ^%ZOSF("RSEL")
 W ! G CONT
ROU ;;
YIHIST ;;8468576
YIHISTF ;;10343510
YIPHYD ;;1697035
YIPROB ;;989921
YSCEN ;;11849413
YSCEN1 ;;14970019
YSCEN10 ;;2390898
YSCEN13 ;;13268299
YSCEN14 ;;2346188
YSCEN2 ;;13761114
YSCEN21 ;;6020265
YSCEN22 ;;13237577
YSCEN23 ;;11948061
YSCEN24 ;;12082473
YSCEN26 ;;8427191
YSCEN3 ;;12845232
YSCEN31 ;;10049085
YSCEN32 ;;2155714
YSCEN33 ;;7271744
YSCEN34 ;;5581679
YSCEN35 ;;13027120
YSCEN36 ;;17823868
YSCEN37 ;;3059517
YSCEN39 ;;13327223
YSCEN4 ;;12774233
YSCEN41 ;;6964492
YSCEN5 ;;9627694
YSCEN51 ;;4346864
YSCEN52 ;;15145443
YSCEN53 ;;10842821
YSCEN54 ;;12159579
YSCEN55 ;;6342431
YSCEN56 ;;2471190
YSCEN6 ;;11341807
YSCEN61 ;;16033384
YSCEN7 ;;9750082
YSCEN8 ;;10628312
YSCEN81 ;;10985668
YSCUP ;;1616148
YSCUP000 ;;5127901
YSCUP001 ;;6125631
YSCUP002 ;;3084310
YSCUP003 ;;4607921
YSCUP004 ;;5583202
YSD40030 ;;1902341
YSD40031 ;;5268804
YSD40032 ;;4632327
YSD40040 ;;1863992
YSD40041 ;;2726900
YSD40042 ;;4949698
YSD40050 ;;2021889
YSD40051 ;;651871
YSD40052 ;;4811159
YSD40060 ;;3990618
YSD40061 ;;2056583
YSD40062 ;;3077320
YSD4C000 ;;979086
YSD4C001 ;;3067937
YSD4C010 ;;87136
YSD4CK00 ;;7454000
YSD4DSM ;;1449955
YSD4E010 ;;6819775
YSD4E020 ;;6408869
YSD4POS0 ;;5888378
YSD4POST ;;5298608
YSD4PRE ;;7203733
YSD4PRE0 ;;5308674
YSD4UT01 ;;5519880
YSDGDEM ;;4462466
YSDGDEM0 ;;7377650
YSDIZ ;;4426660
YSDX0001 ;;2783224
YSDX0002 ;;887578
YSDX3 ;;9508069
YSDX3A ;;5565376
YSDX3B ;;11909349
YSDX3R ;;5071768
YSDX3R1 ;;4582798
YSDX3RU ;;5619249
YSDX3RUA ;;6237047
YSDX3U ;;5641886
YSDX3U00 ;;998386
YSDX3UA ;;4182079
YSDX3UA0 ;;1966163
YSDX3UB ;;4854865
YSDX3UC ;;2769931
YSDXR ;;4182602
YSDXR000 ;;1936257
YSDXR1 ;;1330884
YSDXUT01 ;;2212269
YSEMSG ;;1268332
YSESA ;;2310051
YSESD ;;12832059
YSESE ;;4434706
YSESED ;;2255662
YSESH ;;19033038
YSESL ;;8723066
YSESLOG ;;2703842
YSESM ;;8348408
YSESN ;;2640476
YSESP ;;11844112
YSESR ;;1237226
YSESUT ;;3344231
YSFORM ;;1608924
YSHELP ;;931855
YSHX1 ;;14442962
YSHX1R ;;8170471
YSJOB ;;891799
YSJOBK ;;3148949
YSLOCN ;;1357796
YSLRP ;;6252182
YSMHMENU ;;818334
YSMV ;;12318062
YSMV1 ;;8648321
YSONI001 ;;1361747
YSONIT ;;964656
YSONIT1 ;;1687734
YSONIT2 ;;82306
YSONIT3 ;;10576067
YSPDR1 ;;2855789
YSPDXR ;;2676362
YSPHY ;;12379349
YSPHY1 ;;8832973
YSPHYR ;;1879283
YSPP ;;7833093
YSPP1 ;;5015730
YSPP1A ;;8197800
YSPP2 ;;8374124
YSPP3 ;;1773583
YSPP4 ;;3020685
YSPP5 ;;2340584
YSPP6 ;;3525801
YSPP7 ;;3775005
YSPP8 ;;4493352
YSPP9 ;;807945
YSPPJ ;;10690917
YSPRBR1 ;;9218441
YSPRBR2 ;;8858392
YSPROB ;;19611459
YSPROB1 ;;18193714
YSPROB2 ;;16590961
YSPROB3 ;;9397274
YSPROB4 ;;11648676
YSPROB5 ;;3137695
YSPROBR ;;15543154
YSPROBR1 ;;4379868
YSPROSE ;;7523711
YSPTX ;;14047908
YSPTX1 ;;8697278
YSPTXR ;;10398725
YSSITE ;;4639290
YSSR ;;14139675
YSSR1 ;;12275159
YSSR2 ;;5408717
YSSRU ;;12335670
YSSRU1 ;;4164928
YSUTL ;;8009381
YSWX ;;11105411
YSWX1 ;;3047774
YSWZ ;;3321015
YTACL ;;2786221
YTAR ;;16956951
YTAR1 ;;6398100
YTAR2 ;;3526540
