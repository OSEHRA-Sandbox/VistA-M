DINIT0F7 ;SFISC/MKO-DATA FOR FORM AND BLOCK FILES ;11:00 AM  4 Oct 2000
 ;;22.2;VA FileMan;;Jan 05, 2016;Build 42
 ;;Per VA Directive 6402, this routine should not be modified.
 ;;Submitted to OSEHRA 5 January 2015 by the VISTA Expertise Network.
 ;;Based on Medsphere Systems Corporation's MSC FileMan 1051.
 ;;Licensed under the terms of the Apache License, Version 2.0.
 ;
 F I=1:2 S X=$T(ENTRY+I) G:X="" ^DINIT0F8 S Y=$E($T(ENTRY+I+1),5,999),X=$E(X,4,999),@X=Y
 Q
ENTRY ;
 ;;^DIST(.404,.00109,40,1,4)
 ;;=1
 ;;^DIST(.404,.00109,40,1,11)
 ;;=S:$G(DICATTSC)=3 DDACT="CL"
 ;;^DIST(.404,.00109,40,1,20)
 ;;=F^^1:20
 ;;^DIST(.404,.00109,40,1,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,2,0)
 ;;=36^WILL STAND FOR^2^^MEANS1
 ;;^DIST(.404,.00109,40,2,2)
 ;;=2,30^30^2,14
 ;;^DIST(.404,.00109,40,2,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,2,3.1)
 ;;=D Y^DICATTD3(1,2)
 ;;^DIST(.404,.00109,40,2,4)
 ;;=1
 ;;^DIST(.404,.00109,40,2,20)
 ;;=F^^1:70
 ;;^DIST(.404,.00109,40,2,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,3,0)
 ;;=37^CODE^2^^CODE2
 ;;^DIST(.404,.00109,40,3,2)
 ;;=3,8^4^3,2
 ;;^DIST(.404,.00109,40,3,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,3,3.1)
 ;;=D Y^DICATTD3(2,1)
 ;;^DIST(.404,.00109,40,3,20)
 ;;=F^^1:20
 ;;^DIST(.404,.00109,40,3,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,4,0)
 ;;=38^WILL STAND FOR^2^^MEANS2
 ;;^DIST(.404,.00109,40,4,2)
 ;;=3,30^30^3,14
 ;;^DIST(.404,.00109,40,4,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,4,3.1)
 ;;=D Y^DICATTD3(2,2)
 ;;^DIST(.404,.00109,40,4,20)
 ;;=F^^1:70
 ;;^DIST(.404,.00109,40,4,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,5,0)
 ;;=39^CODE^2^^CODE3
 ;;^DIST(.404,.00109,40,5,2)
 ;;=4,8^4^4,2
 ;;^DIST(.404,.00109,40,5,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,5,3.1)
 ;;=D Y^DICATTD3(3,1)
 ;;^DIST(.404,.00109,40,5,20)
 ;;=F^^1:20
 ;;^DIST(.404,.00109,40,5,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,6,0)
 ;;=40^WILL STAND FOR^2^^MEANS3
 ;;^DIST(.404,.00109,40,6,2)
 ;;=4,30^30^4,14
 ;;^DIST(.404,.00109,40,6,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,6,3.1)
 ;;=D Y^DICATTD3(3,2)
 ;;^DIST(.404,.00109,40,6,20)
 ;;=F^^1:70
 ;;^DIST(.404,.00109,40,6,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,7,0)
 ;;=41^CODE^2^^CODE4
 ;;^DIST(.404,.00109,40,7,2)
 ;;=5,8^4^5,2
 ;;^DIST(.404,.00109,40,7,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,7,3.1)
 ;;=D Y^DICATTD3(4,1)
 ;;^DIST(.404,.00109,40,7,20)
 ;;=F^^1:20
 ;;^DIST(.404,.00109,40,7,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,8,0)
 ;;=42^WILL STAND FOR^2^^MEANS4
 ;;^DIST(.404,.00109,40,8,2)
 ;;=5,30^30^5,14
 ;;^DIST(.404,.00109,40,8,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,8,3.1)
 ;;=D Y^DICATTD3(4,2)
 ;;^DIST(.404,.00109,40,8,20)
 ;;=F^^1:70
 ;;^DIST(.404,.00109,40,8,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,9,0)
 ;;=43^CODE^2^^CODE5
 ;;^DIST(.404,.00109,40,9,2)
 ;;=6,8^4^6,2
 ;;^DIST(.404,.00109,40,9,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,9,3.1)
 ;;=D Y^DICATTD3(5,1)
 ;;^DIST(.404,.00109,40,9,20)
 ;;=F^^1:20
 ;;^DIST(.404,.00109,40,9,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,10,0)
 ;;=44^WILL STAND FOR^2^^MEANS5
 ;;^DIST(.404,.00109,40,10,2)
 ;;=6,30^30^6,14
 ;;^DIST(.404,.00109,40,10,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,10,3.1)
 ;;=D Y^DICATTD3(5,2)
 ;;^DIST(.404,.00109,40,10,20)
 ;;=F^^1:70
 ;;^DIST(.404,.00109,40,10,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,11,0)
 ;;=45^CODE^2^^CODE6
 ;;^DIST(.404,.00109,40,11,2)
 ;;=7,8^4^7,2
 ;;^DIST(.404,.00109,40,11,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,11,3.1)
 ;;=D Y^DICATTD3(6,1)
 ;;^DIST(.404,.00109,40,11,20)
 ;;=F^^1:20
 ;;^DIST(.404,.00109,40,11,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,12,0)
 ;;=46^WILL STAND FOR^2^^MEANS6
 ;;^DIST(.404,.00109,40,12,2)
 ;;=7,30^30^7,14
 ;;^DIST(.404,.00109,40,12,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,12,3.1)
 ;;=D Y^DICATTD3(6,2)
 ;;^DIST(.404,.00109,40,12,20)
 ;;=F^^1:70
 ;;^DIST(.404,.00109,40,12,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,13,0)
 ;;=47^CODE^2^^CODE7
 ;;^DIST(.404,.00109,40,13,2)
 ;;=8,8^4^8,2
 ;;^DIST(.404,.00109,40,13,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,13,3.1)
 ;;=D Y^DICATTD3(7,1)
 ;;^DIST(.404,.00109,40,13,20)
 ;;=F^^1:20
 ;;^DIST(.404,.00109,40,13,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,14,0)
 ;;=48^WILL STAND FOR^2^^MEANS7
 ;;^DIST(.404,.00109,40,14,2)
 ;;=8,30^30^8,14
 ;;^DIST(.404,.00109,40,14,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,14,3.1)
 ;;=D Y^DICATTD3(7,2)
 ;;^DIST(.404,.00109,40,14,20)
 ;;=F^^1:70
 ;;^DIST(.404,.00109,40,14,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,15,0)
 ;;=49^CODE^2^^CODE8
 ;;^DIST(.404,.00109,40,15,2)
 ;;=9,8^4^9,2
 ;;^DIST(.404,.00109,40,15,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,15,3.1)
 ;;=D Y^DICATTD3(8,1)
 ;;^DIST(.404,.00109,40,15,20)
 ;;=F^^1:20
 ;;^DIST(.404,.00109,40,15,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,16,0)
 ;;=50^WILL STAND FOR^2^^MEANS8
 ;;^DIST(.404,.00109,40,16,2)
 ;;=9,30^30^9,14
 ;;^DIST(.404,.00109,40,16,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,16,3.1)
 ;;=D Y^DICATTD3(8,2)
 ;;^DIST(.404,.00109,40,16,20)
 ;;=F^^1:70
 ;;^DIST(.404,.00109,40,16,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,17,0)
 ;;=51^CODE^2^^CODE9
 ;;^DIST(.404,.00109,40,17,2)
 ;;=10,8^4^10,2
 ;;^DIST(.404,.00109,40,17,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,17,3.1)
 ;;=D Y^DICATTD3(9,1)
 ;;^DIST(.404,.00109,40,17,20)
 ;;=F^^1:20
 ;;^DIST(.404,.00109,40,17,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,18,0)
 ;;=52^WILL STAND FOR^2^^MEANS9
 ;;^DIST(.404,.00109,40,18,2)
 ;;=10,30^30^10,14
 ;;^DIST(.404,.00109,40,18,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,18,3.1)
 ;;=D Y^DICATTD3(9,2)
 ;;^DIST(.404,.00109,40,18,20)
 ;;=F^^1:70
 ;;^DIST(.404,.00109,40,18,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,19,0)
 ;;=53^CODE^2^^CODE10
 ;;^DIST(.404,.00109,40,19,2)
 ;;=11,8^4^11,2
 ;;^DIST(.404,.00109,40,19,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,19,3.1)
 ;;=D Y^DICATTD3(10,1)
 ;;^DIST(.404,.00109,40,19,20)
 ;;=F^^1:20
 ;;^DIST(.404,.00109,40,19,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,20,0)
 ;;=54^WILL STAND FOR^2^^MEANS10
 ;;^DIST(.404,.00109,40,20,2)
 ;;=11,30^30^11,14
 ;;^DIST(.404,.00109,40,20,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,20,3.1)
 ;;=D Y^DICATTD3(10,2)
 ;;^DIST(.404,.00109,40,20,20)
 ;;=F^^1:70
 ;;^DIST(.404,.00109,40,20,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,21,0)
 ;;=55^CODE^2^^CODE11
 ;;^DIST(.404,.00109,40,21,2)
 ;;=12,8^4^12,2
 ;;^DIST(.404,.00109,40,21,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,21,3.1)
 ;;=D Y^DICATTD3(11,1)
 ;;^DIST(.404,.00109,40,21,20)
 ;;=F^^1:20
 ;;^DIST(.404,.00109,40,21,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,22,0)
 ;;=56^WILL STAND FOR^2^^MEANS11
 ;;^DIST(.404,.00109,40,22,2)
 ;;=12,30^30^12,14
 ;;^DIST(.404,.00109,40,22,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,22,3.1)
 ;;=D Y^DICATTD3(11,2)
 ;;^DIST(.404,.00109,40,22,20)
 ;;=F^^1:70
 ;;^DIST(.404,.00109,40,22,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,23,0)
 ;;=57^CODE^2^^CODE12
 ;;^DIST(.404,.00109,40,23,2)
 ;;=13,8^4^13,2
 ;;^DIST(.404,.00109,40,23,3)
 ;;=!M
 ;;^DIST(.404,.00109,40,23,3.1)
 ;;=D Y^DICATTD3(12,1)
 ;;^DIST(.404,.00109,40,23,20)
 ;;=F^^1:20
 ;;^DIST(.404,.00109,40,23,22)
 ;;=D C^DICATTD3
 ;;^DIST(.404,.00109,40,24,0)
 ;;=58^WILL STAND FOR^2^^MEANS12
 ;;^DIST(.404,.00109,40,24,2)
 ;;=13,30^30^13,14
