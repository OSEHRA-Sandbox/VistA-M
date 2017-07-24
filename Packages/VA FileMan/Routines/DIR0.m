DIR0 ;SFISC/MKO-FIELD EDITOR ;2JUN2004
 ;;22.2;MSC Fileman;;Jan 05, 2015;
 ;;Submitted to OSEHRA 5 January 2015 by the VISTA Expertise Network.
 ;;Based on Medsphere Systems Corporation's MSC Fileman 1051.
 ;;Licensed under the terms of the Apache License, Version 2.0.
 ;;GFT;**999,1003,1004**
 ;
SM ;
 N DIR0A,DIR0C,DIR0CH,DIR0CHG,DIR0D,DIR0F,DIR0L,DIR0M
 N DIR0P,DIR0QT,DIR0QU,DIR0R,DIR0RJ,DIR0S,DIR0SP,DIR0ST,DIR0SV,DX,DY
 S DIR0P="" D:$D(DIR0("IN"))[0 GETKEY^DIR0K
 S:$P(DIR0,U,6) DIR0RJ=1
 ;
 I $G(DDSH) K DDSH D BOT^DDSCOM ;BEFORE THE READ, WRITE THE COMMAND LINE!
 S (DIR0A,DIR0D)=$G(DIR("B"))
 S DIR0R=$P(DIR0,U),DIR0S=$P(DIR0,U,2),DIR0L=$P(DIR0,U,3),DIR0M=245
 ;
 W $P(DDGLVID,DDGLDEL,10)
 S DY=$P(DIR0,U,4),DX=$P(DIR0,U,5)
 I $D(DIR("A"))=11 D
 . N DIX
 . S DIX="" F  S DIX=$O(DIR("A",DIX)) Q:DIX=""  D
 .. X IOXY W DIR("A",DIX)
 .. S DY=DY+1
 ;
 I $D(DIR("A"))#2 D
 . X IOXY W DIR("A")
 . I DDO,DY=IOSL-1 W $P(DDGLCLR,DDGLDEL)
 ;
 D INIT,^DIR01 ;Go do the READ
 ;
A I $D(DTOUT) W $C(7) S DIR0A=DIR0D
 I DIR0A="@",DIR0D'="@" S DIR0A=""
 S:DIR0CH="QT" DIR0A=DIR0D
 S X=DIR0A
 S:X?1"^".E!(X?1"?".E) DIR0A=DIR0D
 S DIR0N=X=DIR0D S:DIR0A'=DIR0D DIR0("L")=DIR0A
 ;
 D END,PAINT
 X DDGLZOSF("EON"),DDGLZOSF("TRMOFF")
 Q
 ;
 ;
 ;
EN(DIR0R,DIR0S,DIR0L,DIR0NL,DIR0A,DIR0M,DIR0C,DIR0MAP,DIR0FLG,X,Y) ;
 ;$Y   $X   length       default
 ;Field editor
 N DIR0CH,DIR0CHG,DIR0D,DIR0F,DIR0KD,DIR0P,DIR0QT,DIR0QU
 N DIR0RJ,DIR0SP,DIR0ST,DIR0SV,DIR0TO,DX,DY
 ;
 D INIT^DDGLIB0() ;BEFORE, $Y=4   AFTER $Y=0
 ;
 I $D(DIR0MAP)<2 D
 . S DIR0P="D"
 . D:$D(DIR0("DIN"))[0 GETKEY^DIR0K
 E  D
 . S DIR0P="C"
 . I $O(DIR0MAP(""))!($D(DIR0MAP("IN"))[0) D
 .. D GETKEY^DIR0K
 .. K DIR0MAP S DIR0MAP("IN")=DIR0("CIN"),DIR0MAP("OUT")=DIR0("COUT")
 . E  D
 .. S DIR0("CIN")=$G(DIR0MAP("IN")),DIR0("COUT")=$G(DIR0MAP("OUT"))
 .. S:DIR0("CIN")[(U_"KD"_U) DIR0KD=$P(DIR0("COUT"),";",$L($P(DIR0("CIN"),U_"KD"_U),U))
 .. S:DIR0("CIN")[(U_"TO"_U) DIR0TO=$P(DIR0("COUT"),";",$L($P(DIR0("CIN"),U_"TO"_U),U))
 ;
 S (DIR0A,DIR0D)=$G(DIR0A)
 S:'$G(DIR0R) DIR0R=0
 S:'$G(DIR0S) DIR0S=0
 S:'$G(DIR0L) DIR0L=IOM-1-DIR0S
 S:'$G(DIR0M) DIR0M=245
 S:'$G(DIR0FLG)["r" DIR0RJ=1
 ;
 I $G(DIR0NL)>1 D
 . D EN^DIR02,END
 E  D INIT,^DIR01,END,PAINT
 ;
 S X=DIR0A
 I $D(DTOUT) K DTOUT S:Y="" Y="TO"
 S $P(Y,U,2)=+$G(DIR0CHG)
 D KILL^DDGLIB0($G(DIR0FLG))
 K DIR0("CIN"),DIR0("COUT")
 Q
 ;
INIT ;
 K DTOUT
 X DDGLZOSF("EOFF"),DDGLZOSF("TRMON")
 S DIR0SV=$G(DIR0("L"))
 S DIR0C=$S($G(DIR0C)<1:0,1:DIR0C)+1
 S:DIR0C-1>$L(DIR0A) DIR0C=$L(DIR0A)+1
 S (DIR0QT,DIR0QU)=0,DY=DIR0R,DX=DIR0S,DIR0F=DIR0S+DIR0L
 ;
 X IOXY
 S DIR0SP=$J("",DIR0L) S:$G(DDGLVAN) DIR0SP=$TR(DIR0SP," ","_")
 I DIR0C-1>DIR0L D
 . W $S('$D(DDGLVAN):$P(DDGLVID,DDGLDEL,6),1:"")_$E(DIR0A,DIR0C-DIR0L,DIR0C-1)
 . S DX=DIR0F
 E  D
 . W $S('$D(DDGLVAN):$P(DDGLVID,DDGLDEL,6),1:"")_$E(DIR0A,1,DIR0L)_$E(DIR0SP,$L(DIR0A)+1,999)
 . S DX=DIR0S+DIR0C-1
 . X IOXY
 Q
 ;
END ;
 S Y=$P("U^D^R^L^N^NB^NP^PP^SEL^EX^QT^CL^SV^RF^PRNT^MOUSE",U,$L($P("^UP^DOWN^TAB^FDL^CR^NB^NP^PP^SEL^EX^QT^CL^SV^RF^PRNT^MOUSE^",U_DIR0CH_U),U)) ;RETURN Y FROM READER
 S:Y="" Y=$P($G(DIR0QT),U,2)
 N X,Y S DIR0SP=$TR(DIR0SP,"_"," ")
 S DIR0C=DIR0C-1
 Q
 ;
PAINT ;
 N DIR0X
 I $G(DIR0FLG)["P" W $P(DDGLVID,DDGLDEL,10) Q
 I '$G(DIR0RJ) S DIR0X=$E(DIR0A,1,DIR0L)_$E(DIR0SP,$L(DIR0A)+1,999)
 E  S DIR0X=$E(DIR0SP,$L(DIR0A)+1,999)_$E(DIR0A,1,DIR0L)
 S DX=DIR0S X IOXY
 W $P(DDGLVID,DDGLDEL,10)_$P(DDGLVID,DDGLDEL)_DIR0X_$P(DDGLVID,DDGLDEL,10)
 Q
 ;
UPDATE(DIR0NA,DIR0NC) ;Update ans/curs pos
 N DIR0STR,DIR0X
 S:$D(DIR0NA)[0 DIR0NA=DIR0A
 S DIR0NC=$S($D(DIR0NC)[0:DIR0C-1,1:DIR0NC)+1
 S:DIR0NC<1 DIR0NC=1
 S:DIR0NC-1>$L(DIR0NA) DIR0NC=$L(DIR0NA)+1
 S DIR0X=DX+DIR0NC-DIR0C
 ;
 I DIR0A=DIR0NA,DIR0X'<DIR0S,DIR0X'>DIR0F D
 . S DX=DIR0X X IOXY
 E  D
 . S DIR0X=DIR0NC-DIR0L S:DIR0X<1 DIR0X=1
 . S DX=DIR0S X IOXY
 . S DIR0STR=$E(DIR0NA,DIR0X,DIR0X+DIR0L-1)
 . W DIR0STR_$E(DIR0SP,$L(DIR0STR)+1,999)
 . S DX=DIR0S+DIR0NC-DIR0X X IOXY
 ;
 S DIR0A=DIR0NA,DIR0C=DIR0NC
 Q
 ;
KILL ;
 D KILL^DDGLIB0()
 Q
 ;
 ;#8074  Press <F1>H for help
 ;#7002  Insert^Replace
