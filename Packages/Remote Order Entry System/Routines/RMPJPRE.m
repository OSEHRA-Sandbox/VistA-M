RMPJPRE ;DDC/KAW-PRE-INIT FOR ROES PATCH RMPF*1.1*9 [ 09/27/94  7:56 AM ]
 ;;1.1;RMPF;**9**;SEP 27, 1994
 W !!,"Inactivating old models and components ."
A S NM="ACTIVE LOWCUT POT",X="ACTIVE LOW CUT" D SUB1 W "."
B S NM="HELIX AID OPTION",X="HELIX" D SUB1 W "."
C S NM="HYPOALLERGENIC SHELL",X="CLEAR SHELL" D SUB1 W "."
D S NM="K-AMP",X="KAMP" D SUB1 W "."
E S NM="POWER",X="POWER OPTION" D SUB1 W "."
F S NM="T-COIL W/ PRE-AMP",X="TELECOIL PREAMP" D SUB1 W "."
G S NM="TELEPHONE COIL W/SWITCH",X="TELECOIL SWITCH" D SUB1 W "."
H S NM="CRYSTAL CLEAR ASP/POT",X="CRYSTAL CLEAR ASP POT" D SUB1 W "."
I S NM="CRYSTAL CLEAR COMPRESS/POT",X="CRYSTAL CLEAR COMPRESSION POT" D SUB1 W "."
J S NM="CRYSTAL CLR POWER",X="CRYSTAL CLEAR POWER" D SUB1 W "."
K S NM="FEEDBACK REDUCTION CIRC/POT",X="FEEDBACK REDUCTION CIRCUIT POT" D SUB1 W "."
L S NM="HIGH-CUT CONTROL",X="HIGH CUT CONTROL" D SUB1 W "."
M S NM="PUSH PULL W/ DUAL RECEIVER",X="PUSH PULL DUAL RECEIVER" D SUB1 W "."
N S NM="TELE  COIL W/BOOSTER & SWITCH",X="TELECOIL BOOSTER SWITCH" D SUB1 W "."
O S NM="TK TRIMMER",X="TK POT" D SUB1 W "."
P S NM="AUTO GAIN CONTROL INPUT/POT",X="AUTO GAIN CONTROL INPUT POT" D SUB1 W "."
Q S NM="CANAL RESONANCE CIRCUIT/POT",X="CANAL RESONANCE CIRCUIT POT" D SUB1 W "."
R S NM="LOW FREQUENCY CUT CONTROL",X="LOW FREQUENCY CONTROL" D SUB1 W "."
S S NM="HIGH BAND CIRCUIT W/ CLOCK POT",X="HI-BAND CIRCUIT W/ CLOCK POT" D SUB1 W "."
T S NM="HIGH POWER TELEPHONE COIL",X="HIGH POWER TELECOIL" D SUB1 W "."
U S NM="LOW FREQUENCY CUT CONTROL",X="LOW FREQUENCY CONTROL" D SUB1 W "."
V S NM="FREQUENCY",X="FREQUENCY CONTROL" D SUB1 W "."
W S NM="AUTO GAIN CONTROL INPUT",X="AGC-I" D SUB1 W "."
X S NM="PUSH PULL",X="PUSH/PULL" D SUB1 W "."
Y S NM="ACTIVE HIGHCUT POT",X="ACTIVE HIGH CUT" D SUB1 W "."
Z S NM="COMPRESSION",X="COMPRESSION SWITCH" D SUB1 W "."
AA S NM="FULL SPECTRUM LOW CUT W/ SWITCH",X="FULL SPECTRUM LOW CUT SWITCH" D SUB1 W "."
BB S NM="POWER CROS BONE OSCILLATOR & HEADBAND",X="POWER CROS BONE OSCILLATOR & HEADBAND" D SUB1 W "."
CC S NM="STAR TELEPHONE SWITCH",X="STAR TELECOIL SWITCH" D SUB1 W "."
 S RMPFX=0
LOOP S RMPFX=$O(^RMPF(791811,RMPFX)) G END:'RMPFX,LOOP:'$D(^(RMPFX,0)) S S0=^(0) W "."
 S PG=$P(S0,U,3) G LOOP:PG'=2
 S $P(^RMPF(791811,RMPFX,"I"),U,1)=1
 G LOOP
END Q
SUB1 S DA=$O(^RMPF(791811.2,"B",NM,0)) Q:'DA  S DIE="^RMPF(791811.2,"
 S DR=".01////"_X D ^DIE Q
