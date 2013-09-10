VECSG2  
 ;;1
 ; VARIABLE LIST
 ;
 ;NAME    EMPLOYEE'S NAME (INPUT BY USER)
 ;DTIME   DELAY TIME FOR READ (NOT TO BE KILLED)
 ;STATUS  FULLTIME OR PARTTIME STATUS OF EMPLOYEE (INPUT BY USER)
 ;XTYPE   STATUS CONVERTED TO WORDS
 ;HRS     NUMBER OF HOURS WORKED (INPUT)
 ;RATE    EMPLOYEE'S RATE OF PAY (INPUT)
 ;GROSS   GROSS PAY (CALCULATED)
 ;PAY     REGULAR PAY
 ;OT      OVERTIME PAY
BEGIN ;
 W !!,"ENTER EMPLOYEE'S NAME OR"
 R !,"PRESS ENTER TO QUIT: ",NAME:DTIME
 I '$T!(NAME["^") GOTO EXIT
 GOTO EXIT:NAME=""
STATUS ;
 W !!,"ENTER EMPLOYEE'S STATUS"
 R !,"F=FULLTIME, P=PARTTIME: ",STATUS:DTIME
 I '$T!(STATUS["^") GOTO EXIT
 S XTYPE=$S(STATUS="F":"FULLTIME",STATUS="P":"PARTTIME",1:"ERROR")
 I XTYPE="ERROR" W !,"RE-ENTER" GOTO STATUS
HOURS ;
 R !!,"ENTER NUMBER OF HOURS WORKED: ",HRS:DTIME
 I '$T!(HRS["^") GOTO EXIT
RATE ;
 R !,"ENTER EMPLOYEE RATE OF PAY: ",RATE:DTIME
 I '$T!(RATE["^") GOTO EXIT
PICK ;
 IF STATUS="F"&(HRS>40) DO OVTPAY
 I STATUS="P"!(HRS'>40) S GROSS=HRS*RATE
OUTPUT ;
 W !!,"GROSS PAY FOR ",NAME," ,",XTYPE," EMPLOYEE, IS $",GROSS,"."
 GOTO BEGIN
OVTPAY ;
 S PAY=40*RATE
 S OT=(HRS-40)*(RATE*1.5)
 S GROSS=PAY+OT
 QUIT
EXIT ;
 KILL NAME,STATUS,XTYPE,HRS,RATE,GROSS,PAY,OT
 QUIT
  
  
  
  
  
