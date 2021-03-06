EAS126 ;ALB/PHH - EAS*1*26 POST-INSTALL ;05-27-2003
 ;;1.0;ENROLLMENT APPLICATION SYSTEM;**26**;MAR 15,2001
 Q
EN ; Entry point for post-install
 D FIXOPT,START
 Q
RESET ; Reset the data for the cleanup process
 K ^XTMP($$NAMESPC)
 Q
TEST ; Simulate Live Run
 N MODE
 S MODE=0
START ; Start Processor
 N NAMESPC,QTIME
 S NAMESPC=$$NAMESPC
 Q:$$RUNCHK(NAMESPC)   ; Quit if already running or has run to completion
 Q:$$QTIME(.QTIME)
 S:$D(^XTMP(NAMESPC,"CONFIG","RUN MODE")) MODE=^XTMP(NAMESPC,"CONFIG","RUN MODE")
 S:'$D(^XTMP(NAMESPC,"CONFIG","RUN MODE")) ^XTMP(NAMESPC,"CONFIG","RUN MODE")=$S($G(MODE)=0:0,1:1)
 S ^XTMP(NAMESPC,"CONFIG","USER")=$S($G(DUZ)'="":DUZ,1:"UNKNOWN")
 S:'$$QUEUE(QTIME) ^XTMP(NAMESPC,"CONFIG","RUNNING")=""
 Q
NAMESPC() ; API returns the name space for this patch
 Q "EAS26"
RUNCHK(NAMESPC) ; Check to see if clean up is already running
 Q:NAMESPC="" 1                   ; Name Space must be defined
 Q:$D(^XTMP(NAMESPC,"CONFIG","RUNNING")) 1
 Q:$D(^XTMP(NAMESPC,"CONFIG","COMPLETE")) 1
 Q 0
QTIME(WHEN) ; Get the run time for queuing
 N %,%H,%I,X
 D NOW^%DTC
 S WHEN=$P(%,".")_"."_$E($P(%,".",2),1,4)
 Q 0
QUEUE(ZTDTH) ; Queue the process
 N NAMESPC,QUEERR,ZTDESC,ZTRTN,ZTSK
 S NAMESPC=$$NAMESPC
 S QUEERR=0
 S ZTRTN="CLEAN^EAS1"_$P(NAMESPC,"EAS",2)
 S ZTDESC=NAMESPC_" - Patient Merge Cleanup Process"
 S ZTIO=""
 D ^%ZTLOAD
 K ^XTMP(NAMESPC,"CONFIG","ZTSK")
 I '$D(ZTSK) S ^XTMP(NAMESPC,"CONFIG","ZTSK")="Unable to queue post-install process.",QUEERR=1
 I $D(ZTSK) S ^XTMP(NAMESPC,"CONFIG","ZTSK")="Post-install queued. Task ID: "_$G(ZTSK)
 D HOME^%ZIS
 Q QUEERR
CLEAN ; Actual cleanup process
 N NAMESPC,MODE,USER,TASKID,%,%H,%I,X,X1,X2,CHKCNT,TMSWT,TOTPR12,DGPR12
 S NAMESPC=$$NAMESPC
 K ^XTMP(NAMESPC,"CONFIG","ABORT TIME")
 S MODE=$G(^XTMP(NAMESPC,"CONFIG","RUN MODE"),0)
 S USER=$G(^XTMP(NAMESPC,"CONFIG","USER"),"UNKNOWN")
 S TASKID=$G(^XTMP(NAMESPC,"CONFIG","ZTSK"),"UNKNOWN")
 ;
 I '$D(^XTMP(NAMESPC,0)) D
 .K ^XTMP(NAMESPC)
 .S ^XTMP(NAMESPC,"CONFIG","DGPR12")=0
 .S ^XTMP(NAMESPC,"CONFIG","TOTPR")=0
 .S ^XTMP(NAMESPC,"CONFIG","PERCENT COMPLETE")=0
 .S ^XTMP(NAMESPC,"CONFIG","RUN MODE")=MODE
 .S ^XTMP(NAMESPC,"CONFIG","USER")=USER
 .S ^XTMP(NAMESPC,"CONFIG","ZTSK")=TASKID
 .D NOW^%DTC
 .S ^XTMP(NAMESPC,"CONFIG","START TIME")=%
 .S X1=$$DT^XLFDT,X2=90
 .D C^%DTC
 .S ^XTMP(NAMESPC,0)=X_U_$$DT^XLFDT_U_NAMESPC_" - PATIENT MERGE CLEANUP"
 ;
 S CHKCNT=250,(ZTSTOP,TMSWT)=0,TOTPR12=+$P($G(^DGPR(408.12,0)),"^",4)
 S DGPR12=$G(^XTMP(NAMESPC,"CONFIG","DGPR12"))
 F  S DGPR12=$O(^DGPR(408.12,DGPR12)) Q:'DGPR12!(TMSWT)  D
 .D PROC(DGPR12)
 .S ^XTMP(NAMESPC,"CONFIG","TOTPR")=$G(^XTMP(NAMESPC,"CONFIG","TOTPR"))+1
 .S ^XTMP(NAMESPC,"CONFIG","DGPR12")=DGPR12
 .I TOTPR12 D
 ..S ^XTMP(NAMESPC,"CONFIG","PERCENT COMPLETE")=+$G(^XTMP(NAMESPC,"CONFIG","TOTPR"))/TOTPR12
 ..S ^XTMP(NAMESPC,"CONFIG","PERCENT COMPLETE")=+$P((^XTMP(NAMESPC,"CONFIG","PERCENT COMPLETE")*100),".")
 .I +$G(^XTMP(NAMESPC,"CONFIG","TOTPR"))#CHKCNT=0 D
 ..S TMSWT=$$STOPIT()
 ..I TMSWT D
 ...S ZTSTOP=1
 ...N %,%H,%I,X
 ...D NOW^%DTC
 ...S ^XTMP(NAMESPC,"CONFIG","ABORT TIME")=%
 ...D ABORTMSG
 ;
 I 'DGPR12,'TMSWT D
 .N %,%H,%I,X
 .D NOW^%DTC
 .S ^XTMP(NAMESPC,"CONFIG","COMPLETE")=%
 .S ^XTMP(NAMESPC,"CONFIG","PERCENT COMPLETE")=100
 .D DONEMSG
 ;
 K ^XTMP(NAMESPC,"CONFIG","RUNNING")
 Q
PROC(DGPR12) ; Process the DGPR12
 N NAMESPC,NODE0,DFN,PERSON,DATA,DIE,DA,DR,X
 S NAMESPC=$$NAMESPC()
 S NODE0=$G(^DGPR(408.12,DGPR12,0))
 Q:NODE0=""
 S DFN=$P(NODE0,"^")
 Q:DFN=""
 S PERSON=$P(NODE0,"^",3)
 I PERSON'="",PERSON["DPT",DFN=$P(PERSON,";"),'$D(^DGPR(408.12,"C",PERSON,DGPR12)) D
 .S DATA(.03)=$$GET1^DIQ(408.12,DGPR12_",",.01,"I")_";DPT("
 .S DIE="^DGPR(408.12,",DA=DGPR12,DR=".03////^S X=DATA(.03)"
 .D:MODE ^DIE
 .S ^XTMP(NAMESPC,"CONFIG","ANOMALY")=$G(^XTMP(NAMESPC,"CONFIG","ANOMALY"))+1
 .S ^XTMP(NAMESPC,"DATA",DGPR12)=""
 Q
STOPIT() ; Checks if user requested task to stop
 N X,STOPIT
 S STOPIT=0
 S X=$$S^%ZTLOAD
 I X D  ;
 .S STOPIT=1
 .I $G(ZTSK) S ZTSTOP=1
 Q STOPIT
ABORTMSG ; Send the user aborted message:
 N NAMESPC,NAMESPCN,TMP,XMY,XMDUZ,XMTEXT,XMSUB
 S NAMESPC=$$NAMESPC
 S NAMESPCN=$P(NAMESPC,"EAS",2)
 S XMY(DUZ)="",XMDUZ="DG PACKAGE",XMTEXT="TMP("_NAMESPCN_","
 S XMSUB="EAS*1.0*"_NAMESPCN_": PATIENT MERGE CLEANUP - PROCESS STOPPED BY USER"
 S TMP(NAMESPCN,1)="CLEANUP PROCESSING"
 S TMP(NAMESPCN,2)="------------------"
 S TMP(NAMESPCN,3)=""
 S TMP(NAMESPCN,4)="The cleanup process was aborted prematurely.  Here is the current status:"
 S TMP(NAMESPCN,5)=""
 S TMP(NAMESPCN,6)="  Start Date/Time: "_$$FMTE^XLFDT(+$G(^XTMP(NAMESPC,"CONFIG","START TIME")),"P")
 S TMP(NAMESPCN,7)="    End Date/Time: "_$$FMTE^XLFDT(+$G(^XTMP(NAMESPC,"CONFIG","ABORT TIME")),"P")
 S TMP(NAMESPCN,8)=""
 S TMP(NAMESPCN,9)="Current Counts: "
 S TMP(NAMESPCN,10)="       Total Patient Records Processed: "_+$G(^XTMP(NAMESPC,"CONFIG","TOTPR"))
 S TMP(NAMESPCN,11)="             Total Anomalies Corrected: "_+$G(^XTMP(NAMESPC,"CONFIG","ANOMALY"))
 S TMP(NAMESPCN,12)="                  Percentage Completed: "_+$G(^XTMP(NAMESPC,"CONFIG","PERCENT COMPLETE"))_"%"
 S TMP(NAMESPCN,13)=""
 S TMP(NAMESPCN,14)=""
 D ^XMD
 Q
DONEMSG ; Send the user done message:
 N NAMESPC,NAMESPCN,TMP,XMY,XMDUZ,XMTEXT,XMSUB
 S NAMESPC=$$NAMESPC
 S NAMESPCN=$P(NAMESPC,"EAS",2)
 S XMY(DUZ)="",XMDUZ="DG PACKAGE",XMTEXT="TMP("_NAMESPCN_","
 S XMSUB="EAS*1.0*"_NAMESPCN_": PATIENT MERGE CLEANUP - SUMMARY REPORT"
 S TMP(NAMESPCN,1)="CLEANUP PROCESSING"
 S TMP(NAMESPCN,2)="------------------"
 S TMP(NAMESPCN,3)=""
 S TMP(NAMESPCN,4)="The cleanup has run to completion.  Here are the results:"
 S TMP(NAMESPCN,5)=""
 S TMP(NAMESPCN,6)="  Start Date/Time: "_$$FMTE^XLFDT(+$G(^XTMP(NAMESPC,"CONFIG","START TIME")),"P")
 S TMP(NAMESPCN,7)="    End Date/Time: "_$$FMTE^XLFDT(+$G(^XTMP(NAMESPC,"CONFIG","COMPLETE")),"P")
 S TMP(NAMESPCN,8)=""
 S TMP(NAMESPCN,9)="Current Counts: "
 S TMP(NAMESPCN,10)="       Total Patient Records Processed: "_+$G(^XTMP(NAMESPC,"CONFIG","TOTPR"))
 S TMP(NAMESPCN,11)="             Total Anomalies Corrected: "_+$G(^XTMP(NAMESPC,"CONFIG","ANOMALY"))
 S TMP(NAMESPCN,12)="                  Percentage Completed: 100%"
 S TMP(NAMESPCN,13)=""
 S TMP(NAMESPCN,14)=""
 D ^XMD
 Q
FIXOPT ; Fix 'EAS MT 30 DAY LETTER PRINT' option
 N OPTNAME,OPTIEN,DIE,DA,DR,X
 S OPTNAME="EAS MT 30 DAY LETTER PRINT"
 Q:'$D(^DIC(19,"B",OPTNAME))
 S OPTIEN=0
 F  S OPTIEN=$O(^DIC(19,"B",OPTNAME,OPTIEN)) Q:'OPTIEN  D
 .S DIE="^DIC(19,",DA=OPTIEN,DR="25///^S X=""@"""
 .D ^DIE
 Q
