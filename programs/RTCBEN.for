       PROGRAM RTCBEN
C-Title  : RTCBEN Program
C-Purpose: Summarize WLUP RTC Supplementary Benchmarks
C-Version: Original code.
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia (2002)
C-A        Corrected by D. L. Aldama IAEA/NDS Consultant, 2005.
C-M
C-M  Manual for Program RTCBEN
C-M  =========================
C-M  A summary of results for the WLUP RTC
C-M  benchmarks is compiled from pintcr.exe output RTC.O
C-M
C-M  The list files are those produced by analysing the WIMS-D
C-M  outputs with post-processing code developed within the scope
C-M  of the WIMS-D Library Update Project (IAEA-CRP 1999-2000) by
C-M  Lhoussine Erradi.
C-M
C-
      DIMENSION EXPER(10),CALCUL(10),DCALC(10)
      DIMENSION DEXPE(10)
      CHARACTER*120 REC
      CHARACTER*40  BLNK,FLNM,FLNI(8),FLNO
      CHARACTER*10  NAME(16)
C* Filenames and logical file units
      DATA LIN,LOU,LKB,LTT / 1, 2, 5, 6 /
      DATA BLNK/'                                        '/
     1    ,FLNI/'RTC.O      '
     2    ,     '           '
     3    ,     '           '
     4    ,     '           '
     1    ,     '           '
     1    ,     '           '
     1    ,     '           '
     1    ,     '           '/
     1    ,FLNO/'RTCBEN.LST'/
C*
      DATA NAME/'NORA-1.66 '
     1         ,'NORA-3.03 '
     2         ,'   KRITZ1 '
     2         ,'  KRITZ21 '
     3         ,' KRITZ213 '
     4         ,' KRITZ219 '
     4         ,'    VVER1 '
     4         ,'    VVER2 '
     4         ,'    VVER3 '
     1         ,'    VVER4 '
     1         ,'          '
     1         ,'          '
     1         ,'          '
     1         ,'          '
     1         ,'          '
     4         ,'          '/
C*
C* Define input parameters - Write banner to terminal
      WRITE(LTT,693)' RTC comparison - REACTIVITY TEMPERATURE COEFFIC. '
      WRITE(LTT,693)' ================================================='
      WRITE(LTT,692)
C*
C* Define the output file
      WRITE(LTT,691) ' Default output filename              : ',FLNO
      WRITE(LTT,692) '$          Enter new name to redefine : '
      READ (LKB,692) FLNM
      IF(FLNM.NE.BLNK) FLNO=FLNM
C*
C* Define the output file
      IC=1
   11 WRITE(LTT,691) ' Default RTCBENCH Benchmark file  :    ',FLNI(IC)
      WRITE(LTT,692) '$          Enter new name to redefine : '
      READ (LKB,692) FLNM
      IF(FLNM.NE.BLNK) FLNI(IC)=FLNM
      IF(FLNM(1:1).NE.'-') THEN
        OPEN (UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=11)
        CLOSE(UNIT=LIN)
      END IF
C*
      OPEN (UNIT=LOU,FILE=FLNO,STATUS='UNKNOWN')
      WRITE(LOU,693)' RTC-DIFFS.BETWEEN CALCULATION AND MEASUREMENTS   '
      WRITE(LOU,693)' ================================================='
      WRITE(LOU,692)
      WRITE(LOU,691) ' Processed Benchmark analysis reports : '
      IF(FLNI(1).NE.'-')
     1WRITE(LOU,691) '                              1 case  : ',FLNI(1)
      WRITE(LOU,691)
C*
      WRITE(LOU,694)'     Case   D-ALPHA(pcm/C)                        '
      WRITE(LOU,694)' ================================================='
C
C* Begin processing DOPPLER Benchmarks
      IC=1
      OPEN (UNIT=LIN,FILE=FLNI(IC),STATUS='OLD')
   20 READ (LIN,692) REC
      IF(REC(1:8).NE.'  Vm/Vf ') GO TO 20
      DO ILIN=1,3
       READ (LIN,692) REC
      ENDDO
c           NORA 1.66
      READ (LIN,800)DCALC(1)
      DO ILIN=1,4
       READ (LIN,692) REC
      ENDDO
c           NORA 3.03
      READ (LIN,800)DCALC(2)
      DO ILIN=1,19
       READ (LIN,692) REC
      ENDDO
c           KRITZ1
      READ (LIN,801)DCALC(3)
      DO ILIN=1,18
       READ (LIN,692) REC
      ENDDO
c           KRITZ21
      READ (LIN,801)DCALC(4)
      DO ILIN=1,18
       READ (LIN,692) REC
      ENDDO
c           KRITZ213
      READ (LIN,801)DCALC(5)
      DO ILIN=1,17
       READ (LIN,692) REC
      ENDDO
c           KRITZ219
      READ (LIN,801)DCALC(6)
c
c           R1-100H skipped (38 lines)
c
      DO ILIN=1,38
       READ (LIN,692) REC
      ENDDO
c           VVER1
      READ (LIN,802)DCALC(7)
      DO ICAS=8,10
       DO ILIN=1,17
        READ (LIN,692) REC
       ENDDO
c           VVER2 to VVER4
       READ (LIN,802)DCALC(ICAS)
      ENDDO
C* Print the resutls for this benchmark
      DO ICAS=1,10
       DEXPE(ICAS)= 0.0
      ENDDO
      DO ICASO=1,10
       WRITE(LOU,701) NAME(ICASO)
       WRITE(LOU,702) DCALC(ICASO)
       WRITE(LOU,692)
      ENDDO
      CLOSE(UNIT=LIN)
C*
      WRITE(LOU,694)' ================================================='
      SRDC=0.0
      SDDC=0.0
      ADDC=0.0
      DO ICAS=1,10
       SRDC=SRDC+2*DEXPE(ICAS)
       SDDC=SDDC+DCALC(ICAS)*DCALC(ICAS)
       ADDC=ADDC+DCALC(ICAS)
      ENDDO
      SRDC=SRDC/10.0
      SDDC=SQRT(SDDC/10.0)
      ADDC=ADDC/10.0
      WRITE(LOU,703) ' Average  ',SRDC
      WRITE(LOU,704) ' Dif/St.Dv',ADDC,SDDC
C
      CLOSE(UNIT=LOU)
C* End of file processing
  80  STOP 'RTCBEN Completed'
C*
 691  FORMAT(2A40)
 692  FORMAT(A50)
 693  FORMAT(A50)
 694  FORMAT(A50)
 701  FORMAT(A10)
 702  FORMAT(19X,'(',F9.6,')')
 703  FORMAT(A10,9X,'(',F9.6,')')
 704  FORMAT(A10,F9.6,'(',F9.6,')')
 800  FORMAT(65X,F11.8)
 801  FORMAT(45X,F10.7)
 802  FORMAT(54X,F11.8)
C
      END
