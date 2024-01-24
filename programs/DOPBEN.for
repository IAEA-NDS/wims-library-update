       PROGRAM DOPBEN
C-Title  : DOPBEN Program
C-Purpose: Summarize WLUP DOPPLER Supplementary Benchmarks
C-Version: Original code.
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia (2002)
C-M
C-M  Manual for Program DOPBEN
C-M  =========================
C-M  A summary of results for the WLUP Doppler
C-M  benchmarks is compiled from dopbench.exe output DOPPLER.O
C-M
C-M  The list files are those produced by analysing the WIMS-D
C-M  outputs with post-processing code developed within the scope
C-M  of the WIMS-D Library Update Project (IAEA-CRP 1999-2000) by
C-M  Lhoussine Erradi.
C-M
C-
      DIMENSION EXPER(5),CALCUL(5),DCALC(5)
      DIMENSION DEXPE(5)
      CHARACTER*120 REC
      CHARACTER*40  BLNK,FLNM,FLNI(8),FLNO
      CHARACTER*10  NAME(16)
C* Filenames and logical file units
      DATA LIN,LOU,LKB,LTT / 1, 2, 5, 6 /
      DATA BLNK/'                                        '/
     1    ,FLNI/'DOPPLER.O  '
     2    ,     '           '
     3    ,     '           '
     4    ,     '           '
     1    ,     '           '
     1    ,     '           '
     1    ,     '           '
     1    ,     '           '/
     1    ,FLNO/'DOPBEN.LST'/
C*
      DATA NAME/' REF 0.711'
     1         ,' REF 1.600'
     2         ,' REF 2.400'
     2         ,' REF 3.100'
     3         ,' REF 3.900'
     4         ,'          '
     4         ,'          '
     4         ,'          '
     4         ,'          '
     1         ,'          '
     1         ,'          '
     1         ,'          '
     1         ,'          '
     1         ,'          '
     1         ,'          '
     4         ,'          '/
C*
C* Define input parameters - Write banner to terminal
      WRITE(LTT,693)' DOPPLER comparison DOPPLER COEFF. OF REACTIVITY  '
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
   11 WRITE(LTT,691) ' Default DOPBENCH Benchmark file  :    ',FLNI(IC)
      WRITE(LTT,692) '$          Enter new name to redefine : '
      READ (LKB,692) FLNM
      IF(FLNM.NE.BLNK) FLNI(IC)=FLNM
      IF(FLNM(1:1).NE.'-') THEN
        OPEN (UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=11)
        CLOSE(UNIT=LIN)
      END IF
C*
      OPEN (UNIT=LOU,FILE=FLNO,STATUS='UNKNOWN')
      WRITE(LOU,693)' DOPPLER comparison COEFF.OF REACTIVITY (pcm/K)   '
      WRITE(LOU,693)' ================================================='
      WRITE(LOU,692)
      WRITE(LOU,691) ' Processed Benchmark analysis reports : '
      IF(FLNI(1).NE.'-')
     1WRITE(LOU,691) '                              1 case  : ',FLNI(1)
      WRITE(LOU,691)
C*
      WRITE(LOU,694)'     E(wt%) DOPPLER COEF.(pcm/K)-DIF(%)           '
      WRITE(LOU,694)' ================================================='
C
C* Begin processing DOPPLER Benchmarks
      IC=1
      OPEN (UNIT=LIN,FILE=FLNI(IC),STATUS='OLD')
   20 READ (LIN,692) REC
      IF(REC(1:8).NE.' e (wt%)') GO TO 20
      DO ILIN=1,3
       READ (LIN,692) REC
      ENDDO
      DO IEX=1,4
       READ (LIN,700)EXPER(IEX),CALCUL(IEX),DCALC(IEX)
       DO ILIN=1,4
        READ (LIN,692) REC
       ENDDO
      ENDDO
      READ (LIN,700)EXPER(5),CALCUL(5),DCALC(5)
C* Print the resutls for this benchmark
      DO ICAS=1,5
       DEXPE(ICAS)= 0.0
      ENDDO
      DO ICASO=1,5
       WRITE(LOU,702) NAME(ICASO),EXPER(ICASO),DEXPE(ICASO)
       WRITE(LOU,702)'          ',CALCUL(ICASO),DCALC(ICASO)
       WRITE(LOU,692)
      ENDDO
      CLOSE(UNIT=LIN)
C*
      WRITE(LOU,694)' ================================================='
      SRDC=0.0
      SDDC=0.0
      ADDC=0.0
      DO ICAS=1,5
       SRDC=SRDC+2*DEXPE(ICAS)
       SDDC=SDDC+DCALC(ICAS)*DCALC(ICAS)
       ADDC=ADDC+DCALC(ICAS)
      ENDDO
      SRDC=SRDC/5.0
      SDDC=SQRT(SDDC/5.0)
      ADDC=ADDC/5.0
      WRITE(LOU,703) ' Average  ',SRDC
      WRITE(LOU,704) ' Dif/St.Dv',ADDC,SDDC
C
      CLOSE(UNIT=LOU)
C* End of file processing
  80  STOP 'DOPBEN Completed'
C*
 691  FORMAT(2A40)
 692  FORMAT(A50)
 693  FORMAT(A50)
 694  FORMAT(A50)
 700  FORMAT(49X,F6.3,15X,F5.2,9X,F6.2)
 702  FORMAT(A10,F6.2,'(',F6.2,')')
 703  FORMAT(A10,6X,'(',F6.2,')')
 704  FORMAT(A10,F6.2,'(',F6.2,')')
C
      END
