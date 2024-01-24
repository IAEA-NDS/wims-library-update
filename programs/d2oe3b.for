       PROGRAM D2OE3B
C-Title  : D2OE3B Program
C-Purpose: Summarize WLUP D2O Benchmarks
C-Version: Original code.
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia (2000)
C-M
C-M  Manual for Program D2OSMR
C-M  =========================
C-M  A summary of results for the WLUP heavy water reactor
C-M  benchmarks is compiled from partial outputs. The output
C-M  list files are processed:
C-M    Ex.1   ZED-2  Task-1 (2 cases)
C-M                  Task-2 (2 cases)
C-M                  Task-3 (1 case)
C-M                  Task-4 (4 cases)
C-M    Ex.2   DCA    Task-1 (6 cases)
C-M
C-M  The list files are those produced by analysing the WIMS-D
C-M  outputs with post-processing codes developed within the scope
C-M  of the WIMS-D Library Update Project (IAEA-CRP 1999-2000) by
C-M  Francisco Leszczynski.
C-M
C-M  Instructions:
C-M  The filenames for the output report file and the 5 list files
C-M  for the benchmarks described above are entered in response to
C-M  the prompt from the keyboard. Individual files may be omitted
C-M  from the analysis by entering "-" for the filename.
C-M
C-
      DIMENSION AVER(6),STDEV(6),EXPER(3,6),CALCUL(3,6),DCALC(3,6)
      DIMENSION DEXPE(6)
      CHARACTER*120 REC
      CHARACTER*40  BLNK,FLNM,FLNI(8),FLNO
      CHARACTER*10  NAME(16)
C* Filenames and logical file units
      DATA LIN,LOU,LKB,LTT / 1, 2, 5, 6 /
      DATA BLNK/'                                        '/
     1    ,FLNI/'E3.lis     '
     2    ,     '           '
     3    ,     '           '
     4    ,     '           '
     1    ,     '           '
     1    ,     '           '
     1    ,     '           '
     1    ,     '           '/
     1    ,FLNO/'D2OE3B.LST'/
C*
      DATA NAME/' EXP  3250'
     1         ,' EXP  6500'
     2         ,' EXP 10800'
     2         ,'          '
     3         ,'          '
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
      WRITE(LTT,693)' D2OE3B comparison AVERAGE VALUES (Ni/Nj%)(delta%)'
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
   11 WRITE(LTT,691) ' Default E3 Burnup Benchmark file  :    ',FLNI(IC)
      WRITE(LTT,692) '$          Enter new name to redefine : '
      READ (LKB,692) FLNM
      IF(FLNM.NE.BLNK) FLNI(IC)=FLNM
      IF(FLNM(1:1).NE.'-') THEN
        OPEN (UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=11)
        CLOSE(UNIT=LIN)
      END IF
C*
      OPEN (UNIT=LOU,FILE=FLNO,STATUS='UNKNOWN')
      WRITE(LOU,693)' D2OE3B comparison AVERAGE VALUES (Ni/Nj%)(delta%)'
      WRITE(LOU,693)' ================================================='
      WRITE(LOU,692)
      WRITE(LOU,691) ' Processed Benchmark analysis reports : '
      IF(FLNI(1).NE.'-')
     1WRITE(LOU,691) '                              1 case  : ',FLNI(1)
      WRITE(LOU,691)
C*
      WRITE(LOU,694) '       BUP N25/N28     N26/N28     N49/N28     N40
     1/N49     N41/N49     N42/N49'
      WRITE(LOU,694) ' =================================================
     1================================'
C
C* Begin processing the E3 Benchmark
      IC=1
      OPEN (UNIT=LIN,FILE=FLNI(IC),STATUS='OLD')
   20 READ (LIN,692) REC
      IF(REC(1:10).NE.'AVERAGE VA') GO TO 20
      DO ILIN=1,9
       READ (LIN,692) REC
      ENDDO
      READ (LIN,700)(EXPER(1,IEXPE),IEXPE=1,6)
      READ (LIN,700)(CALCUL(1,IEXPE),IEXPE=1,6)
      READ (LIN,701)(DCALC(1,IEXPE),IEXPE=1,6)
      DO ILIN=1,5
       READ (LIN,692) REC
      ENDDO
      READ (LIN,700)(EXPER(2,IEXPE),IEXPE=1,6)
      READ (LIN,700)(CALCUL(2,IEXPE),IEXPE=1,6)
      READ (LIN,701)(DCALC(2,IEXPE),IEXPE=1,6)
      DO ILIN=1,9
       READ (LIN,692) REC
      ENDDO
      READ (LIN,700)(EXPER(3,IEXPE),IEXPE=1,6)
      READ (LIN,700)(CALCUL(3,IEXPE),IEXPE=1,6)
      READ (LIN,701)(DCALC(3,IEXPE),IEXPE=1,6)
      DO ILIN=1,5
       READ (LIN,692) REC
      ENDDO
      DO IEXPE=1,6
       READ (LIN,705)AVER(IEXPE),STDEV(IEXPE)
      ENDDO
C* Print the resutls for this benchmark
      DEXPE= 2.0
      DO ICASO=1,3
      WRITE(LOU,702) NAME(ICASO)
     1              ,(EXPER(ICASO,IEXPE),DEXPE(IEXPE),IEXPE=1,6)
      WRITE(LOU,702)'          '
     1              ,(CALCUL(ICASO,IEXPE),DCALC(ICASO,IEXPE),IEXPE=1,6)
      WRITE(LOU,692)
      ENDDO
      CLOSE(UNIT=LIN)
C*
      WRITE(LOU,694) ' =================================================
     1================================'
      WRITE(LOU,703) ' Average  '
     1              ,    (DEXPE(IEXPE),IEXPE=1,6)
      WRITE(LOU,704) ' Dif/St.Dv'
     1              ,(AVER(IEXPE),STDEV(IEXPE),IEXPE=1,6)
C
      CLOSE(UNIT=LOU)
C* End of file processing
  80  STOP 'D2OE3B Completed'
C*
 691  FORMAT(2A40)
 692  FORMAT(A120)
 693  FORMAT(A50)
 694  FORMAT(A77)
 700  FORMAT(10X,6F8.4)
 701  FORMAT(10X,6(2X,F6.2))
 702  FORMAT(A10,F6.3,'(',F4.1,')'
     2          ,F6.3,'(',F4.1,')'
     3          ,F6.3,'(',F4.1,')'
     4          ,F6.2,'(',F4.1,')'
     5          ,F6.3,'(',F4.1,')'
     6          ,F6.3,'(',F5.1,')' )
 703  FORMAT(A10,6X,'(',F4.1,')'
     2          ,6X,'(',F4.1,')'
     3          ,6X,'(',F4.1,')'
     4          ,6X,'(',F4.1,')'
     5          ,6X,'(',F4.1,')'
     6          ,6X,'(',F5.1,')' )
 704  FORMAT(A10,F6.2,'(',F4.1,')'
     2          ,F6.2,'(',F4.1,')'
     3          ,F6.2,'(',F4.1,')'
     4          ,F6.2,'(',F4.1,')'
     5          ,F6.2,'(',F4.1,')'
     6          ,F6.2,'(',F5.1,')' )
 705  FORMAT(11X,2F5.2)
C
      END
