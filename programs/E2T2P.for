       PROGRAM E2T2P
C-Title  : E2T2P Program
C-Purpose: Summarize WLUP E2T2 Benchmark (54 pin-cluster with Gd)
C-Version: Original code.
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia (2000)
C-M
C-M  Manual for Program E2T2P
C-M  ========================
C-M  A summary of results for the WLUP E2T2 benchmark
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
      DIMENSION AVER(6),STDEV(6),EXPER(17,1),CALCUL(17,1),DCALC(17,1)
      CHARACTER*120 REC
      CHARACTER*40  BLNK,FLNM,FLNI(8),FLNO
      CHARACTER*10  NAME(17)
C* Filenames and logical file units
      DATA LIN,LOU,LKB,LTT / 1, 2, 5, 6 /
      DATA BLNK/'                                        '/
     1    ,FLNI/'E2T2.lis   '
     2    ,     '           '
     3    ,     '           '
     4    ,     '           '
     1    ,     '           '
     1    ,     '           '
     1    ,     '           '
     1    ,     '           '/
     1    ,FLNO/'E2T2.LST   '/
C*
      DATA NAME/' EXP     1'
     1         ,' EXP     2'
     2         ,' EXP     3'
     3         ,' EXP     4'
     4         ,' EXP     5'
     5         ,' EXP     6'
     6         ,' EXP     7'
     7         ,' EXP     8'
     8         ,' EXP     9'
     9         ,' EXP    10'
     A         ,' EXP    11'
     B         ,' EXP    12'
     C         ,' EXP    13'
     D         ,' EXP    14'
     E         ,' EXP    15'
     F         ,' EXP    16'
     G         ,' EXP    17'/
C*
C* Define input parameters - Write banner to terminal
      WRITE(LTT,693)' E2T2 PROCESS - Gd D2O/AIR COOL.Gd 54 PIN CLUSTER '
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
   11 WRITE(LTT,691) ' Default E2  Burnup Benchmark file  :   ',FLNI(IC)
      WRITE(LTT,692) '$          Enter new name to redefine : '
      READ (LKB,692) FLNM
      IF(FLNM.NE.BLNK) FLNI(IC)=FLNM
      IF(FLNM(1:1).NE.'-') THEN
        OPEN (UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=11)
        CLOSE(UNIT=LIN)
      END IF
C*
      IC=1
      OPEN (UNIT=LIN,FILE=FLNI(IC),STATUS='OLD')
      OPEN (UNIT=LOU,FILE=FLNO,STATUS='UNKNOWN')
      DO ILIN=1,3
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
      WRITE(LOU,692)
      WRITE(LOU,691) ' Processed Benchmark analysis reports : '
      IF(FLNI(1).NE.'-')
     1WRITE(LOU,691) '                              1 case  : ',FLNI(1)
      WRITE(LOU,691)
C*
      DO ILIN=1,2
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
C
C* Begin processing the E2T2 Benchmark
C
C TABLE 1
C
      DO ILIN=1,10
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
      WRITE(LOU,719)' Average  '
      WRITE(LOU,719)' Dif/St.Dv'
      WRITE(LOU,718)' # '
C *************
C
C FIG.1A
C
      DO ILIN=1,2
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
      WRITE(LOU,692)
      WRITE(LOU,691) ' Processed Benchmark analysis reports : '
      IF(FLNI(1).NE.'-')
     1WRITE(LOU,691) '                              1 case  : ',FLNI(1)
      WRITE(LOU,691)
C*
      DO ILIN=1,2
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
C
      DO ILIN=1,55
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
      WRITE(LOU,719)' Average  '
      WRITE(LOU,719)' Dif/St.Dv'
      WRITE(LOU,718)' # '
C *************
C
C FIG.1B
C
      DO ILIN=1,2
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
      WRITE(LOU,692)
      WRITE(LOU,691) ' Processed Benchmark analysis reports : '
      IF(FLNI(1).NE.'-')
     1WRITE(LOU,691) '                              1 case  : ',FLNI(1)
      WRITE(LOU,691)
C*
      DO ILIN=1,2
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
C
      DO ILIN=1,55
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
      WRITE(LOU,719)' Average  '
      WRITE(LOU,719)' Dif/St.Dv'
      WRITE(LOU,718)' # '
C *************
C
C FIG.2
C
      DO ILIN=1,2
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
      WRITE(LOU,692)
      WRITE(LOU,691) ' Processed Benchmark analysis reports : '
      IF(FLNI(1).NE.'-')
     1WRITE(LOU,691) '                              1 case  : ',FLNI(1)
      WRITE(LOU,691)
C*
      DO ILIN=1,2
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
C
      DO ILIN=1,49
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
      WRITE(LOU,719)' Average  '
      WRITE(LOU,719)' Dif/St.Dv'
      WRITE(LOU,718)' # '
C *************
C
C TAB.32A
C
      DO ILIN=1,2
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
      WRITE(LOU,692)
      WRITE(LOU,691) ' Processed Benchmark analysis reports : '
      IF(FLNI(1).NE.'-')
     1WRITE(LOU,691) '                              1 case  : ',FLNI(1)
      WRITE(LOU,691)
C*
      DO ILIN=1,2
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
C
      DO ILIN=1,13
       READ (LIN,692) REC
       WRITE (LOU,692) REC
      ENDDO
      WRITE(LOU,719)' Average  '
      WRITE(LOU,719)' Dif/St.Dv'
C ***************************************************
 300  CLOSE(UNIT=LIN)
      CLOSE(UNIT=LOU)
C* End of file processing
  80  STOP 'E2T2P Completed'
C*
 691  FORMAT(2A40)
 692  FORMAT(A120)
 693  FORMAT(A50)
 694  FORMAT(A45)
 700  FORMAT(10X,F6.1)
 701  FORMAT(10X,2F6.1)
 702  FORMAT(A10,F6.1,'(',F4.1,')')
 715  FORMAT(A10,1X,E9.2,2X,E9.2)
 716  FORMAT(11X,E9.2,2X,E9.2)
 717  FORMAT(A10,1X,1PE9.2,'(',1PE9.2,')')
 718  FORMAT(A4)
 719  FORMAT(A10)
C
      END
