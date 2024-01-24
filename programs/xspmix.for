      PROGRAM XSPMIX
C-Title  : XSPMIX Program
C-Purpose: Mix fission spectra for the WIMS-D Library
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana Slovenia
C-Version:
C-V  00/01 Compatibility with direct NJOY output.
C-V  01/05 Correct NG0 initialisation.
C-M
C-M  Manual for Program XSPMIX
C-M  =========================
C-M  A block of numbers (format 5F15.0) is expected on two or more
C-M  separate files to represent the fission spectra of fissile
C-M  isotopes. The block of numbers may be preceeded by the header
C-M  record separating entries in the WIMS-D library, which is
C-M  appropriate for the fission spectrum data and has the form:
C-M  "      999999999              i             gg"
C-M  where "i" and "gg" are optional. If present, "i"=2 (obligatory)
C-M  and "gg" is the number of groups in which the spectrum is given.
C-M    By specifying the fraction of the isotope, the spectra
C-M  are weighted to get the average, which is written on output.
C-M  The average spectrum is written with the header shown above,
C-M  which is recognized automatically by the WILLIE code. The
C-M  spectrum is normalised to 1.
C-M
C-M  Input instructions:
C-M  The filenames are requested interactively from keyboard. On
C-M  error (eg. specifying a non-existent input file) the request for
C-M  a filename is repeated. The fraction of the isotope is also
C-M  requested in a similar way. It can be expressed arbitrarily
C-M  (i.e. percent, fraction, etc.) since the final spectrum is
C-M  normalised to 1. No other input is required.
C-M
C-M  NOTE:
C-M  To enable automatic processing of the files produced with NJOY,
C-M  which do not adhere strictly to the label conventions on the
C-M  delimiter record, the first entry in the spectrum should be
C-M  less than 1.
C-
      PARAMETER (MXSP=200)
      CHARACTER*40 BLNK,FLNM,FLSP,FLNO
      DIMENSION    SP1(MXSP),SP2(MXSP)
      DATA SP2/MXSP*0./
      DATA BLNK/'                                        '/
C* Default filenames
      DATA FLSP/'ISOTOPE.XSP'/
     3     FLNO/'FISSION.XSP'/
C* Default number of groups for fission spectrum
      DATA MG0,NG0/ 0, 0 /
C* Default fraction of fissions in the first isotope
      DEL28=0.06
      DELTO=0.
      WRITE(6,91) ' XSPMIX - Fission Spectrum Mixing       '
      WRITE(6,91) ' ================================       '
      WRITE(6,91)
      NISO =0
C* Define filenames
   10 WRITE(6,91) ' Default output spectrum filename     : ',FLNO
      WRITE(6,91) '$          Enter new name to redefine : '
      READ (5,91) FLNM
      IF(FLNM.NE.BLNK) FLNO=FLNM
      OPEN (UNIT=3,FILE=FLNO,STATUS='UNKNOWN',ERR=10)
   20 WRITE(6,91)
      WRITE(6,91) '$Enter fission spectrum filename      : '
      READ (5,91) FLNM
      IF(FLNM.EQ.BLNK) GO TO 40
      FLSP=FLNM
      OPEN (UNIT=1,FILE=FLSP,STATUS='OLD',ERR=20)
   28 WRITE(6,91) '$Enter the fission spectrum fraction  : '
      DEL28=1-DELTO
      READ (5,91) FLNM
      IF(FLNM.NE.BLNK) READ(FLNM,*,ERR=28) DEL28
      DELTO=DELTO+DEL28
      NG0  =0
C*
C* Process the spectrum file
      NISO =NISO+1
      READ (1,91) FLNM
      REWIND 1
      IF(FLNM(7:15).NE.'999999999') GO TO 33
C* Find the appropriate header identifying the spectrum
   32 READ (1,91,END=81) FLNM
      IF(FLNM(7:15).NE.'999999999') GO TO 32
      READ (FLNM,95) JTERM,NSP,JG0
      IF(NSP.GT.0 .AND. NSP.NE.2) GO TO 32
      IF(JG0.GT.0) NG0=JG0
   33 IF(NG0.GT.0) THEN
        READ (1,96,ERR=32) (SP1(J),J=1,NG0)
        IF(SP1(1).GE.1) GO TO 32
      ELSE
        KG1=0
   34   KG0=KG1+1
        KG1=KG1+5
        READ (1,96,END=35,ERR=32) (SP1(J),J=KG0,KG1)
        IF(SP1( 1 ).GE. 1       ) GO TO 32
        IF(SP1(KG0).GT.999999000) GO TO 35
        GO TO 34
   35   NG0=KG0-1
   36   IF(NG0.LE.1) STOP 'XSPMIX ERROR - Zero spectrum'
        IF(SP1(NG0).GT.0) GO TO 37
        NG0=NG0-1
        GO TO 36
      END IF
   37 IF(NISO.GT.1 .AND. MG0.NE.NG0)
     1WRITE(6,92) ' WARNING - Different No. of spectrum gr.',NG0
      MG0=NG0
C* Add the contribution
      DO 38 J=1,NG0
      SP2(J)=SP2(J)+SP1(J)*DEL28
   38 CONTINUE
      GO TO 20
C*
C* Normalize the spectrum
   40 IF(NISO.LT.1) GO TO 82
      WRITE(6,92) ' Number of spectrum files processed   : ',NISO
      SS2=0.
      DO 46 I=1,MG0
      SS2=SS2+SP2(I)
   46 CONTINUE
      IF(SS2.LE.0) GO TO 84
C*
C* Write the spectrum to output
      WRITE(3,95) 999999999,2,MG0
      WRITE(3,94) (SP2(J)/SS2,J=1,MG0)
      STOP 'XSPMIX Completed'
C*
C* Error traps
   81 WRITE(6,91) ' XSPMIX ERROR - Processing spectr.file: ',FLSP
      STOP 'XSPMIX ERROR - invalid spectrum file'
   82 WRITE(6,91) ' XSPMIX ERROR - No spectra to average   '
      STOP 'XSPMIX ERROR - No spectrum files given'
   84 WRITE(6,91) ' XSPMIX ERROR - Zero spectrum on file   '
      STOP 'XSPMIX ERROR - Zero spectrum'
C*
   91 FORMAT(2A40)
   92 FORMAT(A40,I5)
   93 FORMAT(A40,F10.6)
   94 FORMAT(1P,5E15.8)
   95 FORMAT(5I15)
   96 FORMAT(5F15.0)
      END
