C     Program for calculation of temperature coefficient
C     Authors : L.ERRADI, A.HTET, E.CHAKIR
      PROGRAM TEMPERATURE
       CAll ENTETETCR
       CAll EBTCR
       CALL NORA
       CALL KRITZ1
       CALL KRITZ21
       CALL KRITZ213
       CALL KRITZ219
       CALL R1100H
       CALL VVER1
       CALL VVER2
       CALL VVER3
       CALL VVER4
      END
C *********************************************************
      SUBROUTINE ENTETETCR
C *********************************************************
      OPEN(UNIT=5,file='ENTETETCR.RES',STATUS='UNKNOWN')
      WRITE(5,1000)
1000  FORMAT (20x,44(1H*)/
     1 20x, '*       WIMS LIBRARY UPDATE PROJECT        *'/
     2 20x, '*          LIBRARIES VALIDATION            *'/
     2 20x, '*      FOR THE REACTIVITY TEMPERATURE      *'/
     2 20X, '*          COEFFICIENT (RTC)               *'/
     3 20x,44(1H*)///
     4 15x,54(1H*)/
     5 15x,'*           L.ERRADI, A.HTET AND E.CHAKIR',12x,1(1H*)/
     6 15x,'*      GROUPE DE PHYSIQUE DES REACTEURS NUCLEAIRES',3x,1H*/
     7 15x,'*           FACULTE DES SCIENCES -  RABAT',12x,1(1H*)/
     8 15x,'*                      MOROCCO',23x,1(1H*)/
     9 15x,54(1H*)////)
      close(5)
      RETURN
      END
C *********************************************************
C *********************************************************
      SUBROUTINE EBTCR
C *********************************************************
      OPEN(UNIT=8,file='EBTCR.RES',STATUS='UNKNOWN')
      write(8,1300)
1300  FORMAT('EXPERIMENTAL BENCHMARKS ON REACTIVITY TEMPERATURE '/
     1 49(1H*)/
     2 'COEFFICIENT '/
     3 11(1H*)//)
      close(8)
      RETURN
      END
C *********************************************************
      SUBROUTINE NORA
C *********************************************************
      CHARACTER*120 RECI
      REAL XKI(8),XKE(8),DT(8),C(8),nc(3)
      OPEN (UNIT=3,FILE='NORA.OUT',STATUS='old')
      OPEN (UNIT=5,file='NORA.RES',STATUS='UNKNOWN')
      OPEN (UNIT=6,FILE='NORA.t',STATUS='UNKNOWN')
      OPEN (UNIT=7,FILE='NORA.c',STATUS='UNKNOWN')
C
      DO I=1,4
   20  READ (3,800) RECI
       DO WHILE (RECI(53:63).NE.'k-effective')
        GO TO 20
       ENDDO
       READ(RECI(38:49),802) XKI(i)
       READ(RECI(66:77),802) XKE(i)
       READ (3,800) RECI
      ENDDO
C*
C      DO i=1,4
C       WRITE(*,*)XKI(i),XKE(i)
C      ENDDO
      read(6,*)(DT(i),i=1,4)
      read(7,1200)(nc(i),i=1,2)
      j=0
      do i=2,4,2
       j=j+1
       c(j)=(XKE(i)-XKE(i-1))/(XKE(i)*XKE(i-1)*(DT(i)-DT(i-1))*1.E-5)
      enddo
C
      write(5,950)
      do 60 i=1,4
       ik = mod(i,2)
       if(ik.eq.0) then
        write(5,901)DT(i),XKI(i),XKE(i)
       else
        write(5,900)DT(i),XKI(i),XKE(i)
       endif
       jl = int(i/2)
       if(i.ne.1) then
        goto 601
       else
        write(5,1100)nc(i),c(i)
        goto 60
       endif
 601   if(ik.eq.0) go to 60
       write(5,1100)nc(jl+1),c(jl+1)
  60  continue
C
      write(5,1250)
C*
 800  FORMAT(A120)
 802  FORMAT(E14.7)
 950  FORMAT(1X,24X,33(1H*)/
     1 '                         *        NORA EXPERIMENTS       *'/
     2 25X,33(1H*)///
     3'WIMS OUTPUT AND DISCREPANCIES BETWEEN CALCULATIONS '/
     4'AND MEASUREMENTS FOR THE RTC :'//
     6 17X,'TEMP. (°C)',9x,'K-INF',10x,'K-EFF'/
     7'  Vm/Vf     ',51x,'D-ALPHA (pcm/°C)'//)
 900  FORMAT(16x,F7.2,7x,E14.7,1x,E14.7)
 901  FORMAT(16x,F7.2,7x,E14.7,1x,E14.7//)
 1200 FORMAT(1x,F4.2,1x,F4.2,1x,F4.2,1x,F4.2)
 1100 FORMAT(3x,F4.2,55x,F14.8)
 1250 FORMAT(55(1H-)//)
      close(3)
      close(5)
      close(6)
      close(7)
      RETURN
      END
C *********************************************************
      SUBROUTINE KRITZ1
C *********************************************************
      CHARACTER*120 RECI
      REAL XKI(8),XKE(8),DT(8),C(8)
      OPEN (UNIT=3,FILE='KRITZ1.OUT',STATUS='old')
      OPEN(UNIT=5,file='KRITZ1.RES',STATUS='UNKNOWN')
      OPEN (UNIT=6,FILE='KRITZ1.t',STATUS='UNKNOWN')
C
      DO I=1,2
  20   READ (3,800) RECI
       DO WHILE (RECI(53:63).NE.'k-effective')
        GO TO 20
       ENDDO
       READ(RECI(38:49),802) XKI(i)
       READ(RECI(66:77),802) XKE(i)
       READ (3,800) RECI
      ENDDO
C*
C      do 30 i=1,2
C  30   WRITE(*,*)XKI(i),XKE(i)
      read(6,*)(dt(i),i=1,2)
      i=1
      IF ((XKE(i+1)*XKE(i)).NE.0)THEN
       c(i)=(XKE(i+1)-XKE(i))/(XKE(i+1)*XKE(i)*(DT(i+1)-DT(i))*1.E-5)
      ENDIF
      write(5,950)
      do 60 i=1,2
       IF(i.lt.2) THEN
c         write (5,1000)
         write(5,900)DT(i),XKI(i),XKE(i),C(i)
       ELSE
        write(5,900)DT(i),XKI(i),XKE(i)
       ENDIF
60    continue
      write(5,1250)
C*
 800  FORMAT(A120)
 802  FORMAT(E14.7)
 950  FORMAT(25x,33(1H*)/
     1 '                         *       KRITZ1  EXPERIMENTS     *'/
     2 25x,33(1H*)////
     3'WIMS OUTPUT AND DISCREPANCIES BETWEEN CALCULATIONS '/
     4'AND MEASUREMENTS FOR THE RTC :'//
     6 1x,'TEMP.(°C)',6x,'K-INF',12x,'K-EFF',6x,'D-ALPHA(pcm/°C)'//)
 900  FORMAT(F7.2,3x,E14.7,3x,E14.7,/41x,F14.7)
 1250 FORMAT(55(1H-)//)
 1300 FORMAT (/'  TEMPERATURES EN °C: '/)
      close(3)
      close(5)
      close(6)
      RETURN
      END
C *********************************************************
      SUBROUTINE KRITZ21
C *********************************************************
      CHARACTER*120 RECI
      REAL XKI(8),XKE(8),DT(8),C(8)
      OPEN (UNIT=3,FILE='KRITZ21.OUT',STATUS='old')
      OPEN(UNIT=5,file='KRITZ21.RES',STATUS='UNKNOWN')
      OPEN (UNIT=6,FILE='KRITZ21.t',STATUS='UNKNOWN')
C
      DO I=1,2
   20  READ (3,800) RECI
       DO WHILE (RECI(53:63).NE.'k-effective')
        GO TO 20
       ENDDO
       READ(RECI(38:49),802) XKI(i)
       READ(RECI(66:77),802) XKE(i)
       READ (3,800) RECI
      ENDDO
C*
C     do 30 i=1,2
C  30   WRITE(*,*)XKI(i),XKE(i)
      read(6,*)(dt(i),i=1,2)
      i=1
      IF ((XKE(i+1)*XKE(i)).NE.0)THEN
       c(i)=(XKE(i+1)-XKE(i))/(XKE(i+1)*XKE(i)*(DT(i+1)-DT(i))*1.E-5)
      ENDIF
      write(5,950)
      do 60 i=1,2
       IF(i.lt.2) THEN
        write(5,900)DT(i),XKI(i),XKE(i),C(i)
       ELSE
        write(5,900)DT(i),XKI(i),XKE(i)
       ENDIF
 60   continue
      write(5,1250)
C*
 800  FORMAT(A120)
 802  FORMAT(E14.7)
 950  FORMAT(25x,33(1H*)/
     1 '                         *       KRITZ21 EXPERIMENT      *'/
     2 25x,33(1H*)////
     3'WIMS OUTPUT AND DISCREPANCIES BETWEEN CALCULATIONS '/
     4'AND MEASUREMENTS FOR THE RTC : '//
     6 1x,'TEMP.(°C)',6x,'K-INF',12x,'K-EFF',6x,'D-ALPHA(pcm/°C)'//)
 900  FORMAT(F7.2,3x,E14.7,3x,E14.7,/41x,F14.7)
 1250 FORMAT(55(1H-)//)
      close(3)
      close(5)
      close(6)
      RETURN
      END
C *********************************************************
      SUBROUTINE KRITZ213
C *********************************************************
      CHARACTER*120 RECI
      REAL XKI(8),XKE(8),DT(8),C(8)
      OPEN (UNIT=3,FILE='KRITZ213.OUT',STATUS='old')
      OPEN(UNIT=5,file='KRITZ213.RES',STATUS='UNKNOWN')
      OPEN (UNIT=6,FILE='KRITZ213.t',STATUS='UNKNOWN')
C
      DO I=1,2
   20  READ (3,800) RECI
       DO WHILE (RECI(53:63).NE.'k-effective')
        GO TO 20
       ENDDO
       READ(RECI(38:49),802) XKI(i)
       READ(RECI(66:77),802) XKE(i)
       READ (3,800) RECI
      ENDDO
C*
C      do 30 i=1,2
C  30   WRITE(*,*)XKI(i),XKE(i)
      read(6,*)(dt(i),i=1,2)
      i=1
      IF ((XKE(i+1)*XKE(i)).NE.0.)THEN
       c(i)=(XKE(i+1)-XKE(i))/(XKE(i+1)*XKE(i)*(DT(i+1)-DT(i))*1.E-5)
      ENDIF
      write(5,950)
      do 60 i=1,2
       IF(i.lt.2) THEN
        write(5,900)DT(i),XKI(i),XKE(i),C(i)
       ELSE
        write(5,900)DT(i),XKI(i),XKE(i)
       ENDIF
 60   continue
      write(5,1250)
C*
 800  FORMAT(A120)
 802  FORMAT(E14.7)
 950  FORMAT(25x,33(1H*)/
     1 '                         *       KRITZ213 EXPERIMENT     *'/
     2 25x,33(1H*)////
     3'WIMS OUTPUT AND DISCREPANCIES BETWEEN CALCULATIONS '/
     4'AND MEASUREMENTS FOR THE RTC : '//
     6 1x,'TEMP.(°C)',6x,'K-INF',12x,'K-EFF',6x,'D-ALPHA(pcm/°C)'//)
 900  FORMAT(F7.2,3x,E14.7,3x,E14.7,/41x,F14.7)
 1250 FORMAT(55(1H-)//)
      close(3)
      close(5)
      close(6)
      RETURN
      END
C *********************************************************
C *********************************************************
      SUBROUTINE KRITZ219
C *********************************************************
      CHARACTER*120 RECI
      REAL XKI(8),XKE(8),DT(8),C(8)
      OPEN (UNIT=3,FILE='KRITZ219.OUT',STATUS='old')
      OPEN(UNIT=5,file='KRITZ219.RES',STATUS='UNKNOWN')
      OPEN (UNIT=6,FILE='KRITZ219.t',STATUS='UNKNOWN')
C
      DO I=1,2
   20  READ (3,800) RECI
       DO WHILE (RECI(53:63).NE.'k-effective')
        GO TO 20
       ENDDO
       READ(RECI(38:49),802) XKI(i)
       READ(RECI(66:77),802) XKE(i)
       READ (3,800) RECI
      ENDDO
C*
C       do 30 i=1,2
C  30   WRITE(*,*)XKI(i),XKE(i)
      read(6,*)(dt(i),i=1,2)
      i=1
      IF ((XKE(i+1)*XKE(i)).NE.0.)THEN
       c(i)=(XKE(i+1)-XKE(i))/(XKE(i+1)*XKE(i)*(DT(i+1)-DT(i))*1.E-5)
      ENDIF
      write(5,950)
      do 60 i=1,2
       IF(i.lt.2) THEN
c         write (5,1000)
        write(5,900)DT(i),XKI(i),XKE(i),C(i)
       ELSE
        write(5,900)DT(i),XKI(i),XKE(i)
       ENDIF
 60   continue
      write(5,1250)
C*
 800  FORMAT(A120)
 802  FORMAT(E14.7)
 950  FORMAT(25x,33(1H*)/
     1 '                         *       KRITZ219 EXPERIMENT     *'/
     2 25x,33(1H*)////
     3'WIMS OUTPUT AND DISCREPANCIES BETWEEN CALCULATIONS '/
     4'AND MEASUREMENTS FOR THE RTC : '/
     6 1x,'TEMP.(°C)',6x,'K-INF',12x,'K-EFF',6x,'D-ALPHA(pcm/°C)'//)
 900  FORMAT(F7.2,3x,E14.7,3x,E14.7,/41x,F14.7)
 1250 FORMAT(55(1H-)//)
      close(3)
      close(5)
      close(6)
      RETURN
      END
C *********************************************************
      SUBROUTINE R1100H
C *********************************************************
      CHARACTER*120 RECI
      REAL XKI(8),XKE(8),DT(8),C(8)
      OPEN (UNIT=3,FILE='R1100H.OUT',STATUS='old')
      OPEN(UNIT=5,file='R1100H.RES',STATUS='UNKNOWN')
      OPEN (UNIT=6,FILE='R1100H.t',STATUS='UNKNOWN')
C
      DO I=1,2
   20  READ (3,800) RECI
       DO WHILE (RECI(53:63).NE.'k-effective')
        GO TO 20
       ENDDO
       READ(RECI(38:49),802) XKI(i)
       READ(RECI(66:77),802) XKE(i)
       READ (3,800) RECI
      ENDDO
C*
C      do 30 i=1,2
C  30   WRITE(*,*)XKI(i),XKE(i)
      read(6,*)(dt(i),i=1,2)
      i=1
      IF ((XKE(i+1)*XKE(i)).NE.0)THEN
       c(i)=(XKE(i+1)-XKE(i))/(XKE(i+1)*XKE(i)*(DT(i+1)-DT(i))*1.E-5)
      ENDIF
      write(5,950)
      do 60 i=1,2
       IF(i.lt.2) THEN
        write(5,900)DT(i),XKI(i),XKE(i),C(i)
       ELSE
        write(5,900)DT(i),XKI(i),XKE(i)
       ENDIF
 60   continue
      write(5,1250)
C*
 800  FORMAT(A120)
 802  FORMAT(E14.7)
 950  FORMAT(25x,33(1H*)/
     1 '                         *       R1-100H EXPERIMENT      *'/
     2 25x,33(1H*)////
     3'WIMS OUTPUT AND DISCREPANCIES BETWEEN CALCULATIONS '/
     4'AND MEASUREMENTS FOR THE RTC :'//
     6 1x,'TEMP.(°C)',6x,'K-INF',12x,'K-EFF',6x,'D-ALPHA(pcm/°C)'//)
 900  FORMAT(F7.2,3x,E14.7,3x,E14.7,/41x,F14.7)
 1250 FORMAT(55(1H-)///)
      close(3)
      close(5)
      close(6)
      RETURN
      END
C *********************************************************
C *********************************************************
      SUBROUTINE VVER1
C *********************************************************
      CHARACTER*120 RECI
      REAL XKI(8),XKE(8),C2
      OPEN (UNIT=3,FILE='vver1.OUT',STATUS='old')
      OPEN(UNIT=5,file='VVER1.RES',STATUS='UNKNOWN')
      OPEN (UNIT=6,FILE='VVER.t',STATUS='UNKNOWN')
C
      DO I=1,2
   20  READ (3,800) RECI
       DO WHILE (RECI(53:63).NE.'k-effective')
        GO TO 20
       ENDDO
       READ(RECI(38:49),802) XKI(i)
       READ(RECI(66:77),802) XKE(i)
       READ (3,800) RECI
      ENDDO
C
      do i=1,2
       c2=((XKE(2)-XKE(1))/(XKE(2)*XKE(1)*109.0))*1.E+5
      enddo
      write(5,950)
      write(5,901)XKI(1),XKE(1)
      write(5,1100)c2
      write(5,904)XKI(2),XKE(2)
      write(5,1250)
C*
800   FORMAT(A120)
 802  FORMAT(E14.7)
 950  FORMAT(1X,24X,33(1H*)/
     1 '                         *        VVER1 EXPERIMENTS      *'/
     2 25X,33(1H*)///
     3'WIMS OUTPUT AND DISCREPANCIES BETWEEN CALCULATIONS '/
     4'AND MEASUREMENTS FOR THE RTC :'//
     6 1X,'TEMP. (°C)',10x,'K-INF',10x,'K-EFF'/
     7 51x,'D-ALPHA (pcm/°C)'//)
 900  FORMAT(1x,F7.2,7x,E14.7,1x,E14.7)
 901  FORMAT(1x,'21.0',7x,E14.7,1x,E14.7)
 902  FORMAT(1x,'80.0',7x,E14.7,1x,E14.7)
 904  FORMAT(1x,'130.0',6x,E14.7,1x,E14.7)
 1200 FORMAT(1x,F4.2,1x,F4.2,1x,F4.2,1x,F4.2)
 1100 FORMAT(51x,F14.8)
 1250 FORMAT(55(1H-)//)
      close(3)
      close(5)
      close(6)
      RETURN
      END
C *********************************************************
C *********************************************************
      SUBROUTINE VVER2
C *********************************************************
      CHARACTER*120 RECI
      REAL XKI(8),XKE(8),C2
      OPEN (UNIT=3,FILE='vver2.OUT',STATUS='old')
      OPEN(UNIT=5,file='VVER2.RES',STATUS='UNKNOWN')
      OPEN (UNIT=6,FILE='VVER.t',STATUS='UNKNOWN')
C
      DO I=1,2
   20  READ (3,800) RECI
       DO WHILE (RECI(53:63).NE.'k-effective')
        GO TO 20
       ENDDO
       READ(RECI(38:49),802) XKI(i)
       READ(RECI(66:77),802) XKE(i)
       READ (3,800) RECI
      ENDDO
C
      do i=1,2
       c2=((XKE(2)-XKE(1))/(XKE(2)*XKE(1)*109.0))*1.E+5
      enddo
      write(5,950)
      write(5,901)XKI(1),XKE(1)
      write(5,1100)c2
      write(5,904)XKI(2),XKE(2)
      write(5,1250)
C*
 800  FORMAT(A120)
 802  FORMAT(E14.7)
 950  FORMAT(1X,24X,33(1H*)/
     1 '                         *        VVER2 EXPERIMENTS      *'/
     2 25X,33(1H*)///
     3'WIMS OUTPUT AND DISCREPANCIES BETWEEN CALCULATIONS '/
     4'AND MEASUREMENTS FOR THE RTC :'//
     6 1X,'TEMP. (°C)',10x,'K-INF',10x,'K-EFF'/
     7 51x,'D-ALPHA (pcm/°C)'//)
 900  FORMAT(1x,F7.2,7x,E14.7,1x,E14.7)
 901  FORMAT(1x,'21.0',7x,E14.7,1x,E14.7)
 902  FORMAT(1x,'80.0',7x,E14.7,1x,E14.7)
 904  FORMAT(1x,'130.0',6x,E14.7,1x,E14.7)
 1200 FORMAT(1x,F4.2,1x,F4.2,1x,F4.2,1x,F4.2)
 1100 FORMAT(51x,F14.8)
 1250 FORMAT(55(1H-)//)
      close(3)
      close(5)
      close(6)
      RETURN
      END
C *********************************************************
C *********************************************************
      SUBROUTINE VVER3
C *********************************************************
      CHARACTER*120 RECI
      REAL XKI(8),XKE(8),C2
      OPEN (UNIT=3,FILE='vver3.OUT',STATUS='old')
      OPEN(UNIT=5,file='VVER3.RES',STATUS='UNKNOWN')
      OPEN (UNIT=6,FILE='VVER.t',STATUS='UNKNOWN')
C
      DO I=1,2
   20  READ (3,800) RECI
       DO WHILE (RECI(53:63).NE.'k-effective')
        GO TO 20
       ENDDO
       READ(RECI(38:49),802) XKI(i)
       READ(RECI(66:77),802) XKE(i)
       READ (3,800) RECI
      ENDDO
C
      do i=1,2
       c2=((XKE(2)-XKE(1))/(XKE(2)*XKE(1)*109.0))*1.E+5
      enddo
      write(5,950)
      write(5,901)XKI(1),XKE(1)
      write(5,1100)c2
      write(5,904)XKI(2),XKE(2)
      write(5,1250)
C*
 800  FORMAT(A120)
 802  FORMAT(E14.7)
 950  FORMAT(1X,24X,33(1H*)/
     1 '                         *        VVER3 EXPERIMENTS      *'/
     2 25X,33(1H*)///
     3'WIMS OUTPUT AND DISCREPANCIES BETWEEN CALCULATIONS '/
     4'AND MEASUREMENTS FOR THE RTC :'//
     6 1X,'TEMP. (°C)',10x,'K-INF',10x,'K-EFF'/
     7 51x,'D-ALPHA (pcm/°C)'//)
 900  FORMAT(1x,F7.2,7x,E14.7,1x,E14.7)
 901  FORMAT(1x,'21.0',7x,E14.7,1x,E14.7)
 902  FORMAT(1x,'80.0',7x,E14.7,1x,E14.7/)
 904  FORMAT(1x,'130.0',6x,E14.7,1x,E14.7)
 1200 FORMAT(1x,F4.2,1x,F4.2,1x,F4.2,1x,F4.2)
 1100 FORMAT(51x,F14.8)
 1250 FORMAT(55(1H-)//)
      close(3)
      close(5)
      close(6)
      RETURN
      END
C *********************************************************
C *********************************************************
      SUBROUTINE VVER4
C *********************************************************
      CHARACTER*120 RECI
      REAL XKI(8),XKE(8),C2
      OPEN (UNIT=3,FILE='vver4.OUT',STATUS='old')
      OPEN(UNIT=5,file='VVER4.RES',STATUS='UNKNOWN')
      OPEN (UNIT=6,FILE='VVER.t',STATUS='UNKNOWN')
C
      DO I=1,2
   20  READ (3,800) RECI
       DO WHILE (RECI(53:63).NE.'k-effective')
        GO TO 20
       ENDDO
       READ(RECI(38:49),802) XKI(i)
       READ(RECI(66:77),802) XKE(i)
       READ (3,800) RECI
      ENDDO
      do i=1,2
       c2=((XKE(2)-XKE(1))/(XKE(2)*XKE(1)*109.0))*1.E+5
      enddo
      write(5,950)
      write(5,901)XKI(1),XKE(1)
      write(5,1100)c2
      write(5,904)XKI(2),XKE(2)
      write(5,1250)
C*
 800  FORMAT(A120)
 802  FORMAT(E14.7)
 950  FORMAT(1X,24X,33(1H*)/
     1 '                         *        VVER4 EXPERIMENTS      *'/
     2 25X,33(1H*)///
     3'WIMS OUTPUT AND DISCREPANCIES BETWEEN CALCULATIONS '/
     4'AND MEASUREMENTS FOR THE RTC :'//
     6 1X,'TEMP. (°C)',10x,'K-INF',10x,'K-EFF'/
     7 51x,'D-ALPHA (pcm/°C)'//)
 900  FORMAT(1x,F7.2,7x,E14.7,1x,E14.7)
 901  FORMAT(1x,'21.0',7x,E14.7,1x,E14.7)
 902  FORMAT(1x,'80.0',7x,E14.7,1x,E14.7)
 904  FORMAT(1x,'130.0',6x,E14.7,1x,E14.7)
 1200 FORMAT(1x,F4.2,1x,F4.2,1x,F4.2,1x,F4.2)
 1100 FORMAT(51x,F14.8)
 1250 FORMAT(55(1H-)//)
      close(3)
      close(5)
      close(6)
      RETURN
      END
