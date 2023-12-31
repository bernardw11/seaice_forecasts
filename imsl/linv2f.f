C   IMSL ROUTINE NAME   - LINV2F
C
C-----------------------------------------------------------------------
C
C   COMPUTER            - ELXSI/SINGLE
C
C   LATEST REVISION     - JUNE 1, 1982
C
C   PURPOSE             - INVERSION OF A MATRIX - FULL STORAGE MODE -
C                           HIGH ACCURACY SOLUTION
C
C   USAGE               - CALL LINV2F (A,N,IA,AINV,IDGT,WKAREA,IER)
C
C   ARGUMENTS    A      - INPUT MATRIX OF DIMENSION N BY N CONTAINING
C                           THE MATRIX TO BE INVERTED.
C                N      - ORDER OF A. (INPUT)
C                IA     - ROW DIMENSION OF MATRICES A AND AINV EXACTLY
C                           AS SPECIFIED IN THE DIMENSION STATEMENT IN
C                           THE CALLING PROGRAM. (INPUT)
C                AINV   - OUTPUT MATRIX OF DIMENSION N BY N CONTAINING
C                           THE INVERSE OF A. A AND AINV MUST OCCUPY
C                           SEPARATE CORE LOCATIONS.
C                IDGT   - INPUT OPTION.
C                         IF IDGT IS GREATER THAN 0, THE ELEMENTS OF A
C                           ARE ASSUMED TO BE CORRECT TO IDGT DECIMAL
C                           DIGITS AND THE ROUTINE PERFORMS AN ACCURACY
C                           TEST.
C                         IF IDGT EQUALS 0, THE ACCURACY TEST IS
C                           BYPASSED.
C                         ON OUTPUT, IDGT CONTAINS THE APPROXIMATE
C                           NUMBER OF DIGITS IN THE ANSWER WHICH
C                           WERE UNCHANGED AFTER IMPROVEMENT.
C                WKAREA - WORK AREA OF DIMENSION GREATER THAN OR EQUAL
C                           TO N**2+3N.
C                IER    - ERROR PARAMETER. (OUTPUT)
C                         TERMINAL ERROR
C                           IER=129 INDICATES THAT THE MATRIX IS
C                             ALGORITHMICALLY SINGULAR. (SEE THE
C                             CHAPTER L PRELUDE).
C                           IER=131 INDICATES THAT THE MATRIX IS TOO
C                             ILL-CONDITIONED FOR ITERATIVE IMPROVEMENT
C                             TO BE EFFECTIVE.
C                         WARNING ERROR
C                           IER=34 INDICATES THAT THE ACCURACY TEST
C                             FAILED. THE COMPUTED SOLUTION MAY BE IN
C                             ERROR BY MORE THAN CAN BE ACCOUNTED FOR
C                             BY THE UNCERTAINTY OF THE DATA. THIS
C                             WARNING CAN BE PRODUCED ONLY IF IDGT IS
C                             GREATER THAN 0 ON INPUT. SEE CHAPTER L
C                             PRELUDE FOR FURTHER DISCUSSION.
C
C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32
C                       - SINGLE/H36,H48,H60
C
C   REQD. IMSL ROUTINES - SINGLE/LEQT2F,LUDATN,LUELMN,LUREFN,UERTST,
C                           UGETIO
C                       - DOUBLE/LEQT2F,LUDATN,LUELMN,LUREFN,UERTST,
C                           UGETIO,VXADD,VXMUL,VXSTO
C
C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND
C                           CONVENTIONS IS AVAILABLE IN THE MANUAL
C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP
C
C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.
C
C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN
C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,
C                           EXPRESSED OR IMPLIED, IS APPLICABLE.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LINV2F (A,N,IA,AINV,IDGT,WKAREA,IER)
C
      REAL               A(IA,N),AINV(IA,N),WKAREA(1),ZERO,ONE
      DATA               ONE/1.0/,ZERO/0.0/
C                                  FIRST EXECUTABLE STATEMENT
C                                  INITIALIZE IER
      IER=0
C                                  SET AINV TO THE N X N
C                                  IDENTITY MATRIX
      DO 10 I = 1,N
         DO 5 J = 1,N
            AINV(I,J) = ZERO
    5    CONTINUE
         AINV(I,I) = ONE
   10 CONTINUE
C                                  COMPUTE THE INVERSE OF A
      CALL LEQT2F (A,N,N,IA,AINV,IDGT,WKAREA,IER)
      IF (IER.EQ.0) GO TO 9005
 9000 CONTINUE
      CALL UERTST (IER,'LINV2F')
 9005 RETURN
      END
