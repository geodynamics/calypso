!C***BEGIN PROLOGUE  I8SORT_W_INT
!C***PURPOSE  Sort an array and optionally make the same interchanges in
!C            an auxiliary array.  The array may be sorted in increasing
!C            or decreasing order.  A slightly modified QUICKSORT
!C            algorithm is used.
!C***LIBRARY   SLATEC
!C***CATEGORY  N6A2A
!C***TYPE      INTEGER (SSORT-S, DSORT-D, ISORT-I)
!C***KEYWORDS  SINGLETON QUICKSORT, SORT, SORTING
!C***AUTHOR  Jones, R. E., (SNLA)
!C           Kahaner, D. K., (NBS)
!C           Wisniewski, J. A., (SNLA)
!C***DESCRIPTION
!C
!C   I8SORT_W_INT sorts array I8X and optionally makes the same interchanges in
!C   array IY.  The array I8X may be sorted in increasing order or
!C   decreasing order.  A slightly modified quicksort algorithm is used.
!C
!C   Description of Parameters
!C      I8X - 8-byte integer array of values to be sorted
!C      IY - integer array to be (optionally) carried along
!C      N  - number of values in integer array I8X to be sorted
!C      KFLAG - control parameter
!C            =  2  means sort I8X in increasing order and carry IY along.
!C            =  1  means sort I8X in increasing order (ignoring IY)
!C            = -1  means sort I8X in decreasing order (ignoring IY)
!C            = -2  means sort I8X in decreasing order and carry IY along.
!C
!C***REFERENCES  R. C. Singleton, Algorithm 347, An efficient algorithm
!C                 for sorting with minimal storage, Communications of
!C                 the ACM, 12, 3 (1969), pp. 185-187.
!C***ROUTINES CALLED  XERMSG
!C***REVISION HISTORY  (YYMMDD)
!C   761118  DATE WRITTEN
!C   810801  Modified by David K. Kahaner.
!C   890531  Changed all specific intrinsics to generic.  (WRB)
!C   890831  Modified array declarations.  (WRB)
!C   891009  Removed unreferenced statement labels.  (WRB)
!C   891009  REVISION DATE from Version 3.2
!C   891214  Prologue converted to Version 4.0 format.  (BAB)
!C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!C   901012  Declared all variables; changed X,Y to I8X,IY. (M. McClain)
!C   920501  Reformatted the REFERENCES section.  (DWL, WRB)
!C   920519  Clarified error messages.  (DWL)
!C   920801  Declarations section rebuilt and code restructured to use
!C           IF-THEN-ELSE-ENDIF.  (RWC, WRB)
!C***END PROLOGUE  I8SORT_W_INT
!
      module i8sort_with_int
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      SUBROUTINE I8SORT_W_INT (I8X, IY, N, KFLAG)
!C     .. Scalar Arguments ..
      INTEGER(kind = kint) :: KFLAG, N
!C     .. Array Arguments ..
      INTEGER(kind = kint_gl) :: I8X(N)
      INTEGER(kind = kint) :: IY(N)
!C     .. Local Scalars ..
      REAL R
      INTEGER(kind = kint_gl) :: T8, TT8
      INTEGER(kind = kint) :: I, IJ, J, K, KK, L, M, NN, TY, TTY
!C     .. Local Arrays ..
      INTEGER(kind = kint) IL(21), IU(21)
!C     .. External Subroutines ..
!      EXTERNAL XERMSG
!C     .. Intrinsic Functions ..
      INTRINSIC ABS, INT
!C***FIRST EXECUTABLE STATEMENT  I8SORT_W_INT
      NN = N
      IF (NN .LT. 1) THEN
!         CALL XERMSG ('SLATEC', 'I8SORT_W_INT',
!     +      'The number of values to be sorted is not positive.', 1, 1)
         write(*,*) 'The number of values to be sorted',                &
     &             ' is not positive.'
         RETURN
      ENDIF
!C
      KK = ABS(KFLAG)
      IF (KK.NE.1 .AND. KK.NE.2) THEN
!         CALL XERMSG ('SLATEC', 'I8SORT_W_INT',
!     +      'The sort control parameter, K, is not 2, 1, -1, or -2.', 2,
!     +      1)
         write(*,*) 'The sort control parameter, K,',                   &
     &             'is not 2, 1, -1, or -2.'
         RETURN
      ENDIF
!C
!C     Alter array I8X to get decreasing order if needed
!C
      IF (KFLAG .LE. -1) THEN
         DO 10 I=1,NN
            I8X(I) = -I8X(I)
   10    CONTINUE
      ENDIF
!C
      IF (KK .EQ. 2) GO TO 100
!C
!C     Sort I8X only
!C
      M = 1
      I = 1
      J = NN
      R = 0.375E0
!C
   20 IF (I .EQ. J) GO TO 60
      IF (R .LE. 0.5898437E0) THEN
         R = R+3.90625E-2
      ELSE
         R = R-0.21875E0
      ENDIF
!C
   30 K = I
!C
!C     Select a central element of the array and save it in location T8
!C
      IJ = I + INT((J-I)*R)
      T8 = I8X(IJ)
!C
!C     If first element of array is greater than T8, interchange with T8
!C
      IF (I8X(I) .GT. T8) THEN
         I8X(IJ) = I8X(I)
         I8X(I) = T8
         T8 = I8X(IJ)
      ENDIF
      L = J
!C
!C     If last element of array is less than than T8, interchange with T8
!C
      IF (I8X(J) .LT. T8) THEN
         I8X(IJ) = I8X(J)
         I8X(J) = T8
         T8 = I8X(IJ)
!C
!C        If first element of array is greater than T8, interchange with T8
!C
         IF (I8X(I) .GT. T8) THEN
            I8X(IJ) = I8X(I)
            I8X(I) = T8
            T8 = I8X(IJ)
         ENDIF
      ENDIF
!C
!C     Find an element in the second half of the array which is smaller
!C     than T8
!C
   40 L = L-1
      IF (I8X(L) .GT. T8) GO TO 40
!C
!C     Find an element in the first half of the array which is greater
!C     than T8
!C
   50 K = K+1
      IF (I8X(K) .LT. T8) GO TO 50
!C
!C     Interchange these elements
!C
      IF (K .LE. L) THEN
         TT8 = I8X(L)
         I8X(L) = I8X(K)
         I8X(K) = TT8
         GO TO 40
      ENDIF
!C
!C     Save upper and lower subscripts of the array yet to be sorted
!C
      IF (L-I .GT. J-K) THEN
         IL(M) = I
         IU(M) = L
         I = K
         M = M+1
      ELSE
         IL(M) = K
         IU(M) = J
         J = L
         M = M+1
      ENDIF
      GO TO 70
!C
!C     Begin again on another portion of the unsorted array
!C
   60 M = M-1
      IF (M .EQ. 0) GO TO 190
      I = IL(M)
      J = IU(M)
!C
   70 IF (J-I .GE. 1) GO TO 30
      IF (I .EQ. 1) GO TO 20
      I = I-1
!C
   80 I = I+1
      IF (I .EQ. J) GO TO 60
      T8 = I8X(I+1)
      IF (I8X(I) .LE. T8) GO TO 80
      K = I
!C
   90 I8X(K+1) = I8X(K)
      K = K-1
      IF (T8 .LT. I8X(K)) GO TO 90
      I8X(K+1) = T8
      GO TO 80
!C
!C     Sort I8X and carry IY along
!C
  100 M = 1
      I = 1
      J = NN
      R = 0.375E0
!C
  110 IF (I .EQ. J) GO TO 150
      IF (R .LE. 0.5898437E0) THEN
         R = R+3.90625E-2
      ELSE
         R = R-0.21875E0
      ENDIF
!C
  120 K = I
!C
!C     Select a central element of the array and save it in location T8
!C
      IJ = I + INT((J-I)*R)
      T8 =  I8X(IJ)
      TY = IY(IJ)
!C
!C     If first element of array is greater than T8, interchange with T8
!C
      IF (I8X(I) .GT. T8) THEN
         I8X(IJ) = I8X(I)
         I8X(I) = T8
         T8 = I8X(IJ)
         IY(IJ) = IY(I)
         IY(I) = TY
         TY = IY(IJ)
      ENDIF
      L = J
!C
!C     If last element of array is less than T8, interchange with T8
!C
      IF (I8X(J) .LT. T8) THEN
         I8X(IJ) = I8X(J)
         I8X(J) = T8
         T8 = I8X(IJ)
         IY(IJ) = IY(J)
         IY(J) = TY
         TY = IY(IJ)
!C
!C        If first element of array is greater than T8, interchange with T8
!C
         IF (I8X(I) .GT. T8) THEN
            I8X(IJ) = I8X(I)
            I8X(I) = T8
            T8 = I8X(IJ)
            IY(IJ) = IY(I)
            IY(I) = TY
            TY = IY(IJ)
         ENDIF
      ENDIF
!C
!C     Find an element in the second half of the array which is smaller
!C     than T8
!C
  130 L = L-1
      IF (I8X(L) .GT. T8) GO TO 130
!C
!C     Find an element in the first half of the array which is greater
!C     than T8
!C
  140 K = K+1
      IF (I8X(K) .LT. T8) GO TO 140
!C
!C     Interchange these elements
!C
      IF (K .LE. L) THEN
         TT8 = I8X(L)
         I8X(L) = I8X(K)
         I8X(K) = TT8
         TTY = IY(L)
         IY(L) = IY(K)
         IY(K) = TTY
         GO TO 130
      ENDIF
!C
!C     Save upper and lower subscripts of the array yet to be sorted
!C
      IF (L-I .GT. J-K) THEN
         IL(M) = I
         IU(M) = L
         I = K
         M = M+1
      ELSE
         IL(M) = K
         IU(M) = J
         J = L
         M = M+1
      ENDIF
      GO TO 160
!C
!C     Begin again on another portion of the unsorted array
!C
  150 M = M-1
      IF (M .EQ. 0) GO TO 190
      I = IL(M)
      J = IU(M)
!C
  160 IF (J-I .GE. 1) GO TO 120
      IF (I .EQ. 1) GO TO 110
      I = I-1
!C
  170 I = I+1
      IF (I .EQ. J) GO TO 150
      T8 = I8X(I+1)
      TY = IY(I+1)
      IF (I8X(I) .LE. T8) GO TO 170
      K = I
!C
  180 I8X(K+1) = I8X(K)
      IY(K+1) = IY(K)
      K = K-1
      IF (T8 .LT. I8X(K)) GO TO 180
      I8X(K+1) = T8
      IY(K+1) = TY
      GO TO 170
!C
!C     Clean up
!C
  190 IF (KFLAG .LE. -1) THEN
         DO 200 I=1,NN
            I8X(I) = -I8X(I)
  200    CONTINUE
      ENDIF
      RETURN
      END SUBROUTINE I8SORT_W_INT
!
!-----------------------------------------------------------------------
!
      end module i8sort_with_int
