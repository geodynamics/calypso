!>@file   primefac.f90
!!@brief  module primefac
!!
!!@author  MADISON ACADEMIC COMPUTING CENTER
!!@date    FORTRAN 77 VERSION 88.09
!!@n       Modified in March, 2013
!
!> @brief  DECOMPOSE A NUMBER INTO ITS PRIME FACTORS.
!!
!!@verbatim
!!-----------------------------------------------------------------------
!! 
!!  PRMFAC
!!
!!  DECOMPOSE A NUMBER INTO ITS PRIME FACTORS.
!!
!!  CODED AT MADISON ACADEMIC COMPUTING CENTER,
!!  UNIVERSITY OF WISCONSIN, MADISON
!!
!!  FORTRAN 77 VERSION 88.09
!!
!!-----------------------------------------------------------------------
!!
!!      module primefac
!!
!!      SUBROUTINE prmfac (NUMBER, maxprim, nprm, iprm, iexp)
!!      subroutine primetest(NUMBER, iflag)
!!      subroutine countprimes(number_end, num_prime)
!!      subroutine setprimes(number_end, num_prime, i_prime_list)
!!
!!--------CALLING SEQUENCE
!!
!!  CALL PRMFAC (NUMBER, maxprim, NPRM, IPRM, IEXP)
!!  NUMBER - INTEGER CONSTANT OR VARIABLE, NUMBER TO BE DECOMPOSED
!!           INTO PRIME FACTORS.  NUMBER .GE. 2.
!!
!!  MAXPRIM - maximum number for .
!!
!!  NPRM   - INTEGER VARIABLE, WILL CONTAIN THE NO. OF DISTINCT PRIME
!!           FACTORS OF THE NUMBER.
!!
!!  IPRM   - INTEGER ARRAY OF SIZE AT LEAST 9, WILL CONTAIN THE PRIME
!!           FACTORS OF THE NUMBER.
!!
!!  IEXP   - INTEGER ARRAY OF SIZE AT LEAST 9, WILL CONTAIN THE
!!           EXPONENTS OF THE CORRESPONDING PRIME FACTORS.
!!
!!--------NOTES
!!
!!  (1)  UPON RETURN FROM PRMFAC,
!!       NUMBER = IPRM(1)**IEXP(1) * IPRM(2)**IEXP(2) * ... *
!!               IPRM(NPRM)**IEXP(NPRM)
!!
!!  (2)  A NUMBER REPRESENTED BY A (SINGLE-PRECISION) INTEGER
!!       VALUE ON THE VMS VAX CLUSTER CAN HAVE AT MOST 9 DISTINCT
!!       PRIME FACTORS.  ON MACHINES WHERE THE MAXIMUM INTEGER IS
!!       LARGER THAN 2**31 - 1, IPRM AND IEXP WOULD, IN GENERAL,
!!       HAVE TO BE DIMENSIONED LARGER SINCE LARGER NUMBERS MAY
!!       HAVE MORE THAN 9 DISTINCT PRIME FACTORS.
!!
!!---------DATA TO OBTAIN TRIAL DIVISORS 2, 3, 5, 7 AND ALL
!!         HIGHER NUMBERS NOT DIVISIBLE BY 2, 3, 5, 7.
!!@endverbatim
!
      module primefac
!
      use m_precision
!
      IMPLICIT NONE

      INTEGER(kind = kint), parameter, private  :: base(52) = (/        &
      211, 209, 199, 197, 193, 191, 187, 181, 179, 173, 169, 167, 163,  &
      157, 151, 149, 143, 139, 137, 131, 127, 121, 113, 109, 107, 103,  &
      101,  97,  89,  83,  79,  73,  71,  67,  61,  59,  53,  47,  43,  &
       41,  37,  31,  29,  23,  19,  17,  13,  11,   7,   5,   3,   2/)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      SUBROUTINE prmfac (NUMBER, maxprim, nprm, iprm, iexp)

!--------PARAMETERS

      INTEGER(kind = kint), INTENT(IN)   :: NUMBER, maxprim
      INTEGER(kind = kint), INTENT(OUT)  :: nprm
      INTEGER(kind = kint), INTENT(OUT)  :: iprm(maxprim)
      INTEGER(kind = kint), INTENT(OUT)  :: iexp(maxprim)

!--------LOCAL VARIABLES
      INTEGER(kind = kint)            :: div, indx, j, n
      INTEGER(kind = kint)            :: offset, olddiv, quo, rem

!--------CHECK NUMBER.  MUST BE .GE. 2
      IF (NUMBER < 2) GO TO 140

!--------INITIALIZATIONS.
      j = 0
      n = NUMBER
      olddiv = 0
      offset = 0
      indx = 53

!--------GET NEXT TRIAL DIVISOR.
      DO
        indx = indx - 1
        IF (indx <= 0) THEN
          indx = 48
          offset = offset + 210
        END IF
        div = offset + base(indx)

!--------TEST TRIAL DIVISOR.
        DO
          quo = n / div
          rem = n - quo*div
          IF (rem /= 0) EXIT

!--------FACTOR FOUND, ZERO REMAINDER.
          n = quo
          IF (div <= olddiv) THEN

!--------MULTIPLE FACTOR.
            iexp(j) = iexp(j) + 1
            CYCLE
          END IF

!--------NEW FACTOR.
          j = j + 1
          iprm(j) = div
          iexp(j) = 1
          olddiv = div
        END DO

!--------NOT A FACTOR, POSITIVE REMAINDER.  CHECK DIVISOR SIZE.
        IF (div >= quo) EXIT
      END DO

!--------FINISHED, WHAT ISN'T FACTORED IS A PRIME (OR 1).
      IF (n > 1) THEN
        j = j + 1
        iexp(j) = 1
        iprm(j) = n
      END IF
      nprm = j

!--------NORMAL EXIT
      RETURN

!--------ERROR, ISSUE MESSAGE AND TAKE ERROR EXIT.
  140 WRITE (*,150) NUMBER
  150 FORMAT (' *** ERROR IN PRMFAC, NUMBER =', i12, ',                 &
     &   NUMBER MUST BE >= 2')

!--------ERROR EXIT
      RETURN

!--------END OF PRMFAC
      END SUBROUTINE prmfac
!
!-----------------------------------------------------------------------
!
      subroutine primetest(NUMBER, iflag)

!--------PARAMETERS

      INTEGER(kind = kint), INTENT(IN)   :: NUMBER
      INTEGER(kind = kint), INTENT(OUT)  :: iflag

!--------LOCAL VARIABLES
      INTEGER(kind = kint) :: iexp, iprm
      INTEGER(kind = kint)            :: div, indx, j, n
      INTEGER(kind = kint)            :: offset, olddiv, quo, rem

!--------CHECK NUMBER.  MUST BE .GE. 2
      IF (NUMBER < 2) GO TO 140

!--------INITIALIZATIONS.
      j = 0
      n = NUMBER
      olddiv = 0
      offset = 0
      indx = 53

!--------GET NEXT TRIAL DIVISOR.
      DO
        indx = indx - 1
        IF (indx <= 0) THEN
          indx = 48
          offset = offset + 210
        END IF
        div = offset + base(indx)
!
!--------TEST TRIAL DIVISOR.
        DO
          quo = n / div
          rem = n - quo*div
          IF (rem /= 0) EXIT

!--------FACTOR FOUND, ZERO REMAINDER.
          n = quo
          IF (div <= olddiv) THEN

!--------MULTIPLE FACTOR.
            iexp = iexp + 1
            if(iexp .gt. 1) exit
            CYCLE
          END IF

!--------NEW FACTOR.
          j = j + 1
          if(j .gt. 1) exit
          iprm = div
          iexp = 1
          olddiv = div
        END DO

!--------NOT A FACTOR, POSITIVE REMAINDER.  CHECK DIVISOR SIZE.
        IF (div >= quo) EXIT
      END DO

!--------FINISHED, WHAT ISN'T FACTORED IS A PRIME (OR 1).
      IF (n > 1) THEN
        j = j + 1
        iexp = 1
!        iprm = n
      END IF
      iflag = 0
      if(j.eq.1 .and. iexp.eq.1) iflag = 1
!--------NORMAL EXIT
      RETURN

!--------ERROR, ISSUE MESSAGE AND TAKE ERROR EXIT.
  140 WRITE (*,150) NUMBER
  150 FORMAT (' *** ERROR IN PRMFAC, NUMBER =', i12, ',                 &
     &      NUMBER MUST BE >= 2')

!--------ERROR EXIT
      RETURN

!--------END OF PRMFAC
      END SUBROUTINE primetest
!
!-----------------------------------------------------------------------
!
      subroutine countprimes(number_end, num_prime)
!
      INTEGER(kind = kint), intent(in)  :: number_end
      INTEGER(kind = kint), intent(inout)  :: num_prime
      INTEGER(kind = kint)  :: i, number, iflag
!
!
      num_prime = 0
      do i = 2, number_end
        number = i
        call primetest(number, iflag)
        num_prime = num_prime + iflag
      end do
!
      end subroutine countprimes
!
!-----------------------------------------------------------------------
!
      subroutine setprimes(number_end, num_prime, i_prime_list)
!
      INTEGER(kind = kint), intent(in)  :: number_end
      INTEGER(kind = kint), intent(in)  :: num_prime
      INTEGER(kind = kint), intent(inout)  :: i_prime_list(num_prime)
      INTEGER(kind = kint)  :: i, number, iflag, icou
!
!
      icou = 0
      do i = 2, number_end
        number = i
        call primetest(number, iflag)
        if(iflag .eq. 1) then
          icou = icou + 1
          i_prime_list(icou) = i
        end if
      end do
!
      end subroutine setprimes
!
!-----------------------------------------------------------------------
!
      end module primefac
