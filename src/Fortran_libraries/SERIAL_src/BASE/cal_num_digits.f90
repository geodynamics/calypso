!>@file   cal_num_digits.f90
!!@brief  module cal_num_digits
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief  subtract constant from field data
!!@n      need $omp parallel to use these routines
!!
!!@verbatim
!!      subroutine cal_num_digit_int(x_input, i_digit, i_exp)
!!      subroutine cal_num_digit_real(x_input, r_digit, i_exp)
!!@endverbatim
!!
!!@n @param  x_input  Input number
!!@n @param  r_digit  first integer digit for input number
!!@n @param  i_digit  real digit for input number
!!@n @param  i_exp    exponent of 10
!
      module cal_num_digits
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_num_digit_int(x_input, i_digit, i_exp)
!
      real(kind = kreal), intent(in) :: x_input
      integer(kind = kint), intent(inout) :: i_exp, i_digit
!
!
      i_exp = int( aint( log10(x_input) ), KIND(i_exp))
!
      if(x_input .ge. one) then
        i_digit = nint(x_input * ten**(-i_exp))
      else
        i_digit = nint(x_input * ten**(-i_exp+1))
        if(i_digit .eq. iten) then
          i_digit = 1
        else
          i_exp = i_exp-1
        end if
      end if
!
      end subroutine cal_num_digit_int
!
!  ---------------------------------------------------------------------
!
      subroutine cal_num_digit_real(x_input, r_digit, i_exp)
!
      real(kind = kreal), intent(in) :: x_input
      real(kind = kreal), intent(inout) :: r_digit
      integer(kind = kint), intent(inout) :: i_exp
!
!
      i_exp = int( aint( log10(x_input) ),KIND(i_exp))
!
      if(x_input .ge. one) then
        r_digit = x_input * ten**(-i_exp)
      else
        r_digit = x_input * ten**(-i_exp+1)
        if(r_digit .ge. ten) then
          r_digit = one
        else
          i_exp = i_exp-1
        end if
      end if
!
      end subroutine cal_num_digit_real
!
!  ---------------------------------------------------------------------
!
      end module cal_num_digits
