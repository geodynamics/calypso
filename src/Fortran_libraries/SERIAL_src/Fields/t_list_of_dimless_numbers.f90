!>@file   t_list_of_dimless_numbers.f90
!!@brief  module t_list_of_dimless_numbers
!!
!!@author H. Matsui
!!@date Programmed in 2005
!!@date Modified in Jan., 2007
!
!>@brief  dimensionless number list
!!
!!@verbatim
!!      subroutine alloc_dimless_list(dimless_list)
!!      subroutine dealloc_dimless_list(dimless_list)
!!      subroutine copy_dimless_from_ctl(coef_ctl, dimless_list)
!!      subroutine check_dimless_numbers(dimless_list)
!!        type(ctl_array_cr), intent(in) :: coef_ctl
!!        type(list_of_dimless), intent(inout) :: dimless_list
!!@endverbatim
!!
      module t_list_of_dimless_numbers
!
      use m_precision
!
      implicit  none
!
!
!>      Structure of dimensionless numbers
      type list_of_dimless
!>        Number of parameters
        integer(kind=kint) :: num
!>        List of dimensionless number's name
        character(len=kchara), allocatable :: name(:)
!>        List of values
        real (kind = kreal), allocatable :: value(:)
      end type list_of_dimless
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_dimless_list(dimless_list)
!
      type(list_of_dimless), intent(inout) :: dimless_list
!
!
      allocate(dimless_list%name(dimless_list%num))
      allocate(dimless_list%value(dimless_list%num))
      if(dimless_list%num .gt. 0) dimless_list%value = 0.0d0
!
      end subroutine alloc_dimless_list
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_dimless_list(dimless_list)
!
      type(list_of_dimless), intent(inout) :: dimless_list
!
!
      deallocate(dimless_list%name, dimless_list%value)
!
      end subroutine dealloc_dimless_list
!
! -----------------------------------------------------------------------
!
      subroutine copy_dimless_from_ctl(coef_ctl, dimless_list)
!
      use t_control_array_charareal
!
      type(ctl_array_cr), intent(in) :: coef_ctl
      type(list_of_dimless), intent(inout) :: dimless_list
!
!
      call alloc_dimless_list(dimless_list)
      if (dimless_list%num .le. 0) return
!
      dimless_list%name(1:dimless_list%num)                            &
     &             = coef_ctl%c_tbl(1:dimless_list%num)
      dimless_list%value(1:dimless_list%num)                           &
     &             = coef_ctl%vect(1:dimless_list%num)
!
      end subroutine copy_dimless_from_ctl
!
! -----------------------------------------------------------------------
!
      subroutine check_dimless_numbers(dimless_list)
!
      type(list_of_dimless), intent(in) :: dimless_list
      integer(kind = kint) :: i
!
!
      write(*,*) '# of dimensionless numbers: ', dimless_list%num
!
      do i = 1, dimless_list%num
        write(*,*) i, trim(dimless_list%name(i)),                      &
     &               '  ', dimless_list%value(i)
      end do
!
      end subroutine check_dimless_numbers
!
! -----------------------------------------------------------------------
!
      end module t_list_of_dimless_numbers
