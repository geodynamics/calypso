!>@file  t_field_labels.f90
!!       module t_field_labels
!!
!!@author H. Matsui
!!@date   Programmed on Feb., 2020
!!
!>@brief Structure of field labels
!!
!!@verbatim
!!  Field label structure
!!       n_comp:  Numer of component
!!       name:    Field name
!!       math:    Math expression 
!!
!!      type(field_def), parameter :: velocity                          &
!!     &  = field_def(name = 'velocity',                                &
!!     &              math = '$u_{i}$',                                 &
!!     &              n_comp = n_vector)
!!@endverbatim
!
      module t_field_labels
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      implicit none
!
!>      Structure of field label
      type field_def
!>        Field name
        character(len=kchara) :: name
!>        Math expression
        character(len=kchara) :: math
!>        Number of component
        integer(kind = kint) ::  n_comp
      end type field_def
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_field_labels(field, n_comps, field_names, maths)
!
      type(field_def), intent(in) :: field
      integer(kind = kint), intent(inout) :: n_comps
      character(len = kchara), intent(inout) :: field_names
      character(len = kchara), intent(inout) :: maths
!
      integer(kind = kint) :: i, icou
!
      n_comps = field%n_comp
      write(field_names, '(a,a1)') trim(field%name) // char(0)
!
      icou = 0
      do i = 1, kchara-1
        if(field%math(i:i) .eq. '$') icou = icou + 1
        maths(i:i) = field%math(i:i)
        if(icou .ge. 2) exit
      end do
      maths(i+1:i+1) = char(0)
!
      end subroutine set_field_labels
!
! ----------------------------------------------------------------------
!
      end module t_field_labels
