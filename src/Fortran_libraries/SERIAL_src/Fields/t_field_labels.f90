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
!!
!!      subroutine set_field_label_to_ctl(field, array_c2i)
!!        type(field_def), intent(in) :: field
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!        logical function cmp_field_no_case(cmp_chara, field)
!!        type(field_def), intent(in) :: field
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
      subroutine set_field_label_to_ctl(field, array_c2i)
!
      use t_control_array_chara2int
!
      type(field_def), intent(in) :: field
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      character(len = kchara) :: maths, field_names
      integer(kind = kint) :: i, icou
!
      write(field_names, '(a,a1)') trim(field%name) // char(0)
!
      icou = 0
      do i = 1, kchara-1
        if(field%math(i:i) .eq. '$') icou = icou + 1
        maths(i:i) = field%math(i:i)
        if(icou .ge. 2) exit
      end do
      maths(i+1:i+1) = char(0)
      call append_c2i_to_ctl_array(field_names, maths, field%n_comp,    &
     &                             array_c2i)
!
      end subroutine set_field_label_to_ctl
!
! ----------------------------------------------------------------------
!
      logical function cmp_field_no_case(cmp_chara, field)
!
      use skip_comment_f
!
      type(field_def), intent(in) :: field
      character(len=kchara), intent(in) :: cmp_chara
!
      character(len=kchara)  :: field_name
!
!
      write(field_name,'(a)')  trim(field%name)
      cmp_field_no_case = cmp_no_case(cmp_chara, field_name)
!
      end function cmp_field_no_case
!
!-----------------------------------------------------------------------
!
      end module t_field_labels
