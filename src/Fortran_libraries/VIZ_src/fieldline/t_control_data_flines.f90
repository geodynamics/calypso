!>@file   t_control_data_flines.f90
!!@brief  module t_control_data_flines
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine dealloc_fline_ctl_struct(fline_ctls)
!!      subroutine alloc_fline_ctl_struct(fline_ctls)
!!      subroutine append_new_fline_control(fline_ctls)
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!
!!      subroutine add_fields_4_flines_to_fld_ctl(fline_ctls, field_ctl)
!!        type(fieldline_controls), intent(in) :: fline_ctls
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  fieldline  1
!!      file  fieldline  'ctl_fline_magne'
!!    end array fieldline
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_flines
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use t_ctl_data_field_line
!
      implicit  none
!
      type fieldline_controls
        integer(kind = kint) :: num_fline_ctl = 0
        character(len = kchara), allocatable :: fname_fline_ctl(:)
        type(fline_ctl), allocatable :: fline_ctl_struct(:)
      end type fieldline_controls
!
!      fieldline flag
!
      private :: dup_control_4_flines
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_fline_ctl_struct(fline_ctls)
!
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      integer(kind = kint) :: i
!
      if(allocated(fline_ctls%fline_ctl_struct) .eqv. .FALSE.) return
!
      do i = 1, fline_ctls%num_fline_ctl
        call deallocate_cont_dat_fline(fline_ctls%fline_ctl_struct(i))
      end do
      deallocate(fline_ctls%fline_ctl_struct)
      deallocate(fline_ctls%fname_fline_ctl)
      fline_ctls%num_fline_ctl = 0
!
      end subroutine dealloc_fline_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_fline_ctl_struct(fline_ctls)
!
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      allocate(fline_ctls%fline_ctl_struct(fline_ctls%num_fline_ctl))
      allocate(fline_ctls%fname_fline_ctl(fline_ctls%num_fline_ctl))
!
      end subroutine alloc_fline_ctl_struct
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_new_fline_control(fline_ctls)
!
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      type(fieldline_controls) :: tmp_fline_c
!
!
      tmp_fline_c%num_fline_ctl = fline_ctls%num_fline_ctl
      call alloc_fline_ctl_struct(tmp_fline_c)
      call dup_control_4_flines                                         &
     &    (tmp_fline_c%num_fline_ctl, fline_ctls, tmp_fline_c)
!
      call dealloc_fline_ctl_struct(fline_ctls)
!
      fline_ctls%num_fline_ctl = tmp_fline_c%num_fline_ctl + 1
      call alloc_fline_ctl_struct(fline_ctls)
!
      call dup_control_4_flines                                         &
     &   (tmp_fline_c%num_fline_ctl, tmp_fline_c, fline_ctls)
!
      call dealloc_fline_ctl_struct(tmp_fline_c)
!
      end subroutine append_new_fline_control
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_4_flines                                   &
     &         (num_fline, org_fline_ctls, new_fline_ctls)
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_controls), intent(in) :: org_fline_ctls
      type(fieldline_controls), intent(inout) :: new_fline_ctls
!
      integer(kind = kint) :: i
!
      do i = 1, num_fline
        call dup_control_4_fline(org_fline_ctls%fline_ctl_struct(i),    &
              new_fline_ctls%fline_ctl_struct(i))
      end do
      new_fline_ctls%fname_fline_ctl(1:num_fline)                       &
     &        = org_fline_ctls%fname_fline_ctl(1:num_fline)
!
      end subroutine dup_control_4_flines
!
!  ---------------------------------------------------------------------
!
      subroutine add_fields_4_flines_to_fld_ctl(fline_ctls, field_ctl)
!
      use t_control_array_character3
!
      type(fieldline_controls), intent(in) :: fline_ctls
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      integer(kind = kint) :: i_fline
!
!
      do i_fline = 1, fline_ctls%num_fline_ctl
        call add_field_4_fline_to_fld_ctl                               &
     &     (fline_ctls%fline_ctl_struct(i_fline), field_ctl)
      end do
!
      end subroutine add_fields_4_flines_to_fld_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_flines
