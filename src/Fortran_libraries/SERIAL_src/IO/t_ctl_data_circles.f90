!>@file   t_ctl_data_circles.f90
!!        module t_ctl_data_circles
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!>@brief control date for data ouput on circles
!!
!!@verbatim
!!      subroutine read_data_on_circles_ctl                             &
!!     &         (id_control, hd_block, circ_ctls, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(data_on_circles_ctl), intent(inout) :: circ_ctls
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_data_on_circles_ctl                            &
!!     &         (id_control, hd_block, circ_ctls, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(data_on_circles_ctl), intent(in) :: circ_ctls
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine alloc_data_on_circles_ctl(circ_ctls)
!!      subroutine dealloc_data_on_circles_ctl(circ_ctls)
!!        type(data_on_circles_ctl), intent(inout) :: circ_ctls
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  array fields_on_circle_ctl
!!    begin fields_on_circle_ctl
!!      field_on_circle_prefix         'monitor/dbench_field'
!!      spectr_on_circle_prefix        'monitor/dbench_spectr'
!!      field_on_circle_format         'gzip'
!!
!!      pick_circle_coord_ctl         spherical
!!      nphi_mid_eq_ctl               500
!!      pick_cylindrical_radius_ctl   0.75
!!      pick_vertical_position_ctl    0.6
!!    end fields_on_circle_ctl
!!  end array fields_on_circle_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_circles
!
      use m_precision
!
      use t_read_control_elements
      use t_ctl_data_mid_equator
      use skip_comment_f
!
      implicit  none
!
      type data_on_circles_ctl
        integer(kind = kint) :: num_circ_ctl = 0
!>        Structure for data on circle
        type(mid_equator_control), allocatable :: meq_ctl(:)
      end type data_on_circles_ctl
!
      private :: append_data_on_circles_ctl
      private :: dup_data_on_circles_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_data_on_circles_ctl                               &
     &         (id_control, hd_block, circ_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(data_on_circles_ctl), intent(inout) :: circ_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(circ_ctls%meq_ctl)) return
      circ_ctls%num_circ_ctl = 0
      call alloc_data_on_circles_ctl(circ_ctls)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          call append_data_on_circles_ctl(circ_ctls)
          write(*,'(3a,i4,a)') 'Control for ', trim(hd_block), ' No. ', &
     &        circ_ctls%num_circ_ctl, ' is included'
          call read_mid_eq_monitor_ctl(id_control, hd_block,            &
     &        circ_ctls%meq_ctl(circ_ctls%num_circ_ctl), c_buf)
        end if
      end do
!
      end subroutine read_data_on_circles_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_data_on_circles_ctl                              &
     &         (id_control, hd_block, circ_ctls, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(data_on_circles_ctl), intent(in) :: circ_ctls
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
      level = write_array_flag_for_ctl(id_control, level, hd_block)
      do i = 1, circ_ctls%num_circ_ctl
        write(*,'(2a,i4)', ADVANCE='NO') trim(hd_block), ' No. ', i
        call write_mid_eq_monitor_ctl(id_control, hd_block,             &
     &                                circ_ctls%meq_ctl(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_data_on_circles_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_data_on_circles_ctl(circ_ctls)
!
      type(data_on_circles_ctl), intent(inout) :: circ_ctls
      integer(kind = kint) :: i
!
!
      allocate(circ_ctls%meq_ctl(circ_ctls%num_circ_ctl))
!
      end subroutine alloc_data_on_circles_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_data_on_circles_ctl(circ_ctls)
!
      type(data_on_circles_ctl), intent(inout) :: circ_ctls
!
      integer(kind = kint) :: i
!
      if(allocated(circ_ctls%meq_ctl) .eqv. .FALSE.) return
!
      do i = 1, circ_ctls%num_circ_ctl
        call reset_mid_equator_control(circ_ctls%meq_ctl(i))
      end do
!
      deallocate(circ_ctls%meq_ctl)
      circ_ctls%num_circ_ctl = 0
!
      end subroutine dealloc_data_on_circles_ctl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_data_on_circles_ctl(circ_ctls)
!
      type(data_on_circles_ctl), intent(inout) :: circ_ctls
!
      type(data_on_circles_ctl) :: tmp_circ_ctls
!
!
      tmp_circ_ctls%num_circ_ctl = circ_ctls%num_circ_ctl
      call alloc_data_on_circles_ctl(tmp_circ_ctls)
      call dup_data_on_circles_ctl                                      &
     &    (tmp_circ_ctls%num_circ_ctl, circ_ctls, tmp_circ_ctls)
!
      call dealloc_data_on_circles_ctl(circ_ctls)
!
      circ_ctls%num_circ_ctl = tmp_circ_ctls%num_circ_ctl + 1
      call alloc_data_on_circles_ctl(circ_ctls)
!
      call dup_data_on_circles_ctl                                      &
     &   (tmp_circ_ctls%num_circ_ctl, tmp_circ_ctls, circ_ctls)
!
      call dealloc_data_on_circles_ctl(tmp_circ_ctls)
!
      end subroutine append_data_on_circles_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dup_data_on_circles_ctl                                &
     &         (num_psf, org_circ_ctls, new_circ_ctls)
!
      integer(kind = kint), intent(in) :: num_psf
      type(data_on_circles_ctl), intent(in) :: org_circ_ctls
      type(data_on_circles_ctl), intent(inout) :: new_circ_ctls
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call dup_mid_equator_control(org_circ_ctls%meq_ctl(i),          &
                                     new_circ_ctls%meq_ctl(i))
      end do
!
      end subroutine dup_data_on_circles_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_circles
