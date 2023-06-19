!>@file   t_ctl_data_psf_compares.f90
!!@brief  module t_ctl_data_psf_compares
!!
!!@date  Programmed by H.Matsui in Jan., 2021
!
!>@brief control data to compare multiple sectiong data
!!
!!@verbatim
!!      subroutine read_ctl_file_psf_compares(my_rank, psf_compares)
!!      subroutine read_ctl_data_psf_compares                           &
!!     &         (id_control, hd_block, psf_compares, c_buf)
!!      subroutine dealloc_psf_compares_ctl(psf_compares)
!!        type(psf_compare_controls), intent(inout) :: psf_compares
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!        type(psf_compare_controls), intent(inout) :: psf_compares
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  array compare_surface_file
!!    begin compare_surface_file
!!      begin first_file_ctl
!!        surface_file_prefix    'isosurface/iso_w10n'
!!        surface_file_format            VTK
!!      end first_file_ctl
!!      begin second_file_ctl
!!        surface_file_prefix    'reference/iso_w10n'
!!        surface_file_format            VTK_GZ
!!      end second_file_ctl
!!      i_step_surface_ctl      10
!!    end compare_surface_file
!
!!    ...
!
!!    begin compare_surface_file
!!      ...
!!    end compare_surface_file
!!  end array compare_surface_file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_psf_compares
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_psf_compare
!
!
!>        Control file name
      character(len = kchara), parameter, private                       &
     &             :: fname_ctl_psf_compare = 'control_compare_psf'
!>        Control file ID
      integer(kind = kint), parameter, private :: id_control = 11
!
      type psf_compare_controls
        integer(kind = kint) :: num_psf_cmp = 0
        type(psf_compare_control), allocatable :: psf_cmp_ctls(:)
!
        integer (kind=kint) :: i_psf_compare_list = 0
      end type psf_compare_controls
!
      character(len=kchara), parameter, private                         &
     &             :: hd_compare_psf_file = 'compare_surface_file'
!
      private :: alloc_psf_compares_ctl, append_ctl_data_psf_compare
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_file_psf_compares(my_rank, psf_compares)
!
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      type(psf_compare_controls), intent(inout) :: psf_compares
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        open(id_control, file = fname_ctl_psf_compare, status='old')
        do
          call load_one_line_from_control                               &
     &       (id_control, hd_compare_psf_file, c_buf1)
          if(c_buf1%iend .gt. 0) exit
!
          call read_ctl_data_psf_compares                               &
     &       (id_control, hd_compare_psf_file, psf_compares, c_buf1)
!
          if(psf_compares%i_psf_compare_list .gt. 0) exit
        end do
        close(id_control)
      end if
      if(c_buf1%iend .gt. 0) stop 'control file is broken'
!
      end subroutine read_ctl_file_psf_compares
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_data_psf_compares                             &
     &         (id_control, hd_block, psf_compares, c_buf)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(psf_compare_controls), intent(inout) :: psf_compares
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(psf_compares%psf_cmp_ctls)) return
      psf_compares%num_psf_cmp = 0
      call alloc_psf_compares_ctl(psf_compares)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          call append_ctl_data_psf_compare(psf_compares)
!
          call write_multi_ctl_file_message                             &
     &       (hd_block, psf_compares%num_psf_cmp, c_buf%level)
          call read_ctl_data_psf_compare(id_control, hd_block,          &
     &        psf_compares%psf_cmp_ctls(psf_compares%num_psf_cmp),      &
     &        c_buf)
        end if
      end do
      psf_compares%i_psf_compare_list = 1
!
      end subroutine read_ctl_data_psf_compares
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_psf_compares_ctl(psf_compares)
!
      type(psf_compare_controls), intent(inout) :: psf_compares
!
      integer(kind = kint) :: i
!
      do i = 1, psf_compares%num_psf_cmp
        call reset_ctl_data_psf_compare(psf_compares%psf_cmp_ctls(i))
      end do
      deallocate(psf_compares%psf_cmp_ctls)
      psf_compares%i_psf_compare_list = 0
!
      end subroutine dealloc_psf_compares_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_compares_ctl(psf_compares)
!
      type(psf_compare_controls), intent(inout) :: psf_compares
!
      integer(kind = kint) :: i
!
!
      allocate(psf_compares%psf_cmp_ctls(psf_compares%num_psf_cmp))
!
      do i = 1, psf_compares%num_psf_cmp
        call reset_ctl_data_psf_compare(psf_compares%psf_cmp_ctls(i))
      end do
!
      end subroutine alloc_psf_compares_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine append_ctl_data_psf_compare(psf_compares)
!
      type(psf_compare_controls), intent(inout) :: psf_compares
!
      type(psf_compare_controls) :: psf_cmp_tmp
!
!
      psf_cmp_tmp%num_psf_cmp = psf_compares%num_psf_cmp
      call alloc_psf_compares_ctl(psf_cmp_tmp)
      do i = 1, psf_compares%num_psf_cmp
        call copy_ctl_data_psf_compare(psf_compares%psf_cmp_ctls(i),   &
     &                                 psf_cmp_tmp%psf_cmp_ctls(i))
      end do
      call dealloc_psf_compares_ctl(psf_compares)
!
      psf_compares%num_psf_cmp = psf_compares%num_psf_cmp + 1
      call alloc_psf_compares_ctl(psf_compares)
!
      do i = 1, psf_cmp_tmp%num_psf_cmp
        call copy_ctl_data_psf_compare(psf_cmp_tmp%psf_cmp_ctls(i),     &
     &                                 psf_compares%psf_cmp_ctls(i))
      end do
      call dealloc_psf_compares_ctl(psf_cmp_tmp)
!
      end subroutine append_ctl_data_psf_compare
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_psf_compares
