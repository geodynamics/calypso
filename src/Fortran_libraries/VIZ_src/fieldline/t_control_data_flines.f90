!>@file   t_control_data_flines.f90
!!@brief  module t_control_data_flines
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine read_files_4_fline_ctl                               &
!!     &         (id_control, hd_block, fline_ctls, c_buf)
!!      subroutine bcast_files_4_fline_ctl(fline_ctls)
!!      subroutine dealloc_fline_fhead_ctl(fline_ctls)
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
      use t_control_data_4_fline
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
      private :: read_fline_control_file, alloc_fline_ctl_struct
      private :: append_new_fline_control
      private :: dup_control_4_flines, dealloc_cont_dat_4_flines
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_fline_control_file(id_control, fname_fline_ctl,   &
     &          hd_block, fline_ctl_struct)
!
      use t_control_data_4_fline
      use read_field_line_ctl
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: fname_fline_ctl
      character(len=kchara), intent(in) :: hd_block
      type(fline_ctl), intent(inout)  :: fline_ctl_struct
!
      type(buffer_for_control) :: c_buf1
!
!
      write(*,*) 'read fieldline control file: ', trim(fname_fline_ctl)
      call reset_fline_control_flags(fline_ctl_struct)
      open(id_control, file=fname_fline_ctl, status='old')
!
      do
        call load_one_line_from_control(id_control, c_buf1)
        call s_read_field_line_ctl(id_control, hd_block,                &
     &      fline_ctl_struct, c_buf1)
        if(fline_ctl_struct%i_vr_fline_ctl .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_fline_control_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_files_4_fline_ctl                                 &
     &         (id_control, hd_block, fline_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
      use read_field_line_ctl
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(fieldline_controls), intent(inout) :: fline_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(fline_ctls%fname_fline_ctl)) return
      fline_ctls%num_fline_ctl = 0
      call alloc_fline_ctl_struct(fline_ctls)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
!
        if(check_file_flag(c_buf, hd_block)) then
          call append_new_fline_control(fline_ctls)
          fline_ctls%fname_fline_ctl(fline_ctls%num_fline_ctl)          &
     &        = third_word(c_buf)
!
          write(*,'(3a,i4,a)', ADVANCE='NO') 'Read file for ',          &
     &        trim(hd_block), ' No. ', fline_ctls%num_fline_ctl, '... '
          call read_fline_control_file(id_control+2,                    &
     &        fline_ctls%fname_fline_ctl(fline_ctls%num_fline_ctl),     &
     &        hd_block,                                                 &
     &        fline_ctls%fline_ctl_struct(fline_ctls%num_fline_ctl))
        end if
!
        if(check_begin_flag(c_buf, hd_block)) then
          call append_new_fline_control(fline_ctls)
          fline_ctls%fname_fline_ctl(fline_ctls%num_fline_ctl)          &
     &                                                = 'NO_FILE'
!
          write(*,*) 'Control for', trim(hd_block), ' No. ',            &
     &              fline_ctls%num_fline_ctl, ' is included'
          call s_read_field_line_ctl(id_control, hd_block,              &
     &        fline_ctls%fline_ctl_struct(fline_ctls%num_fline_ctl),    &
     &        c_buf)
        end if
      end do
!
      end subroutine read_files_4_fline_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_files_4_fline_ctl(fline_ctls)
!
      use calypso_mpi
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(fieldline_controls), intent(inout) :: fline_ctls
      integer (kind=kint) :: i_fline
!
!
      call calypso_mpi_bcast_one_int(fline_ctls%num_fline_ctl, 0)
      if(fline_ctls%num_fline_ctl .le. 0) return
!
      if(my_rank .gt. 0)  call alloc_fline_ctl_struct(fline_ctls)
!
      call calypso_mpi_bcast_character(fline_ctls%fname_fline_ctl,      &
     &    cast_long(kchara*fline_ctls%num_fline_ctl), 0)
      do i_fline = 1, fline_ctls%num_fline_ctl
        if(fline_ctls%fname_fline_ctl(i_fline) .eq. 'NO_FILE') then
          call bcast_field_line_ctl                                     &
     &       (fline_ctls%fline_ctl_struct(i_fline))
        end if
      end do
!
      end subroutine bcast_files_4_fline_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_fline_fhead_ctl(fline_ctls)
!
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      if(allocated(fline_ctls%fline_ctl_struct)) then
        deallocate(fline_ctls%fname_fline_ctl)
        deallocate(fline_ctls%fline_ctl_struct)
      end if
      fline_ctls%num_fline_ctl = 0
!
      end subroutine dealloc_fline_fhead_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_fline_ctl_struct(fline_ctls)
!
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      allocate(fline_ctls%fname_fline_ctl(fline_ctls%num_fline_ctl))
      allocate(fline_ctls%fline_ctl_struct(fline_ctls%num_fline_ctl))
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
      call dealloc_cont_dat_4_flines                                    &
     &   (fline_ctls%num_fline_ctl, fline_ctls%fline_ctl_struct)
      call dealloc_fline_fhead_ctl(fline_ctls)
!
      fline_ctls%num_fline_ctl = tmp_fline_c%num_fline_ctl + 1
      call alloc_fline_ctl_struct(fline_ctls)
!
      call dup_control_4_flines                                         &
     &   (tmp_fline_c%num_fline_ctl, tmp_fline_c, fline_ctls)
!
      call dealloc_cont_dat_4_flines                                    &
     &   (tmp_fline_c%num_fline_ctl, tmp_fline_c%fline_ctl_struct)
      call dealloc_fline_fhead_ctl(tmp_fline_c)
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
        new_fline_ctls%fname_fline_ctl(i)                               &
     &           = org_fline_ctls%fname_fline_ctl(i)
      end do
!
      end subroutine dup_control_4_flines
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_cont_dat_4_flines(num_fline, fln)
!
      integer(kind = kint), intent(in) :: num_fline
      type(fline_ctl), intent(inout) :: fln(num_fline)
!
      integer(kind = kint) :: i
!
      do i = 1, num_fline
        call deallocate_cont_dat_fline(fln(i))
      end do
!
      end subroutine dealloc_cont_dat_4_flines
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
