!>@file   t_control_data_sections.f90
!!@brief  module t_control_data_sections
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine alloc_psf_ctl_stract(psf_ctls)
!!      subroutine dealloc_psf_ctl_stract(psf_ctls)
!!
!!      subroutine read_files_4_psf_ctl                                 &
!!     &         (id_control, hd_block, psf_ctls, c_buf)
!!      subroutine bcast_files_4_psf_ctl(psf_ctls)
!!        type(section_controls), intent(inout) :: psf_ctls
!!
!!      subroutine read_control_4_psf_file                              &
!!     &         (id_control, fname_psf_ctl, psf_ctl_struct)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array cross_section_ctl  1
!!      file   cross_section_ctl   'ctl_psf_eq'
!!    end array cross_section_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_sections
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_psf
!
      implicit  none
!
!
      type section_controls
        integer(kind = kint) :: num_psf_ctl = 0
        character(len = kchara), allocatable :: fname_psf_ctl(:)
        type(psf_ctl), allocatable :: psf_ctl_struct(:)
      end type section_controls
!
      private :: append_new_section_control
      private :: dup_control_4_psfs, dealloc_cont_dat_4_psfs
!
      character(len=kchara), parameter                                  &
     &             :: hd_section_ctl = 'cross_section_ctl'
!      Deprecated labels
      character(len=kchara), parameter                                  &
     &             :: hd_psf_ctl = 'surface_rendering'
      private :: hd_section_ctl, hd_psf_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_ctl_stract(psf_ctls)
!
      type(section_controls), intent(inout) :: psf_ctls
      integer(kind = kint) :: i
!
!
      allocate(psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl))
      allocate(psf_ctls%psf_ctl_struct(psf_ctls%num_psf_ctl))
!
      do i = 1, psf_ctls%num_psf_ctl
        call init_psf_ctl_stract(psf_ctls%psf_ctl_struct(i))
      end do
!
      end subroutine alloc_psf_ctl_stract
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_ctl_stract(psf_ctls)
!
      type(section_controls), intent(inout) :: psf_ctls
!
      if(allocated(psf_ctls%fname_psf_ctl)) then
        deallocate(psf_ctls%psf_ctl_struct)
        deallocate(psf_ctls%fname_psf_ctl)
      end if
      psf_ctls%num_psf_ctl = 0
!
      end subroutine dealloc_psf_ctl_stract
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_files_4_psf_ctl                                   &
     &         (id_control, hd_block, psf_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(section_controls), intent(inout) :: psf_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(allocated(psf_ctls%fname_psf_ctl)) return
      psf_ctls%num_psf_ctl = 0
      call alloc_psf_ctl_stract(psf_ctls)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_file_flag(c_buf, hd_block)) then
          call append_new_section_control(psf_ctls)
          psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl)                  &
     &        = third_word(c_buf)
!
          write(*,'(3a,i4,a)', ADVANCE='NO') 'Read file for ',          &
     &        trim(hd_block), ' No. ', psf_ctls%num_psf_ctl, '... '
          call read_control_4_psf_file(id_control+2,                    &
     &        psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl),             &
     &        psf_ctls%psf_ctl_struct(psf_ctls%num_psf_ctl))
        else if(check_begin_flag(c_buf, hd_block)) then
          call append_new_section_control(psf_ctls)
          psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl) = 'NO_FILE'
!
          write(*,*) 'Control for', trim(hd_block), ' No. ',            &
     &              psf_ctls%num_psf_ctl, ' is included'
          call read_psf_control_data(id_control, hd_block,              &
     &        psf_ctls%psf_ctl_struct(psf_ctls%num_psf_ctl), c_buf)
        end if
      end do
!
      end subroutine read_files_4_psf_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_files_4_psf_ctl(psf_ctls)
!
      use calypso_mpi
      use t_control_data_4_psf
!
      type(section_controls), intent(inout) :: psf_ctls
      integer (kind=kint) :: i_psf
!
!
      call MPI_BCAST(psf_ctls%num_psf_ctl, 1,                           &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      if(psf_ctls%num_psf_ctl .le. 0) return
!
      if(my_rank .gt. 0) call alloc_psf_ctl_stract(psf_ctls)
!
      call MPI_BCAST                                                    &
     &   (psf_ctls%fname_psf_ctl, int(kchara*psf_ctls%num_psf_ctl),     &
     &    CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
      do i_psf = 1, psf_ctls%num_psf_ctl
        call bcast_psf_control_data(psf_ctls%psf_ctl_struct(i_psf))
      end do
!
      end subroutine bcast_files_4_psf_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_new_section_control(psf_ctls)
!
      type(section_controls), intent(inout) :: psf_ctls
!
      type(section_controls) :: tmp_psf_c
!
!
      tmp_psf_c%num_psf_ctl = psf_ctls%num_psf_ctl
      call alloc_psf_ctl_stract(tmp_psf_c)
      call dup_control_4_psfs                                           &
     &    (tmp_psf_c%num_psf_ctl, psf_ctls, tmp_psf_c)
!
      call dealloc_cont_dat_4_psfs                                      &
     &   (psf_ctls%num_psf_ctl, psf_ctls%psf_ctl_struct)
      call dealloc_psf_ctl_stract(psf_ctls)
!
      psf_ctls%num_psf_ctl = tmp_psf_c%num_psf_ctl + 1
      call alloc_psf_ctl_stract(psf_ctls)
!
      call dup_control_4_psfs                                           &
     &   (tmp_psf_c%num_psf_ctl, tmp_psf_c, psf_ctls)
!
      call dealloc_cont_dat_4_psfs                                      &
     &   (tmp_psf_c%num_psf_ctl, tmp_psf_c%psf_ctl_struct)
      call dealloc_psf_ctl_stract(tmp_psf_c)
!
      end subroutine append_new_section_control
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_4_psfs                                     &
     &         (num_psf, org_psf_ctls, new_psf_ctls)
!
      integer(kind = kint), intent(in) :: num_psf
      type(section_controls), intent(in) :: org_psf_ctls
      type(section_controls), intent(inout) :: new_psf_ctls
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call dup_control_4_psf(org_psf_ctls%psf_ctl_struct(i),          &
            new_psf_ctls%psf_ctl_struct(i))
        new_psf_ctls%fname_psf_ctl(i) = org_psf_ctls%fname_psf_ctl(i)
      end do
!
      end subroutine dup_control_4_psfs
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_cont_dat_4_psfs(num_psf, psf_c)
!
      integer(kind = kint), intent(in) :: num_psf
      type(psf_ctl), intent(inout) :: psf_c(num_psf)
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call dealloc_cont_dat_4_psf(psf_c(i))
      end do
!
      end subroutine dealloc_cont_dat_4_psfs
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_psf_file                                &
     &         (id_control, fname_psf_ctl, psf_ctl_struct)
!
      use t_read_control_elements
      use t_control_data_4_psf
!
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: fname_psf_ctl
      type(psf_ctl), intent(inout) :: psf_ctl_struct
!
      type(buffer_for_control) :: c_buf1
!
!
      write(*,*) 'Section control file: ', trim(fname_psf_ctl)
      open(id_control, file=fname_psf_ctl, status='old')
!
      do
        call load_one_line_from_control(id_control, c_buf1)
        call read_psf_control_data(id_control, hd_section_ctl,          &
     &      psf_ctl_struct, c_buf1)
        call read_psf_control_data(id_control, hd_psf_ctl,              &
     &      psf_ctl_struct, c_buf1)
        if(psf_ctl_struct%i_psf_ctl .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_control_4_psf_file
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_sections
