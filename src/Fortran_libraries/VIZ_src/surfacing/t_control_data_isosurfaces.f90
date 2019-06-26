!>@file   t_control_data_isosurfaces.f90
!!@brief  module t_control_data_isosurfaces
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for isosurfaces
!!
!!@verbatim
!!      subroutine read_files_4_iso_ctl                                 &
!!     &         (id_control, hd_block, iso_ctls, c_buf)
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine bcast_files_4_iso_ctl(iso_ctls)
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!      subroutine dealloc_iso_ctl_stract(iso_ctls)
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array isosurface_ctl     2
!!      file   isosurface_ctl   'ctl_iso_p_n1e4'
!!      file   isosurface_ctl   'ctl_iso_p_p1e4'
!!    end array isosurface_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_isosurfaces
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_iso
!
      implicit  none
!
!
      type isosurf_controls
        integer(kind = kint) :: num_iso_ctl = 0
        character(len = kchara), allocatable :: fname_iso_ctl(:)
        type(iso_ctl), allocatable :: iso_ctl_struct(:)
      end type isosurf_controls
!
!     Top level
      character(len=kchara), parameter                                  &
     &             :: hd_isosurf_ctl = 'isosurface_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_ctl = 'isosurf_rendering'
      private :: hd_isosurf_ctl, hd_iso_ctl
!
!
      private :: alloc_iso_ctl_stract
      private :: dealloc_cont_dat_4_isos
      private :: append_new_isosurface_control, dup_control_4_isos
      private :: read_control_4_iso_file
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_files_4_iso_ctl                                   &
     &         (id_control, hd_block, iso_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(isosurf_controls), intent(inout) :: iso_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(allocated(iso_ctls%fname_iso_ctl)) return
      iso_ctls%num_iso_ctl = 0
      call alloc_iso_ctl_stract(iso_ctls)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_file_flag(c_buf, hd_block)) then
          call append_new_isosurface_control(iso_ctls)
          iso_ctls%fname_iso_ctl(iso_ctls%num_iso_ctl)                  &
     &        = third_word(c_buf)
          write(*,'(3a,i4,a)', ADVANCE='NO') 'Read file for ',          &
     &        trim(hd_block), ' No. ', iso_ctls%num_iso_ctl, '... '
          call read_control_4_iso_file(id_control+2,                    &
&             iso_ctls%fname_iso_ctl(iso_ctls%num_iso_ctl),             &
&             iso_ctls%iso_ctl_struct(iso_ctls%num_iso_ctl))
        else if(check_begin_flag(c_buf, hd_block)) then
          call append_new_isosurface_control(iso_ctls)
          iso_ctls%fname_iso_ctl(iso_ctls%num_iso_ctl) = 'NO_FILE'
!
          write(*,*) 'Control for', trim(hd_block), ' No. ',            &
     &              iso_ctls%num_iso_ctl, ' is included'
          call read_iso_control_data(id_control, hd_block,              &
     &        iso_ctls%iso_ctl_struct(iso_ctls%num_iso_ctl), c_buf)
        end if
      end do
!
      end subroutine read_files_4_iso_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_files_4_iso_ctl(iso_ctls)
!
      use calypso_mpi
      use t_control_data_4_iso
!
      type(isosurf_controls), intent(inout) :: iso_ctls
      integer (kind=kint) :: i_iso
!
!
      call MPI_BCAST(iso_ctls%num_iso_ctl,  1,                          &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      if(iso_ctls%num_iso_ctl .le. 0) return
!
      if(my_rank .gt. 0) call alloc_iso_ctl_stract(iso_ctls)
!
      call MPI_BCAST                                                    &
     &   (iso_ctls%fname_iso_ctl, int(kchara*iso_ctls%num_iso_ctl),     &
     &    CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
      do i_iso = 1, iso_ctls%num_iso_ctl
        call bcast_iso_control_data(iso_ctls%iso_ctl_struct(i_iso))
      end do
!
      end subroutine bcast_files_4_iso_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_iso_ctl_stract(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      if(allocated(iso_ctls%fname_iso_ctl)) then
        deallocate(iso_ctls%iso_ctl_struct)
        deallocate(iso_ctls%fname_iso_ctl)
      end if
      iso_ctls%num_iso_ctl = 0
!
      end subroutine dealloc_iso_ctl_stract
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_new_isosurface_control(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      type(isosurf_controls) :: tmp_iso_c
!
!
      tmp_iso_c%num_iso_ctl = iso_ctls%num_iso_ctl
      call alloc_iso_ctl_stract(tmp_iso_c)
      call dup_control_4_isos                                           &
     &    (tmp_iso_c%num_iso_ctl, iso_ctls, tmp_iso_c)
!
      call dealloc_cont_dat_4_isos                                      &
     &   (iso_ctls%num_iso_ctl, iso_ctls%iso_ctl_struct)
      call dealloc_iso_ctl_stract(iso_ctls)
!
      iso_ctls%num_iso_ctl = tmp_iso_c%num_iso_ctl + 1
      call alloc_iso_ctl_stract(iso_ctls)
!
      call dup_control_4_isos                                           &
     &   (tmp_iso_c%num_iso_ctl, tmp_iso_c, iso_ctls)
!
      call dealloc_cont_dat_4_isos                                      &
     &   (tmp_iso_c%num_iso_ctl, tmp_iso_c%iso_ctl_struct)
      call dealloc_iso_ctl_stract(tmp_iso_c)
!
      end subroutine append_new_isosurface_control
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_4_isos                                     &
     &         (num_iso, org_iso_ctls, new_iso_ctls)
!
      integer(kind = kint), intent(in) :: num_iso
      type(isosurf_controls), intent(in) :: org_iso_ctls
      type(isosurf_controls), intent(inout) :: new_iso_ctls
!
      integer(kind = kint) :: i
!
      do i = 1, num_iso
        if(org_iso_ctls%fname_iso_ctl(i) .eq. 'NO_FILE') then
          call dup_control_4_iso(org_iso_ctls%iso_ctl_struct(i),        &
              new_iso_ctls%iso_ctl_struct(i))
        end if
        new_iso_ctls%fname_iso_ctl(i) = org_iso_ctls%fname_iso_ctl(i)
      end do
!
      end subroutine dup_control_4_isos
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_iso_ctl_stract(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      allocate(iso_ctls%fname_iso_ctl(iso_ctls%num_iso_ctl))
      allocate(iso_ctls%iso_ctl_struct(iso_ctls%num_iso_ctl))
!
      end subroutine alloc_iso_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_cont_dat_4_isos(num_iso, iso_c)
!
      integer(kind = kint), intent(in) :: num_iso
      type(iso_ctl), intent(inout) :: iso_c(num_iso)
!
      integer(kind = kint) :: i
!
      do i = 1, num_iso
        call dealloc_cont_dat_4_iso(iso_c(i))
      end do
!
      end subroutine dealloc_cont_dat_4_isos
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_iso_file                                &
     &         (id_control, fname_iso_ctl, iso_ctl_struct)
!
      use t_read_control_elements
      use t_control_data_4_iso
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: fname_iso_ctl
      type(iso_ctl), intent(inout) :: iso_ctl_struct
!
      type(buffer_for_control) :: c_buf1
!
!
      write(*,*) 'Isosurface control file: ', trim(fname_iso_ctl)
      open(id_control, file=fname_iso_ctl, status='old')
!
      do
        call load_one_line_from_control(id_control, c_buf1)
        call read_iso_control_data                                      &
     &     (id_control, hd_isosurf_ctl, iso_ctl_struct, c_buf1)
        call read_iso_control_data                                      &
     &     (id_control, hd_iso_ctl, iso_ctl_struct, c_buf1)
        if(iso_ctl_struct%i_iso_ctl .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_control_4_iso_file
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_isosurfaces
