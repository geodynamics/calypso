!
!      module t_control_data_sections
!
!      Written by H. Matsui on July, 2006
!
!      subroutine alloc_psf_ctl_stract(psf_ctls)
!
!      subroutine dealloc_psf_ctl_stract(psf_ctls)
!      subroutine dealloc_iso_ctl_stract(iso_ctls)
!
!      subroutine read_sections_control_data(psf_ctls, iso_ctls)
!
!      subroutine read_files_4_psf_ctl(psf_ctls)
!      subroutine read_files_4_iso_ctl(iso_ctls)
!
!      subroutine bcast_files_4_psf_ctl(psf_ctls)
!      subroutine bcast_files_4_iso_ctl(iso_ctls)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array cross_section_ctl  1
!!      file   cross_section_ctl   'ctl_psf_eq'
!!    end array cross_section_ctl
!!
!!    array isosurface_ctl     2
!!      file   isosurface_ctl   'ctl_iso_p_n1e4'
!!      file   isosurface_ctl   'ctl_iso_p_p1e4'
!!    end array isosurface_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module t_control_data_sections
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_psf
      use t_control_data_4_iso
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
      type isosurf_controls
        integer(kind = kint) :: num_iso_ctl = 0
        character(len = kchara), allocatable :: fname_iso_ctl(:)
        type(iso_ctl), allocatable :: iso_ctl_struct(:)
      end type isosurf_controls
!
!   entry label
!
      character(len=kchara), parameter :: hd_viz_ctl = 'visual_control'
      integer (kind=kint) :: i_viz_ctl = 0
!
!     Top level
      character(len=kchara), parameter                                  &
     &             :: hd_section_ctl = 'cross_section_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_isosurf_ctl = 'isosurface_ctl'
!
!      Deprecated labels
      character(len=kchara), parameter                                  &
     &             :: hd_psf_ctl = 'surface_rendering'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_ctl = 'isosurf_rendering'
!
      private :: hd_section_ctl, hd_psf_ctl
      private :: hd_isosurf_ctl, hd_iso_ctl
      private :: hd_viz_ctl, i_viz_ctl
!
      private :: alloc_iso_ctl_stract
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
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_ctl_stract(psf_ctls)
!
      type(section_controls), intent(inout) :: psf_ctls
!
      deallocate(psf_ctls%psf_ctl_struct)
      deallocate(psf_ctls%fname_psf_ctl)
!
      end subroutine dealloc_psf_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_iso_ctl_stract(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      deallocate(iso_ctls%iso_ctl_struct)
      deallocate(iso_ctls%fname_iso_ctl)
!
      end subroutine dealloc_iso_ctl_stract
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_sections_control_data(psf_ctls, iso_ctls)
!
      use m_read_control_elements
      use skip_comment_f
!
      type(section_controls), intent(inout) :: psf_ctls
      type(isosurf_controls), intent(inout) :: iso_ctls
!
!
      if(right_begin_flag(hd_viz_ctl) .eq. 0) return
      if (i_viz_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_viz_ctl = find_control_end_flag(hd_viz_ctl)
        if(i_viz_ctl .eq. 1) exit
!
        call find_control_array_flag(hd_psf_ctl, psf_ctls%num_psf_ctl)
        call read_files_4_psf_ctl(psf_ctls)
        call find_control_array_flag                                    &
     &     (hd_section_ctl, psf_ctls%num_psf_ctl)
        call read_files_4_psf_ctl(psf_ctls)
!
        call find_control_array_flag(hd_iso_ctl, iso_ctls%num_iso_ctl)
        call read_files_4_iso_ctl(iso_ctls)
        call find_control_array_flag                                    &
     &     (hd_isosurf_ctl, iso_ctls%num_iso_ctl)
        call read_files_4_iso_ctl(iso_ctls)
      end do
!
      end subroutine read_sections_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_files_4_psf_ctl(psf_ctls)
!
      use m_read_control_elements
      use skip_comment_f
!
      type(section_controls), intent(inout) :: psf_ctls
      integer (kind=kint) :: i_psf_ctl1 = 0, i_psf_ctl2 = 0
!
!
      if(psf_ctls%num_psf_ctl .le. 0) return
      if((i_psf_ctl1+i_psf_ctl2) .gt. 0) return
!
      call alloc_psf_ctl_stract(psf_ctls)
      do
        call load_ctl_label_and_line
!
        call find_control_end_array_flag(hd_psf_ctl,                    &
     &      psf_ctls%num_psf_ctl, i_psf_ctl2)
        if(i_psf_ctl2 .ge. psf_ctls%num_psf_ctl) exit
        call find_control_end_array_flag(hd_section_ctl,                &
     &      psf_ctls%num_psf_ctl, i_psf_ctl1)
        if(i_psf_ctl1 .ge. psf_ctls%num_psf_ctl) exit
!
        if(right_file_flag(hd_section_ctl) .gt. 0) then
          call read_file_names_from_ctl_line                            &
     &       (psf_ctls%num_psf_ctl, i_psf_ctl1, psf_ctls%fname_psf_ctl)
        else if(right_begin_flag(hd_section_ctl) .gt. 0) then
          i_psf_ctl1 = i_psf_ctl1 + 1
          psf_ctls%fname_psf_ctl(i_psf_ctl1) = 'NO_FILE'
          call read_psf_control_data                                    &
     &       (hd_section_ctl, psf_ctls%psf_ctl_struct(i_psf_ctl1))
!
        else if(right_file_flag(hd_psf_ctl) .gt. 0) then
          call read_file_names_from_ctl_line                            &
     &       (psf_ctls%num_psf_ctl, i_psf_ctl2, psf_ctls%fname_psf_ctl)
        else if(right_begin_flag(hd_psf_ctl) .gt. 0) then
          i_psf_ctl2 = i_psf_ctl2 + 1
          psf_ctls%fname_psf_ctl(i_psf_ctl2) = 'NO_FILE'
          call read_psf_control_data                                    &
     &        (hd_psf_ctl, psf_ctls%psf_ctl_struct(i_psf_ctl2))
        end if
      end do
!
      end subroutine read_files_4_psf_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_files_4_iso_ctl(iso_ctls)
!
      use m_read_control_elements
      use skip_comment_f
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      integer (kind=kint) :: i_iso_ctl1 = 0, i_iso_ctl2 = 0
!
!
      if(iso_ctls%num_iso_ctl .le. 0) return
      if ((i_iso_ctl1+i_iso_ctl2) .gt. 0) return
!
      call alloc_iso_ctl_stract(iso_ctls)
      do
        call load_ctl_label_and_line
!
        call find_control_end_array_flag(hd_isosurf_ctl,                &
     &      iso_ctls%num_iso_ctl, i_iso_ctl1)
        if(i_iso_ctl1 .ge. iso_ctls%num_iso_ctl) exit
        call find_control_end_array_flag(hd_iso_ctl,                    &
     &      iso_ctls%num_iso_ctl, i_iso_ctl2)
        if(i_iso_ctl2 .ge. iso_ctls%num_iso_ctl) exit
!
        if(right_file_flag(hd_isosurf_ctl) .gt. 0) then
          call read_file_names_from_ctl_line                            &
     &       (iso_ctls%num_iso_ctl, i_iso_ctl1, iso_ctls%fname_iso_ctl)
        else if(right_begin_flag(hd_isosurf_ctl) .gt. 0) then
          i_iso_ctl1 = i_iso_ctl1 + 1
          iso_ctls%fname_iso_ctl(i_iso_ctl1) = 'NO_FILE'
          call read_iso_control_data                                    &
     &       (hd_isosurf_ctl, iso_ctls%iso_ctl_struct(i_iso_ctl1))
!
        else if(right_file_flag(hd_isosurf_ctl) .gt. 0                  &
     &     .or. right_file_flag(hd_iso_ctl) .gt. 0) then
          call read_file_names_from_ctl_line                            &
     &       (iso_ctls%num_iso_ctl, i_iso_ctl2, iso_ctls%fname_iso_ctl)
        else if(right_begin_flag(hd_isosurf_ctl) .gt. 0                 &
     &     .or. right_begin_flag(hd_iso_ctl) .gt. 0) then
          i_iso_ctl2 = i_iso_ctl2 + 1
          iso_ctls%fname_iso_ctl(i_iso_ctl2) = 'NO_FILE'
          call read_iso_control_data                                    &
     &        (hd_iso_ctl, iso_ctls%iso_ctl_struct(i_iso_ctl2))
        end if
!
      end do
!
      end subroutine read_files_4_iso_ctl
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
        if(psf_ctls%fname_psf_ctl(i_psf) .eq. 'NO_FILE') then
          call bcast_psf_control_data(psf_ctls%psf_ctl_struct(i_psf))
        end if
      end do
!
      end subroutine bcast_files_4_psf_ctl
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
        if(iso_ctls%fname_iso_ctl(i_iso) .eq. 'NO_FILE') then
          call bcast_iso_control_data(iso_ctls%iso_ctl_struct(i_iso))
        end if
      end do
!
      end subroutine bcast_files_4_iso_ctl
!
!   --------------------------------------------------------------------
!
      end module t_control_data_sections
