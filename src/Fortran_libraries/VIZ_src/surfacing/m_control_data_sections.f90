!
!      module m_control_data_sections
!
!      Written by H. Matsui on July, 2006
!
!      subroutine allocate_psf_ctl_stract
!      subroutine allocate_iso_ctl_stract
!
!      subroutine deallocate_psf_ctl_stract
!      subroutine deallocate_iso_ctl_stract
!
!      subroutine read_sections_control_data
!
!      subroutine read_files_4_psf_ctl
!      subroutine read_files_4_iso_ctl
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
      module m_control_data_sections
!
      use m_precision
!
      use m_machine_parameter
      use m_control_data_4_psf
      use m_control_data_4_iso
!
      implicit  none
!
!
      integer(kind = kint) :: num_psf_ctl = 0
      character(len = kchara), allocatable :: fname_psf_ctl(:)
      type(psf_ctl), allocatable, save :: psf_ctl_struct(:)
!
      integer(kind = kint) :: num_iso_ctl = 0
      character(len = kchara), allocatable :: fname_iso_ctl(:)
      type(iso_ctl), allocatable, save :: iso_ctl_struct(:)
!
!   entry label
!
      character(len=kchara), parameter :: hd_viz_ctl = 'visual_control'
      integer (kind=kint) :: i_viz_ctl = 0
!
      private :: hd_viz_ctl, i_viz_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_psf_ctl_stract
!
      integer(kind = kint) :: i
!
      allocate(fname_psf_ctl(num_psf_ctl))
      allocate(psf_ctl_struct(num_psf_ctl))
!
      if(num_psf_ctl .gt. 0) then
        do i = 1, num_psf_ctl
          psf_ctl_struct(i)%radius_psf_ctl%realvalue = 0.0d0
          psf_ctl_struct(i)%psf_out_field_ctl%num = 0
          psf_ctl_struct(i)%psf_area_ctl%num =      0
        end do
      end if
!
      end subroutine allocate_psf_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_iso_ctl_stract
!
!
      allocate(fname_iso_ctl(num_iso_ctl))
      allocate(iso_ctl_struct(num_iso_ctl))
!
      end subroutine allocate_iso_ctl_stract
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_psf_ctl_stract
!
      deallocate(psf_ctl_struct)
      deallocate(fname_psf_ctl)
!
      end subroutine deallocate_psf_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_iso_ctl_stract
!
      deallocate(iso_ctl_struct)
      deallocate(fname_iso_ctl)
!
      end subroutine deallocate_iso_ctl_stract
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_sections_control_data
!
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_viz_ctl) .eq. 0) return
      if (i_viz_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_viz_ctl, i_viz_ctl)
        if(i_viz_ctl .eq. 1) exit
!
        call find_control_array_flag(hd_psf_ctl, num_psf_ctl)
        if(num_psf_ctl .gt. 0) call read_files_4_psf_ctl
        call find_control_array_flag(hd_section_ctl, num_psf_ctl)
        if(num_psf_ctl .gt. 0) call read_files_4_psf_ctl
!
        call find_control_array_flag(hd_iso_ctl, num_iso_ctl)
        if(num_iso_ctl .gt. 0) call read_files_4_iso_ctl
        call find_control_array_flag(hd_isosurf_ctl, num_iso_ctl)
        if(num_iso_ctl .gt. 0) call read_files_4_iso_ctl
      end do
!
      end subroutine read_sections_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_files_4_psf_ctl
!
      use m_read_control_elements
      use skip_comment_f
!
      integer (kind=kint) :: i_psf_ctl1 = 0, i_psf_ctl2 = 0
!
!
      if((i_psf_ctl1+i_psf_ctl2) .gt. 0) return
!
      call allocate_psf_ctl_stract
      do
        call load_ctl_label_and_line
!
        call find_control_end_array_flag(hd_psf_ctl,                    &
     &      num_psf_ctl, i_psf_ctl2)
        if(i_psf_ctl2 .ge. num_psf_ctl) exit
        call find_control_end_array_flag(hd_section_ctl,                &
     &      num_psf_ctl, i_psf_ctl1)
        if(i_psf_ctl1 .ge. num_psf_ctl) exit
!
        if(right_file_flag(hd_section_ctl) .gt. 0) then
          call read_file_names_from_ctl_line(num_psf_ctl, i_psf_ctl1,   &
     &        fname_psf_ctl)
        else if(right_begin_flag(hd_section_ctl) .gt. 0) then
          i_psf_ctl1 = i_psf_ctl1 + 1
          fname_psf_ctl(i_psf_ctl1) = 'NO_FILE'
          call read_psf_control_data(psf_ctl_struct(i_psf_ctl1))
!
        else if(right_file_flag(hd_psf_ctl) .gt. 0) then
          call read_file_names_from_ctl_line(num_psf_ctl, i_psf_ctl2,   &
     &        fname_psf_ctl)
        else if(right_begin_flag(hd_psf_ctl) .gt. 0) then
          i_psf_ctl2 = i_psf_ctl2 + 1
          fname_psf_ctl(i_psf_ctl2) = 'NO_FILE'
          call read_psf_control_data(psf_ctl_struct(i_psf_ctl2))
        end if
      end do
!
      end subroutine read_files_4_psf_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_files_4_iso_ctl
!
      use m_read_control_elements
      use skip_comment_f
!
      integer (kind=kint) :: i_iso_ctl1 = 0, i_iso_ctl2 = 0
!
!
      if ((i_iso_ctl1+i_iso_ctl2) .gt. 0) return
!
      call allocate_iso_ctl_stract
      do
        call load_ctl_label_and_line
!
        call find_control_end_array_flag(hd_isosurf_ctl,                &
     &      num_iso_ctl, i_iso_ctl1)
        if(i_iso_ctl1 .ge. num_iso_ctl) exit
        call find_control_end_array_flag(hd_iso_ctl,                    &
     &      num_iso_ctl, i_iso_ctl2)
        if(i_iso_ctl2 .ge. num_iso_ctl) exit
!
        if(right_file_flag(hd_isosurf_ctl) .gt. 0) then
          call read_file_names_from_ctl_line(num_iso_ctl, i_iso_ctl1,   &
     &        fname_iso_ctl)
        else if(right_begin_flag(hd_isosurf_ctl) .gt. 0) then
          i_iso_ctl1 = i_iso_ctl1 + 1
          fname_iso_ctl(i_iso_ctl1) = 'NO_FILE'
          call read_control_data_4_iso(iso_ctl_struct(i_iso_ctl1))
!
        else if(right_file_flag(hd_isosurf_ctl) .gt. 0                  &
     &     .or. right_file_flag(hd_iso_ctl) .gt. 0) then
          call read_file_names_from_ctl_line(num_iso_ctl, i_iso_ctl2,   &
     &        fname_iso_ctl)
        else if(right_begin_flag(hd_isosurf_ctl) .gt. 0                 &
     &     .or. right_begin_flag(hd_iso_ctl) .gt. 0) then
          i_iso_ctl2 = i_iso_ctl2 + 1
          fname_iso_ctl(i_iso_ctl2) = 'NO_FILE'
          call read_control_data_4_iso(iso_ctl_struct(i_iso_ctl2))
        end if
!
      end do
!
      end subroutine read_files_4_iso_ctl
!
!   --------------------------------------------------------------------
!
      end module m_control_data_sections
