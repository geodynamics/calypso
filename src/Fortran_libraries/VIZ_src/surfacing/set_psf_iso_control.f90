!set_psf_iso_control.f90
!      module set_psf_iso_control
!
!     Written by H. Matsui on May., 2006
!
!!      subroutine set_psf_control(num_psf, num_mat, mat_name,          &
!!     &          num_surf, surf_name, num_nod_phys, phys_nod_name,     &
!!     &          psf_param, psf_fld, psf_pat)
!!      subroutine set_iso_control                                      &
!!     &         (num_iso, num_mat, mat_name, num_nod_phys,             &
!!     &          phys_nod_name, iso_param, iso_fld, iso_pat)
!
      module set_psf_iso_control
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      integer(kind = kint), parameter :: psf_ctl_file_code = 11
      integer(kind = kint), parameter :: iso_ctl_file_code = 11
!
      private :: psf_ctl_file_code, iso_ctl_file_code
      private :: read_control_4_psf, read_control_4_iso
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_psf_control(num_psf, num_mat, mat_name,            &
     &          num_surf, surf_name, num_nod_phys, phys_nod_name,       &
     &          psf_param, psf_fld, psf_pat)
!
      use calypso_mpi
      use m_control_data_sections
      use m_control_data_4_psf
      use m_control_params_4_psf
      use m_read_control_elements
      use t_phys_data
      use t_psf_patch_data
!
      use set_field_comp_for_viz
!
      integer(kind= kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_surf
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(psf_parameters), intent(inout) :: psf_param(num_psf)
      type(phys_data), intent(inout) :: psf_fld(num_psf)
      type(psf_patch_data), intent(inout) :: psf_pat
!
      integer(kind = kint) :: i_psf, ierr
!
!
      ctl_file_code = psf_ctl_file_code
!
      call allocate_control_params_4_psf(num_psf)
      call allocate_psf_ctl_stract
!
      do i_psf = 1, num_psf
        call read_control_4_psf(i_psf)
      end do
!
      do i_psf = 1, num_psf
        call count_control_4_psf(i_psf, psf_ctl_struct(i_psf),          &
     &      num_mat, mat_name, num_nod_phys, phys_nod_name,             &
     &      psf_fld(i_psf), psf_param(i_psf), ierr)
        if(ierr.gt.0) call calypso_MPI_abort                            &
     &                   (ierr, 'set correct element group')
      end do
!
      do i_psf = 1, num_psf
        call alloc_phys_name_type(psf_fld(i_psf))
        call set_control_4_psf                                          &
     &     (i_psf, psf_ctl_struct(i_psf), num_mat, mat_name,            &
     &      num_surf, surf_name, num_nod_phys, phys_nod_name,           &
     &      psf_fld(i_psf), psf_param(i_psf))
        call deallocate_cont_dat_4_psf(psf_ctl_struct(i_psf))
      end do
!
      call deallocate_psf_file_header_ctl
!
      call count_total_comps_4_viz                                      &
     &   (num_psf, psf_fld, psf_pat%max_ncomp_psf)
!
      end subroutine set_psf_control
!
!   --------------------------------------------------------------------
!
      subroutine set_iso_control                                        &
     &         (num_iso, num_mat, mat_name, num_nod_phys,               &
     &          phys_nod_name, iso_param, iso_fld, iso_pat)
!
      use calypso_mpi
      use m_control_data_sections
      use m_control_data_4_iso
      use m_control_params_4_iso
      use m_read_control_elements
      use t_phys_data
      use t_psf_patch_data
!
      use set_field_comp_for_viz
!
      integer(kind= kint), intent(in) :: num_iso
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(psf_parameters), intent(inout) :: iso_param(num_iso)
      type(phys_data), intent(inout) :: iso_fld(num_iso)
      type(psf_patch_data), intent(inout) :: iso_pat
!
      integer(kind = kint) :: i
!
!
      ctl_file_code = iso_ctl_file_code
!
      call allocate_control_params_4_iso(num_iso)
      call allocate_iso_ctl_stract
!
      do i = 1, num_iso
        call read_control_4_iso(i)
      end do
!
      do i = 1, num_iso
        call count_control_4_iso(i, iso_ctl_struct(i),                  &
     &      num_mat, mat_name, num_nod_phys, phys_nod_name,             &
     &      iso_fld(i), iso_param(i))
      end do
!
      do i = 1, num_iso
        call alloc_phys_name_type(iso_fld(i))
        call set_control_4_iso(i, iso_ctl_struct(i),                    &
     &      num_mat, mat_name, num_nod_phys, phys_nod_name,             &
     &      iso_fld(i), iso_param(i))
        call deallocate_cont_dat_4_iso(iso_ctl_struct(i))
      end do
!
      call deallocate_iso_file_header_ctl
!
      call count_total_comps_4_viz                                      &
     &   (num_iso, iso_fld, iso_pat%max_ncomp_psf)
!
      if(iflag_debug .gt. 0) then
        do i = 1, num_iso
          write(*,*) 'id_isosurf_data', i,                              &
     &        id_isosurf_data(i), id_isosurf_comp(i)
        end do
      end if
!
      end subroutine set_iso_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_4_psf(i_psf)
!
      use m_read_control_elements
!
      use m_control_data_4_psf
      use m_control_params_4_psf
      use m_control_data_sections
!
!
      integer(kind = kint), intent(in) :: i_psf
!
!
      if(fname_psf_ctl(i_psf) .eq. 'NO_FILE') return
!
      open(psf_ctl_file_code, file=fname_psf_ctl(i_psf), status='old')
      call read_control_data_4_psf(psf_ctl_struct(i_psf))
      close(psf_ctl_file_code)
!
      end subroutine read_control_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_iso(i_iso)
!
      use m_read_control_elements
!
      use m_control_data_4_iso
      use m_control_params_4_iso
      use m_control_data_sections
!
      integer(kind = kint), intent(in) :: i_iso
!
!
      if(fname_iso_ctl(i_iso) .eq. 'NO_FILE') return
!
      open(iso_ctl_file_code, file=fname_iso_ctl(i_iso), status='old')
      call read_control_data_4_iso(iso_ctl_struct(i_iso))
      close(iso_ctl_file_code)
!
      end subroutine read_control_4_iso
!
!  ---------------------------------------------------------------------
!
      end module set_psf_iso_control
