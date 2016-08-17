!>@file   set_psf_iso_control.f90
!!@brief  module set_psf_iso_control
!!
!!@author H. Matsui
!!@date Programmed in May., 2006
!!@n    Modified in  June, 1015
!
!>@brief Structure for parallel sectioned data
!!
!!@verbatim
!!      subroutine dealloc_psf_field_name(num_psf, psf_mesh)
!!      subroutine set_psf_control(num_psf, num_mat, mat_name,          &
!!     &          num_surf, surf_name, num_nod_phys, phys_nod_name,     &
!!     &          psf_param, psf_mesh)
!!      subroutine set_iso_control                                      &
!!     &         (num_iso, num_mat, mat_name, num_nod_phys,             &
!!     &          phys_nod_name, iso_param, iso_mesh)
!!@endverbatim
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
      subroutine dealloc_psf_field_name(num_psf, psf_mesh)
!
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_psf
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call dealloc_phys_name_type(psf_mesh(i)%field)
      end do
!
      end subroutine dealloc_psf_field_name
!
!  ---------------------------------------------------------------------
!
      subroutine set_psf_control(num_psf, num_mat, mat_name,            &
     &          num_surf, surf_name, num_nod_phys, phys_nod_name,       &
     &          psf_param, psf_mesh)
!
      use calypso_mpi
      use m_control_data_sections
      use m_control_data_4_psf
      use m_control_params_4_psf
      use m_read_control_elements
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
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i_psf, ierr
!
!
      call allocate_control_params_4_psf(num_psf)
!
      ctl_file_code = psf_ctl_file_code
      do i_psf = 1, num_psf
        call read_control_4_psf(i_psf)
      end do
!
      do i_psf = 1, num_psf
        call count_control_4_psf(i_psf, psf_ctl_struct(i_psf),          &
     &      num_mat, mat_name, num_nod_phys, phys_nod_name,             &
     &      psf_mesh(i_psf)%field, psf_param(i_psf), ierr)
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
      end do
!
      do i_psf = 1, num_psf
        call alloc_phys_name_type(psf_mesh(i_psf)%field)
        call set_control_4_psf                                          &
     &     (i_psf, psf_ctl_struct(i_psf), num_mat, mat_name,            &
     &      num_surf, surf_name, num_nod_phys, phys_nod_name,           &
     &      psf_mesh(i_psf)%field, psf_param(i_psf), ierr)
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
!
        call deallocate_cont_dat_4_psf(psf_ctl_struct(i_psf))
!
        call count_total_comps_4_viz(psf_mesh(i_psf)%field)
      end do
!
      call deallocate_psf_ctl_stract
!
!
      end subroutine set_psf_control
!
!   --------------------------------------------------------------------
!
      subroutine set_iso_control                                        &
     &         (num_iso, num_mat, mat_name, num_nod_phys,               &
     &          phys_nod_name, iso_param, iso_mesh)
!
      use calypso_mpi
      use m_control_data_sections
      use m_control_data_4_iso
      use m_control_params_4_iso
      use m_read_control_elements
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
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!
      integer(kind = kint) :: i
!
!
      call allocate_control_params_4_iso(num_iso)
!
      ctl_file_code = iso_ctl_file_code
      do i = 1, num_iso
        call read_control_4_iso(i)
      end do
!
      do i = 1, num_iso
        call count_control_4_iso(i, iso_ctl_struct(i),                  &
     &      num_mat, mat_name, num_nod_phys, phys_nod_name,             &
     &      iso_mesh(i)%field, iso_param(i))
      end do
!
      do i = 1, num_iso
        call alloc_phys_name_type(iso_mesh(i)%field)
        call set_control_4_iso(i, iso_ctl_struct(i),                    &
     &      num_mat, mat_name, num_nod_phys, phys_nod_name,             &
     &      iso_mesh(i)%field, iso_param(i))
        call deallocate_cont_dat_4_iso(iso_ctl_struct(i))
!
        call count_total_comps_4_viz(iso_mesh(i)%field)
      end do
!
      call deallocate_iso_ctl_stract
!
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
