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
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!      subroutine set_psf_control(num_psf, ele_grp, sf_grp,            &
!!     &          nod_fld, psf_param, psf_mesh)
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data), intent(in) :: nod_fld
!!        type(psf_parameters), intent(inout) :: psf_param(num_psf)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!      subroutine set_iso_control                                      &
!!     &         (num_iso, ele_grp, nod_fld, iso_param, iso_mesh)
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
      private :: hd_section_ctl, hd_psf_ctl
      private :: hd_isosurf_ctl, hd_iso_ctl
!
      private :: psf_ctl_file_code, iso_ctl_file_code
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
      subroutine set_psf_control(num_psf, ele_grp, sf_grp,              &
     &          nod_fld, psf_param, psf_mesh)
!
      use calypso_mpi
      use m_control_data_sections
      use m_control_params_4_psf
      use m_read_control_elements
      use t_group_data
      use t_phys_data
      use t_control_data_4_psf
      use t_psf_patch_data
!
      use set_field_comp_for_viz
!
      integer(kind= kint), intent(in) :: num_psf
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data), intent(in) :: nod_fld
!
      type(psf_parameters), intent(inout) :: psf_param(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i_psf, ierr
!
!
      do i_psf = 1, num_psf
        call count_control_4_psf(i_psf, psf_ctl_struct(i_psf),          &
     &      ele_grp%num_grp, ele_grp%grp_name,                          &
     &      nod_fld%num_phys, nod_fld%phys_name,                        &
     &      psf_mesh(i_psf)%field, psf_param(i_psf), ierr)
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
      end do
!
      do i_psf = 1, num_psf
        call alloc_phys_name_type(psf_mesh(i_psf)%field)
        call set_control_4_psf(i_psf, psf_ctl_struct(i_psf),            &
     &      ele_grp%num_grp, ele_grp%grp_name,                          &
     &      sf_grp%num_grp, sf_grp%grp_name,                            &
     &      nod_fld%num_phys, nod_fld%phys_name,                        &
     &      psf_mesh(i_psf)%field,  psf_param(i_psf), ierr)
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
!
        call deallocate_cont_dat_4_psf(psf_ctl_struct(i_psf))
!
        call count_total_comps_4_viz(psf_mesh(i_psf)%field)
      end do
!
      call deallocate_psf_ctl_stract
!
      end subroutine set_psf_control
!
!   --------------------------------------------------------------------
!
      subroutine set_iso_control                                        &
     &         (num_iso, ele_grp, nod_fld, iso_param, iso_mesh)
!
      use calypso_mpi
      use m_control_data_sections
      use m_control_params_4_iso
      use m_read_control_elements
      use t_group_data
      use t_phys_data
      use t_control_data_4_iso
      use t_psf_patch_data
!
      use set_field_comp_for_viz
!
      integer(kind= kint), intent(in) :: num_iso
      type(group_data), intent(in) :: ele_grp
      type(phys_data), intent(in) :: nod_fld
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
     &      ele_grp%num_grp, ele_grp%grp_name,                          &
     &      nod_fld%num_phys, nod_fld%phys_name,                        &
     &      iso_mesh(i)%field, iso_param(i))
      end do
!
      do i = 1, num_iso
        call alloc_phys_name_type(iso_mesh(i)%field)
        call set_control_4_iso(i, iso_ctl_struct(i),                    &
     &      ele_grp%num_grp, ele_grp%grp_name,                          &
     &      nod_fld%num_phys, nod_fld%phys_name,                        &
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
      use calypso_mpi
      use m_read_control_elements
!
      use t_control_data_4_psf
      use m_control_params_4_psf
      use m_control_data_sections
!
!
      integer(kind = kint), intent(in) :: i_psf
!
!
      if(fname_psf_ctl(i_psf) .eq. 'NO_FILE') return
!
      if(my_rank .eq. 0) then
        ctl_file_code = psf_ctl_file_code
        open(ctl_file_code, file=fname_psf_ctl(i_psf), status='old')
!
        call load_ctl_label_and_line
        call read_psf_control_data                                      &
     &     (hd_section_ctl, psf_ctl_struct(i_psf))
        call read_psf_control_data(hd_psf_ctl, psf_ctl_struct(i_psf))
!
        close(ctl_file_code)
      end if
!
      call bcast_psf_control_data(psf_ctl_struct(i_psf))
!
      end subroutine read_control_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_iso(i_iso)
!
      use calypso_mpi
      use m_read_control_elements
!
      use t_control_data_4_iso
      use m_control_params_4_iso
      use m_control_data_sections
!
      integer(kind = kint), intent(in) :: i_iso
!
!
      if(fname_iso_ctl(i_iso) .eq. 'NO_FILE') return
!
      if(my_rank .eq. 0) then
        ctl_file_code = iso_ctl_file_code
        open(ctl_file_code, file=fname_iso_ctl(i_iso), status='old')
!
        call load_ctl_label_and_line
        call read_iso_control_data                                      &
     &     (hd_isosurf_ctl, iso_ctl_struct(i_iso))
        call read_iso_control_data(hd_iso_ctl, iso_ctl_struct(i_iso))
        close(ctl_file_code)
      end if
!
      call bcast_iso_control_data(iso_ctl_struct(i_iso))
!
      end subroutine read_control_4_iso
!
!  ---------------------------------------------------------------------
!
      end module set_psf_iso_control
