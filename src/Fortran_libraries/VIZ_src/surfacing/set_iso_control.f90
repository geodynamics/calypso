!>@file   set_iso_control.f90
!!@brief  module set_iso_control
!!
!!@author H. Matsui
!!@date Programmed in May., 2006
!!@n    Modified in  June, 1015
!
!>@brief Structure for parallel sectioned data
!!
!!@verbatim
!!      subroutine s_set_iso_control(num_iso, group, nod_fld,           &
!!     &          iso_ctls, iso_param, iso_def, iso_mesh, iso_file_IO)
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!        type(psf_parameters), intent(inout) :: iso_param(num_iso)
!!        type(isosurface_define), intent(inout) :: iso_def(num_iso)
!!        type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!!        type(field_IO_params), intent(inout) :: iso_file_IO(num_iso)
!!@endverbatim
!
      module set_iso_control
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_group_data
      use t_control_data_4_iso
      use t_control_params_4_iso
      use t_phys_data
      use t_psf_patch_data
      use t_file_IO_parameter
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                      :: default_iso_prefix = 'iso'
!
      private :: count_control_4_iso, set_control_4_iso
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_iso_control(num_iso, group, nod_fld,             &
     &          iso_ctls, iso_param, iso_def, iso_mesh, iso_file_IO)
!
      use t_read_control_elements
      use t_control_data_isosurfaces
      use t_control_data_sections
!
      use set_field_comp_for_viz
      use mpi_abort_by_missing_zlib
!
      integer(kind= kint), intent(in) :: num_iso
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      type(isosurf_controls), intent(inout) :: iso_ctls
      type(psf_parameters), intent(inout) :: iso_param(num_iso)
      type(isosurface_define), intent(inout) :: iso_def(num_iso)
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
      type(field_IO_params), intent(inout) :: iso_file_IO(num_iso)
!
      integer(kind = kint) :: i, ierr
!
!
      ierr = 0
      do i = 1, num_iso
        call count_control_4_iso(my_rank, iso_ctls%iso_ctl_struct(i),   &
     &      group%ele_grp, nod_fld%num_phys, nod_fld%phys_name,         &
     &      iso_mesh(i)%field, iso_param(i), iso_def(i),                &
     &      iso_file_IO(i), ierr)
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
        call mpi_abort_by_no_zlib_in_fld(iso_file_IO(i)%file_prefix,    &
     &                                   iso_file_IO(i)%iflag_format)
      end do
!
      do i = 1, num_iso
        call alloc_phys_name(iso_mesh(i)%field)
        call set_control_4_iso(iso_ctls%iso_ctl_struct(i),              &
     &      group%ele_grp, nod_fld%num_phys, nod_fld%phys_name,         &
     &      iso_mesh(i)%field, iso_param(i), iso_def(i))
        call dealloc_cont_dat_4_iso(iso_ctls%iso_ctl_struct(i))
!
        call count_total_comps_4_viz(iso_mesh(i)%field)
      end do
!
      call dealloc_iso_ctl_stract(iso_ctls)
!
!
      if(iflag_debug .gt. 0) then
        do i = 1, num_iso
          write(*,*) 'id_isosurf_data', i,                              &
     &        iso_def(i)%id_isosurf_data, iso_def(i)%id_isosurf_comp
        end do
      end if
!
      end subroutine s_set_iso_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_control_4_iso                                    &
     &         (id_rank, iso_c, ele_grp, num_nod_phys, phys_nod_name,   &
     &          iso_fld, iso_param, iso_def, iso_file_IO, ierr)
!
      use m_error_IDs
      use m_file_format_switch
      use set_field_comp_for_viz
!
      use set_isosurface_file_ctl
      use delete_data_files
      use skip_comment_f
!
      type(group_data), intent(in) :: ele_grp
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(iso_ctl), intent(in) :: iso_c
      type(phys_data), intent(inout) :: iso_fld
      type(psf_parameters), intent(inout) :: iso_param
      type(isosurface_define), intent(inout) :: iso_def
      type(field_IO_params), intent(inout) :: iso_file_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!
      ierr = 0
      call s_set_isosurface_file_ctl(default_iso_prefix,                &
     &    iso_c%iso_file_head_ctl, iso_c%iso_output_type_ctl,           &
     &    iso_file_IO)
      if((iso_file_IO%iflag_format/iflag_single) .eq. 0) then
        iso_file_IO%iflag_format = iso_file_IO%iflag_format             &
     &                            + iflag_single
      end if
!
      if(check_file_writable(id_rank, iso_file_IO%file_prefix)          &
     &                                             .eqv. .FALSE.) then
        ierr = ierr_VIZ
        return
      end if
!
      call count_control_iso_def(iso_c%iso_def_c, ele_grp, iso_param)
!
      call count_control_4_field_on_iso                                 &
     &   (iso_c%fld_on_iso_c, num_nod_phys, phys_nod_name,              &
     &    iso_fld, iso_def)
!
      end subroutine count_control_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_iso                                      &
     &         (iso_c, ele_grp, num_nod_phys, phys_nod_name,            &
     &          iso_fld, iso_param, iso_def)
!
      use m_error_IDs
!
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(iso_ctl), intent(in) :: iso_c
      type(phys_data), intent(inout) :: iso_fld
      type(psf_parameters), intent(inout) :: iso_param
      type(isosurface_define), intent(inout) :: iso_def
!
      integer(kind = kint) :: ierr
!
!
      call alloc_area_group_psf(iso_param)
      call set_control_iso_def(iso_c%iso_def_c, ele_grp,                &
     &    num_nod_phys, phys_nod_name, iso_param, iso_def, ierr)
      if(ierr .gt. 0) then
        call calypso_MPI_abort(ierr_VIZ, 'set scalar for rendering')
      end if
!
!
      call set_control_4_field_on_iso                                   &
     &   (iso_c%fld_on_iso_c, num_nod_phys, phys_nod_name,              &
     &    iso_fld, iso_param, iso_def)
!
      end subroutine set_control_4_iso
!
!  ---------------------------------------------------------------------
!
      end module set_iso_control
