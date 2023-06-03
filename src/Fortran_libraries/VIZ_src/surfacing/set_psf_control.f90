!>@file   set_psf_control.f90
!!@brief  module set_psf_control
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
!!      subroutine s_set_psf_control(num_psf, group, nod_fld,           &
!!     &          psf_ctls, psf_param, psf_def, psf_mesh, psf_file_IO)
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(section_controls), intent(inout) :: psf_ctls
!!        type(psf_parameters), intent(inout) :: psf_param(num_psf)
!!        type(section_define), intent(inout) :: psf_def(num_psf)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!@endverbatim
!
      module set_psf_control
!
      use m_precision
      use m_machine_parameter
!
      use t_control_data_4_psf
      use t_control_params_4_psf
      use t_file_IO_parameter
      use t_mesh_data
      use t_group_data
      use t_phys_data
      use t_psf_patch_data
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                      :: default_psf_prefix = 'psf'
!
      private :: count_control_4_psf, set_control_4_psf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_field_name(num_psf, psf_mesh)
!
      integer(kind = kint), intent(in) :: num_psf
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call dealloc_phys_name(psf_mesh(i)%field)
      end do
!
      end subroutine dealloc_psf_field_name
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_psf_control(num_psf, group, nod_fld,             &
     &          psf_ctls, psf_param, psf_def, psf_mesh, psf_file_IO)
!
      use calypso_mpi
      use t_read_control_elements
      use t_control_data_sections
!
      use set_field_comp_for_viz
      use mpi_abort_by_missing_zlib
!
      integer(kind= kint), intent(in) :: num_psf
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      type(section_controls), intent(inout) :: psf_ctls
      type(psf_parameters), intent(inout) :: psf_param(num_psf)
      type(section_define), intent(inout) :: psf_def(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
      type(field_IO_params), intent(inout)  :: psf_file_IO(num_psf)
!
      integer(kind = kint) :: i, ierr
!
!
      do i = 1, num_psf
        call count_control_4_psf(my_rank, psf_ctls%psf_ctl_struct(i),   &
     &      group%ele_grp, nod_fld%num_phys, nod_fld%phys_name,         &
     &      psf_mesh(i)%field, psf_param(i), psf_file_IO(i), ierr)
!
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
        call mpi_abort_by_no_zlib_in_fld(psf_file_IO(i)%file_prefix,    &
     &                                   psf_file_IO(i)%iflag_format)
      end do
!
      do i = 1, num_psf
        call alloc_phys_name(psf_mesh(i)%field)
        call set_control_4_psf                                          &
     &     (psf_ctls%psf_ctl_struct(i), group%ele_grp, group%surf_grp,  &
     &      nod_fld%num_phys, nod_fld%phys_name,                        &
     &      psf_mesh(i)%field,  psf_param(i), psf_def(i), ierr)
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
!
        call dealloc_cont_dat_4_psf(psf_ctls%psf_ctl_struct(i))
!
        call count_total_comps_4_viz(psf_mesh(i)%field)
      end do
!
      call dealloc_psf_ctl_stract(psf_ctls)
!
      end subroutine s_set_psf_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_control_4_psf                                    &
     &         (id_rank, psf_c, ele_grp, num_nod_phys, phys_nod_name,   &
     &          psf_fld, psf_param, psf_file_IO, ierr)
!
      use m_error_IDs
      use m_file_format_switch
      use set_area_4_viz
      use set_field_comp_for_viz
      use set_sections_file_ctl
      use delete_data_files
!
      type(group_data), intent(in) :: ele_grp
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(psf_ctl), intent(in) :: psf_c
      type(phys_data), intent(inout) :: psf_fld
      type(psf_parameters), intent(inout) :: psf_param
      type(field_IO_params), intent(inout) :: psf_file_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      call s_set_sections_file_ctl(default_psf_prefix,                  &
     &    psf_c%psf_file_head_ctl, psf_c%psf_output_type_ctl,           &
     &    psf_file_IO)
      if((psf_file_IO%iflag_format/iflag_single) .eq. 0) then
        psf_file_IO%iflag_format = psf_file_IO%iflag_format             &
     &                            + iflag_single
      end if
!
      if(check_file_writable(id_rank, psf_file_IO%file_prefix)          &
     &                                             .eqv. .FALSE.) then
        ierr = ierr_VIZ
        return
      end if
!
      call count_control_4_field_on_psf                                 &
     &   (psf_c%fld_on_psf_c, num_nod_phys, phys_nod_name, psf_fld)
      call count_control_4_psf_define                                   &
     &   (psf_c%psf_def_c, ele_grp, psf_param, ierr)
!
      end subroutine count_control_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_psf                                      &
     &         (psf_c, ele_grp, sf_grp, num_nod_phys, phys_nod_name,    &
     &          psf_fld, psf_param, psf_def, ierr)
!
      use m_error_IDs
      use set_sections_file_ctl
!
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
      type(psf_ctl), intent(in) :: psf_c
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(phys_data), intent(inout) :: psf_fld
      type(psf_parameters), intent(inout) :: psf_param
      type(section_define), intent(inout) :: psf_def
      integer(kind = kint), intent(inout) :: ierr
!
!
      call alloc_area_group_psf(psf_param)
      call set_control_psf_define                                       &
     &   (psf_c%psf_def_c, ele_grp, sf_grp, psf_param, psf_def, ierr)
!
      call alloc_output_comps_psf(psf_fld%num_phys, psf_param)
      call set_control_4_field_on_psf(psf_c%fld_on_psf_c,               &
     &    num_nod_phys, phys_nod_name,  psf_fld, psf_param)
!
      end subroutine set_control_4_psf
!
!  ---------------------------------------------------------------------
!
      end module set_psf_control
