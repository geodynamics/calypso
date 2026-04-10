!>@file   set_fline_control.f90
!!@brief  module set_fline_control
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for field lines
!!
!!@verbatim
!!      subroutine s_set_fline_control                                  &
!!     &         (mesh, group, nod_fld, num_tracer, tracer_prm,         &
!!     &          fline_c, fln_prm)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(in) :: num_tracer
!!      type(fieldline_paramter), intent(in) :: tracer_prm(num_tracer)
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fline_ctl), intent(inout)  :: fline_c
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!      subroutine s_set_tracer_control(init_d, rst_step, mesh, group,  &
!!     &          nod_fld, fline_c, tcr_prm)
!!        type(time_data), intent(in) :: init_d
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(tracer_module), intent(in) :: tracer
!!        type(fline_ctl), intent(inout)  :: fline_c
!!        type(fieldline_paramter), intent(inout) :: tcr_prm
!!@endverbatim
!
      module set_fline_control
!
      use m_precision
!
      use m_machine_parameter
!
      use t_time_data
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_phys_data
      use t_IO_step_parameter
      use t_control_params_4_fline
!
      implicit none
!
      private :: set_control_4_fline, set_control_4_tracer
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_fline_control                                    &
     &         (mesh, group, nod_fld, num_tracer, tracer_prm,           &
     &          fline_c, fln_prm)
!
      use t_control_data_flines
      use set_control_each_fline
      use set_control_fline_seeds
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: num_tracer
      type(fieldline_paramter), intent(in) :: tracer_prm(num_tracer)
!
      type(fline_ctl), intent(inout)  :: fline_c
      type(fieldline_paramter), intent(inout) :: fln_prm
!
      integer(kind = kint) :: i, ierr
!
!
      call count_control_4_fline(fline_c,                               &
     &    group%ele_grp, group%surf_grp, fln_prm, ierr)
      fln_prm%id_fline_direction                                        &
     &    = set_ctl_fieldline_direction(fline_c%line_direction_ctl)
!
      call count_control_fline_seeds(fline_c%seeds_ctl, fln_prm)
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr,                                    &
     &                         'Check Directory for Fieldline output')
      end if
!
      call alloc_iflag_fline_used_ele(mesh%ele, fln_prm)
      call alloc_fline_starts_ctl(fln_prm)
!
      call set_control_4_fline(fline_c, mesh, group%ele_grp,            &
     &                        nod_fld, num_tracer, tracer_prm, fln_prm)
      call deallocate_cont_dat_fline(fline_c)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'field line parameters for No.', i
        call check_control_params_fline(fln_prm)
      end if
!
      end subroutine s_set_fline_control
!
!   --------------------------------------------------------------------
!
      subroutine s_set_tracer_control(init_d, rst_step, mesh, group,    &
     &          nod_fld, fline_c, tcr_prm)
!
      use t_control_data_flines
      use set_control_each_fline
      use set_control_fline_seeds
!
      type(time_data), intent(in) :: init_d
      type(IO_step_param), intent(in) :: rst_step
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      type(fline_ctl), intent(inout)  :: fline_c
      type(fieldline_paramter), intent(inout) :: tcr_prm
!
      integer(kind = kint) :: i, ierr
!
!
      call count_control_4_fline(fline_c,                               &
     &    group%ele_grp, group%surf_grp, tcr_prm, ierr)
      tcr_prm%id_fline_direction =  iflag_forward_trace
!
      call count_control_fline_seeds(fline_c%seeds_ctl, tcr_prm)
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr,                                    &
     &                         'Check Directory for tracer output')
      end if
!
      call alloc_iflag_fline_used_ele(mesh%ele, tcr_prm)
      call alloc_fline_starts_ctl(tcr_prm)
!
      call set_control_4_tracer(fline_c, init_d, rst_step,              &
     &    mesh, group%ele_grp, nod_fld, tcr_prm)
      call deallocate_cont_dat_fline(fline_c)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'field line parameters for No.', i
        call check_control_params_fline(tcr_prm)
      end if
!
      end subroutine s_set_tracer_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_control_4_fline(fln, mesh, ele_grp, nod_fld,       &
     &                               num_tracer, tracer_prm, fln_prm)
!
      use t_ctl_data_field_line
      use t_source_of_filed_line
      use set_components_flags
      use set_area_4_viz
      use coordinate_converter
      use set_control_each_fline
      use set_control_fline_seeds
      use set_iflag_for_used_ele
!
      type(mesh_geometry), intent(in) :: mesh
      type(group_data), intent(in) :: ele_grp
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: num_tracer
      type(fieldline_paramter), intent(in) :: tracer_prm(num_tracer)
!
      type(fline_ctl), intent(in) :: fln
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
!
      fln_prm%iphys_4_fline                                             &
     &          = set_ctl_fieldline_field(fln%fline_field_ctl, nod_fld)
!
      call set_ctl_params_viz_fields(fln%fline_field_output_ctl,        &
     &                               nod_fld, fln_prm%fline_fields)
!
      call s_set_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    fln%fline_area_grp_ctl%num, fln%fline_area_grp_ctl%c_tbl,     &
     &    fln_prm%nele_grp_area_fline, fln_prm%id_ele_grp_area_fline)
!
      call s_set_control_fline_seeds(fln%seeds_ctl, fln_prm)
      call set_fline_ctl_4_tracer_seed(num_tracer, tracer_prm,          &
     &                                 fln, fln_prm)
!      call s_set_iflag_for_used_ele(mesh%ele, ele_grp,                 &
!     &    fln_prm%nele_grp_area_fline, fln_prm%id_ele_grp_area_fline,  &
!     &    fln_prm%iflag_fline_used_ele)
      call set_iflag_used_ele_w_overlap(mesh%ele, ele_grp,              &
     &    fln_prm%nele_grp_area_fline, fln_prm%id_ele_grp_area_fline,   &
     &    fln_prm%iflag_fline_used_ele)
!
      end subroutine set_control_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_tracer(fln, init_d, rst_step,            &
     &          mesh, ele_grp, nod_fld, tcr_prm)
!
      use m_base_field_labels
      use t_ctl_data_field_line
      use t_source_of_filed_line
      use calypso_mpi_logical
      use set_components_flags
      use set_area_4_viz
      use coordinate_converter
      use set_control_platform_data
      use set_control_each_fline
      use set_control_fline_seeds
      use set_iflag_for_used_ele
      use particle_MPI_IO_select
!
      type(time_data), intent(in) :: init_d
      type(IO_step_param), intent(in) :: rst_step
      type(mesh_geometry), intent(in) :: mesh
      type(group_data), intent(in) :: ele_grp
      type(phys_data), intent(in) :: nod_fld
!
      type(fline_ctl), intent(in) :: fln
!
      type(fieldline_paramter), intent(inout) :: tcr_prm
!
      integer(kind = kint) :: istep_rst
      logical :: bflag_lc, bflag_gl
!
!
      tcr_prm%iphys_4_fline                                             &
     &          = set_ctl_field_to_trace(velocity%name, nod_fld)
!
      call set_ctl_parallel_file_w_def(default_tracer_prefix,           &
     &    fln%tracer_rst_prefix_ctl, fln%tracer_rst_format_ctl,         &
     &    tcr_prm%tracer_rst_IO)
!
      istep_rst = set_IO_step(init_d%i_time_step, rst_step)
      bflag_lc = check_particle_file_exist(tcr_prm%tracer_rst_IO,       &
     &                                     istep_rst)
      call calypso_mpi_allreduce_one_bin(bflag_lc, bflag_gl, MPI_LAND)
      if(bflag_gl) tcr_prm%id_fline_seed_type = iflag_read_reastart
!
      call set_control_tracer_density(fln, nod_fld, tcr_prm)
!
      call set_ctl_params_viz_fields(fln%fline_field_output_ctl,        &
     &                               nod_fld, tcr_prm%fline_fields)
!
      call s_set_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    fln%fline_area_grp_ctl%num, fln%fline_area_grp_ctl%c_tbl,     &
     &    tcr_prm%nele_grp_area_fline, tcr_prm%id_ele_grp_area_fline)
!
!      call s_set_iflag_for_used_ele(mesh%ele, ele_grp,                 &
!     &    tcr_prm%nele_grp_area_fline, tcr_prm%id_ele_grp_area_fline,  &
!     &    tcr_prm%iflag_fline_used_ele)
      call set_iflag_used_ele_w_overlap(mesh%ele, ele_grp,              &
     &    tcr_prm%nele_grp_area_fline, tcr_prm%id_ele_grp_area_fline,   &
     &    tcr_prm%iflag_fline_used_ele)
!
      call s_set_control_fline_seeds(fln%seeds_ctl,  tcr_prm)
!
      end subroutine set_control_4_tracer
!
!  ---------------------------------------------------------------------
!
      end module set_fline_control
