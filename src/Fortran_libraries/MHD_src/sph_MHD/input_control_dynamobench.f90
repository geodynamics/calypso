!>@file   input_control_dynamobench.f90
!!@brief  module input_control_dynamobench
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine input_control_SPH_dynamobench                        &
!!     &          (MHD_files, bc_IO, refs, DMHD_ctl, SPH_MHD, nod_fld,  &
!!     &           MHD_step, MHD_prop, MHD_BC, SPH_WK, cdat, bench)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(boundary_spectra), intent(inout) :: bc_IO
!!        type(radial_reference_field), intent(inout) :: refs
!!        type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
!!        type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(MHD_BC_lists), intent(inout) :: MHD_BC
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(circle_fld_maker), intent(inout) :: cdat
!!        type(dynamobench_monitor), intent(inout) :: bench
!!@endverbatim
!
!
      module input_control_dynamobench
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_const_spherical_grid
      use t_MHD_file_parameter
      use t_MHD_step_parameter
      use t_SPH_mesh_field_array
      use t_spheric_parameter
      use t_mesh_data
      use t_phys_data
      use t_spheric_group
      use t_rms_4_sph_spectr
      use t_file_IO_parameter
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_SPH_mesh_field_data
      use t_flex_delta_t_data
      use t_field_4_dynamobench
      use t_work_SPH_MHD
      use t_radial_reference_field
!
      implicit none
!
      private :: set_ctl_params_dynamobench
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_dynamobench                          &
     &          (MHD_files, bc_IO, refs, DMHD_ctl, SPH_MHD, nod_fld,    &
     &           MHD_step, MHD_prop, MHD_BC, SPH_WK, cdat, bench)
!
      use t_ctl_data_MHD
      use t_field_on_circle
      use set_control_sph_mhd
      use set_control_sph_data_MHD
      use parallel_load_data_4_sph
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(boundary_spectra), intent(inout) :: bc_IO
      type(radial_reference_field), intent(inout) :: refs
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
      type(phys_data), intent(inout) :: nod_fld
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(circle_fld_maker), intent(inout) :: cdat
      type(dynamobench_monitor), intent(inout) :: bench
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD                                        &
     &   (DMHD_ctl%plt, DMHD_ctl%org_plt, DMHD_ctl%model_ctl,           &
     &    DMHD_ctl%smctl_ctl, DMHD_ctl%nmtr_ctl, DMHD_ctl%psph_ctl,     &
     &    MHD_files, bc_IO, refs, MHD_step, MHD_prop, MHD_BC,           &
     &    SPH_WK%trans_p, SPH_WK%trns_WK, SPH_MHD)
!
      call set_control_SPH_MHD_w_viz(DMHD_ctl%model_ctl,                &
     &    DMHD_ctl%psph_ctl, DMHD_ctl%smonitor_ctl, DMHD_ctl%zm_ctls,   &
     &    MHD_prop, SPH_MHD%sph, SPH_MHD%fld, nod_fld, SPH_WK%monitor)
!
      call set_ctl_params_dynamobench                                   &
     &   (DMHD_ctl%model_ctl%fld_ctl%field_ctl,                         &
     &    DMHD_ctl%smonitor_ctl%meq_ctl, cdat%circle, cdat%d_circle,    &
     &    bench)
!
      if (iflag_debug.eq.1) write(*,*) 'load_sph_mesh'
      call check_and_make_SPH_mesh(MHD_files%sph_file_param, SPH_MHD)
      if (iflag_debug.gt.0) write(*,*) 'sph_index_flags_and_params'
      call sph_index_flags_and_params                                   &
     &   (SPH_MHD%groups, SPH_MHD%sph, SPH_MHD%comms)
!
      call dealloc_sph_mhd_ctl_data(DMHD_ctl)
!
      end subroutine input_control_SPH_dynamobench
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_params_dynamobench                             &
     &         (fld_ctl, meq_ctl, circle, d_circle, bench)
!
      use t_mid_equator_control
      use t_control_array_character3
      use t_phys_data
      use t_circle_transform
      use m_base_field_labels
      use m_phys_constants
!
      type(ctl_array_c3), intent(in) :: fld_ctl
      type(mid_equator_control), intent(in) :: meq_ctl
!
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
      type(dynamobench_monitor), intent(inout) :: bench
!
      integer(kind = kint) :: ifld
!
!
      circle%iflag_circle_coord = iflag_circle_sph
!
      circle%mphi_circle = -1
      if(meq_ctl%nphi_mid_eq_ctl%iflag .gt. 0) then
        circle%mphi_circle = meq_ctl%nphi_mid_eq_ctl%intvalue
      end if
!
      do ifld = 1, fld_ctl%num
        if(fld_ctl%c1_tbl(ifld) .eq. temperature%name)                  &
     &                                   bench%ibench_temp =   1
        if(fld_ctl%c1_tbl(ifld) .eq. velocity%name)                     &
     &                                   bench%ibench_velo =   1
        if(fld_ctl%c1_tbl(ifld) .eq. magnetic_field%name)               &
     &                                   bench%ibench_magne = 1
      end do
!
      d_circle%num_phys = bench%ibench_velo + bench%ibench_temp         &
     &                   + bench%ibench_magne
      call alloc_phys_name(d_circle)
!
      ifld = 0
      if(bench%ibench_temp .gt. 0) then
        ifld = ifld + 1
        bench%ibench_temp = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     temperature%name
        d_circle%num_component(ifld) = n_scalar
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_scalar
      end if
      if(bench%ibench_velo .gt. 0) then
        ifld = ifld + 1
        bench%ibench_velo = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     velocity%name
        d_circle%num_component(ifld) = n_vector
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_vector
      end if
      if(bench%ibench_magne .gt. 0) then
        ifld = ifld + 1
        bench%ibench_magne = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     magnetic_field%name
        d_circle%num_component(ifld) = n_vector
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_vector
      end if
      d_circle%flag_monitor = .TRUE.
      d_circle%ntot_phys =     d_circle%istack_component(ifld)
      d_circle%num_phys_viz =  d_circle%num_phys
      d_circle%ntot_phys_viz = d_circle%ntot_phys
!
      end subroutine set_ctl_params_dynamobench
!
! -----------------------------------------------------------------------
!
      end module input_control_dynamobench
