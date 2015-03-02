!>@file   set_control_sph_data_MHD.f90
!!@brief  module set_control_sph_data_MHD
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2009
!
!>@brief  Set control parameters for spherical harmonics dynamo from IO
!!
!!@verbatim
!!     subroutine s_set_control_sph_data_MHD
!!     subroutine set_ctl_params_pick_circle
!!     subroutine set_ctl_params_dynamobench
!!     subroutine check_SPH_MHD_dependencies
!!@endverbatim
!
      module set_control_sph_data_MHD
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_sph_data_MHD
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_node_phys_data
      use m_ctl_data_4_platforms
      use m_ctl_data_4_fields
      use m_ctl_data_mhd_forces
      use m_ctl_data_mhd_evo_scheme
      use m_control_params_2nd_files
      use m_node_id_spherical_IO
      use m_physical_property
      use m_work_4_sph_trans
      use m_file_format_switch
!
      use m_field_data_IO
      use m_sph_boundary_input_data
      use m_sel_spherical_SRs
      use m_FFT_selector
!
      use skip_comment_f
      use set_control_sph_data
      use legendre_transform_select
      use add_nodal_fields_4_MHD
      use add_sph_MHD_fields_2_ctl
!
      integer(kind = kint) :: ierr
!
!   overwrite restart header for magnetic field extension
!
      if( (iflag_org_sph_rj_head*iflag_org_rst) .gt. 0) then
        phys_file_head = org_rst_header
      end if
!
!   set physical values
!
      if(field_ctl%icou .eq. 0) then
        call calypso_MPI_abort(ierr_fld, 'Set field for simulation')
      end if
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'original number of field ', field_ctl%num
!
      if ( field_ctl%num .ne. 0 ) then
!
!     add terms for potentials
!
        call add_field_name_4_mhd
        call add_field_name_4_sph_mhd
        if (iflag_debug.eq.1) write(*,*)                                &
     &    'field_ctl%num after modified ', field_ctl%num
!
!    set nodal data
!
        if (iflag_debug.gt.0) write(*,*) 's_set_control_sph_data'
        call s_set_control_sph_data(ierr)
      end if
!
!
      if(i_legendre_vect_len .gt. 0) then
        nvector_legendre = legendre_vector_len_ctl
      else
        nvector_legendre = 0
      end if
!      
      if(i_sph_transform_mode .gt. 0) then
        call set_legendre_trans_mode_ctl(Legendre_trans_loop_ctl)
      end if
!
      if(i_FFT_package .gt. 0) then
        call set_fft_library_ctl(FFT_library_ctl)
      end if
      if(i_import_mode .gt. 0) then
        call set_import_table_ctl(import_mode_ctl)
      end if
      if(i_SR_routine .gt. 0) then
        call set_sph_comm_routine_ctl(SR_routine_ctl)
      end if
!
      if (bc_data_file_name_ctl%iflag .gt. 0) then
        bc_sph_file_name = bc_data_file_name_ctl%charavalue
      end if
!
      end subroutine s_set_control_sph_data_MHD
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_dynamobench
!
      use m_ctl_data_4_pickup_sph
      use m_ctl_data_4_fields
      use m_phys_labels
      use m_phys_constants
      use m_field_on_circle
      use m_circle_transform
      use m_field_at_mid_equator
!
      integer(kind = kint) :: ifld
!
!
      iflag_circle_coord = iflag_circle_sph
      if(i_nphi_mid_eq .gt. 0) then
        mphi_circle = nphi_mid_eq_ctl
      else
        mphi_circle = -1
      end if
!
      do ifld = 1, field_ctl%num
        if(field_ctl%c1_tbl(ifld) .eq. fhd_temp) ibench_temp = 1
        if(field_ctl%c1_tbl(ifld) .eq. fhd_velo) ibench_velo = 1
        if(field_ctl%c1_tbl(ifld) .eq. fhd_magne) ibench_magne = 1
      end do
!
      d_circle%num_phys = ibench_velo + ibench_temp + ibench_magne
      call alloc_phys_name_type(d_circle)
!
      ifld = 0
      if(ibench_temp .gt. 0) then
        ifld = ifld + 1
        ibench_temp = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     fhd_temp
        d_circle%num_component(ifld) = n_scalar
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_scalar
      end if
      if(ibench_velo .gt. 0) then
        ifld = ifld + 1
        ibench_velo = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     fhd_velo
        d_circle%num_component(ifld) = n_vector
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_vector
      end if
      if(ibench_magne .gt. 0) then
        ifld = ifld + 1
        ibench_magne = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     fhd_magne
        d_circle%num_component(ifld) = n_vector
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_vector
      end if
      d_circle%iflag_monitor = ione
      d_circle%ntot_phys =     d_circle%istack_component(ifld)
      d_circle%num_phys_viz =  d_circle%num_phys
      d_circle%ntot_phys_viz = d_circle%ntot_phys
!
      end subroutine set_ctl_params_dynamobench
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_pick_circle
!
      use m_ctl_data_4_pickup_sph
      use m_ctl_data_4_fields
      use m_field_on_circle
      use m_circle_transform
      use t_phys_data
      use ordering_field_by_viz
      use skip_comment_f
!
!
      iflag_circle_coord = iflag_circle_sph
      if (i_circle_coord .ne. 0) then
        if(    cmp_no_case(pick_circle_coord_ctl,'spherical')           &
     &    .or. cmp_no_case(pick_circle_coord_ctl,'rtp')) then
          iflag_circle_coord = iflag_circle_sph
        else if(cmp_no_case(pick_circle_coord_ctl,'cyrindrical')        &
      &    .or. cmp_no_case(pick_circle_coord_ctl,'spz')) then
          iflag_circle_coord = iflag_circle_cyl
        end if
      end if
!
      if(i_nphi_mid_eq .gt. 0) then
        mphi_circle = nphi_mid_eq_ctl
      else
        mphi_circle = -1
      end if
!
      if(i_pick_z_ctl .gt. 0) then
        s_circle = pick_s_ctl
      else
        s_circle = 7.0d0/13.0d0 + 0.5d0
      end if
!
      if(i_pick_z_ctl .gt. 0) then
        z_circle = pick_z_ctl
      else
        z_circle = 0.0d0
      end if
!
      d_circle%num_phys = field_ctl%num
      call alloc_phys_name_type(d_circle)
      call s_ordering_field_by_viz(d_circle%num_phys,                   &
     &    d_circle%num_phys_viz, d_circle%num_component,                &
     &    d_circle%phys_name, d_circle%iflag_monitor)
!
      call set_istack_4_nodal_field(d_circle%num_phys,                  &
     &    d_circle%num_phys_viz, d_circle%num_component,                &
     &    d_circle%ntot_phys, d_circle%ntot_phys_viz,                   &
     &    d_circle%istack_component)
!
      end subroutine set_ctl_params_pick_circle
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_SPH_MHD_dependencies
!
      use m_sph_spectr_data
      use check_dependency_for_MHD
!
!
      call check_dependencies(num_phys_rj, phys_name_rj)
!
      end subroutine check_SPH_MHD_dependencies
!
! -----------------------------------------------------------------------
!
      end module set_control_sph_data_MHD
