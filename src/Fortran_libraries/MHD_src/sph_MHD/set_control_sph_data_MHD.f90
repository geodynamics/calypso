!>@file   set_control_sph_data_MHD.f90
!!@brief  module set_control_sph_data_MHD
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2009
!
!>@brief  Set control parameters for spherical harmonics dynamo from IO
!!
!!@verbatim
!!      subroutine set_control_sph_mhd_fields                           &
!!     &        (MHD_prop, field_ctl, rj_fld)
!!      subroutine s_set_control_sph_data_MHD                           &
!!     &         (MHD_prop, plt, mevo_ctl, rj_org_param, rst_org_param, &
!!     &          fst_file_IO, bc_IO, WK_sph)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(platform_data_control), intent(in) :: plt
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!        type(field_IO_params), intent(in) :: rj_org_param
!!        type(field_IO_params), intent(in) :: rst_org_param
!!        type(field_IO_params), intent(inout) :: fst_file_IO
!!        type(boundary_spectra), intent(inout) :: bc_IO
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!      subroutine set_ctl_params_pick_circle                           &
!!     &         (field_ctl, meq_ctl, circle, d_circle)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!        type(mid_equator_control), intent(in) :: meq_ctl
!!        type(fields_on_circle), intent(inout) :: circle
!!        type(phys_data), intent(inout) :: d_circle
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
      subroutine set_control_sph_mhd_fields                             &
     &        (MHD_prop, field_ctl, rj_fld)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
!
      use t_control_parameter
      use t_control_array_character3
      use t_phys_data
!
      use add_nodal_fields_4_MHD
      use add_sph_MHD_fields_2_ctl
      use set_control_sph_data
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ierr
!
!   set physical values
!
      if(field_ctl%icou .eq. 0) then
        call calypso_MPI_abort(ierr_fld, 'Set field for simulation')
      end if
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'original number of field ', field_ctl%num
!
      if(field_ctl%num .le. 0) return
!
!     add fields for simulation
      call add_field_name_4_mhd(MHD_prop, field_ctl)
      call add_field_name_4_sph_mhd                                     &
     &     (MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop, field_ctl)
!
!    set nodal data
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_sph_data'
      call s_set_control_sph_data(field_ctl, rj_fld, ierr)
!
      end subroutine set_control_sph_mhd_fields
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_sph_data_MHD                             &
     &         (MHD_prop, plt, mevo_ctl, rj_org_param, rst_org_param,   &
     &          fst_file_IO, bc_IO, WK_sph)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_file_format_switch
!
      use m_sel_spherical_SRs
      use m_FFT_selector
      use m_legendre_transform_list
!
      use t_ctl_data_4_platforms
      use t_ctl_data_mhd_evo_scheme
      use t_phys_data
      use t_field_data_IO
      use t_sph_transforms
      use t_control_parameter
      use t_sph_boundary_input_data
!
      use skip_comment_f
      use set_control_sph_data
      use add_nodal_fields_4_MHD
      use add_sph_MHD_fields_2_ctl
      use sph_mhd_rst_IO_control
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(platform_data_control), intent(in) :: plt
      type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
      type(field_IO_params), intent(in) :: rj_org_param, rst_org_param
!
      type(field_IO_params), intent(inout) :: fst_file_IO
      type(boundary_spectra), intent(inout) :: bc_IO
      type(spherical_trns_works), intent(inout) :: WK_sph
!
      integer(kind = kint) :: ierr
!
!   overwrite restart header for magnetic field extension
!
      if( (rj_org_param%iflag_IO*rst_org_param%iflag_IO) .gt. 0)        &
     &   fst_file_IO%file_prefix = rst_org_param%file_prefix
!
      if(mevo_ctl%leg_vector_len%iflag .gt. 0) then
        nvector_legendre = mevo_ctl%leg_vector_len%intvalue
      else
        nvector_legendre = 0
      end if
!      
      if(mevo_ctl%Legendre_trans_type%iflag .gt. 0) then
        WK_sph%WK_leg%id_legendre = set_legendre_trans_mode_ctl         &
     &                       (mevo_ctl%Legendre_trans_type%charavalue)
      end if
!
      if(mevo_ctl%FFT_library%iflag .gt. 0) then
        call set_fft_library_ctl(mevo_ctl%FFT_library%charavalue)
      end if
      if(mevo_ctl%import_mode%iflag .gt. 0) then
        call set_import_table_ctl(mevo_ctl%import_mode%charavalue)
      end if
      if(mevo_ctl%SR_routine%iflag .gt. 0) then
        call set_sph_comm_routine_ctl(mevo_ctl%SR_routine%charavalue)
      end if
!
      if (plt%bc_data_file_name_ctl%iflag .gt. 0) then
        bc_IO%file_name = plt%bc_data_file_name_ctl%charavalue
      end if
!
      end subroutine s_set_control_sph_data_MHD
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_pick_circle                             &
     &         (field_ctl, meq_ctl, circle, d_circle)
!
      use t_ctl_data_sph_vol_spectr
      use t_mid_equator_control
      use t_control_array_character3
      use t_circle_transform
      use t_phys_data
      use ordering_field_by_viz
      use skip_comment_f
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(mid_equator_control), intent(in) :: meq_ctl
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
      character(len = kchara) :: tmpchara
!
!
      circle%iflag_circle_coord = iflag_circle_sph
      if (meq_ctl%pick_circle_coord_ctl%iflag .ne. 0) then
        tmpchara = meq_ctl%pick_circle_coord_ctl%charavalue
        if(    cmp_no_case(tmpchara,'spherical')                        &
     &    .or. cmp_no_case(tmpchara,'rtp')) then
          circle%iflag_circle_coord = iflag_circle_sph
        else if(cmp_no_case(tmpchara,'cyrindrical')                     &
      &    .or. cmp_no_case(tmpchara,'spz')) then
          circle%iflag_circle_coord = iflag_circle_cyl
        end if
      end if
!
      circle%mphi_circle = -1
      if(meq_ctl%nphi_mid_eq_ctl%iflag .gt. 0) then
        circle%mphi_circle = meq_ctl%nphi_mid_eq_ctl%intvalue
      end if
!
      circle%s_circle = 7.0d0/13.0d0 + 0.5d0
      if(meq_ctl%pick_s_ctl%iflag .gt. 0) then
        circle%s_circle = meq_ctl%pick_s_ctl%realvalue
      end if
!
      circle%z_circle = 0.0d0
      if(meq_ctl%pick_z_ctl%iflag .gt. 0) then
        circle%z_circle = meq_ctl%pick_z_ctl%realvalue
      end if
!
      d_circle%num_phys = field_ctl%num
      call alloc_phys_name_type(d_circle)
      call s_ordering_field_by_viz(field_ctl, d_circle%num_phys,        &
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
!
      end module set_control_sph_data_MHD
