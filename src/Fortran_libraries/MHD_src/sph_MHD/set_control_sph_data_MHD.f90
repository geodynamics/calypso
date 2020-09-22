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
!!     &         (plt, mevo_ctl, rj_org_param, rst_org_param,           &
!!     &          fst_file_IO, bc_IO, trans_p, WK_leg)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(platform_data_control), intent(in) :: plt
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!        type(field_IO_params), intent(in) :: rj_org_param
!!        type(field_IO_params), intent(in) :: rst_org_param
!!        type(field_IO_params), intent(inout) :: fst_file_IO
!!        type(boundary_spectra), intent(inout) :: bc_IO
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(legendre_trns_works), intent(inout) :: WK_leg
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
      use set_control_field_data
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
      call add_dependent_field(MHD_prop, field_ctl)
!
!    set nodal data
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_field_data'
      call s_set_control_field_data(field_ctl, rj_fld, ierr)
!
      end subroutine set_control_sph_mhd_fields
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_sph_data_MHD                             &
     &         (plt, mevo_ctl, rj_org_param, rst_org_param,             &
     &          fst_file_IO, bc_IO, trans_p, WK_leg)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_file_format_switch
!
      use m_FFT_selector
      use m_legendre_transform_list
!
      use t_ctl_data_4_platforms
      use t_ctl_data_mhd_evo_scheme
      use t_phys_data
      use t_field_data_IO
      use t_legendre_trans_select
      use t_control_parameter
      use t_sph_boundary_input_data
      use t_work_4_sph_trans
!
      use skip_comment_f
      use add_nodal_fields_4_MHD
      use add_sph_MHD_fields_2_ctl
      use sph_mhd_rst_IO_control
      use sel_spherical_SRs
!
      type(platform_data_control), intent(in) :: plt
      type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
      type(field_IO_params), intent(in) :: rj_org_param, rst_org_param
!
      type(field_IO_params), intent(inout) :: fst_file_IO
      type(boundary_spectra), intent(inout) :: bc_IO
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(legendre_trns_works), intent(inout) :: WK_leg
!
!   overwrite restart header for magnetic field extension
!
      if( (rj_org_param%iflag_IO*rst_org_param%iflag_IO) .gt. 0)        &
     &   fst_file_IO%file_prefix = rst_org_param%file_prefix
!
      if(mevo_ctl%leg_vector_len%iflag .gt. 0) then
        trans_p%nvector_legendre = mevo_ctl%leg_vector_len%intvalue
      else
        trans_p%nvector_legendre = 0
      end if
!      
      if(mevo_ctl%Legendre_trans_type%iflag .gt. 0) then
        WK_leg%id_legendre = set_legendre_trans_mode_ctl                &
     &                   (mevo_ctl%Legendre_trans_type%charavalue)
      end if
!
      if(mevo_ctl%FFT_library%iflag .gt. 0) then
        call set_fft_library_ctl                                        &
     &     (mevo_ctl%FFT_library%charavalue, trans_p%iflag_FFT)
      end if
      if(mevo_ctl%import_mode%iflag .gt. 0) then
        call set_import_table_ctl                                       &
     &     (mevo_ctl%import_mode%charavalue, trans_p)
      end if
!
      if (plt%bc_data_file_name_ctl%iflag .gt. 0) then
        bc_IO%file_name = plt%bc_data_file_name_ctl%charavalue
      end if
!
      end subroutine s_set_control_sph_data_MHD
!
! -----------------------------------------------------------------------
!
      end module set_control_sph_data_MHD
