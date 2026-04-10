!>@file   t_radial_reference_field.f90
!!@brief  module t_radial_reference_field
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  reference temperature as a function of radius
!!
!!
!!@verbatim
!!      subroutine dealloc_reference_field(refs)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(radial_reference_field), intent(inout) :: refs
!!      subroutine set_default_reference_file_name(refs)
!!        type(radial_reference_field), intent(in) :: refs
!!      subroutine overwrite_sources_by_reference                       &
!!     &         (sph_rj, iref_base, ipol_base, ref_field, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(base_field_address), intent(in) :: iref_base
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(phys_data), intent(inout) :: ref_field
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module t_radial_reference_field
!
      use m_precision
      use m_constants
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_grad_field_labels
      use t_field_component_labels
      use t_file_IO_parameter
      use t_sph_radial_interpolate
      use t_control_parameter
!
      implicit  none
!
      character(len = kchara), parameter, private                       &
     &     :: default_input_reference_file = 'input_reference.dat'
      character(len = kchara), parameter, private                       &
     &     :: default_output_reference_file = 'reference_fields.dat'
!
      character(len = kchara), parameter, private                       &
     &     :: drho_dr_name =   'normalized_drho_dr'
      character(len = kchara), parameter, private                       &
     &     :: d2rho_dr2_name = 'normalized_d2rho_dr2'
!
!>      Structure of reference temperature
      type radial_reference_field
!>        MPI process for reference field
        integer :: irank_reference
!>        Address of radius
        integer(kind = kint) :: iref_radius
!>        Address of reference field
        type(base_field_address) :: iref_base
!>        Address of gradient of reference field
        type(gradient_field_address) :: iref_grad
!>        Address of reference vector components
        type(field_component_address) :: iref_cmp
!>        Diffusivity address for reference field
        type(diffusivity_adress) :: iref_diffusivity
!>        Dradient of diffusivity address for reference field
        type(diffusivity_adress) :: iref_grad_diffusivity
!
!>        Structure of reference field (include center at the end)
        type(phys_data) :: ref_field
!
!>        file name to read radial reference data
        type(field_IO_params) :: ref_input_IO
!>        file name to write radial reference data
        type(field_IO_params) :: ref_output_IO
!
!>        Interpolation table from radial data input 
        type(sph_radial_interpolate) :: r_itp
      end type radial_reference_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_reference_field(refs)
!
      type(radial_reference_field), intent(inout) :: refs
!
      call dealloc_phys_data(refs%ref_field)
      call dealloc_phys_name(refs%ref_field)
!
      end subroutine dealloc_reference_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_default_reference_file_name(refs)
!
      type(radial_reference_field), intent(inout) :: refs
!
      refs%ref_output_IO%file_prefix = default_output_reference_file
!
      end subroutine set_default_reference_file_name
!
! -----------------------------------------------------------------------
!
      subroutine overwrite_sources_by_reference                         &
     &         (sph_rj, iref_base, ipol_base, ref_field, rj_fld)
!
      use interpolate_reference_data
      use init_external_magne_sph
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: iref_base
      type(base_field_address), intent(in) :: ipol_base
!
      type(phys_data), intent(inout) :: ref_field
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(sph_rj%idx_rj_degree_zero .eq. 0) return
!
      call overwrite_each_field_by_ref(sph_rj,                          &
     &    iref_base%i_heat_source, ipol_base%i_heat_source,             &
     &    ref_field, rj_fld)
      call overwrite_each_field_by_ref(sph_rj,                          &
     &    iref_base%i_light_source, ipol_base%i_light_source,           &
     &    ref_field, rj_fld)
!
!      call overwrite_dipole_by_ref(sph_rj,                             &
!     &    iref_base%i_back_B, ipol_base%i_back_B,                      &
!     &    ref_field, rj_fld)
!
      end subroutine overwrite_sources_by_reference
!
! -----------------------------------------------------------------------
!
      end module t_radial_reference_field
