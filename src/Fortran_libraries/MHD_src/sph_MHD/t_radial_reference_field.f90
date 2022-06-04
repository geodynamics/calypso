!>@file   t_radial_reference_field.f90
!!@brief  module t_radial_reference_field
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  reference temperature as a function of r
!!
!!
!!@verbatim
!!      subroutine init_reft_rj_data(sph_rj, ipol, refs)
!!      subroutine dealloc_reference_field(refs)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(radial_reference_field), intent(inout) :: refs
!!      subroutine set_default_reference_file_name(refs)
!!      subroutine output_reference_field(refs)
!!        type(radial_reference_field), intent(in) :: refs
!!      subroutine read_alloc_sph_reference_data(sph_rj, ipol,          &
!!     &                                         rj_fld, refs)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(radial_reference_field), intent(inout) :: refs
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
      use t_file_IO_parameter
      use t_sph_radial_interpolate
!
      implicit  none
!
      character(len = kchara), parameter, private                       &
     &             :: radius_name = 'radius'
!
      character(len = kchara), parameter, private                       &
     &             :: default_reference_file = 'reference_fields.dat'
!
!>      Structure of reference temperature
      type radial_reference_field
!>        Address of radius
        integer(kind = kint) :: iref_radius
!>        Address of reference field
        type(base_field_address) :: iref_base
!>        Address of gradient of reference field
        type(gradient_field_address) :: iref_grad
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
      private :: append_reference_field_names
      private :: overwrite_sources_by_reference
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_reft_rj_data(sph_rj, ipol, refs)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(radial_reference_field), intent(inout) :: refs
!
!
      refs%ref_field%num_phys =   0
      refs%ref_field%ntot_phys =  0
      call alloc_phys_name(refs%ref_field)

      call append_reference_field_names                                 &
     &   (ipol%base, refs%iref_radius, refs%iref_base, refs%iref_grad,  &
     &    refs%ref_field)
      call alloc_phys_data((sph_rj%nidx_rj(1)+1), refs%ref_field)
!
      refs%ref_field%d_fld(1,refs%iref_radius) = 0.0d0
!$omp parallel workshare
      refs%ref_field%d_fld(2:sph_rj%nidx_rj(1)+1,refs%iref_radius)      &
     &                    = sph_rj%radius_1d_rj_r(1:sph_rj%nidx_rj(1))
!$omp end parallel workshare
!
      end subroutine init_reft_rj_data
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
!  --------------------------------------------------------------------
!
      subroutine set_default_reference_file_name(refs)
!
      type(radial_reference_field), intent(inout) :: refs
!
      refs%ref_output_IO%file_prefix = default_reference_file
!
      end subroutine set_default_reference_file_name
!
!  --------------------------------------------------------------------
!
      subroutine output_reference_field(refs)
!
      use calypso_mpi
      use t_time_data
      use t_field_data_IO
      use field_file_IO
!
      use copy_rj_phys_data_4_IO
      use set_sph_extensions
!
      type(radial_reference_field), intent(in) :: refs
!
      type(field_IO) :: sph_out_IO
      type(time_data) :: time_IO
!
!
      if(my_rank .ne. 0) return
!
      time_IO%i_time_step = izero
      time_IO%time = zero
      time_IO%dt = zero
!
      call copy_rj_phys_name_to_IO                                      &
     &   (refs%ref_field%num_phys_viz, refs%ref_field, sph_out_IO)
      call alloc_phys_data_IO(sph_out_IO)
      call copy_rj_phys_data_to_IO                                      &
     &   (refs%ref_field%num_phys_viz, refs%ref_field, sph_out_IO)
!
      call write_step_field_file(refs%ref_output_IO%file_prefix,        &
     &                           my_rank, time_IO, sph_out_IO)
!
      call dealloc_phys_data_IO(sph_out_IO)
      call dealloc_phys_name_IO(sph_out_IO)
!
      end subroutine output_reference_field
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_sph_reference_data(sph_rj, ipol,            &
     &                                         rj_fld, refs)
!
      use calypso_mpi
      use calypso_mpi_int
      use calypso_mpi_real
      use t_time_data
      use t_field_data_IO
      use t_file_IO_parameter
      use field_file_IO
      use interpolate_reference_data
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(radial_reference_field), intent(inout) :: refs
!
      type(time_data) :: time_IO
      type(field_IO) :: ref_fld_IO
      integer(kind = kint_gl) :: num64
!
!
      refs%ref_field%iflag_update(1:refs%ref_field%ntot_phys) = 0
      if(refs%ref_input_IO%iflag_IO .eq. 0) return
      if(my_rank .eq. 0) then
        call read_and_alloc_step_field(refs%ref_input_IO%file_prefix,   &
     &                                  my_rank, time_IO, ref_fld_IO)
!
        call interpolate_reference_data_IO(radius_name,                 &
     &      refs%iref_radius, ref_fld_IO, refs%ref_field, refs%r_itp)
!
        call dealloc_phys_data_IO(ref_fld_IO)
        call dealloc_phys_name_IO(ref_fld_IO)
      end if
!
      num64 = refs%ref_field%ntot_phys
      call calypso_mpi_bcast_int(refs%ref_field%iflag_update, num64, 0)
      num64 = refs%ref_field%n_point * refs%ref_field%ntot_phys
      call calypso_mpi_bcast_real(refs%ref_field%d_fld, num64, 0)
!
      call overwrite_sources_by_reference                               &
     &   (sph_rj, refs%iref_base, ipol%base, refs%ref_field, rj_fld)
!
      end subroutine read_alloc_sph_reference_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine append_reference_field_names(ipol_base, iref_radius,   &
     &          iref_base, iref_grad, ref_field)
!
      use m_base_field_labels
      use m_grad_field_labels
      use append_phys_data
!
      type(base_field_address), intent(in) :: ipol_base
!
      integer(kind = kint), intent(inout) :: iref_radius
      type(base_field_address), intent(inout) :: iref_base
      type(gradient_field_address), intent(inout) :: iref_grad
      type(phys_data), intent(inout) :: ref_field
!
!
      iref_radius = ref_field%ntot_phys + 1
      call append_field_name_list(radius_name,                          &
     &    ione, .TRUE., .FALSE., izero, ref_field)
!
      if(ipol_base%i_heat_source .gt. 0) then
        iref_base%i_heat_source = ref_field%ntot_phys + 1
        call append_field_name_list(heat_source%name,                   &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
      if(ipol_base%i_light_source .gt. 0) then
        iref_base%i_light_source = ref_field%ntot_phys + 1
        call append_field_name_list(composition_source%name,            &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
!
      if(ipol_base%i_ref_density .gt. 0) then
        iref_base%i_ref_density = ref_field%ntot_phys + 1
        call append_field_name_list(reference_density%name,             &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
!
      if(ipol_base%i_temp .gt. 0) then
        iref_base%i_temp = ref_field%ntot_phys + 1
        call append_field_name_list(temperature%name,                   &
     &      ione, .TRUE., .FALSE., izero, ref_field)
!
        iref_grad%i_grad_temp = ref_field%ntot_phys + 1
        call append_field_name_list(grad_temp%name,                     &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
      if(ipol_base%i_light .gt. 0) then
        iref_base%i_light = ref_field%ntot_phys + 1
        call append_field_name_list(composition%name,                   &
     &      ione, .TRUE., .FALSE., izero, ref_field)
!
        iref_grad%i_grad_composit = ref_field%ntot_phys + 1
        call append_field_name_list(grad_composition%name,              &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
!
      end subroutine append_reference_field_names
!
! -----------------------------------------------------------------------
!
      subroutine overwrite_sources_by_reference                         &
     &         (sph_rj, iref_base, ipol_base, ref_field, rj_fld)
!
      use interpolate_reference_data
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
      call overwrite_each_field_by_ref(sph_rj,                          &
     &    iref_base%i_ref_density, ipol_base%i_ref_density,             &
     &    ref_field, rj_fld)
!
      end subroutine overwrite_sources_by_reference
!
! -----------------------------------------------------------------------
!
      end module t_radial_reference_field
