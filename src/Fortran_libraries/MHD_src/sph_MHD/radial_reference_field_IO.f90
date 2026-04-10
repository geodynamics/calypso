!>@file   radial_reference_field_IO.f90
!!@brief  module radial_reference_field_IO
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2022
!
!>@brief  reference temperature as a function of r
!!
!!@verbatim
!!      subroutine init_radial_reference_data(sph_rj, ipol, refs)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(radial_reference_field), intent(inout) :: refs
!!      subroutine copy_const_diffusivity_to_ref(i_kappa, i_dkdr,       &
!!     &                                         ref_field)
!!        integer(kind = kint), intent(in) :: i_kappa, i_dkdr
!!        type(phys_data), intent(inout) :: ref_field
!!
!!      subroutine output_reference_field(refs)
!!        type(radial_reference_field), intent(in) :: refs
!!      subroutine load_sph_reference_sources(refs)
!!        type(radial_reference_field), intent(inout) :: refs
!!
!!      subroutine load_sph_reference_two_field                         &
!!     &         (sph_rj, sph_bc_T, sph_bc_S, ref_file_IO, iref_radius, &
!!     &          phys_name, source_name, iref_in, iref_src, ncomp,     &
!!     &          r_itp, ref_field)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_S
!!        type(field_IO_params), intent(in) :: ref_file_IO
!!        character(len = kchara), intent(in) :: phys_name, source_name
!!        integer(kind = kint), intent(in) :: iref_radius, ncomp
!!        integer(kind = kint), intent(in) :: iref_in, iref_src
!!@endverbatim
!
      module radial_reference_field_IO
!
      use m_precision
      use m_constants
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_radial_reference_field
      use t_field_data_IO
      use t_time_data
      use t_control_parameter
!
      implicit  none
!
      character(len = kchara), parameter, private                       &
     &             :: radius_name = 'radius'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_radial_reference_data(sph_rj, ipol,               &
     &                                      MHD_prop, refs)
!
      use append_reference_field_names
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(radial_reference_field), intent(inout) :: refs
!
!
      refs%ref_field%num_phys =   0
      refs%ref_field%ntot_phys =  0
      call alloc_phys_name(refs%ref_field)

      call append_ref_field_names(radius_name, ipol%base,               &
     &                            MHD_prop, refs)
      call alloc_phys_data((sph_rj%nidx_rj(1)+1), refs%ref_field)
!
      call copy_reference_radius_data                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r(1),                  &
     &    refs%ref_field%d_fld(1,refs%iref_radius))
!
      end subroutine init_radial_reference_data
!
! -----------------------------------------------------------------------
!
      subroutine copy_reference_radius_data(nri, radius_rj, ref_r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius_rj(nri)
!
      real(kind = kreal), intent(inout) :: ref_r(0:nri)
!
!
      ref_r(0) = 0.0d0
!$omp parallel workshare
      ref_r(1:nri) = radius_rj(1:nri)
!$omp end parallel workshare
!
      end subroutine copy_reference_radius_data
!
! -----------------------------------------------------------------------
!
      subroutine copy_const_diffusivity_to_ref(i_kappa, i_dkdr,         &
     &                                         ref_field)
!
      integer(kind = kint), intent(in) :: i_kappa, i_dkdr
      type(phys_data), intent(inout) :: ref_field
!
!
      if(i_kappa .gt. 0) then
!$omp parallel workshare
        ref_field%d_fld(1:ref_field%n_point,i_kappa) = one
!$omp end parallel workshare
      end if
      if(i_dkdr .gt. 0) then
!$omp parallel workshare
        ref_field%d_fld(1:ref_field%n_point,i_dkdr) = zero
!$omp end parallel workshare
      end if
!
      end subroutine copy_const_diffusivity_to_ref
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_reference_field(refs)
!
      use calypso_mpi
      use t_time_data
      use field_file_IO
!
      use copy_rj_phys_data_4_IO
      use set_sph_extensions
!
      type(radial_reference_field), intent(inout) :: refs
!
      type(time_data) :: time_IO
      type(field_IO) :: ref_fld_IO
!
!
      if(my_rank .ne. 0) return
!
      time_IO%i_time_step = izero
      time_IO%time = zero
      time_IO%dt = zero
!
!
      call copy_rj_phys_name_to_IO                                      &
     &   (refs%ref_field%num_phys_viz, refs%ref_field, ref_fld_IO)
      call alloc_phys_data_IO(ref_fld_IO)
      call copy_rj_phys_data_to_IO                                      &
     &   (refs%ref_field%num_phys_viz, refs%ref_field, ref_fld_IO)
!
      call write_step_field_file(refs%ref_output_IO%file_prefix,        &
     &                           my_rank, time_IO, ref_fld_IO)
!
      call dealloc_phys_data_IO(ref_fld_IO)
      call dealloc_phys_name_IO(ref_fld_IO)
!
      end subroutine output_reference_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine load_sph_reference_sources(ref_file_IO, refs)
!
      use calypso_mpi
      use calypso_mpi_int
      use calypso_mpi_real
      use t_file_IO_parameter
      use interpolate_reference_data
      use transfer_to_long_integers
      use m_base_field_labels
!
      type(field_IO_params), intent(in) :: ref_file_IO
      type(radial_reference_field), intent(inout) :: refs
!
      integer(kind = kint_gl) :: num64
!
!
      if(my_rank .eq. 0) then
        write(e_message,'(a)') 'Read radial variation file failed'
        call load_sph_reference_two_field(ref_file_IO,                  &
     &     refs%iref_radius, heat_source%name, composition_source%name, &
     &     refs%iref_base%i_heat_source, refs%iref_base%i_light_source, &
     &     n_scalar, refs%r_itp, refs%ref_field)
      end if
!
      call calypso_mpi_bcast_int(refs%ref_field%iflag_update,           &
     &    cast_long(refs%ref_field%ntot_phys), 0)
      num64 = cast_long(refs%ref_field%n_point                          &
     &                  * refs%ref_field%ntot_phys)
      call calypso_mpi_bcast_real(refs%ref_field%d_fld, num64, 0)
!
      end subroutine load_sph_reference_sources
!
! -----------------------------------------------------------------------
!
      subroutine load_sph_reference_two_field(ref_file_IO, iref_radius, &
     &          phys_name, source_name, iref_in, iref_src, ncomp,       &
     &          r_itp, ref_field)
!
      use calypso_mpi
      use t_file_IO_parameter
      use field_file_IO
      use interpolate_reference_data
      use fill_scalar_field
!
      type(field_IO_params), intent(in) :: ref_file_IO
      character(len = kchara), intent(in) :: phys_name, source_name
      integer(kind = kint), intent(in) :: iref_radius, ncomp
      integer(kind = kint), intent(in) :: iref_in, iref_src
!
      type(sph_radial_interpolate), intent(inout) :: r_itp
      type(phys_data), intent(inout) :: ref_field
!
      type(time_data) :: time_IO
      type(field_IO) :: ref_fld_IO
      integer(kind = kint) :: iend
!
!
      if(ref_file_IO%iflag_IO .eq. 0) return
!
      call read_and_alloc_step_field(ref_file_IO%file_prefix,           &
     &    my_rank, time_IO, ref_fld_IO, iend)
      if(iend .gt. 0) call calypso_mpi_abort(iend,                      &
     &                                       'Read file failed')
!
      call const_radial_interpolate_table(radius_name, iref_radius,     &
     &                                    ref_fld_IO, ref_field, r_itp)
!
      if(iref_in .gt. 0) then
        call interepolate_one_ref_field(ref_fld_IO, r_itp,              &
     &      ref_field%n_point, n_scalar, phys_name,                     &
     &      ref_field%iflag_update(iref_in),                            &
     &      ref_field%d_fld(1,iref_in))
      end if
!
      if(iref_src .gt. 0) then
        call interepolate_one_ref_field(ref_fld_IO, r_itp,              &
     &      ref_field%n_point, n_scalar, source_name,                   &
     &      ref_field%iflag_update(iref_src),                           &
     &      ref_field%d_fld(1,iref_src))
      end if
      call dealloc_original_sph_data(r_itp)
!
      call dealloc_radial_interpolate(r_itp)
      call dealloc_org_radius_interpolate(r_itp)
!
      call dealloc_phys_data_IO(ref_fld_IO)
      call dealloc_phys_name_IO(ref_fld_IO)
!
      end subroutine load_sph_reference_two_field
!
! -----------------------------------------------------------------------
!
      end module radial_reference_field_IO
