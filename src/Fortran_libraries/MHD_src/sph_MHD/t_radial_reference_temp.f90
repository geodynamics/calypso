!>@file   t_radial_reference_temp.f90
!!@brief  module t_radial_reference_temp
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  reference temperature as a function of r
!!
!!
!!@verbatim
!!        subroutine init_reft_rj_data(sph_rj, ipol, refs)
!!          type(sph_rj_grid), intent(in) ::  sph_rj
!!          type(phys_address), intent(in) :: ipol
!!          type(reference_field), intent(inout) :: refs
!!@endverbatim
!!
!!@n @param my_rank process ID
!
      module t_radial_reference_temp
!
      use m_precision
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_grad_field_labels
!
      implicit  none
!
!>      Structure of reference temperature
      type reference_field
!>        Address of radius
        integer(kind = kint) :: iref_radius
!>        Address of reference field
        type(base_field_address) :: iref_base
!>        Address of gradient of reference field
        type(gradient_field_address) :: iref_grad
!>        Structure of reference field (include center at the end)
        type(phys_data) :: ref_field
      end type reference_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_reft_rj_data(sph_rj, ipol, refs)
!
      use m_base_field_labels
      use m_grad_field_labels
      use append_phys_data
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(reference_field), intent(inout) :: refs
!
      character(len = kchara), parameter :: radius_name = 'radius'
!
!
      refs%ref_field%num_phys =   0
      refs%ref_field%ntot_phys =  0
      call alloc_phys_name(refs%ref_field)

      refs%iref_radius = refs%ref_field%ntot_phys + 1
      call append_field_name_list(radius_name,                          &
     &    ione, .TRUE., .FALSE., izero, refs%ref_field)
!
      if(ipol%base%i_heat_source .gt. 0) then
        refs%iref_base%i_heat_source = refs%ref_field%ntot_phys + 1
        call append_field_name_list(temperature%name,                   &
     &      ione, .TRUE., .FALSE., izero, refs%ref_field)
      end if
      if(ipol%base%i_light_source .gt. 0) then
        refs%iref_base%i_light_source = refs%ref_field%ntot_phys + 1
        call append_field_name_list(composition_source%name,            &
     &      ione, .TRUE., .FALSE., izero, refs%ref_field)
      end if
!
      if(ipol%base%i_temp .gt. 0) then
        refs%iref_base%i_temp = refs%ref_field%ntot_phys + 1
        call append_field_name_list(temperature%name,                   &
     &      ione, .TRUE., .FALSE., izero, refs%ref_field)
!
        refs%iref_grad%i_grad_temp = refs%ref_field%ntot_phys + 1
        call append_field_name_list(grad_temp%name,                     &
     &      ione, .TRUE., .FALSE., izero, refs%ref_field)
      end if
      if(ipol%base%i_light .gt. 0) then
        refs%iref_base%i_light = refs%ref_field%ntot_phys + 1
        call append_field_name_list(heat_source%name,                   &
     &      ione, .TRUE., .FALSE., izero, refs%ref_field)
!
        refs%iref_grad%i_grad_composit = refs%ref_field%ntot_phys + 1
        call append_field_name_list(grad_composition%name,              &
     &      ione, .TRUE., .FALSE., izero, refs%ref_field)
      end if
!
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
!  --------------------------------------------------------------------
!
      subroutine output_reference_field(file_IO, ref_fld)
!
      use calypso_mpi
      use t_time_data
      use t_field_data_IO
      use t_file_IO_parameter
      use field_file_IO
!
      use copy_rj_phys_data_4_IO
      use set_sph_extensions
!
      type(phys_data), intent(in) :: ref_fld
      type(field_IO_params), intent(in) :: file_IO
!
      type(field_IO) :: sph_out_IO
      type(time_data) :: time_IO
      character(len=kchara) :: file_name
!
!
      if(my_rank .ne. 0) return
!
      time_IO%i_time_step = izero
      time_IO%time = zero
      time_IO%dt = zero
!
      call copy_rj_phys_name_to_IO                                      &
     &   (ref_fld%num_phys_viz, ref_fld, sph_out_IO)
      call alloc_phys_data_IO(sph_out_IO)
      call copy_rj_phys_data_to_IO                                      &
     &   (ref_fld%num_phys_viz, ref_fld, sph_out_IO)
!
      file_name = add_fst_extension(file_IO%file_prefix)
      call write_step_field_file(file_name, my_rank,                    &
     &                           time_IO, sph_out_IO)
!
      call dealloc_phys_data_IO(sph_out_IO)
      call dealloc_phys_name_IO(sph_out_IO)
!
      end subroutine output_reference_field
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_sph_reference_data                          &
     &         (file_IO, rj_fld)
!
      use calypso_mpi
      use calypso_mpi_int
      use calypso_mpi_real
      use t_time_data
      use t_field_data_IO
      use t_file_IO_parameter
      use field_file_IO
      use set_sph_restart_IO
      use set_sph_extensions
!
      type(field_IO_params), intent(in) :: file_IO
!
      type(phys_data), intent(inout) :: rj_fld
!
      type(time_data) :: time_IO
      type(field_IO) :: sph_fst_IO
      character(len=kchara) :: file_name
      integer(kind = kint_gl) :: num64
!
!
      rj_fld%iflag_update(1:rj_fld%ntot_phys) = 0
      if(my_rank .eq. 0) then
!
        file_name = add_fst_extension(file_IO%file_prefix)
        call read_and_alloc_step_field                                  &
     &     (file_name, my_rank, time_IO, sph_fst_IO)
!
        call set_sph_restart_from_IO(sph_fst_IO, rj_fld)
!
        call dealloc_phys_data_IO(sph_fst_IO)
        call dealloc_phys_name_IO(sph_fst_IO)
      end if
!
      num64 = rj_fld%ntot_phys
      call calypso_mpi_bcast_int(rj_fld%iflag_update, num64, 0)
      num64 = rj_fld%n_point * rj_fld%ntot_phys
      call calypso_mpi_bcast_real(rj_fld%d_fld, num64, 0)
!
      end subroutine read_alloc_sph_reference_data
!
! -----------------------------------------------------------------------
!
      end module t_radial_reference_temp
