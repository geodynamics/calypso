!>@file   interpolate_reference_data.f90
!!@brief  module interpolate_reference_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  reference temperature as a function of r
!!
!!
!!@verbatim
!!      subroutine overwrite_each_field_by_ref                         &
!!     &         (sph_rj, iref_source, ipol_source, ref_field, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        integer(kind = kint), intent(in) :: iref_source
!!        integer(kind = kint), intent(in) :: ipol_source
!!        type(phys_data), intent(inout) :: ref_field
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine interpolate_reference_data_IO(radius_name,           &
!!     &         (iref_radius, ref_fld_IO, ref_field, r_itp)
!!        character(len = kchara), intent(in) :: radius_name
!!        integer(kind = kint), intent(in) :: iref_radius
!!        type(field_IO), intent(in) :: ref_fld_IO
!!        type(phys_data), intent(inout) :: ref_field
!!        type(sph_radial_interpolate), intent(inout) :: r_itp
!!@endverbatim
!!
!!@n @param my_rank process ID
!
      module interpolate_reference_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_spheric_rj_data
      use t_phys_data
      use t_field_data_IO
      use t_sph_radial_interpolate
!
      implicit  none
!
      private :: copy_reference_radius_from_IO
      private :: interepolate_reference_fields
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine overwrite_each_field_by_ref                           &
     &         (sph_rj, iref_source, ipol_source, ref_field, rj_fld)
!
      use fill_scalar_field
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: iref_source
      integer(kind = kint), intent(in) :: ipol_source
!
      type(phys_data), intent(inout) :: ref_field
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: i
      logical :: flag
      character(len=kchara) :: field_name
!
!
      if(ipol_source .le. 0 .and. iref_source .le. 0) return
      flag =  .FALSE.
      do i = 1, ref_field%num_phys
        if((ref_field%istack_component(i-1)+1) .eq. iref_source) then
          if(ref_field%iflag_update(i) .gt. 0) flag = .TRUE.
          field_name = ref_field%phys_name(i)
          exit
        end if
      end do
      if(flag) then
        write(*,'(3a)') 'Overwrite ', trim(field_name),                 &
     &                ' FROM radial field file'
        call copy_degree0_comps_from_sol(sph_rj%nidx_rj(2),             &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      sph_rj%nidx_rj(1), ref_field%d_fld(1,iref_source),          &
     &      rj_fld%n_point, rj_fld%d_fld(1,ipol_source))
      else
        write(*,'(3a)') 'Overwrite ', trim(field_name),                 &
     &                ' TO radial field data'
        call copy_degree0_comps_to_sol(sph_rj%nidx_rj(2),               &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      rj_fld%n_point, rj_fld%d_fld(1,ipol_source),                &
     &      sph_rj%nidx_rj(1), ref_field%d_fld(1,iref_source))
      end if
!
      end subroutine overwrite_each_field_by_ref
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine interpolate_reference_data_IO(radius_name,             &
     &          iref_radius, ref_fld_IO, ref_field, r_itp)
!
      use r_interpolate_sph_data
      use radial_interpolation
!
      character(len = kchara), intent(in) :: radius_name
      integer(kind = kint), intent(in) :: iref_radius
      type(field_IO), intent(in) :: ref_fld_IO
      type(phys_data), intent(inout) :: ref_field
!
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
!
      call alloc_org_radius_interpolate(ref_fld_IO%nnod_IO, r_itp)
      call alloc_radial_interpolate(ref_field%n_point, r_itp)
      call alloc_original_sph_data(ref_fld_IO%nnod_IO, r_itp)
      call copy_reference_radius_from_IO(ref_fld_IO,                    &
     &                                   radius_name, r_itp)
!
      call cal_radial_interpolation_coef                                &
     &   (r_itp%nri_source, r_itp%source_radius,                        &
     &    ref_field%n_point, ref_field%d_fld(1,iref_radius),            &
     &    r_itp%kr_inner_source, r_itp%kr_outer_source,                 &
     &    r_itp%k_old2new_in, r_itp%k_old2new_out,                      &
     &    r_itp%coef_old2new_in)
!
      if(iflag_debug .gt. 0) then
        call check_sph_radial_interpolate                               &
     &     (r_itp%nri_source, r_itp%source_radius,                      &
     &      ref_field%n_point, ref_field%d_fld(1,iref_radius), r_itp)
      end if
!
      call interepolate_reference_fields                                &
     &   (radius_name, ref_fld_IO, r_itp, ref_field)
      call dealloc_original_sph_data(r_itp)
!
      call dealloc_radial_interpolate(r_itp)
      call dealloc_org_radius_interpolate(r_itp)
!
      end subroutine interpolate_reference_data_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_reference_radius_from_IO                          &
     &         (radial_fld_IO, radius_name, r_itp)
!
      character(len = kchara), intent(in) :: radius_name
      type(field_IO), intent(in) :: radial_fld_IO
!
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      integer(kind = kint) :: i, icomp
!
!
      do i = 1, radial_fld_IO%num_field_IO
        if(radial_fld_IO%fld_name(i) .eq. radius_name) then
          icomp = radial_fld_IO%istack_comp_IO(i-1) + 1
          r_itp%source_radius(1:r_itp%n_rj_org)                         &
     &                  = radial_fld_IO%d_IO(1:r_itp%n_rj_org,icomp)
          exit
        end if
      end do
!
      end subroutine copy_reference_radius_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine interepolate_reference_fields                          &
     &         (radius_name, radial_fld_IO, r_itp, ref_field)
!
      use radial_interpolation
!
      character(len = kchara), intent(in) :: radius_name
      type(field_IO), intent(in) :: radial_fld_IO
!
      type(sph_radial_interpolate), intent(inout) :: r_itp
      type(phys_data), intent(inout) :: ref_field
!
!
      integer(kind = kint) :: i_fld, j_fld, ist, ied
!
!
      do i_fld = 1, ref_field%num_phys
        if(ref_field%phys_name(i_fld) .eq. radius_name) cycle
        do j_fld = 1, radial_fld_IO%num_field_IO
          if(ref_field%phys_name(i_fld)                                 &
     &             .eq. radial_fld_IO%fld_name(j_fld)) then
            ref_field%iflag_update(i_fld) = 1
            ist = ref_field%istack_component(i_fld-1) + 1
            ied = ref_field%istack_component(i_fld)
            call set_org_radius_data_from_IO                            &
     &          (j_fld, radial_fld_IO, r_itp%n_rj_org, r_itp%d_rj_org)
            call interpolate_radial_field(ref_field%n_point,            &
     &         r_itp%k_old2new_in, r_itp%k_old2new_out,                 &
     &         r_itp%coef_old2new_in, radial_fld_IO%num_comp_IO(j_fld), &
     &         r_itp%n_rj_org, r_itp%d_rj_org(1,1),                     &
     &         ref_field%d_fld(1,ist))
            ref_field%iflag_update(ist+1:ist+ied) = 1
            exit
          end if
        end do
      end do
!
      end subroutine interepolate_reference_fields
!
! ----------------------------------------------------------------------
!
      end module interpolate_reference_data
