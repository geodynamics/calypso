!>@file   init_external_magne_sph.f90
!!@brief  module init_external_magne_sph
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  reference temperature as a function of r
!!
!!
!!@verbatim
!!      subroutine init_sph_contant_ext_magne(cd_prop, sph_rj,          &
!!     &          iref_cmp, ipol_base, ref_field, rj_fld)
!!        type(MHD_evolution_param), intent(in) :: cd_prop
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(field_component_address), intent(in) :: iref_cmp
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(phys_data), intent(inout) :: ref_field, rj_fld
!!        logical, intent(inout) :: flag_ref
!!      subroutine copy_external_magne_to_rj(sph_rj,                    &
!!     &          ext_bsx, ext_bsy, ext_bsz, ext_b_rj)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        real(kind = kreal), intent(in) :: ext_bsx(sph_rj%nidx_rj(1),3)
!!        real(kind = kreal), intent(in) :: ext_bsy(sph_rj%nidx_rj(1),3)
!!        real(kind = kreal), intent(in) :: ext_bsz(sph_rj%nidx_rj(1),3)
!!        real(kind = kreal), intent(inout) :: ext_b_rj(sph_rj%nnod_rj,3)
!!@endverbatim
!
      module init_external_magne_sph
!
      use m_precision
      use m_constants
!
      implicit  none
!
      private :: copy_ext_magne_comp_to_rj, set_constant_external_magne
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_contant_ext_magne(cd_prop, sph_rj,            &
     &          iref_cmp, ipol_base, ref_field, rj_fld, flag_ref)
!
      use t_spheric_rj_data
      use t_base_field_labels
      use t_field_component_labels
      use t_phys_data
      use t_physical_property
!
      type(conductive_property), intent(in) :: cd_prop
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(field_component_address), intent(in) :: iref_cmp
      type(base_field_address), intent(in) :: ipol_base
!
      type(phys_data), intent(inout) :: ref_field, rj_fld
      logical, intent(inout) :: flag_ref
!
!
      if((iref_cmp%i_magne_x*ipol_base%i_back_B) .eq. 0) return
      flag_ref = .TRUE.
!
      if(ref_field%iflag_update(iref_cmp%i_magne_x) .le. 0) then
        call set_constant_external_magne(sph_rj, cd_prop%ex_magne,      &
     &      ref_field%d_fld(1,iref_cmp%i_magne_x),                      &
     &      ref_field%d_fld(1,iref_cmp%i_magne_y),                      &
     &      ref_field%d_fld(1,iref_cmp%i_magne_z))
      end if
!
      call copy_external_magne_to_rj(sph_rj,                            &
     &    ref_field%d_fld(1,iref_cmp%i_magne_x),                        &
     &    ref_field%d_fld(1,iref_cmp%i_magne_y),                        &
     &    ref_field%d_fld(1,iref_cmp%i_magne_z),                        &
     &    rj_fld%d_fld(1,ipol_base%i_back_B))
!
      end subroutine init_sph_contant_ext_magne
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_constant_external_magne(sph_rj, ext_b,             &
     &          ext_bsx, ext_bsy, ext_bsz)
!
      use t_spheric_rj_data
!
      real(kind = kreal), intent(in) :: ext_b(3)
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      real(kind = kreal), intent(inout) :: ext_bsx(sph_rj%nidx_rj(1),3)
      real(kind = kreal), intent(inout) :: ext_bsy(sph_rj%nidx_rj(1),3)
      real(kind = kreal), intent(inout) :: ext_bsz(sph_rj%nidx_rj(1),3)
!
      integer(kind = kint) :: kr
!
!$omp parallel do private(kr)
      do kr = 1, sph_rj%nidx_rj(1)
        ext_bsx(kr,1) =  half*ext_b(1) * sph_rj%radius_1d_rj_r(kr)**2
        ext_bsx(kr,2) =       ext_b(1) * sph_rj%radius_1d_rj_r(kr)
        ext_bsx(kr,3) =  zero
!
        ext_bsy(kr,1) = half*ext_b(2) * sph_rj%radius_1d_rj_r(kr)**2
        ext_bsy(kr,2) =      ext_b(2) * sph_rj%radius_1d_rj_r(kr)
        ext_bsy(kr,3) = zero
!
        ext_bsz(kr,1) =  half*ext_b(3) * sph_rj%radius_1d_rj_r(kr)**2
        ext_bsz(kr,2) =       ext_b(3) * sph_rj%radius_1d_rj_r(kr)
        ext_bsz(kr,3) =  zero
      end do
!$omp end parallel do
!
      end subroutine set_constant_external_magne
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine copy_external_magne_to_rj(sph_rj,                      &
     &          ext_bsx, ext_bsy, ext_bsz, ext_b_rj)
!
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: ext_bsx(sph_rj%nidx_rj(1),3)
      real(kind = kreal), intent(in) :: ext_bsy(sph_rj%nidx_rj(1),3)
      real(kind = kreal), intent(in) :: ext_bsz(sph_rj%nidx_rj(1),3)
!
      real(kind = kreal), intent(inout) :: ext_b_rj(sph_rj%nnod_rj,3)
!
!
!$omp parallel workshare
      ext_b_rj(1:sph_rj%nnod_rj,1:3) = 0.0d0
!$omp end parallel workshare
!
      call copy_ext_magne_comp_to_rj(sph_rj%idx_rj_degree_one( 1),      &
     &    sph_rj%nnod_rj, sph_rj%nidx_rj(1), sph_rj%istep_rj(1),        &
     &    ext_bsx, ext_b_rj)
      call copy_ext_magne_comp_to_rj(sph_rj%idx_rj_degree_one(-1),      &
     &    sph_rj%nnod_rj, sph_rj%nidx_rj(1), sph_rj%istep_rj(1),        &
     &    ext_bsy, ext_b_rj)
      call copy_ext_magne_comp_to_rj(sph_rj%idx_rj_degree_one( 0),      &
     &    sph_rj%nnod_rj, sph_rj%nidx_rj(1), sph_rj%istep_rj(1),        &
     &    ext_bsz, ext_b_rj)
!
      end subroutine copy_external_magne_to_rj
!
!  -------------------------------------------------------------------
!
      subroutine copy_ext_magne_comp_to_rj(idx_rj_degree_one, nnod_rj,  &
     &          nri, istep_rj, ext_bs, ext_b_rj)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_one
      integer(kind = kint), intent(in) :: nnod_rj, nri
      integer(kind = kint), intent(in) :: istep_rj(2)
      real(kind = kreal), intent(in) :: ext_bs(nri,3)
!
      real(kind = kreal), intent(inout) :: ext_b_rj(nnod_rj,3)
!
      integer(kind = kint) :: kr, inod
!
!
      if(idx_rj_degree_one .le. 0) return
!$omp parallel do private(kr,inod)
      do kr = 1, nri
        inod = 1 + (kr-1) * istep_rj(1)                                 &
     &           + (idx_rj_degree_one- 1) * istep_rj(2) 
        ext_b_rj(inod,1) = ext_bs(kr,1)
        ext_b_rj(inod,2) = ext_bs(kr,2)
        ext_b_rj(inod,3) = ext_bs(kr,3)
      end do
!$omp end parallel do
!
      end subroutine copy_ext_magne_comp_to_rj
!
!  -------------------------------------------------------------------
!
      end module init_external_magne_sph
