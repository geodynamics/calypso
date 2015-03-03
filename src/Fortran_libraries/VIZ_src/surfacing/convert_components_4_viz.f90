!convert_components_4_viz.f90
!      module convert_components_4_viz
!
!      Written by H. Matsui on June, 2006
!
!      subroutine convert_comps_4_viz(nnod, istack_n_smp, xx,           &
!     &          sph_r, a_r, cyl_s, a_s, ncomp_viz, ncomp_org,          &
!     &          icomp_viz, dat_xyz, dat_viz)
!
      module convert_components_4_viz
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine convert_comps_4_viz(nnod, istack_n_smp, xx,            &
     &          sph_r, a_r, cyl_s, a_s, ncomp_viz, ncomp_org,           &
     &          icomp_viz, dat_xyz, dat_viz)
!
      use set_components_flags
      use set_nodal_field_for_psf
      use mag_of_field_smp
      use copy_field_smp
      use cvt_xyz_vector_2_sph_smp
      use cvt_xyz_vector_2_cyl_smp
      use cvt_xyz_tensor_2_sph_smp
      use cvt_xyz_tensor_2_cyl_smp
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: istack_n_smp(0:np_smp)
      real(kind = kreal), intent(in) :: xx(nnod,3)
      real(kind = kreal), intent(in) :: sph_r(nnod), a_r(nnod)
      real(kind = kreal), intent(in) :: cyl_s(nnod), a_s(nnod)
      integer(kind = kint), intent(in) :: ncomp_viz
      integer(kind = kint), intent(in) :: ncomp_org
      integer(kind = kint), intent(in) :: icomp_viz
      real(kind = kreal), intent(in) :: dat_xyz(nnod,ncomp_org)
!
      real(kind = kreal), intent(inout) :: dat_viz(nnod,ncomp_viz)
!
!
!$omp parallel
        if (icomp_viz .eq. 0) then
          if( ncomp_org .eq. ncomp_SCALAR) then
            call copy_nod_scalar_smp(np_smp, nnod, istack_n_smp,        &
     &          dat_xyz(1,1), dat_viz(1,1))
          else if ( ncomp_org .eq. ncomp_VECTOR) then
            call cal_vector_magnitude(np_smp, nnod, istack_n_smp,       &
     &          dat_xyz(1,1), dat_viz(1,1))
          else if ( ncomp_org .eq. ncomp_SYM_TENSOR) then
            call cal_sym_tensor_magnitude(np_smp, nnod, istack_n_smp,   &
     &          dat_xyz(1,1), dat_viz(1,1))
          end if
!
        else if ( icomp_viz.eq.icomp_VECTOR                             &
     &        .or. icomp_viz.eq.icomp_ASYM_TENSOR) then
          call copy_nod_vector_smp(np_smp, nnod, istack_n_smp,          &
     &        dat_xyz(1,1), dat_viz(1,1))
!
        else if ( icomp_viz .eq. icomp_SYM_TENSOR ) then
          call copy_nod_sym_tensor_smp(np_smp, nnod, istack_n_smp,      &
     &        dat_xyz(1,1), dat_viz(1,1))
!
!
        else if (icomp_viz .eq. icomp_SPH_VECTOR) then
          call cvt_vector_2_sph_smp(np_smp, nnod, istack_n_smp,         &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), xx(1,3),    &
     &        sph_r, cyl_s, a_r, a_s)
!
        else if (icomp_viz .eq. icomp_CYL_VECTOR) then
          call cvt_vector_2_cyl_smp(np_smp, nnod, istack_n_smp,         &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), cyl_s, a_s)
!
        else if ( icomp_viz .eq. icomp_SPHL_SYM_TENSOR ) then
          call cal_sph_tensor_smp(np_smp, nnod, istack_n_smp,           &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), xx(1,3),    &
     &        sph_r, cyl_s, a_r, a_s)
!
        else if ( icomp_viz .eq. icomp_CYL_SYM_TENSOR ) then
          call cal_cyl_tensor_smp(np_smp, nnod, istack_n_smp,           &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), cyl_s, a_s)
!
!
        else if (icomp_viz.ge.icomp_XX                                  &
     &           .and. icomp_viz.le.icomp_ZZ) then
          call copy_nod_scalar_smp(np_smp, nnod, istack_n_smp,          &
     &        dat_xyz(1,icomp_viz), dat_viz(1,1))
!
        else if (icomp_viz .eq. icomp_RADIAL) then
          call cal_radial_comp_smp(np_smp, nnod, istack_n_smp,          &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), xx(1,3),    &
     &        sph_r, a_r)
        else if (icomp_viz .eq. icomp_THETA) then
          call cal_theta_comp_smp(np_smp, nnod, istack_n_smp,           &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), xx(1,3),    &
     &        sph_r, cyl_s, a_r, a_s)
!
        else if (icomp_viz .eq. icomp_PHI) then
          call cal_phi_comp_smp(np_smp, nnod, istack_n_smp,             &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), cyl_s, a_s)
!
        else if (icomp_viz .eq. icomp_CYLINDER_R) then
          call cal_cylinder_r_comp_smp(np_smp, nnod, istack_n_smp,      &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), cyl_s, a_s)
!
!
        else if ( icomp_viz .eq. icomp_RR ) then
          call cal_rr_tensor_smp(np_smp, nnod, istack_n_smp,            &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), xx(1,3),    &
     &        sph_r, cyl_s, a_r)
!
        else if ( icomp_viz .eq. icomp_RT ) then
          call cal_rt_tensor_smp(np_smp, nnod, istack_n_smp,            &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), xx(1,3),    &
     &        sph_r, cyl_s, a_r, a_s)
!
        else if ( icomp_viz .eq. icomp_RP ) then
          call cal_rp_tensor_smp(np_smp, nnod, istack_n_smp,            &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), xx(1,3),    &
     &        sph_r, cyl_s, a_r, a_s)
!
        else if ( icomp_viz .eq. icomp_TT ) then
          call cal_tt_tensor_smp(np_smp, nnod, istack_n_smp,            &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), xx(1,3),    &
     &        sph_r, cyl_s, a_r, a_s)
!
        else if ( icomp_viz .eq. icomp_TP ) then
          call cal_tp_tensor_smp(np_smp, nnod, istack_n_smp,            &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), xx(1,3),    &
     &        sph_r, cyl_s, a_r, a_s)
!
        else if ( icomp_viz .eq. icomp_PP ) then
          call cal_pp_tensor_smp(np_smp, nnod, istack_n_smp,            &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), cyl_s, a_s)
!
!
        else if ( icomp_viz .eq. icomp_SS ) then
          call cal_ss_tensor_smp(np_smp, nnod, istack_n_smp,            &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), cyl_s, a_s)
!
        else if ( icomp_viz .eq. icomp_SP ) then
          call cal_sp_tensor_smp(np_smp, nnod, istack_n_smp,            &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), cyl_s, a_s)
!
        else if ( icomp_viz .eq. icomp_SZ ) then
          call cal_sz_tensor_smp(np_smp, nnod, istack_n_smp,            &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), cyl_s, a_s)
!
        else if ( icomp_viz .eq. icomp_PP_cyl ) then
          call cal_pp_cyl_tensor_smp(np_smp, nnod, istack_n_smp,        &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), cyl_s, a_s)
!
        else if ( icomp_viz .eq. icomp_PZ) then
          call cal_pz_tensor_smp(np_smp, nnod, istack_n_smp,            &
     &        dat_xyz(1,1), dat_viz(1,1), xx(1,1), xx(1,2), cyl_s, a_s)
!
        else if ( icomp_viz .eq. icomp_ZZ_cyl ) then
          call cal_zz_tensor_smp(np_smp, nnod, istack_n_smp,            &
     &        dat_xyz(1,1), dat_viz(1,1) )
        end if
!$omp end parallel
!
      end subroutine convert_comps_4_viz
!
!  ---------------------------------------------------------------------
!
      end module convert_components_4_viz
