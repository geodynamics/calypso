!> @file  copy_MHD_4_pole_trans.f90
!!      module copy_MHD_4_pole_trans
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief copy spectr data for spherical transform at poles
!!
!!@verbatim
!!      subroutine copy_snap_vec_from_pole_trans(m_folding,             &
!!     &          sph_rtp, bs_trns, ncomp_snap_rj_2_rtp, fls_pl,        &
!!     &          node, iphys, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(phys_address), intent(in) :: bs_trns
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module copy_MHD_4_pole_trans
!
      use m_precision
      use m_constants
!
      use t_spheric_rtp_data
      use t_geometry_data
      use t_phys_address
      use t_phys_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_snap_vec_from_pole_trans(m_folding,               &
     &          sph_rtp, bs_trns, ncomp_snap_rj_2_rtp, fls_pl,          &
     &          node, iphys, nod_fld)
!
      use m_machine_parameter
!
      use copy_pole_field_sph_trans
!
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: bs_trns
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      integer(kind = kint), intent(in) :: ncomp_snap_rj_2_rtp
      real(kind = kreal), intent(in)                                    &
     &           :: fls_pl(sph_rtp%nnod_pole,ncomp_snap_rj_2_rtp)
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_pole_vec_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%base%i_velo), iphys%base%i_velo, nod_fld)
      call copy_pole_vec_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%base%i_vort), iphys%base%i_vort, nod_fld)
      call copy_pole_vec_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%base%i_magne), iphys%base%i_magne, nod_fld)
      call copy_pole_vec_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%base%i_current), iphys%base%i_current,       &
     &    nod_fld)
!
      call copy_pole_vec_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%diffusion%i_v_diffuse),                      &
     &    iphys%diffusion%i_v_diffuse, nod_fld)
      call copy_pole_vec_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%diffusion%i_w_diffuse),                      &
     &    iphys%diffusion%i_w_diffuse, nod_fld)
      call copy_pole_vec_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%diffusion%i_vp_diffuse),                     &
     &    iphys%diffusion%i_vp_diffuse, nod_fld)
      call copy_pole_vec_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%diffusion%i_b_diffuse),                      &
     &    iphys%diffusion%i_b_diffuse, nod_fld)
!
      call copy_pole_vec_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%forces%i_induction),                         &
     &    iphys%forces%i_induction, nod_fld)
!
      call copy_pole_vec_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%grad_fld%i_grad_temp),                       &
     &    iphys%grad_fld%i_grad_temp, nod_fld)
      call copy_pole_vec_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%grad_fld%i_grad_composit),                   &
     &    iphys%grad_fld%i_grad_composit, nod_fld)
!
!
!
      call copy_pole_scl_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%base%i_temp), iphys%base%i_temp, nod_fld)
      call copy_pole_scl_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%base%i_light), iphys%base%i_light, nod_fld)
!
      call copy_pole_scl_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%base%i_press), iphys%base%i_press, nod_fld)
      call copy_pole_scl_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%base%i_per_temp),                            &
     &    iphys%base%i_per_temp, nod_fld)
      call copy_pole_scl_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%diffusion%i_t_diffuse),                      &
     &    iphys%diffusion%i_t_diffuse, nod_fld)
      call copy_pole_scl_fld_from_trans(m_folding, sph_rtp, node,       &
     &    fls_pl(1,bs_trns%diffusion%i_c_diffuse),                      &
     &    iphys%diffusion%i_c_diffuse, nod_fld)
!
      end subroutine copy_snap_vec_from_pole_trans
!
! -----------------------------------------------------------------------
!
      end module copy_MHD_4_pole_trans
