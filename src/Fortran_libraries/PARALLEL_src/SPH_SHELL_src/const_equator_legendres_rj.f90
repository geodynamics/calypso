!>@file   const_equator_legendres_rj.f90
!!@brief  module const_equator_legendres_rj
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform considering symmetry
!!
!!@verbatim
!!      subroutine s_const_equator_legendres_rj                         &
!!     &         (sph_params, sph_rj, sph_rlm, sph_rtm, comms_sph,      &
!!     &          trans_p, P_circ, dPdt_circ, SR_sig, SR_r)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        real(kind = kreal), intent(inout)                             &
!!       &                   :: P_circ(sph_rj%nidx_rj(2))
!!        real(kind = kreal), intent(inout)                             &
!!       &                   :: dPdt_circ(sph_rj%nidx_rj(2))
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module const_equator_legendres_rj
!
      use m_precision
      use m_machine_parameter
      use m_constants
!
      use t_legendre_work_sym_matmul
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_phys_data
      use t_work_4_sph_trans
      use t_solver_SR
!
      implicit none
!
      private :: set_equator_lagende, set_equator_legendre_lj
      private :: eq_leg_fwd_trans_vector_sym
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_equator_legendres_rj                           &
     &         (colat, sph_params, sph_rj, sph_rlm, sph_rtm,            &
     &          comms_sph, trans_p, P_circ, dPdt_circ, SR_sig, SR_r)
!
      use calypso_mpi
      use set_legendre_matrices
      use spherical_SRs_N
      use copy_spectr_4_sph_trans
!
      real(kind = kreal), intent(in) :: colat
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: P_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout)                                 &
     &                   :: dPdt_circ(sph_rj%nidx_rj(2))
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!>        Legendre polynomials at equator in sph_rj configuration
!!              Pvec_rj%d_fld(:,1) :: P_l^m
!!              Pvec_rj%d_fld(:,2) :: d P_l^m / d theta
      type(phys_data) :: Pvec_rj
!
      real(kind = kreal), allocatable :: P_eq(:)
      real(kind = kreal), allocatable :: dPdt_eq(:)
!
      real(kind = kreal), allocatable :: Ps_eq(:)
      real(kind = kreal), allocatable :: dPsdt_eq(:)
!
      integer(kind = kint) :: i, j
!
      allocate( P_eq(sph_rlm%nidx_rlm(2)) )
      allocate( dPdt_eq(sph_rlm%nidx_rlm(2)) )
!
      allocate(Ps_eq(sph_rlm%nidx_rlm(2)))
      allocate(dPsdt_eq(sph_rlm%nidx_rlm(2)))
!
!$omp parallel workshare
      P_eq(1:sph_rlm%nidx_rlm(2)) = 0.0d0
      dPdt_eq(1:sph_rlm%nidx_rlm(2)) = 0.0d0
!
      Ps_eq(1:sph_rlm%nidx_rlm(2)) =    0.0d0
      dPsdt_eq(1:sph_rlm%nidx_rlm(2)) = 0.0d0
!$omp end parallel workshare
!
      Pvec_rj%num_phys =  1
      call  alloc_phys_name(Pvec_rj)
      Pvec_rj%phys_name(1) = 'P_vector'
      Pvec_rj%istack_component(0) = 0
      Pvec_rj%num_component(1) =    n_vector
      Pvec_rj%istack_component(1) = n_vector
!
      Pvec_rj%ntot_phys = Pvec_rj%istack_component(1)
      Pvec_rj%num_phys_viz =  Pvec_rj%num_phys
      Pvec_rj%ntot_phys_viz = Pvec_rj%ntot_phys
      call alloc_phys_data(sph_rj%nnod_rj, Pvec_rj)
!
      call set_equator_lagende(colat, sph_params%l_truncation,          &
     &    sph_rtm, sph_rlm, trans_p%idx_trns, P_eq, dPdt_eq)
!
      call set_equator_legendre_lj                                      &
     &  (sph_rtm%nidx_rtm(3), sph_rlm%nidx_rlm(2),                      &
     &   trans_p%idx_trns%lstack_rlm, trans_p%idx_trns%lstack_even_rlm, &
     &   P_eq, dPdt_eq, Ps_eq, dPsdt_eq)
      deallocate(P_eq, dPdt_eq)
!
      call eq_leg_fwd_trans_vector_sym(sph_rtm, sph_rlm,                &
     &    comms_sph%comm_rlm, trans_p%idx_trns, Ps_eq, dPsdt_eq,        &
     &    SR_r%n_WS, SR_r%WS(1))
      deallocate(Ps_eq, dPsdt_eq)
!
!
      call calypso_sph_comm_N(ithree,                                   &
     &    comms_sph%comm_rlm, comms_sph%comm_rj, SR_sig, SR_r)
      call finish_send_recv_sph(comms_sph%comm_rlm, SR_sig)
!
      call sel_sph_rj_vector_from_recv(trans_p%iflag_SPH_recv, ithree,  &
     &   ione, ione, comms_sph%comm_rj, SR_r%n_WR, SR_r%WR(1), Pvec_rj)
!
!$omp parallel do private(j,i)
      do j = 1, sph_rj%nidx_rj(2)
        i = 1 +(j-1) * sph_rj%istep_rj(2)
        P_circ(j) =    Pvec_rj%d_fld(i,1)
        dPdt_circ(j) = Pvec_rj%d_fld(i,2)
      end do
!$omp end parallel do
      call dealloc_phys_data(Pvec_rj)
      call dealloc_phys_name(Pvec_rj)
!
      end subroutine s_const_equator_legendres_rj
!
! -----------------------------------------------------------------------
!
      subroutine eq_leg_fwd_trans_vector_sym(sph_rtm, sph_rlm,          &
     &          comm_rlm, idx_trns, Ps_eq, dPsdt_eq, n_WS, WS)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in) :: Ps_eq(sph_rlm%nidx_rlm(2))
      real(kind = kreal), intent(in) :: dPsdt_eq(sph_rlm%nidx_rlm(2))
      integer(kind = kint), intent(in) :: n_WS
!
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      integer(kind = kint) :: k_rlm, ie_rlm, io_rlm
      integer(kind = kint) :: mp_rlm, ie_send, io_send
      integer(kind = kint) :: jst, nj_rlm, jj, je_rlm, jo_rlm
      integer(kind = kint) :: nle_rtm, nlo_rtm, n_jk_e, n_jk_o
!
!
!$omp parallel workshare
      WS(1:n_vector*comm_rlm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nle_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nlo_rtm = sph_rtm%nidx_rtm(2) / 2
      k_rlm = 1
!$omp parallel do schedule(static) private(jj,je_rlm,jo_rlm,mp_rlm,jst, &
!$omp&                                   nj_rlm,n_jk_e,n_jk_o,          &
!$omp&                                   ie_rlm,io_rlm,ie_send,io_send)
          do mp_rlm = 1, sph_rtm%nidx_rtm(3)
            jst = idx_trns%lstack_rlm(mp_rlm-1)
            nj_rlm = idx_trns%lstack_rlm(mp_rlm)                        &
     &              - idx_trns%lstack_rlm(mp_rlm-1)
            n_jk_e = (nj_rlm+1) / 2
            n_jk_o =  nj_rlm - n_jk_e
!    even l-m
!    odd  l-m
            do jj = 1, nj_rlm/2
              je_rlm = 2*jj + jst - 1
              jo_rlm = 2*jj + jst
              ie_rlm = 1 + (je_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1) *  sph_rlm%istep_rlm(1)
              io_rlm = 1 + (jo_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1)  * sph_rlm%istep_rlm(1)
              ie_send = (comm_rlm%irev_sr(ie_rlm) - 1) * n_vector
              io_send = (comm_rlm%irev_sr(io_rlm) - 1) * n_vector
!
              WS(ie_send+1) = Ps_eq(jj+jst)
              WS(ie_send+2) = dPsdt_eq(jj+jst)
              WS(ie_send+3) = zero
!
              WS(io_send+1) = Ps_eq(jj+jst+n_jk_e)
              WS(io_send+2) = dPsdt_eq(jj+jst+n_jk_e)
              WS(io_send+3) = zero
            end do
!
!   the last even l-m
            do jj = nj_rlm/2+1, (nj_rlm+1)/2
              je_rlm = 2*jj + jst - 1
              ie_rlm = 1 + (je_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1) *  sph_rlm%istep_rlm(1)
              ie_send = (comm_rlm%irev_sr(ie_rlm) - 1) * n_vector
!
              WS(ie_send+1) = Ps_eq(jj+jst)
              WS(ie_send+2) = dPsdt_eq(jj+jst)
              WS(ie_send+3) = zero
            end do
!
          end do
!$omp end parallel do
!
      end subroutine eq_leg_fwd_trans_vector_sym
!
! -----------------------------------------------------------------------
!
      subroutine set_equator_legendre_lj                                &
     &         (mphi_rtm, jmax_rlm, lstack_rlm, lstack_even_rlm,        &
     &          P_eq, dPdt_eq, Ps_eq, dPsdt_eq)
!
      integer(kind = kint), intent(in) :: mphi_rtm, jmax_rlm
      integer(kind = kint), intent(in) :: lstack_rlm(0:mphi_rtm)
      integer(kind = kint), intent(in) :: lstack_even_rlm(0:mphi_rtm)
!
      real(kind= kreal), intent(in) :: P_eq(jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_eq(jmax_rlm)
!
      real(kind= kreal), intent(inout) :: Ps_eq(jmax_rlm)
      real(kind= kreal), intent(inout) :: dPsdt_eq(jmax_rlm)
!
      integer(kind = kint) :: j_rlm
      integer(kind = kint) :: mp_rlm, jst, n_jk_e, n_jk_o, jj
!
!
!$omp parallel do private(jst,j_rlm,jj,n_jk_e,n_jk_o)
      do mp_rlm = 1, mphi_rtm
        jst = lstack_rlm(mp_rlm-1)
        n_jk_e = lstack_even_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
        n_jk_o = lstack_rlm(mp_rlm) - lstack_even_rlm(mp_rlm)
!
        do jj = 1, n_jk_e
          j_rlm = 2*jj + jst - 1
          Ps_eq(jj+jst) =     P_eq(j_rlm)
          dPsdt_eq(jj+jst) =  dPdt_eq(j_rlm)
        end do
!
        do jj = 1, n_jk_o
          j_rlm = 2*jj + jst
          Ps_eq(jj+jst+n_jk_e) =     P_eq(j_rlm)
          dPsdt_eq(jj+jst+n_jk_e) =  dPdt_eq(j_rlm)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_equator_legendre_lj
!
! -----------------------------------------------------------------------
!
      subroutine set_equator_lagende(colat, ltr, sph_rtm, sph_rlm,      &
     &                               idx_trns, P_eq, dPdt_eq)
!
      use schmidt_fix_m
!
      integer(kind = kint), intent(in) :: ltr
      real(kind = kreal), intent(in) :: colat
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      real(kind= kreal), intent(inout) :: P_eq(sph_rlm%nidx_rlm(2))
      real(kind= kreal), intent(inout) :: dPdt_eq(sph_rlm%nidx_rlm(2))
!
      integer(kind = kint) :: j, l, m, mm, jj
      integer(kind = kint) :: jst, jed
      real(kind = kreal) :: p_m(0:ltr), dp_m(0:ltr)
      real(kind = kreal) :: pmp1(0:ltr), pmn1(0:ltr)
      real(kind = kreal) :: df_m(0:ltr+2)
!
!$omp parallel do private(j,l,m,mm,jj,jst,jed,p_m,dp_m,pmn1,pmp1,df_m)
      do m = 1, sph_rtm%nidx_rtm(3)
        mm = abs(sph_rtm%idx_gl_1d_rtm_m(m,2))
        jst = idx_trns%lstack_rlm(m-1) + 1
        jed = idx_trns%lstack_rlm(m)
        call schmidt_legendres_m(ltr, mm, colat,                        &
     &                           p_m, dp_m, pmn1, pmp1, df_m)
!
        do j = jst, jed
          jj = sph_rlm%idx_gl_1d_rlm_j(j,1)
          l =  sph_rlm%idx_gl_1d_rlm_j(j,2)
          P_eq(j) =    p_m(l)
          dPdt_eq(j) = dp_m(l)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_equator_lagende
!
! -----------------------------------------------------------------------
!
      end module const_equator_legendres_rj
