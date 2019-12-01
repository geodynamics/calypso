!>@file   set_vr_rtm_sym_mat_tsmp.f90
!!@brief  module set_vr_rtm_sym_mat_tsmp
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine set_vr_rtm_sym_mat_rout(nnod_rtm, nidx_rtm,          &
!!     &         istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,    &
!!     &         mp_rlm, lst_rtm, nle_rtm, nlo_rtm,                     &
!!     &         ncomp_recv, nvector, nscalar, irev_sr_rtm, n_WR, WR,   &
!!     &         symp_r, asmp_p, asmp_r, symp_p)
!!      subroutine set_vr_rtm_sym_mat_rin(nnod_rtm, nidx_rtm,           &
!!     &         istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,    &
!!     &         mp_rlm, lst_rtm, nle_rtm, nlo_rtm,                     &
!!     &         ncomp_recv, nvector, nscalar, irev_sr_rtm, n_WR, WR,   &
!!     &         symp_r, asmp_p, asmp_r, symp_p)
!!@endverbatim
!!
      module set_vr_rtm_sym_mat_tsmp
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
!
      implicit none
!
      integer, external :: omp_get_max_threads
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_sym_mat_rout(nnod_rtm, nidx_rtm,            &
     &         istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,      &
     &         mp_rlm, lst_rtm, nle_rtm, nlo_rtm,                       &
     &         ncomp_recv, nvector, nscalar, irev_sr_rtm, n_WR, WR,     &
     &         symp_r, asmp_p, asmp_r, symp_p)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: lst_rtm, nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp_recv
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nvector, nscalar
!
      real(kind=kreal), intent(inout)                                   &
     &         :: symp_r(nle_rtm,nidx_rlm(1),3*nvector+nscalar)
      real(kind=kreal), intent(inout)                                   &
     &         :: asmp_p(nle_rtm,nidx_rlm(1),2*nvector)
      real(kind=kreal), intent(inout)                                   &
     &         :: asmp_r(nle_rtm,nidx_rlm(1),3*nvector+nscalar)
      real(kind=kreal), intent(inout)                                   &
     &         :: symp_p(nle_rtm,nidx_rlm(1),2*nvector)
!
      integer(kind = kint) :: k_rlm, nd, mn_rlm
      integer(kind = kint) :: lt, lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      real(kind = kreal) :: wp_rtm, asin_rtm
!
!
      mn_rlm = nidx_rtm(3) - mp_rlm + 1
!
      do nd = 1, nvector
        do lt = 1, nlo_rtm
          lp_rtm = lst_rtm + lt
          ln_rtm = nidx_rtm(2) - lp_rtm + 1
          wp_rtm =   weight_rtm(lp_rtm)
          asin_rtm = asin_theta_1d_rtm(lp_rtm)
          do k_rlm = 1, nidx_rlm(1)
!
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
            ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
            in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
            in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
            ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_recv
            ipn_recv = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_recv
            inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_recv
            inn_recv = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp_recv
!
            symp_r(lt,k_rlm,3*nd-2)                                     &
     &          = (WR(ipp_recv-2) + WR(ipn_recv-2)) * wp_rtm
            symp_p(lt,k_rlm,2*nd  )                                     &
     &          = (WR(ipp_recv-1) + WR(ipn_recv-1)) * wp_rtm
            symp_p(lt,k_rlm,2*nd-1)                                     &
     &          = (WR(ipp_recv  ) + WR(ipn_recv  )) * wp_rtm
!
            asmp_r(lt,k_rlm,3*nd-2)                                     &
     &          = (WR(ipp_recv-2) - WR(ipn_recv-2)) * wp_rtm
            asmp_p(lt,k_rlm,2*nd  )                                     &
     &          = (WR(ipp_recv-1) - WR(ipn_recv-1)) * wp_rtm
            asmp_p(lt,k_rlm,2*nd-1)                                     &
     &          = (WR(ipp_recv  ) - WR(ipn_recv  )) * wp_rtm
!
            symp_r(lt,k_rlm,3*nd-1)                                     &
     &          = (WR(inp_recv-1) + WR(inn_recv-1)) * wp_rtm * asin_rtm
            symp_r(lt,k_rlm,3*nd  )                                     &
     &          = (WR(inp_recv  ) + WR(inn_recv  )) * wp_rtm * asin_rtm
!
            asmp_r(lt,k_rlm,3*nd-1)                                     &
     &          = (WR(inp_recv-1) - WR(inn_recv-1)) * wp_rtm * asin_rtm
            asmp_r(lt,k_rlm,3*nd  )                                     &
     &          = (WR(inp_recv  ) - WR(inn_recv  )) * wp_rtm * asin_rtm
          end do
        end do
      end do
!
!   Equator (if necessary)
      do nd = 1, nvector
        do lt = nlo_rtm+1, nle_rtm
          lp_rtm = lst_rtm + lt
          wp_rtm = weight_rtm(lp_rtm)
          asin_rtm = asin_theta_1d_rtm(lp_rtm)
          do k_rlm = 1, nidx_rlm(1)
!
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
            in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
            ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_recv
            inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_recv
!
            symp_r(lt,k_rlm,3*nd-2) = WR(ipp_recv-2) * wp_rtm
            symp_p(lt,k_rlm,2*nd  ) = WR(ipp_recv-1) * wp_rtm
            symp_p(lt,k_rlm,2*nd-1) = WR(ipp_recv  ) * wp_rtm
!
            asmp_r(lt,k_rlm,3*nd-2) = 0.0d0
            asmp_p(lt,k_rlm,2*nd  ) = 0.0d0
            asmp_p(lt,k_rlm,2*nd-1) = 0.0d0
!
            symp_r(lt,k_rlm,3*nd-1) = WR(inp_recv-1)                    &
     &                                   * wp_rtm * asin_rtm
            symp_r(lt,k_rlm,3*nd  ) = WR(inp_recv  )                    &
     &                                   * wp_rtm * asin_rtm
!
            asmp_r(lt,k_rlm,3*nd-1) = 0.0d0
            asmp_r(lt,k_rlm,3*nd  ) = 0.0d0
          end do
        end do
      end do
!
!
      do nd = 1, nscalar
        do k_rlm = 1, nidx_rlm(1)
          do lt = 1, nlo_rtm
            lp_rtm = lst_rtm + lt
            ln_rtm = nidx_rtm(2) - lp_rtm + 1
            wp_rtm = weight_rtm(lp_rtm)
!
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
            ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
!
            ipp_recv = nd + 3*nvector                                   &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_recv
            ipn_recv = nd + 3*nvector                                   &
     &                  + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_recv
!
            symp_r(lt,k_rlm,nd+3*nvector)                           &
     &               = (WR(ipp_recv) + WR(ipn_recv)) * wp_rtm
            asmp_r(lt,k_rlm,nd+3*nvector)                           &
     &               = (WR(ipp_recv) - WR(ipn_recv)) * wp_rtm
          end do
        end do
      end do
!
!   Equator (if necessary)
      do nd = 1, nscalar
        do k_rlm = 1, nidx_rlm(1)
          do lt = nlo_rtm+1, nle_rtm
            lp_rtm = lst_rtm + lt
            wp_rtm = weight_rtm(lp_rtm)
              ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                   &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
!
            ipp_recv = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_recv
!
            symp_r(lt,k_rlm,nd+3*nvector) = WR(ipp_recv) * wp_rtm
            asmp_r(lt,k_rlm,nd+3*nvector) = 0.0d0
          end do
        end do
      end do
!
      end subroutine set_vr_rtm_sym_mat_rout
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_sym_mat_rin(nnod_rtm, nidx_rtm,             &
     &         istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,      &
     &         mp_rlm, lst_rtm, nle_rtm, nlo_rtm,                       &
     &         ncomp_recv, nvector, nscalar, irev_sr_rtm, n_WR, WR,     &
     &         symp_r, asmp_p, asmp_r, symp_p)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: lst_rtm, nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp_recv
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nvector, nscalar
!
      real(kind=kreal), intent(inout)                                   &
     &         :: symp_r(3*nvector+nscalar,nidx_rlm(1),nle_rtm)
      real(kind=kreal), intent(inout)                                   &
     &         :: asmp_p(2*nvector,nidx_rlm(1),nle_rtm)
      real(kind=kreal), intent(inout)                                   &
     &         :: asmp_r(3*nvector+nscalar,nidx_rlm(1),nle_rtm)
      real(kind=kreal), intent(inout)                                   &
     &         :: symp_p(2*nvector,nidx_rlm(1),nle_rtm)
!
      integer(kind = kint) :: k_rlm, nd, mn_rlm
      integer(kind = kint) :: lt, lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      real(kind = kreal) :: wp_rtm, asin_rtm
!
!
      mn_rlm = nidx_rtm(3) - mp_rlm + 1
!
      do lt = 1, nlo_rtm
        lp_rtm = lst_rtm + lt
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm =   weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do k_rlm = 1, nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          do nd = 1, nvector
            ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_recv
            ipn_recv = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_recv
            inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_recv
            inn_recv = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp_recv
!
            symp_r(3*nd-2,k_rlm,lt)                                     &
     &          = (WR(ipp_recv-2) + WR(ipn_recv-2)) * wp_rtm
            symp_p(2*nd,  k_rlm,lt)                                     &
     &          = (WR(ipp_recv-1) + WR(ipn_recv-1)) * wp_rtm
            symp_p(2*nd-1,k_rlm,lt)                                     &
     &          = (WR(ipp_recv  ) + WR(ipn_recv  )) * wp_rtm
!
            asmp_r(3*nd-2,k_rlm,lt)                                     &
     &          = (WR(ipp_recv-2) - WR(ipn_recv-2)) * wp_rtm
            asmp_p(2*nd,  k_rlm,lt)                                     &
     &          = (WR(ipp_recv-1) - WR(ipn_recv-1)) * wp_rtm
            asmp_p(2*nd-1,k_rlm,lt)                                     &
     &          = (WR(ipp_recv  ) - WR(ipn_recv  )) * wp_rtm
!
            symp_r(3*nd-1,k_rlm,lt)                                     &
     &          = (WR(inp_recv-1) + WR(inn_recv-1)) * wp_rtm * asin_rtm
            symp_r(3*nd,  k_rlm,lt)                                     &
     &          = (WR(inp_recv  ) + WR(inn_recv  )) * wp_rtm * asin_rtm
!
            asmp_r(3*nd-1,k_rlm,lt)                                     &
     &          = (WR(inp_recv-1) - WR(inn_recv-1)) * wp_rtm * asin_rtm
            asmp_r(3*nd,  k_rlm,lt)                                     &
     &          = (WR(inp_recv  ) - WR(inn_recv  )) * wp_rtm * asin_rtm
          end do
        end do
!
        wp_rtm = weight_rtm(lp_rtm)
        do k_rlm = 1, nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
!
          do nd = 1, nscalar
            ipp_recv = nd + 3*nvector                                   &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_recv
            ipn_recv = nd + 3*nvector                                   &
     &                  + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_recv
!
            symp_r(nd+3*nvector,k_rlm,lt)                               &
     &               = (WR(ipp_recv) + WR(ipn_recv)) * wp_rtm
            asmp_r(nd+3*nvector,k_rlm,lt)                               &
     &               = (WR(ipp_recv) - WR(ipn_recv)) * wp_rtm
          end do
        end do
      end do
!
!   Equator (if necessary)
      do lt = nlo_rtm+1, nle_rtm
        lp_rtm = lst_rtm + lt
        wp_rtm = weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do k_rlm = 1, nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          do nd = 1, nvector
            ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_recv
            inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_recv
!
            symp_r(3*nd-2,k_rlm,lt) = WR(ipp_recv-2) * wp_rtm
            symp_p(2*nd,  k_rlm,lt) = WR(ipp_recv-1) * wp_rtm
            symp_p(2*nd-1,k_rlm,lt) = WR(ipp_recv  ) * wp_rtm
!
            asmp_r(3*nd-2,k_rlm,lt) = 0.0d0
            asmp_p(2*nd,  k_rlm,lt) = 0.0d0
            asmp_p(2*nd-1,k_rlm,lt) = 0.0d0
!
            symp_r(3*nd-1,k_rlm,lt) = WR(inp_recv-1)                    &
     &                                   * wp_rtm * asin_rtm
            symp_r(3*nd,  k_rlm,lt) = WR(inp_recv  )                    &
     &                                   * wp_rtm * asin_rtm
!
            asmp_r(3*nd-1,k_rlm,lt) = 0.0d0
            asmp_r(3*nd,  k_rlm,lt) = 0.0d0
          end do
        end do
!
        lp_rtm = lst_rtm + lt
        wp_rtm = weight_rtm(lp_rtm)
        do k_rlm = 1, nidx_rlm(1)
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
!
          do nd = 1, nscalar
            ipp_recv = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_recv
!
            symp_r(nd+3*nvector,k_rlm,lt) = WR(ipp_recv) * wp_rtm
            asmp_r(nd+3*nvector,k_rlm,lt) = 0.0d0
          end do
        end do
      end do
!
      end subroutine set_vr_rtm_sym_mat_rin
!
! -----------------------------------------------------------------------
!
      end module set_vr_rtm_sym_mat_tsmp
