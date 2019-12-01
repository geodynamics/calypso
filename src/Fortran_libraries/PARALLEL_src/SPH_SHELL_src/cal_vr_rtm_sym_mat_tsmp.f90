!>@file   cal_vr_rtm_sym_mat_tsmp.f90
!!@brief  module cal_vr_rtm_sym_mat_tsmp
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine cal_vr_rtm_sym_mat_rin(nnod_rtm, nidx_rtm,           &
!!     &          istep_rtm, nidx_rlm, asin_theta_1d_rtm,               &
!!     &          mp_rlm, lst_rtm, nle_rtm, nlo_rtm, symp_r, asmp_p,    &
!!     &          asmp_r, symp_p, ncomp_send, nvector, nscalar,         &
!!     &          irev_sr_rtm, n_WS, WS)
!!      subroutine cal_vr_rtm_sym_mat_rout(nnod_rtm, nidx_rtm,          &
!!     &          istep_rtm, nidx_rlm, asin_theta_1d_rtm,               &
!!     &          mp_rlm, lst_rtm, nle_rtm, nlo_rtm, symp_r, asmp_p,    &
!!     &          asmp_r, symp_p, ncomp_send, nvector, nscalar,         &
!!     &          irev_sr_rtm, n_WS, WS)
!!@endverbatim
!!
      module cal_vr_rtm_sym_mat_tsmp
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
      subroutine cal_vr_rtm_sym_mat_rin(nnod_rtm, nidx_rtm,             &
     &          istep_rtm, nidx_rlm, asin_theta_1d_rtm,                 &
     &          mp_rlm, lst_rtm, nle_rtm, nlo_rtm, symp_r, asmp_p,      &
     &          asmp_r, symp_p, ncomp_send, nvector, nscalar,           &
     &          irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in)                                &
     &           :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: lst_rtm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: nvector, nscalar
      real(kind = kreal), intent(inout)                             &
     &           :: symp_r(3*nvector+nscalar,nidx_rlm(1),nle_rtm)
      real(kind = kreal), intent(in)                                &
     &           :: asmp_p(2*nvector,nidx_rlm(1),nle_rtm)
      real(kind = kreal), intent(inout)                             &
     &           :: asmp_r(3*nvector+nscalar,nidx_rlm(1),nle_rtm)
      real(kind = kreal), intent(in)                                &
     &           :: symp_p(2*nvector,nidx_rlm(1),nle_rtm)
!
      integer(kind = kint), intent(in) :: ncomp_send
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: k_rlm, nd, mn_rlm
      integer(kind = kint) :: lt, lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
!
!
      mn_rlm = nidx_rtm(3) - mp_rlm + 1
!
      do lt = 1, nle_rtm
        lp_rtm = lst_rtm + lt
        do nd = 1, nvector
          do k_rlm = 1, nidx_rlm(1)
            symp_r(3*nd-1,k_rlm,lt) = - symp_r(3*nd-1,k_rlm,lt) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
            symp_r(3*nd,  k_rlm,lt) = - symp_r(3*nd,  k_rlm,lt) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
            asmp_r(3*nd-1,k_rlm,lt) = - asmp_r(3*nd-1,k_rlm,lt) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
            asmp_r(3*nd,  k_rlm,lt) = - asmp_r(3*nd,  k_rlm,lt) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
          end do
        end do
      end do
!
      do lt = 1, nlo_rtm
        lp_rtm = lst_rtm + lt
        do k_rlm = 1, nidx_rlm(1)
          ln_rtm =  nidx_rtm(2) - lp_rtm + 1
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          do nd = 1, nvector
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_send
            ipn_send = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_send
            inn_send = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp_send
!
            WS(ipp_send-2) = WS(ipp_send-2)                             &
     &                      + symp_r(3*nd-2,k_rlm,lt)               &
     &                      + asmp_r(3*nd-2,k_rlm,lt)
            WS(ipp_send-1) = WS(ipp_send-1)                             &
     &                      + asmp_p(2*nd,  k_rlm,lt)               &
     &                      + symp_p(2*nd,  k_rlm,lt)
            WS(ipp_send  ) = WS(ipp_send  )                             &
     &                      - asmp_p(2*nd-1,k_rlm,lt)               &
     &                      - symp_p(2*nd-1,k_rlm,lt)
!
            WS(inp_send-1) = WS(inp_send-1)                             &
     &                      + symp_r(3*nd-1,k_rlm,lt)               &
     &                      + asmp_r(3*nd-1,k_rlm,lt)
            WS(inp_send  ) = WS(inp_send  )                             &
     &                      + symp_r(3*nd,  k_rlm,lt)               &
     &                      + asmp_r(3*nd,  k_rlm,lt)
!
!
            WS(ipn_send-2) = WS(ipn_send-2)                             &
     &                      + symp_r(3*nd-2,k_rlm,lt)               &
     &                      - asmp_r(3*nd-2,k_rlm,lt)
            WS(ipn_send-1) = WS(ipn_send-1)                             &
     &                      - asmp_p(2*nd,  k_rlm,lt)               &
     &                      + symp_p(2*nd,  k_rlm,lt)
            WS(ipn_send  ) = WS(ipn_send  )                             &
     &                      + asmp_p(2*nd-1,k_rlm,lt)               &
     &                      - symp_p(2*nd-1,k_rlm,lt)
!
            WS(inn_send-1) = WS(inn_send-1)                             &
     &                      + symp_r(3*nd-1,k_rlm,lt)               &
     &                      - asmp_r(3*nd-1,k_rlm,lt)
            WS(inn_send  ) = WS(inn_send  )                             &
     &                      + symp_r(3*nd,  k_rlm,lt)               &
     &                      - asmp_r(3*nd,  k_rlm,lt)
          end do
!
          do nd = 1, nscalar
            ipp_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
            ipn_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_send
!
            WS(ipp_send) = WS(ipp_send)                                 &
     &                  + symp_r(nd+3*nvector,k_rlm,lt)             &
     &                  + asmp_r(nd+3*nvector,k_rlm,lt)
            WS(ipn_send) = WS(ipn_send)                                 &
     &                  + symp_r(nd+3*nvector,k_rlm,lt)             &
     &                  - asmp_r(nd+3*nvector,k_rlm,lt)
          end do
        end do
      end do
!
      do lt = nlo_rtm+1, nle_rtm
        lp_rtm = lst_rtm + lt
        do k_rlm = 1, nidx_rlm(1)
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          do nd = 1, nvector
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_send
!
            WS(ipp_send-2) = WS(ipp_send-2) + symp_r(3*nd-2,k_rlm,lt)
            WS(ipp_send-1) = WS(ipp_send-1) + symp_p(2*nd,  k_rlm,lt)
            WS(ipp_send  ) = WS(ipp_send  ) - symp_p(2*nd-1,k_rlm,lt)
!
            WS(inp_send-1) = WS(inp_send-1) + symp_r(3*nd-1,k_rlm,lt)
            WS(inp_send  ) = WS(inp_send  ) + symp_r(3*nd,  k_rlm,lt)
          end do
!
          do nd = 1, nscalar
            ipp_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
!
            WS(ipp_send) = WS(ipp_send)                                 &
     &                    + symp_r(nd+3*nvector,k_rlm,lt)
          end do
        end do
      end do
!
      end subroutine cal_vr_rtm_sym_mat_rin
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_sym_mat_rout(nnod_rtm, nidx_rtm,            &
     &          istep_rtm, nidx_rlm, asin_theta_1d_rtm,                 &
     &          mp_rlm, lst_rtm, nle_rtm, nlo_rtm, symp_r, asmp_p,      &
     &          asmp_r, symp_p, ncomp_send, nvector, nscalar,           &
     &          irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in)                                    &
     &           :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: lst_rtm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: nvector, nscalar
      real(kind = kreal), intent(inout)                                 &
     &           :: symp_r(nle_rtm,nidx_rlm(1),3*nvector+nscalar)
      real(kind = kreal), intent(in)                                    &
     &           :: asmp_p(nle_rtm,nidx_rlm(1),2*nvector)
      real(kind = kreal), intent(inout)                                 &
     &           :: asmp_r(nle_rtm,nidx_rlm(1),3*nvector+nscalar)
      real(kind = kreal), intent(in)                                    &
     &           :: symp_p(nle_rtm,nidx_rlm(1),2*nvector)
!
      integer(kind = kint), intent(in) :: ncomp_send
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: k_rlm, nd, mn_rlm
      integer(kind = kint) :: lt, lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
!
!
      mn_rlm = nidx_rtm(3) - mp_rlm + 1
!
      do nd = 1, nvector
        do k_rlm = 1, nidx_rlm(1)
          do lt = 1, nle_rtm
            lp_rtm = lst_rtm + lt
            symp_r(lt,k_rlm,3*nd-1) = - symp_r(lt,k_rlm,3*nd-1) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
            symp_r(lt,k_rlm,3*nd  ) = - symp_r(lt,k_rlm,3*nd  ) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
            asmp_r(lt,k_rlm,3*nd-1) = - asmp_r(lt,k_rlm,3*nd-1) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
            asmp_r(lt,k_rlm,3*nd  ) = - asmp_r(lt,k_rlm,3*nd  ) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
          end do
        end do
      end do
!
      do nd = 1, nvector
        do k_rlm = 1, nidx_rlm(1)
          do lt = 1, nlo_rtm
            lp_rtm = lst_rtm + lt
            ln_rtm =  nidx_rtm(2) - lp_rtm + 1
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
            in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
            ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
            in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_send
            ipn_send = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_send
            inn_send = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp_send
!
            WS(ipp_send-2) = WS(ipp_send-2)                             &
     &                      + symp_r(lt,k_rlm,3*nd-2)                   &
     &                      + asmp_r(lt,k_rlm,3*nd-2)
            WS(ipp_send-1) = WS(ipp_send-1)                             &
     &                      + asmp_p(lt,k_rlm,2*nd  )                   &
     &                      + symp_p(lt,k_rlm,2*nd  )
            WS(ipp_send  ) = WS(ipp_send  )                             &
     &                      - asmp_p(lt,k_rlm,2*nd-1)                   &
     &                      - symp_p(lt,k_rlm,2*nd-1)
!
            WS(inp_send-1) = WS(inp_send-1)                             &
     &                      + symp_r(lt,k_rlm,3*nd-1)                   &
     &                      + asmp_r(lt,k_rlm,3*nd-1)
            WS(inp_send  ) = WS(inp_send  )                             &
     &                      + symp_r(lt,k_rlm,3*nd  )                   &
     &                      + asmp_r(lt,k_rlm,3*nd  )
!
!
            WS(ipn_send-2) = WS(ipn_send-2)                             &
     &                      + symp_r(lt,k_rlm,3*nd-2)                   &
     &                      - asmp_r(lt,k_rlm,3*nd-2)
            WS(ipn_send-1) = WS(ipn_send-1)                             &
     &                      - asmp_p(lt,k_rlm,2*nd  )                   &
     &                      + symp_p(lt,k_rlm,2*nd  )
            WS(ipn_send  ) = WS(ipn_send  )                             &
     &                      + asmp_p(lt,k_rlm,2*nd-1)                   &
     &                      - symp_p(lt,k_rlm,2*nd-1)
!
            WS(inn_send-1) = WS(inn_send-1)                             &
     &                      + symp_r(lt,k_rlm,3*nd-1)                   &
     &                      - asmp_r(lt,k_rlm,3*nd-1)
            WS(inn_send  ) = WS(inn_send  )                             &
     &                      + symp_r(lt,k_rlm,3*nd  )                   &
     &                      - asmp_r(lt,k_rlm,3*nd  )
          end do
        end do
      end do
!
      do nd = 1, nscalar
        do k_rlm = 1, nidx_rlm(1)
          do lt = 1, nlo_rtm
            lp_rtm = lst_rtm + lt
            ln_rtm =  nidx_rtm(2) - lp_rtm + 1
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
            in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
            ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
            in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
            ipp_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
            ipn_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_send
!
            WS(ipp_send) = WS(ipp_send)                                 &
     &                  + symp_r(lt,k_rlm,nd+3*nvector)                 &
     &                  + asmp_r(lt,k_rlm,nd+3*nvector)
            WS(ipn_send) = WS(ipn_send)                                 &
     &                  + symp_r(lt,k_rlm,nd+3*nvector)                 &
     &                  - asmp_r(lt,k_rlm,nd+3*nvector)
          end do
        end do
      end do
!
      do lt = nlo_rtm+1, nle_rtm
        lp_rtm = lst_rtm + lt
        do nd = 1, nvector
          do k_rlm = 1, nidx_rlm(1)
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
            in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_send
!
            WS(ipp_send-2) = WS(ipp_send-2) + symp_r(lt,k_rlm,3*nd-2)
            WS(ipp_send-1) = WS(ipp_send-1) + symp_p(lt,k_rlm,2*nd  )
            WS(ipp_send  ) = WS(ipp_send  ) - symp_p(lt,k_rlm,2*nd-1)
!
            WS(inp_send-1) = WS(inp_send-1) + symp_r(lt,k_rlm,3*nd-1)
            WS(inp_send  ) = WS(inp_send  ) + symp_r(lt,k_rlm,3*nd  )
          end do
        end do
!
        do nd = 1, nscalar
          do k_rlm = 1, nidx_rlm(1)
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rlm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rlm-1) *  istep_rtm(1)                     &
     &                  + (mn_rlm-1) * istep_rtm(3)
!
            ipp_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
!
            WS(ipp_send) = WS(ipp_send)                                 &
     &                    + symp_r(lt,k_rlm,nd+3*nvector)
          end do
        end do
      end do
!
      end subroutine cal_vr_rtm_sym_mat_rout
!
! -----------------------------------------------------------------------
!
      end module cal_vr_rtm_sym_mat_tsmp
