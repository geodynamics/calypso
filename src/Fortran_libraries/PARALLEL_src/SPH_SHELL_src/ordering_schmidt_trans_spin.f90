!>@file   ordering_schmidt_trans_spin.f90
!!@brief  module ordering_schmidt_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Copy data for Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine order_b_trans_fields_spin(ncomp, nvector, nscalar,   &
!!     &          irev_sr_rlm, n_WR, WR, sp_rlm_spin)
!!      subroutine order_f_trans_fields_spin(ncomp, nvector, nscalar,   &
!!     &          irev_sr_rtm, n_WR, WR, vr_rtm_spin)
!!
!!      subroutine back_f_trans_fields_spin(ncomp, nvector, nscalar,    &
!!     &          sp_rlm_spin, nmax_sr_rj, nneib_domain_rlm,            &
!!     &          istack_sr_rlm, item_sr_rlm, WS)
!!      subroutine back_b_trans_fields_spin(ncomp, nvector, nscalar,    &
!!     &          vr_rtm_spin, nmax_sr_rtp, nneib_domain_rtm,           &
!!     &          istack_sr_rtm, item_sr_rtm, WS)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module ordering_schmidt_trans_spin
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_fields_spin(ncomp, nvector, nscalar,     &
     &          irev_sr_rlm, n_WR, WR, sp_rlm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1),ncomp)
!
      integer(kind = kint) :: ip, ist, ied, nd, i_recv
      integer(kind = kint) :: i_rlm, j_rlm, k_rlm
      real(kind = kreal) :: a2r_1d_rlm_r
!
!
!$omp parallel do schedule(static)                                      &
!$omp&  private(ip,ist,ied,nd,i_rlm,j_rlm,k_rlm,i_recv,a2r_1d_rlm_r)
      do ip = 1, np_smp
        ist = idx_rlm_smp_stack(ip-1,1) + 1
        ied = idx_rlm_smp_stack(ip,  1)
        do nd = 1, nvector
          do k_rlm = ist, ied
            a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
            do j_rlm = 1, nidx_rlm(2)
              i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                      &
     &                  + (k_rlm-1) * istep_rlm(1)
              i_recv = 3*nd + (irev_sr_rlm(i_rlm)-1) * ncomp
!
              sp_rlm_spin(j_rlm,k_rlm,nd          )                     &
     &               = WR(i_recv-2) * a2r_1d_rlm_r
              sp_rlm_spin(j_rlm,k_rlm,nd+nvector  )                     &
     &               = WR(i_recv-1) * a_r_1d_rlm_r(k_rlm)
              sp_rlm_spin(j_rlm,k_rlm,nd+2*nvector)                     &
     &               = WR(i_recv  ) * a_r_1d_rlm_r(k_rlm)
            end do
          end do
        end do
!
        do nd = 1, nscalar
          do k_rlm = ist, ied
            do j_rlm = 1, nidx_rlm(2)
              i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                      &
     &                  + (k_rlm-1) * istep_rlm(1)
              i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm)-1) * ncomp
!
              sp_rlm_spin(j_rlm,k_rlm,nd+3*nvector) = WR(i_recv)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_fields_spin
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_fields_spin(ncomp, nvector, nscalar,     &
     &          irev_sr_rtm, n_WR, WR, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_spin(nidx_rtm(2),nidx_rtm(1),ncomp,nidx_rtm(3))
!
      integer(kind = kint) :: ip, ist, ied, nd, i_recv
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm, m_rtm
!
!
!$omp parallel do schedule(static)                                      &
!$omp&   private(ip,ist,ied,i_rtm,nd,k_rtm,l_rtm,m_rtm,i_recv)
      do ip = 1, np_smp
        ist = idx_rtm_smp_stack(ip-1,3) + 1
        ied = idx_rtm_smp_stack(ip,3)
        do m_rtm = ist, ied
          do nd = 1, nvector
            do k_rtm = 1, nidx_rtm(1)
              do l_rtm = 1, nidx_rtm(2)
                i_rtm = 1 + (l_rtm-1) * istep_rtm(2)                    &
     &                    + (k_rtm-1) * istep_rtm(1)                    &
     &                    + (m_rtm-1) * istep_rtm(3)
                i_recv = 3*nd + (irev_sr_rtm(i_rtm)-1) * ncomp
!
                vr_rtm_spin(l_rtm,k_rtm,nd,          m_rtm)             &
     &              = WR(i_recv-2)
                vr_rtm_spin(l_rtm,k_rtm,nd+nvector,  m_rtm)             &
     &              = WR(i_recv-1)
                vr_rtm_spin(l_rtm,k_rtm,nd+2*nvector,m_rtm)             &
     &              = WR(i_recv  )
              end do
            end do
          end do
!
          do nd = 1, nscalar
            do k_rtm = 1, nidx_rtm(1)
              do l_rtm = 1, nidx_rtm(2)
                i_rtm =  1 + (l_rtm-1) * istep_rtm(2)                   &
     &                     + (k_rtm-1) * istep_rtm(1)                   &
     &                     + (m_rtm-1) * istep_rtm(3)
                i_recv = nd + 3*nvector                                 &
     &                      + (irev_sr_rtm(i_rtm)-1) * ncomp
!
                vr_rtm_spin(l_rtm,k_rtm,nd+3*nvector,m_rtm)             &
     &              = WR(i_recv  )
              end do
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_fields_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_fields_spin(ncomp, nvector, nscalar,      &
     &          sp_rlm_spin, nmax_sr_rj, nneib_domain_rlm,              &
     &          istack_sr_rlm, item_sr_rlm, WS)
!
      use m_sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: nneib_domain_rlm, nmax_sr_rj
      integer(kind = kint), intent(in)                                  &
     &           :: istack_sr_rlm(0:nneib_domain_rlm)
      integer(kind = kint), intent(in)                                  &
     &           :: item_sr_rlm(istack_sr_rlm(nneib_domain_rlm))
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1),ncomp)
!
      real (kind=kreal), intent(inout)                                  &
     &           :: WS(ncomp*istack_sr_rlm(nneib_domain_rlm))
!
!
!
      integer(kind = kint) :: ip, ist, inum, i, num, inod
      integer(kind = kint) :: i_rlm, nd, j_rlm, k_rlm
!
!
      if(iflag_sph_commN .eq. iflag_alltoall) then
!$omp parallel private(nd,ip,ist,num)
        do ip = 1, nneib_domain_rlm
          ist = istack_sr_rlm(ip-1)
          num = istack_sr_rlm(ip  ) - istack_sr_rlm(ip-1)
          do nd = 1, nvector
!$omp do private(inum,i_rlm,j_rlm,k_rlm,i,inod)
            do inum = 1, num
              i = 3*nd + (inum-1)*ncomp + (ip-1)*nmax_sr_rj*ncomp
              inod = item_sr_rlm(inum+ist)
              i_rlm = 3*nd + (inod - 1) * ncomp
              j_rlm = 1 + mod((inod-1),nidx_rlm(2))
              k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
              WS(i-2) = sp_rlm_spin(j_rlm,k_rlm,nd          )
              WS(i-1) = sp_rlm_spin(j_rlm,k_rlm,nd+nvector  )
              WS(i  ) = sp_rlm_spin(j_rlm,k_rlm,nd+2*nvector)
            end do
!$omp end do nowait
          end do
          do nd = 1, nscalar
!$omp do private(inum,i_rlm,j_rlm,k_rlm,i,inod)
            do inum = 1, num
              i = nd + 3*nvector + (inum-1)*ncomp                       &
     &                           + (ip-1)*nmax_sr_rj*ncomp
              inod = item_sr_rlm(inum+ist)
              i_rlm = nd + 3*nvector + (inod - 1) * ncomp
              j_rlm = 1 + mod((inod-1),nidx_rlm(2))
              k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
              WS(i  ) = sp_rlm_spin(j_rlm,k_rlm,nd+3*nvector)
            end do
!$omp end do nowait
          end do
        end do
!$omp end parallel
!
      else
!
!$omp parallel private(nd)
        do nd = 1, nvector
!$omp do private(inum,i_rlm,j_rlm,k_rlm,i,inod)
          do inum = 1, istack_sr_rlm(nneib_domain_rlm)
            i = 3*nd + (inum-1) * ncomp
            inod = item_sr_rlm(inum)
            i_rlm = 3*nd + (inod-1) * ncomp
            j_rlm = 1 + mod((inod-1),nidx_rlm(2))
            k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
            WS(i-2) = sp_rlm_spin(j_rlm,k_rlm,nd          )
            WS(i-1) = sp_rlm_spin(j_rlm,k_rlm,nd+nvector  )
            WS(i  ) = sp_rlm_spin(j_rlm,k_rlm,nd+2*nvector)
          end do
!$omp end do nowait
        end do
        do nd = 1, nscalar
!$omp do private(inum,i_rlm,j_rlm,k_rlm,i,inod)
          do inum = 1, istack_sr_rlm(nneib_domain_rlm)
            i = nd + 3*nvector + (inum-1) * ncomp
            inod = item_sr_rlm(inum)
            i_rlm = nd + 3*nvector + (inod-1) * ncomp
            j_rlm = 1 + mod((inod-1),nidx_rlm(2))
            k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
            WS(i  ) = sp_rlm_spin(j_rlm,k_rlm,nd+3*nvector)
        end do
!$omp end do nowait
        end do
!$omp end parallel
      end if
!
      end subroutine back_f_trans_fields_spin
!
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_fields_spin(ncomp, nvector, nscalar,      &
     &          vr_rtm_spin, nmax_sr_rtp, nneib_domain_rtm,             &
     &          istack_sr_rtm, item_sr_rtm, WS)
!
      use m_sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: nneib_domain_rtm, nmax_sr_rtp
      integer(kind = kint), intent(in)                                  &
     &       :: istack_sr_rtm(0:nneib_domain_rtm)
      integer(kind = kint), intent(in)                                  &
     &       :: item_sr_rtm(istack_sr_rtm(nneib_domain_rtm))
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_spin(nidx_rtm(2),nidx_rtm(1),ncomp,nidx_rtm(3))
      real (kind=kreal), intent(inout)                                  &
     &       :: WS(ncomp*istack_sr_rtm(nneib_domain_rtm))
!
      integer(kind = kint) :: ip, ist, num, inum, nd, i, inod
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm, m_rtm, km_rtm
!
!
      if(iflag_sph_commN .eq. iflag_alltoall) then
!$omp parallel private(nd,ip,ist,num)
        do ip = 1, nneib_domain_rtm
          ist = istack_sr_rtm(ip-1)
          num = istack_sr_rtm(ip  ) - istack_sr_rtm(ip-1)
          do nd = 1, nvector
!$omp do private(inum,i_rtm,l_rtm,km_rtm,k_rtm,m_rtm,i,inod)
            do inum = 1, num
              i = 3*nd + (inum-1)*ncomp + (ip-1)*nmax_sr_rtp*ncomp
              inod = item_sr_rtm(inum+ist)
              i_rtm = 3*nd + (inod - 1) * ncomp
              l_rtm = 1 + mod((inod-1),nidx_rtm(2))
              km_rtm = 1 + (inod - l_rtm) / nidx_rtm(2)
              k_rtm = 1 + mod((km_rtm-1),nidx_rtm(1))
              m_rtm = 1 + (km_rtm - k_rtm) / nidx_rtm(1)
!
              WS(i-2) = vr_rtm_spin(l_rtm,k_rtm,nd,          m_rtm)
              WS(i-1) = vr_rtm_spin(l_rtm,k_rtm,nd+nvector,  m_rtm)
              WS(i  ) = vr_rtm_spin(l_rtm,k_rtm,nd+2*nvector,m_rtm)
            end do
!$omp end do nowait
          end do
          do nd = 1, nscalar
!$omp do private(inum,i_rtm,l_rtm,km_rtm,k_rtm,m_rtm,i,inod)
            do inum = 1, num
              i = nd + 3*nvector + (inum-1)*ncomp                       &
     &                           + (ip-1)*nmax_sr_rtp*ncomp
              inod = item_sr_rtm(inum+ist)
              i_rtm = nd + 3*nvector + (inod - 1) * ncomp
              l_rtm = 1 + mod((inod-1),nidx_rtm(2))
              km_rtm = 1 + (inod - l_rtm) / nidx_rtm(2)
              k_rtm = 1 + mod((km_rtm-1),nidx_rtm(1))
              m_rtm = 1 + (km_rtm - k_rtm) / nidx_rtm(1)
!
              WS(i  ) = vr_rtm_spin(l_rtm,k_rtm,nd+3*nvector,m_rtm)
            end do
!$omp end do nowait
          end do
        end do
!$omp end parallel
!
      else
!
!$omp parallel private(nd)
        do nd = 1, nvector
!$omp do private(inum,i_rtm,l_rtm,km_rtm,k_rtm,m_rtm,i,inod)
          do inum = 1, istack_sr_rtm(nneib_domain_rtm)
            i = 3*nd + (inum-1) * ncomp
            inod = item_sr_rtm(inum)
            i_rtm = 3*nd + (inod-1) * ncomp
            l_rtm = 1 + mod((inod-1),nidx_rtm(2))
            km_rtm = 1 + (inod - l_rtm) / nidx_rtm(2)
            k_rtm = 1 + mod((km_rtm-1),nidx_rtm(1))
            m_rtm = 1 + (km_rtm - k_rtm) / nidx_rtm(1)
!
            WS(i-2) = vr_rtm_spin(l_rtm,k_rtm,nd,          m_rtm)
            WS(i-1) = vr_rtm_spin(l_rtm,k_rtm,nd+nvector,  m_rtm)
            WS(i  ) = vr_rtm_spin(l_rtm,k_rtm,nd+2*nvector,m_rtm)
          end do
!$omp end do nowait
        end do
        do nd = 1, nscalar
!$omp do private(inum,i_rtm,l_rtm,km_rtm,k_rtm,m_rtm,i,inod)
          do inum = 1, istack_sr_rtm(nneib_domain_rtm)
            i = nd + 3*nvector + (inum-1) * ncomp
            inod = item_sr_rtm(inum)
            i_rtm = nd + 3*nvector + (inod-1) * ncomp
            l_rtm = 1 + mod((inod-1),nidx_rtm(2))
            km_rtm = 1 + (inod - l_rtm) / nidx_rtm(2)
            k_rtm = 1 + mod((km_rtm-1),nidx_rtm(1))
            m_rtm = 1 + (km_rtm - k_rtm) / nidx_rtm(1)
!
            WS(i  ) = vr_rtm_spin(l_rtm,k_rtm,nd+3*nvector,m_rtm)
        end do
!$omp end do nowait
        end do
!$omp end parallel
      end if
!
!
      end subroutine back_b_trans_fields_spin
!
! -----------------------------------------------------------------------
!
      end module ordering_schmidt_trans_spin
