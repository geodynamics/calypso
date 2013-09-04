!>@file   merge_polidal_toroidal_v.f90
!!@brief  module merge_polidal_toroidal_v
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Take products with radius for spherical transform
!!
!!@verbatim
!!      subroutine const_vect_sph_b_trans(nb, vr_rtp)
!!      subroutine prod_r_vect_sph_f_trans(nb, vr_rtp)
!!@endverbatim
!
       module merge_polidal_toroidal_v
!
      use m_precision
!
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
      subroutine const_vect_sph_b_trans(nb, vr_rtp)
!
      integer(kind = kint), intent(in) :: nb
      real(kind = kreal), intent(inout) :: vr_rtp(3*nb*nnod_rtp)
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: i_rtp, k_rtp
      integer(kind = kint) :: nd, inod
!
!$omp parallel do private(ip,ist,ied,i_rtp,k_rtp,nd,inod)
      do ip = 1, np_smp
        ist = nb*inod_rtp_smp_stack(ip-1) + 1
        ied = nb*inod_rtp_smp_stack(ip)
!cdir nodep
        do i_rtp = ist, ied
          nd = 1 + mod(i_rtp-1,nb)
          inod = 1 + (i_rtp - nd) / nb
          k_rtp = 1 + mod((inod-1),nidx_rtp(1))
          vr_rtp(3*i_rtp-2) = a_r_1d_rtp_r(k_rtp)*a_r_1d_rtp_r(k_rtp)   &
     &                       * vr_rtp(3*i_rtp-2)
          vr_rtp(3*i_rtp-1) = a_r_1d_rtp_r(k_rtp) * vr_rtp(3*i_rtp-1)
          vr_rtp(3*i_rtp  ) = a_r_1d_rtp_r(k_rtp) * vr_rtp(3*i_rtp  )
        end do
      end do
!$omp end parallel do
!
      end subroutine const_vect_sph_b_trans
!
! -----------------------------------------------------------------------
!
      subroutine prod_r_vect_sph_f_trans(nb, vr_rtp)
!
      integer(kind = kint), intent(in) :: nb
      real(kind = kreal), intent(inout) :: vr_rtp(3*nb*nnod_rtp)
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: i_rtp, k_rtp
      integer(kind = kint) :: nd, inod
!
!$omp parallel do private(ip,ist,ied,i_rtp,k_rtp,nd,inod)
      do ip = 1, np_smp
        ist = nb*inod_rtp_smp_stack(ip-1) + 1
        ied = nb*inod_rtp_smp_stack(ip)
!cdir nodep
        do i_rtp = ist, ied
          nd = 1 + mod(i_rtp-1,nb)
          inod = 1 + (i_rtp - nd) / nb
          k_rtp = 1 + mod((inod-1),nidx_rtp(1))
          vr_rtp(3*i_rtp-2)                                             &
     &              = radius_1d_rtp_r(k_rtp)*radius_1d_rtp_r(k_rtp)     &
     &               * vr_rtp(3*i_rtp-2) 
          vr_rtp(3*i_rtp-1) = radius_1d_rtp_r(k_rtp)*vr_rtp(3*i_rtp-1)
          vr_rtp(3*i_rtp  ) = radius_1d_rtp_r(k_rtp)*vr_rtp(3*i_rtp  )
        end do
      end do
!$omp end parallel do
!
      end subroutine prod_r_vect_sph_f_trans
!
! -----------------------------------------------------------------------
!
      end module merge_polidal_toroidal_v
