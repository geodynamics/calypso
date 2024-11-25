!>@file   interpolate_scalar_ele8.f90
!!@brief  module interpolate_scalar_ele8
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in July, 2006
!!@n     Modified by H. Matsui in Nov., 2013
!
!> @brief Interpolation for scalar in tri-linear element
!!
!!@verbatim
!!      subroutine itp_matvec_scalar_edge2(np_smp, numnod, v_org,       &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine single_interpolate_scalar_ele8(numnod, numele, ie,   &
!!     &          v_org, iele_gauss, xi_gauss, scalar)
!!      subroutine s_interpolate_scalar_ele8(np_smp, numnod, numele, ie,&
!!     &          v_org, istack_smp, num_points, iele_gauss,            &
!!     &          xi_gauss, vect)
!!
!!      subroutine itp_matvec_scalar_node(np_smp, NP, v_org,            &
!!     &          NC, NCM, INM, IAM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_scalar_edge2(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_scalar_surf4(np_smp, NP, v_org,           &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!      subroutine itp_matvec_scalar_ele8(np_smp, NP, v_org,            &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!!@endverbatim
!
      module interpolate_scalar_ele8
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_scalar_edge2(np_smp, numnod, v_org,         &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod
      real (kind=kreal), intent(in) :: v_org(numnod)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(NC)
!
      integer (kind = kint) :: ip, ist_s, ied_s, ist, ig
      integer (kind = kint) :: i1, i2
!
!
!$omp parallel do private(ist_s,ied_s,ist,ig,i1,i2)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip)
        do ig = ist_s, ied_s
          ist = INM(ig-1)
          i1 = IAM(ist+ 1)
          i2 = IAM(ist+ 2)
          vect(ig) = AM(ist+1) * v_org(i1 ) + AM(ist+2) * v_org(i2 )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_scalar_edge2
!
! ----------------------------------------------------------------------
!
      subroutine single_interpolate_scalar_ele8(numnod, numele, ie,     &
     &          v_org, iele_gauss, xi_gauss, scalar)
!
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,8)
      integer (kind = kint), intent(in) :: iele_gauss
      real (kind=kreal), intent(in) :: xi_gauss(3)
      real (kind=kreal), intent(in) :: v_org(numnod)
!
      real (kind=kreal), intent(inout) :: scalar
!
      real (kind=kreal) :: xi, ei, zi
      real (kind=kreal) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal) :: xi_posi, ei_posi, zi_posi
!
      real (kind=kreal) :: an1, an2, an3, an4, an5, an6, an7, an8
!
      integer (kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8
!
          i1 = ie(iele_gauss,1)
          i2 = ie(iele_gauss,2)
          i3 = ie(iele_gauss,3)
          i4 = ie(iele_gauss,4)
          i5 = ie(iele_gauss,5)
          i6 = ie(iele_gauss,6)
          i7 = ie(iele_gauss,7)
          i8 = ie(iele_gauss,8)
!
          xi = xi_gauss(1)
          ei = xi_gauss(2)
          zi = xi_gauss(3)
!
          xi_nega = one - xi
          xi_posi = one + xi
!
          ei_nega = one - ei
          ei_posi = one + ei
!
          zi_nega = one - zi
          zi_posi = one + zi
!
          an1  = r125 * xi_nega * ei_nega * zi_nega
          an2  = r125 * xi_posi * ei_nega * zi_nega
          an3  = r125 * xi_posi * ei_posi * zi_nega
          an4  = r125 * xi_nega * ei_posi * zi_nega
          an5  = r125 * xi_nega * ei_nega * zi_posi
          an6  = r125 * xi_posi * ei_nega * zi_posi
          an7  = r125 * xi_posi * ei_posi * zi_posi
          an8  = r125 * xi_nega * ei_posi * zi_posi
!
!
          scalar     =  an1  * v_org(i1 ) + an2  * v_org(i2 )           &
     &                + an3  * v_org(i3 ) + an4  * v_org(i4 )           &
     &                + an5  * v_org(i5 ) + an6  * v_org(i6 )           &
     &                + an7  * v_org(i7 ) + an8  * v_org(i8 )
!
      end subroutine single_interpolate_scalar_ele8
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_scalar_ele8(np_smp, numnod, numele, ie,  &
     &          v_org, istack_smp, num_points, iele_gauss,              &
     &          xi_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,8)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(numnod)
!
      real (kind=kreal), intent(inout) :: vect(num_points)
!
      real (kind=kreal) :: xi, ei, zi
      real (kind=kreal) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal) :: xi_posi, ei_posi, zi_posi
!
      real (kind=kreal) :: an1, an2, an3, an4, an5, an6, an7, an8
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, i2, i3, i4, i5, i6, i7, i8
!
      integer (kind = kint) :: ig
!
!
!$omp parallel do private(ist,ied,ig,iele,i1,i2,i3,i4,i5,i6,i7,i8,      &
!$omp&                    xi,ei,zi,xi_nega, xi_posi, ei_nega, ei_posi,  &
!$omp&                    zi_nega, zi_posi, an1,an2,an3,an4,an5,an6,    &
!$omp&                    an7,an8)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do ig = ist, ied
!
          iele = iele_gauss(ig)
!
          i1 = ie(iele,1)
          i2 = ie(iele,2)
          i3 = ie(iele,3)
          i4 = ie(iele,4)
          i5 = ie(iele,5)
          i6 = ie(iele,6)
          i7 = ie(iele,7)
          i8 = ie(iele,8)
!
          xi = xi_gauss(ig,1)
          ei = xi_gauss(ig,2)
          zi = xi_gauss(ig,3)
!
          xi_nega = one - xi
          xi_posi = one + xi
!
          ei_nega = one - ei
          ei_posi = one + ei
!
          zi_nega = one - zi
          zi_posi = one + zi
!
          an1  = r125 * xi_nega * ei_nega * zi_nega
          an2  = r125 * xi_posi * ei_nega * zi_nega
          an3  = r125 * xi_posi * ei_posi * zi_nega
          an4  = r125 * xi_nega * ei_posi * zi_nega
          an5  = r125 * xi_nega * ei_nega * zi_posi
          an6  = r125 * xi_posi * ei_nega * zi_posi
          an7  = r125 * xi_posi * ei_posi * zi_posi
          an8  = r125 * xi_nega * ei_posi * zi_posi
!
!
          vect(ig)   =  an1  * v_org(i1 ) + an2  * v_org(i2 )           &
     &                + an3  * v_org(i3 ) + an4  * v_org(i4 )           &
     &                + an5  * v_org(i5 ) + an6  * v_org(i6 )           &
     &                + an7  * v_org(i7 ) + an8  * v_org(i8 )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_scalar_ele8
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_scalar_node(np_smp, NP, v_org,              &
     &          NC, NCM, INM, IAM, IEND_SUM_smp, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP
      real (kind=kreal), intent(in) :: v_org(NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(NC)
!
      integer (kind = kint) :: ip, ist_s, ied_s, ist, ig
      integer (kind = kint) :: i1
!
!
!$omp parallel do private(ist_s,ied_s,ist,ig,i1)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip)
        do ig = ist_s, ied_s
          ist = INM(ig-1)
          i1 = IAM(ist+ 1)
          vect(ig) = v_org(i1 )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_scalar_node
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_scalar_surf4(np_smp, NP, v_org,             &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP
      real (kind=kreal), intent(in) :: v_org(NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(NC)
!
      integer (kind = kint) :: ip, ist_s, ied_s, ist, ig
      integer (kind = kint) :: i1, i2, i3, i4
!
!
!$omp parallel do private(ist_s,ied_s,ist,ig,i1,i2,i3,i4)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip)
        do ig = ist_s, ied_s
          ist = INM(ig-1)
          i1 = IAM(ist+ 1)
          i2 = IAM(ist+ 2)
          i3 = IAM(ist+ 3)
          i4 = IAM(ist+ 4)
!
          vect(ig) = AM(ist+1) * v_org(i1 ) + AM(ist+2) * v_org(i2 )    &
                   + AM(ist+3) * v_org(i3 ) + AM(ist+4) * v_org(i4 )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_scalar_surf4
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_scalar_ele8(np_smp, NP, v_org,              &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP
      real (kind=kreal), intent(in) :: v_org(NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(NC)
!
      integer (kind = kint) :: ip, ist_s, ied_s, ist, ig
      integer (kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8
!
!
!$omp parallel do private(ist_s,ied_s,ist,ig,i1,i2,i3,i4,i5,i6,i7,i8)
      do ip = 1, np_smp
        ist_s = IEND_SUM_smp(ip-1) + 1
        ied_s = IEND_SUM_smp(ip)
        do ig = ist_s, ied_s
          ist = INM(ig-1)
          i1 = IAM(ist+ 1)
          i2 = IAM(ist+ 2)
          i3 = IAM(ist+ 3)
          i4 = IAM(ist+ 4)
          i5 = IAM(ist+ 5)
          i6 = IAM(ist+ 6)
          i7 = IAM(ist+ 7)
          i8 = IAM(ist+ 8)
!
          vect(ig) = AM(ist+1) * v_org(i1 ) + AM(ist+2) * v_org(i2 )    &
                   + AM(ist+3) * v_org(i3 ) + AM(ist+4) * v_org(i4 )    &
                   + AM(ist+5) * v_org(i5 ) + AM(ist+6) * v_org(i6 )    &
                   + AM(ist+7) * v_org(i7 ) + AM(ist+8) * v_org(i8 )
        end do
      end do
!$omp end parallel do
!
      end subroutine itp_matvec_scalar_ele8
!
! ----------------------------------------------------------------------
!
      end module interpolate_scalar_ele8
