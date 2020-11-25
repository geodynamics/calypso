!>@file   cal_sph_zonal_ave_rms_data.f90
!!@brief  module cal_sph_zonal_ave_rms_data
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Get zonal mean and RMS fields in spherical grid
!!
!!@verbatim
!!      subroutine cal_sph_zonal_ave_data_rin                           &
!!     &         (nidx_rtp, nnod, numdir, irtp_fld, d_rtp)
!!      subroutine cal_sph_zonal_ave_data_pin                           &
!!     &         (nidx_rtp, nnod, numdir, irtp_fld, d_rtp)
!!
!!      subroutine cal_sph_zonal_rms_data_rin                           &
!!     &         (nidx_rtp, nnod, numdir, irtp_fld, d_rtp)
!!      subroutine cal_sph_zonal_rms_data_pin                           &
!!     &         (nidx_rtp, nnod, numdir, irtp_fld, d_rtp)
!!@endverbatim
!!
!!@n @param  numdir     Number of component of field
!!@n @param  irtp_fld   Start address for field @f$ f(\r,\theta\phi) @f$
!
      module cal_sph_zonal_ave_rms_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine cal_sph_zonal_ave_data_rin                             &
     &         (nidx_rtp, nnod, numdir, irtp_fld, d_rtp)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: nnod, numdir, irtp_fld
      real(kind = kreal), intent(inout) :: d_rtp(nnod,numdir)
!
      integer(kind = kint) :: nd, kt, mphi, inod
      real(kind = kreal) :: anphi
!
!
      anphi = one / dble(nidx_rtp(3))
!
!$omp parallel
      do nd = irtp_fld, irtp_fld+numdir-1
        do mphi = 2, nidx_rtp(3)
!$omp do private(kt,inod)
          do kt = 1, nidx_rtp(1)*nidx_rtp(2)
            inod = kt + (mphi-1) * nidx_rtp(1)*nidx_rtp(2)
            d_rtp(kt,nd) = d_rtp(kt,nd) + d_rtp(inod,nd)
          end do
!$omp end do nowait
        end do
!
!$omp do private(kt)
        do kt = 1, nidx_rtp(1)*nidx_rtp(2)
          d_rtp(kt,nd) = d_rtp(kt,nd) * anphi
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel
      do nd = irtp_fld, irtp_fld+numdir-1
!$omp do private(kt,inod)
        do mphi = 2, nidx_rtp(3)
          do kt = 1, nidx_rtp(1)*nidx_rtp(2)
            inod = kt + (mphi-1) * nidx_rtp(1)*nidx_rtp(2)
            d_rtp(inod,nd) = d_rtp(kt,nd)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine cal_sph_zonal_ave_data_rin
!
! -------------------------------------------------------------------
!
      subroutine cal_sph_zonal_ave_data_pin                             &
     &         (nidx_rtp, nnod, numdir, irtp_fld, d_rtp)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: nnod, numdir, irtp_fld
      real(kind = kreal), intent(inout) :: d_rtp(nnod,numdir)
!
      integer(kind = kint) :: nd, kt, mphi, inod, i1
      real(kind = kreal) :: anphi
!
!
      anphi = one / dble(nidx_rtp(3))
!
!$omp parallel
      do nd = irtp_fld, irtp_fld+numdir-1
!$omp do private(kt,inod,i1,mphi)
        do kt = 1, nidx_rtp(1)*nidx_rtp(2)
          i1 = 1 + (kt-1) * nidx_rtp(3)
          do mphi = 2, nidx_rtp(3)
            inod = mphi + (kt-1) * nidx_rtp(3)
            d_rtp(i1,nd) = d_rtp(i1,nd) + d_rtp(inod,nd)
          end do
!
          d_rtp(i1,nd) = d_rtp(i1,nd) * anphi
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel
      do nd = irtp_fld, irtp_fld+numdir-1
!$omp do private(kt,inod,mphi)
        do kt = 1, nidx_rtp(1)*nidx_rtp(2)
          i1 = 1 + (kt-1) * nidx_rtp(3)
          do mphi = 2, nidx_rtp(3)
            inod = mphi + (kt-1) * nidx_rtp(3)
            d_rtp(inod,nd) = d_rtp(i1,nd)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine cal_sph_zonal_ave_data_pin
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine cal_sph_zonal_rms_data_rin                             &
     &         (nidx_rtp, nnod, numdir, irtp_fld, d_rtp)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: nnod, numdir, irtp_fld
      real(kind = kreal), intent(inout) :: d_rtp(nnod,numdir)
!
      integer(kind = kint) :: nd, kt, mphi, inod
      real(kind = kreal) :: anphi
!
!
      anphi = one / dble(nidx_rtp(3))
!
!$omp parallel
      do nd = irtp_fld, irtp_fld+numdir-1
!$omp do private(kt)
        do kt = 1, nidx_rtp(1)*nidx_rtp(2)
          d_rtp(kt,nd) = d_rtp(kt,nd)**2
        end do
!$omp end do nowait
        do mphi = 2, nidx_rtp(3)
!$omp do private(kt,inod)
          do kt = 1, nidx_rtp(1)*nidx_rtp(2)
            inod = kt + (mphi-1) * nidx_rtp(1)*nidx_rtp(2)
            d_rtp(kt,nd) = d_rtp(kt,nd) + d_rtp(inod,nd)**2
          end do
!$omp end do nowait
        end do

!
!$omp do private(kt)
        do kt = 1, nidx_rtp(1)*nidx_rtp(2)
          d_rtp(kt,nd) = sqrt( d_rtp(kt,nd) * anphi)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel
      do nd = irtp_fld, irtp_fld+numdir-1
        do mphi = 2, nidx_rtp(3)
!$omp do private(kt,inod)
          do kt = 1, nidx_rtp(1)*nidx_rtp(2)
            inod = kt + (mphi-1) * nidx_rtp(1)*nidx_rtp(2)
            d_rtp(inod,nd) = d_rtp(kt,nd)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine cal_sph_zonal_rms_data_rin
!
! -------------------------------------------------------------------
!
      subroutine cal_sph_zonal_rms_data_pin                             &
     &         (nidx_rtp, nnod, numdir, irtp_fld, d_rtp)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: nnod, numdir, irtp_fld
      real(kind = kreal), intent(inout) :: d_rtp(nnod,numdir)
!
      integer(kind = kint) :: nd, kt, mphi, inod, i1
      real(kind = kreal) :: anphi
!
!
      anphi = one / dble(nidx_rtp(3))
!
!$omp parallel
      do nd = irtp_fld, irtp_fld+numdir-1
!$omp do private(kt,i1,inod,mphi)
        do kt = 1, nidx_rtp(1)*nidx_rtp(2)
          i1 = 1 + (kt-1) * nidx_rtp(3)
          d_rtp(i1,nd) = d_rtp(i1,nd)**2
!
          do mphi = 2, nidx_rtp(3)
            inod = mphi + (kt-1) * nidx_rtp(3)
            d_rtp(kt,nd) = d_rtp(kt,nd) + d_rtp(inod,nd)**2
          end do
!
          d_rtp(i1,nd) = sqrt(d_rtp(i1,nd) * anphi)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel
      do nd = irtp_fld, irtp_fld+numdir-1
!$omp do private(kt,inod,mphi)
        do kt = 1, nidx_rtp(1)*nidx_rtp(2)
          i1 = 1 + (kt-1) * nidx_rtp(3)
          do mphi = 2, nidx_rtp(3)
            inod = mphi + (kt-1) * nidx_rtp(3)
            d_rtp(inod,nd) = d_rtp(i1,nd)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine cal_sph_zonal_rms_data_pin
!
! -------------------------------------------------------------------
!
      end module cal_sph_zonal_ave_rms_data
