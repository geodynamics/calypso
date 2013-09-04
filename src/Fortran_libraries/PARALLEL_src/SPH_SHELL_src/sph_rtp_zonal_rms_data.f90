!>@file   sph_rtp_zonal_rms_data.f90
!!@brief  module sph_rtp_zonal_rms_data
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Get zonal mean and RMS fields in spherical grid
!!
!!@verbatim
!!      subroutine zonal_rms_all_rtp_field
!!      subroutine zonal_mean_all_rtp_field
!!      subroutine zonal_cyl_rms_all_rtp_field
!!
!!      subroutine cal_sph_zonal_rms_data(numdir, irtp_fld)
!!      subroutine cal_sph_zonal_ave_data(numdir, irtp_fld)
!!@endverbatim
!!
!!@n @param  numdir     Number of component of field
!!@n @param  irtp_fld   Start address for field @f$ f(\r,\theta\phi) @f$
!
      module sph_rtp_zonal_rms_data
!
      use m_precision
      use m_constants
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine zonal_mean_all_rtp_field
!
      use m_sph_spectr_data
!
!
      call cal_sph_zonal_ave_data(ntot_phys_rtp, ione)
!
      end subroutine zonal_mean_all_rtp_field
!
! -------------------------------------------------------------------
!
      subroutine zonal_rms_all_rtp_field
!
      use m_sph_spectr_data
!
!
      call cal_sph_zonal_rms_data(ntot_phys_rtp, ione)
!
      end subroutine zonal_rms_all_rtp_field
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine zonal_cyl_rms_all_rtp_field
!
      use m_sph_spectr_data
      use sph_overwrite_sph_and_cyl
!
      integer(kind = kint) :: i, i_fld
!
!
      do i = 1, num_phys_rtp
        i_fld = istack_phys_comp_rtp(i-1) + 1
        if     (num_phys_comp_rtp(i) .eq. 6) then
          call sph_overwrte_sph_tsr_to_cyl(ntot_phys_rtp,               &
     &        i_fld, d_rtp)
        else if(num_phys_comp_rtp(i) .eq. 3) then
          call sph_overwrte_sph_vect_to_cyl(ntot_phys_rtp,              &
     &         i_fld, d_rtp)
        end if
      end do
!
      call cal_sph_zonal_rms_data(ntot_phys_rtp, ione)
!
      do i = 1, num_phys_rtp
        i_fld = istack_phys_comp_rtp(i-1) + 1
        if     (num_phys_comp_rtp(i) .eq. 6) then
          call sph_overwrte_cyl_tsr_to_sph(ntot_phys_rtp,               &
     &         i_fld, d_rtp)
        else if(num_phys_comp_rtp(i) .eq. 3) then
          call sph_overwrte_cyl_vect_to_sph(ntot_phys_rtp,              &
     &         i_fld, d_rtp)
        end if
      end do
!
      end subroutine zonal_cyl_rms_all_rtp_field
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine cal_sph_zonal_rms_data(numdir, irtp_fld)
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: numdir, irtp_fld
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
      end subroutine cal_sph_zonal_rms_data
!
! -------------------------------------------------------------------
!
      subroutine cal_sph_zonal_ave_data(numdir, irtp_fld)
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: numdir, irtp_fld
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
      end subroutine cal_sph_zonal_ave_data
!
! -------------------------------------------------------------------
!
      end module sph_rtp_zonal_rms_data
