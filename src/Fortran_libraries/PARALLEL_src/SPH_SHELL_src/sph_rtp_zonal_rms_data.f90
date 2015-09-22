!>@file   sph_rtp_zonal_rms_data.f90
!!@brief  module sph_rtp_zonal_rms_data
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Get zonal mean and RMS fields in spherical grid
!!
!!@verbatim
!!      subroutine zonal_mean_all_rtp_field
!!      subroutine zonal_rms_all_rtp_field
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
      use m_machine_parameter
!
      implicit  none
!
      private :: cal_sph_zonal_rms_data, cal_sph_zonal_ave_data
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine zonal_mean_all_rtp_field
!
      use m_geometry_data
      use m_node_phys_data
      use coordinate_convert_4_sph
!
!
      call cal_sph_zonal_ave_data                                       &
     &   (node1%numnod, nod_fld1%ntot_phys, ione, nod_fld1%d_fld)
      call overwrite_nodal_sph_2_xyz
!
      end subroutine zonal_mean_all_rtp_field
!
! -------------------------------------------------------------------
!
      subroutine zonal_rms_all_rtp_field
!
      use m_geometry_data
      use m_node_phys_data
      use coordinate_convert_4_sph
!
!
      call cal_sph_zonal_rms_data                                       &
     &   (node1%numnod, nod_fld1%ntot_phys, ione, nod_fld1%d_fld)
      call overwrite_nodal_sph_2_xyz
!
      end subroutine zonal_rms_all_rtp_field
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine zonal_cyl_rms_all_rtp_field
!
      use m_geometry_data
      use m_node_phys_data
      use coordinate_convert_4_sph
!
!
      call overwrite_nodal_sph_2_cyl
      call cal_sph_zonal_rms_data                                       &
     &   (node1%numnod, nod_fld1%ntot_phys, ione, nod_fld1%d_fld)
      call overwrite_nodal_cyl_2_xyz
!
      end subroutine zonal_cyl_rms_all_rtp_field
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine cal_sph_zonal_rms_data(nnod, numdir, irtp_fld, d_rtp)
!
      use m_spheric_parameter
!
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
      end subroutine cal_sph_zonal_rms_data
!
! -------------------------------------------------------------------
!
      subroutine cal_sph_zonal_ave_data(nnod, numdir, irtp_fld, d_rtp)
!
      use m_spheric_parameter
!
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
      end subroutine cal_sph_zonal_ave_data
!
! -------------------------------------------------------------------
!
      end module sph_rtp_zonal_rms_data
