!
!      module volume_average_4_sph
!
!     Written by H. Matsui on Feb., 2008
!
!!      subroutine allocate_ave_4_sph_spectr(nri_ave, ntot_rms)
!!      subroutine deallocate_ave_4_sph_spectr
!!      subroutine write_sph_vol_ave_file(istep, time)
!!
!!      subroutine cal_volume_average_sph(kg_st, kg_ed, avol)
!
      module volume_average_4_sph
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
!
      implicit none
!
!
      integer(kind = kint) :: nri_ave
!
      integer(kind = kint), parameter :: id_file_ave =      43
!
      real(kind = kreal), allocatable :: ave_sph(:,:)
      real(kind = kreal), allocatable :: ave_sph_vol(:)
!
      private :: id_file_ave
      private :: nri_ave, ave_sph, ave_sph_vol
      private :: cal_sphere_average_sph, averaging_4_sph_ave_int
      private :: cal_ave_vector_sph_spectr, cal_ave_scalar_sph_spectr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ave_4_sph_spectr(ntot_rms)
!
      integer(kind = kint), intent(in) :: ntot_rms
!
!
      if(idx_rj_degree_zero .eq. izero) return
!
      nri_ave = nidx_rj(1)
!
      allocate(ave_sph_vol(ntot_rms))
      allocate(ave_sph(0:nri_ave,ntot_rms))
!
      if(nri_ave*ntot_rms .gt. 0) then
        ave_sph=     0.0d0
        ave_sph_vol = 0.0d0
      end if
!
      end subroutine allocate_ave_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ave_4_sph_spectr
!
!
      if(idx_rj_degree_zero .eq. izero) return
      deallocate(ave_sph, ave_sph_vol)
!
      end subroutine deallocate_ave_4_sph_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_vol_ave_file(istep, time)
!
      use set_parallel_file_name
      use m_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(idx_rj_degree_zero .eq. 0)  return
      if(ntot_rms_rj .eq. 0)  return
!
      write(fname_rms, '(a,a4)') trim(fhead_ave_vol), '.dat'
      write(mode_label,'(a)') 'EMPTY'
      call open_sph_mean_sq_file                                        &
     &      (id_file_ave, fname_rms, mode_label)
!
      write(id_file_ave,'(i15,1pe23.14e3,1p200e23.14e3)')               &
     &                 istep, time, ave_sph_vol(1:ntot_rms_rj)
      close(id_file_ave)
!
      end subroutine write_sph_vol_ave_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_volume_average_sph(kg_st, kg_ed, avol)
!
      use m_phys_constants
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_sph_phys_address
      use cal_ave_4_rms_vector_sph
      use radial_int_for_sph_spec
!
      integer(kind = kint), intent(in) :: kg_st, kg_ed
      real(kind = kreal), intent(in) :: avol
!
!
      if(idx_rj_degree_zero .gt. izero) then
        call cal_sphere_average_sph
!
        call radial_integration(kg_st, kg_ed, nidx_rj(1),               &
     &      radius_1d_rj_r, ntot_rms_rj, ave_sph(0,1),  ave_sph_vol(1))
        call averaging_4_sph_ave_int(avol)
      end if
!
      end subroutine cal_volume_average_sph
!
! -----------------------------------------------------------------------
!
      subroutine cal_sphere_average_sph
!
      use m_phys_constants
      use m_sph_spectr_data
      use m_sph_phys_address
      use cal_rms_by_sph_spectr
      use m_rms_4_sph_spectr
!
      integer(kind = kint) :: i_fld, j_fld, icomp_st, jcomp_st
!
!
!
      ave_sph = 0.0d0
!
      do j_fld = 1, num_rms_rj
        i_fld = ifield_rms_rj(j_fld)
        icomp_st = istack_phys_comp_rj(i_fld-1) + 1
        jcomp_st = istack_rms_comp_rj(j_fld-1) +  1
        if (num_phys_comp_rj(i_fld) .eq. n_scalar) then
          call cal_ave_scalar_sph_spectr(icomp_st, jcomp_st)
        else if (num_phys_comp_rj(i_fld) .eq. n_vector) then
          call cal_ave_vector_sph_spectr(icomp_st, jcomp_st)
        end if
      end do
!
      end subroutine cal_sphere_average_sph
!
! -----------------------------------------------------------------------
!
      subroutine averaging_4_sph_ave_int(avol)
!
      use m_rms_4_sph_spectr
!
      real(kind = kreal), intent(in) :: avol
      integer(kind = kint) :: k, icou
!
!
!$omp parallel do private(k,icou)
      do icou = 1, ntot_rms_rj
        ave_sph_vol(icou) = avol * ave_sph_vol(icou)
      end do
!$omp end parallel do
!
      end subroutine averaging_4_sph_ave_int
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_ave_scalar_sph_spectr(icomp, jcomp)
!
      use m_constants
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: icomp, jcomp
      integer(kind = kint) :: k, inod
!
!
      do k = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (k-1) * nidx_rj(2)
        ave_sph(k,jcomp) = d_rj(inod,icomp) * radius_1d_rj_r(k)**2
      end do
!
      if(inod_rj_center .eq. 0) return
      ave_sph(0,jcomp) = d_rj(inod_rj_center,icomp)
!
      end subroutine cal_ave_scalar_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine cal_ave_vector_sph_spectr(icomp, jcomp)
!
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: icomp, jcomp
      integer(kind = kint) :: k, inod
!
!
      do k = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (k-1) * nidx_rj(2)
        ave_sph(k,jcomp  ) = d_rj(inod,icomp) * radius_1d_rj_r(k)**2
        ave_sph(k,jcomp+1) = zero
        ave_sph(k,jcomp+2) = ave_sph(k,jcomp  )
      end do
!
      if(inod_rj_center .eq. 0) return
      ave_sph(0,jcomp  ) = d_rj(inod_rj_center,icomp)
      ave_sph(0,jcomp+1) = zero
      ave_sph(0,jcomp+2) = ave_sph(0,jcomp  )
!
      end subroutine cal_ave_vector_sph_spectr
!
! -----------------------------------------------------------------------
!
      end module volume_average_4_sph
