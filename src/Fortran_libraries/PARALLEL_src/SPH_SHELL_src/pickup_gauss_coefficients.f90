!>@file   pickup_gauss_coefficients.f90
!!@brief      module pickup_gauss_coefficients
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief choose Gauss coefficients to output
!!
!!@verbatim
!!      subroutine init_gauss_coefs_4_monitor
!!      subroutine cal_gauss_coefficients
!!@endverbatim
!
      module pickup_gauss_coefficients
!
      use m_precision
      use m_constants
!
      use m_spheric_parameter
      use m_gauss_coefs_monitor_data
      use pickup_sph_spectr
!
      implicit  none
!
      private :: set_gauss_coefs_labels
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_gauss_coefs_4_monitor
!
      use m_sph_phys_address
!
!
      if (ipol%i_magne .gt. 0) then
        call count_picked_sph_adrress(l_truncation,                     &
     &      num_pick_gauss_coefs, num_pick_gauss_l, num_pick_gauss_m,   &
     &      idx_pick_gauss_l, idx_pick_gauss_m, ntot_pick_gauss_mode)
      else
        ntot_pick_gauss_mode = 0
      end if
!
      call allocate_gauss_coef_monitor
      call allocate_iflag_pick_sph(l_truncation)
!
      if (ipol%i_magne .gt. 0) then
        call set_picked_sph_adrress(l_truncation, ist_rj(2), ied_rj(2), &
     &    num_pick_gauss_coefs, num_pick_gauss_l, num_pick_gauss_m,     &
     &    idx_pick_gauss_mode, idx_pick_gauss_l, idx_pick_gauss_m,      &
     &    ntot_pick_gauss_mode, num_pick_gauss_mode,                    &
     &    idx_pick_gauss_coef_gl, idx_pick_gauss_coef_lc)
      else
        num_pick_gauss_mode = 0
      end if
!
      call deallocate_iflag_pick_sph
      call deallocate_pick_gauss
!
      call set_gauss_coefs_labels
!
      end subroutine init_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine cal_gauss_coefficients
!
      use calypso_mpi
      use m_sph_spectr_data
      use m_sph_phys_address
!
      integer(kind = kint) :: inum, j, l, inod
      real(kind = kreal) :: a2r_4_gauss
!
!
      if(num_pick_gauss_mode .eq. 0) return
!
!$omp parallel do
      do inum = 1, num_pick_gauss_mode
        gauss_coef_lc(inum) = 0.0d0
      end do
!$omp end parallel do
!
      a2r_4_gauss = one / (r_4_gauss_coefs**2)
      rcmb_to_Re = radius_1d_rj_r(nlayer_CMB) / r_4_gauss_coefs
!$omp parallel do private(j,inod)
      do inum = 1, num_pick_gauss_mode
        j = idx_pick_gauss_coef_lc(inum)
        l = int( aint(sqrt(dble(j))) )
        if(j .gt. izero) then
          inod =  j +    (nlayer_CMB-1) * nidx_rj(2)
          gauss_coef_lc(inum) = d_rj(inod,ipol%i_magne) * dble(l)       &
     &                        * rcmb_to_Re**l *a2r_4_gauss
        end if
      end do
!$omp end parallel do
!
      call MPI_allREDUCE(gauss_coef_lc(1), gauss_coef_gl(1),            &
     &    num_pick_gauss_mode, CALYPSO_REAL, MPI_SUM, CALYPSO_COMM,     &
     &    ierr_MPI)
!
      end subroutine cal_gauss_coefficients
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_gauss_coefs_labels
!
      use set_parallel_file_name
!
      integer(kind = kint) :: j, l, m, mm, inum
      character(len=kchara) :: gauss_head
!
!
      do inum = 1, num_pick_gauss_mode
        j = idx_pick_gauss_coef_gl(inum)
        l = int( aint(sqrt(dble(j))) )
        m = j - l*(l+1)
        mm = abs(m)
!
        if(m .lt. izero) then
          write(gauss_head,'(a1)') 'h'
        else
          write(gauss_head,'(a1)') 'g'
        end if
!
        call add_index_after_name(l, gauss_head, gauss_mode_name(inum))
        write(gauss_head,'(a,a1)') trim(gauss_mode_name(inum)), '_'
        call add_index_after_name(mm, gauss_head, gauss_mode_name(inum))
      end do
!
      end subroutine set_gauss_coefs_labels
!
! -----------------------------------------------------------------------
!
      end module pickup_gauss_coefficients
