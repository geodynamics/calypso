!>@file   pickup_gauss_coefficients.f90
!!@brief      module pickup_gauss_coefficients
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief Pick Gauss coefficients to output
!>@n      Evaluate Nusselt number without heat source
!!
!!@verbatim
!!      subroutine init_gauss_coefs_4_monitor
!!      subroutine cal_gauss_coefficients
!!
!!      subroutine cal_no_heat_source_Nu(kr_ICB, kr_CMB, r_in, r_out)
!!@endverbatim
!
      module pickup_gauss_coefficients
!
      use m_precision
      use m_constants
!
      use m_spheric_parameter
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
      use m_gauss_coefs_monitor_data
!
      integer(kind = kint) :: l
!
!
      if (ipol%i_magne .gt. 0) then
!
        if(num_pick_gauss_l .eq. -9999) then
          num_pick_gauss_l = l_truncation+1
          call allocate_pick_gauss_l
          do l = 0, l_truncation
            idx_pick_gauss_l(l+1) = l
          end do
        end if
!
        call count_picked_sph_adrress                                   &
     &     (num_pick_gauss_coefs, num_pick_gauss_l, num_pick_gauss_m,   &
     &      idx_pick_gauss_mode, idx_pick_gauss_l, idx_pick_gauss_m,    &
     &      ntot_pick_gauss_mode)
      else
        ntot_pick_gauss_mode = 0
      end if
!
      call allocate_gauss_coef_monitor
      call allocate_iflag_pick_sph(l_truncation)
!
      if (ipol%i_magne .gt. 0) then
      call set_picked_sph_address                                       &
     &   (num_pick_gauss_coefs, num_pick_gauss_l, num_pick_gauss_m,     &
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
      use m_gauss_coefs_monitor_data
!
      integer(kind = kint) :: inum, j, l, inod
      real(kind = kreal) :: rcmb_to_Re, ricb_to_Rref
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
      if(r_4_gauss_coefs .ge. radius_1d_rj_r(nlayer_CMB)) then
        a2r_4_gauss = one / (r_4_gauss_coefs**2)
        rcmb_to_Re = radius_1d_rj_r(nlayer_CMB) / r_4_gauss_coefs
!$omp parallel do private(j,l,inod)
        do inum = 1, num_pick_gauss_mode
          j = idx_pick_gauss_coef_lc(inum)
          l = idx_pick_gauss_coef_gl(inum,2)
          if(j .gt. izero) then
            inod =  j +    (nlayer_CMB-1) * nidx_rj(2)
            gauss_coef_lc(inum) = d_rj(inod,ipol%i_magne) * dble(l)     &
     &                        * rcmb_to_Re**l *a2r_4_gauss
          end if
        end do
!$omp end parallel do
!
      else if(r_4_gauss_coefs .le. radius_1d_rj_r(nlayer_ICB)) then
        a2r_4_gauss = one / (radius_1d_rj_r(nlayer_ICB)**2)
        ricb_to_Rref = r_4_gauss_coefs / radius_1d_rj_r(nlayer_ICB)
!$omp parallel do private(j,l,inod)
        do inum = 1, num_pick_gauss_mode
          j = idx_pick_gauss_coef_lc(inum)
          l = idx_pick_gauss_coef_gl(inum,2)
          if(j .gt. izero) then
            inod =  j +    (nlayer_ICB-1) * nidx_rj(2)
            gauss_coef_lc(inum) = - d_rj(inod,ipol%i_magne) * dble(l+1) &
     &                            * ricb_to_Rref**(l-1) *a2r_4_gauss
          end if
        end do
!$omp end parallel do
      end if
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
      use m_gauss_coefs_monitor_data
!
      integer(kind = kint) :: j, l, m, mm, inum
      character(len=kchara) :: gauss_head
!
!
      do inum = 1, num_pick_gauss_mode
        j = idx_pick_gauss_coef_gl(inum,1)
        l = idx_pick_gauss_coef_gl(inum,2)
        m = idx_pick_gauss_coef_gl(inum,3)
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
! -----------------------------------------------------------------------
!
      subroutine cal_no_heat_source_Nu(kr_ICB, kr_CMB, r_in, r_out)
!
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_no_heat_Nusselt_num
!
      integer(kind = kint), intent(in) :: kr_ICB, kr_CMB
      real(kind = kreal), intent(in) :: r_in, r_out
      real(kind = kreal) :: temp_ICB, temp_CMB
!      real(kind = kreal) :: dTdr_ICB, dTdr_CMB
!
      real(kind = kreal) :: c1, c2
!      real(kind = kreal) :: dTdr_diff_ICB, dTdr_diff_CMB
      integer(kind = kint) :: inod_ICB, inod_CMB
!
!
      if(iflag_no_source_Nu .eq. izero) return
      if(idx_rj_degree_zero .eq. 0) return
!
      r_ICB_Nu = r_in
      r_CMB_Nu = r_out
!
      inod_ICB = idx_rj_degree_zero + (kr_ICB-1) * nidx_rj(2)
      temp_ICB = d_rj(inod_ICB,ipol%i_temp)
!      dTdr_ICB = half*d_rj(inod_ICB,ipol%i_grad_t)                     &
!     &           * a_r_1d_rj_r(kr_ICB)**2
!
      inod_CMB = idx_rj_degree_zero + (kr_CMB-1) * nidx_rj(2)
      temp_CMB = d_rj(inod_CMB,ipol%i_temp)
!      dTdr_CMB = half*d_rj(inod_CMB,ipol%i_grad_t)                     &
!     &          * a_r_1d_rj_r(kr_CMB)**2
!
      c1 = (r_CMB_Nu*temp_CMB - r_ICB_Nu*temp_ICB)                      &
     &    / ( r_CMB_Nu - r_ICB_Nu )
      c2 = r_CMB_Nu * r_ICB_Nu * (temp_ICB - temp_CMB)                  &
     &    / ( r_CMB_Nu - r_ICB_Nu )
!
!      dTdr_diff_ICB = - c2 * a_r_1d_rj_r(kr_ICB)**2
!      dTdr_diff_CMB = - c2 * a_r_1d_rj_r(kr_CMB)**2
!      Nu_ICB = dTdr_ICB / dTdr_diff_ICB
!      Nu_CMB = dTdr_CMB / dTdr_diff_CMB
!
      Nu_ICB = - half*d_rj(inod_ICB,ipol%i_grad_t) / c2
      Nu_CMB = - half*d_rj(inod_CMB,ipol%i_grad_t) / c2
!
      end subroutine cal_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      end module pickup_gauss_coefficients
