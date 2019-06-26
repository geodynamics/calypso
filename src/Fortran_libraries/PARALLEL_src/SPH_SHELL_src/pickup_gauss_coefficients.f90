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
!!      subroutine init_gauss_coefs_4_monitor                           &
!!     &          (sph_params, sph_rj, ipol, gauss_list, gauss_coef)
!!      subroutine gauss_coefficients_4_write                           &
!!     &        (sph_params, sph_rj, ipol, rj_fld, gauss_coef, d_rj_out)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(picked_spectrum_data), intent(inout) :: gauss_coef
!!
!!      subroutine cal_no_heat_source_Nu(kr_in, kr_out, r_in, r_out,    &
!!     &          idx_rj_degree_zero, nidx_rj,                          &
!!     &          nnod_rj, ntot_phys_rj, d_rj, Nu_type)
!!        type(phys_address), intent(in) :: ipol
!!        type(nusselt_number_data), intent(inout) :: Nu_type
!!@endverbatim
!
      module pickup_gauss_coefficients
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_phys_address
      use t_phys_data
      use t_pickup_sph_spectr_data
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
      subroutine init_gauss_coefs_4_monitor                             &
     &          (sph_params, sph_rj, ipol, gauss_list, gauss_coef)
!
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use m_phys_labels
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(pickup_mode_list), intent(inout) :: gauss_list
      type(picked_spectrum_data), intent(inout) :: gauss_coef
!
      integer(kind = kint) :: l
!
!
      gauss_coef%num_field_rj = 1
      gauss_coef%ntot_comp_rj = 1
!
      if (ipol%i_magne .gt. 0) then
        if(gauss_list%num_degree .eq. -9999) then
          gauss_list%num_degree = sph_params%l_truncation + 1
          call alloc_pick_sph_l(gauss_list)
          do l = 0, sph_params%l_truncation
            gauss_list%idx_pick_l(l+1) = l
          end do
        end if
!
        call const_picked_sph_address(izero, sph_params%l_truncation,   &
     &      sph_rj, gauss_list, gauss_coef)
!
      else
        gauss_coef%num_sph_mode = 0
        call alloc_pick_sph_monitor(gauss_coef)
        call dealloc_pick_sph_mode(gauss_list)
      end if
!
      gauss_coef%spectr_name(1) = fhd_magne
      gauss_coef%istack_comp_rj(1) = 1
      gauss_coef%ifield_monitor_rj(1) = 1
      call alloc_gauss_coef_monitor_lc(gauss_coef)
      call set_gauss_coefs_labels(gauss_coef)
!
      end subroutine init_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine gauss_coefficients_4_write                             &
     &        (sph_params, sph_rj, ipol, rj_fld, gauss_coef, d_rj_out)
!
      use calypso_mpi
      use t_pickup_sph_spectr_data
!
      type(phys_address), intent(in) :: ipol
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(picked_spectrum_data), intent(in) :: gauss_coef
      real(kind=kreal), intent(inout)                                   &
     &                 :: d_rj_out(gauss_coef%num_sph_mode_lc)
!
      integer(kind = kint) :: inum, l, i
      real(kind = kreal) :: r_ICB, r_CMB, rcmb_to_Re, ricb_to_Rref
      real(kind = kreal) :: r_4_gauss_coefs, a2r_4_gauss
!
!
      r_4_gauss_coefs = gauss_coef%radius_gl(1)
      r_ICB = sph_rj%radius_1d_rj_r(sph_params%nlayer_ICB)
      r_CMB = sph_rj%radius_1d_rj_r(sph_params%nlayer_CMB)
      if(r_4_gauss_coefs .ge. r_CMB) then
        a2r_4_gauss = one / (r_4_gauss_coefs**2)
        rcmb_to_Re = r_CMB / r_4_gauss_coefs
!$omp parallel do private(l,i)
        do inum = 1, gauss_coef%num_sph_mode_lc
          l = gauss_coef%idx_out(inum,1)
          i = gauss_coef%idx_out(inum,4)                                &
     &          + (sph_params%nlayer_CMB-1) * sph_rj%nidx_rj(2)
          d_rj_out(inum) = rj_fld%d_fld(i,ipol%i_magne)                 &
     &                    * dble(l) * rcmb_to_Re**l *a2r_4_gauss
        end do
!$omp end parallel do
!
      else if(r_4_gauss_coefs .le. r_ICB) then
        a2r_4_gauss = one / (r_ICB**2)
        ricb_to_Rref = r_4_gauss_coefs / r_ICB
!$omp parallel do private(l,i)
        do inum = 1, gauss_coef%num_sph_mode_lc
          l = gauss_coef%idx_out(inum,1)
          i = gauss_coef%idx_out(inum,4)                                &
     &          + (sph_params%nlayer_ICB-1) * sph_rj%nidx_rj(2)
          d_rj_out(inum) = - rj_fld%d_fld(i,ipol%i_magne)               &
     &                  * dble(l+1) * a2r_4_gauss * ricb_to_Rref**(l-1)
        end do
!$omp end parallel do
      end if
!
      end subroutine gauss_coefficients_4_write
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_gauss_coefs_labels(gauss)
!
      use set_parallel_file_name
      use t_pickup_sph_spectr_data
!
      type(picked_spectrum_data), intent(inout) :: gauss
!
      integer(kind = kint) :: l, m, mm, inum
      character(len=kchara) :: gauss_head
!
!
      do inum = 1, gauss%num_sph_mode_lc
        l = gauss%idx_out(inum,1)
        m = gauss%idx_out(inum,2)
        mm = abs(m)
!
        if(m .lt. izero) then
          write(gauss_head,'(a1)') 'h'
        else
          write(gauss_head,'(a1)') 'g'
        end if
!
        call add_index_after_name                                       &
     &     (l, gauss_head, gauss%gauss_mode_name_lc(inum))
        write(gauss_head,'(a,a1)')                                      &
     &     trim(gauss%gauss_mode_name_lc(inum)), '_'
        call add_index_after_name                                       &
     &     (mm, gauss_head, gauss%gauss_mode_name_lc(inum))
      end do
!
      end subroutine set_gauss_coefs_labels
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_no_heat_source_Nu(kr_in, kr_out, r_in, r_out,      &
     &          idx_rj_degree_zero, nidx_rj, ipol,                      &
     &          nnod_rj, ntot_phys_rj, d_rj, Nu_type)
!
      use t_no_heat_Nusselt
!
      type(phys_address), intent(in) :: ipol
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: r_in, r_out
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nidx_rj(2)
!
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(in) :: d_rj(nnod_rj,ntot_phys_rj)
!
      type(nusselt_number_data), intent(inout) :: Nu_type
!
      real(kind = kreal) :: temp_ICB, temp_CMB
!      real(kind = kreal) :: dTdr_ICB, dTdr_CMB
      real(kind = kreal) :: c1, c2
!      real(kind = kreal) :: dTdr_diff_ICB, dTdr_diff_CMB
      integer(kind = kint) :: inod_ICB, inod_CMB
!
!
      if(Nu_type%iflag_no_source_Nu .eq. izero) return
      if(idx_rj_degree_zero .eq. 0) return
!
      Nu_type%r_ICB_Nu = r_in
      Nu_type%r_CMB_Nu = r_out
!
      inod_ICB = idx_rj_degree_zero + (kr_in-1) * nidx_rj(2)
      temp_ICB = d_rj(inod_ICB,ipol%i_temp)
!      dTdr_ICB = half*d_rj(inod_ICB,ipol%i_grad_t)                     &
!     &           * a_r_1d_rj_r(kr_in)**2
!
      inod_CMB = idx_rj_degree_zero + (kr_out-1) * nidx_rj(2)
      temp_CMB = d_rj(inod_CMB,ipol%i_temp)
!      dTdr_CMB = half*d_rj(inod_CMB,ipol%i_grad_t)                     &
!     &          * a_r_1d_rj_r(kr_out)**2
!
      c1 = (Nu_type%r_CMB_Nu*temp_CMB - Nu_type%r_ICB_Nu*temp_ICB)      &
     &    / ( Nu_type%r_CMB_Nu - Nu_type%r_ICB_Nu )
      c2 =  Nu_type%r_CMB_Nu * Nu_type%r_ICB_Nu * (temp_ICB - temp_CMB) &
     &    / ( Nu_type%r_CMB_Nu - Nu_type%r_ICB_Nu )
!
!      dTdr_diff_ICB = - c2 * a_r_1d_rj_r(kr_in)**2
!      dTdr_diff_CMB = - c2 * a_r_1d_rj_r(kr_out)**2
!      Nu_type%Nu_ICB = dTdr_ICB / dTdr_diff_ICB
!      Nu_type%Nu_CMB = dTdr_CMB / dTdr_diff_CMB
!
      Nu_type%Nu_ICB = - half*d_rj(inod_ICB,ipol%i_grad_t) / c2
      Nu_type%Nu_CMB = - half*d_rj(inod_CMB,ipol%i_grad_t) / c2
!
      end subroutine cal_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      end module pickup_gauss_coefficients
