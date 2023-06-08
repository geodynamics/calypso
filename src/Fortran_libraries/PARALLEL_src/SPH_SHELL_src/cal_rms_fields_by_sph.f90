!>@file   cal_rms_fields_by_sph.f90
!!@brief      module cal_rms_fields_by_sph
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief evaluate mean square data from spectr data
!!
!!@verbatim
!!      subroutine cal_mean_squre_in_shell(sph_params,                  &
!!     &          sph_rj, ipol, rj_fld, leg, pwr, WK_pwr)
!!      subroutine cal_correlate_in_shell(sph_params,                   &
!!     &          sph_rj, rj_fld1, rj_fld2, leg, cor, WK_pwr)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!
!!      subroutine global_sum_sph_layerd_square                         &
!!     &         (l_truncation, WK_pwr, pwr)
!!        type(sph_mean_square_work), intent(in) :: WK_pwr
!!        type(sph_mean_squares), intent(inout) :: pwr
!!      subroutine global_sum_sph_volume_square                         &
!!     &         (l_truncation, ntot_rms_rj, WK_pwr,                    &
!!     &          num_vol_spectr, v_pwr)
!!        type(sph_mean_square_work), intent(in) :: WK_pwr
!!        type(sph_vol_mean_squares), intent(inout)                     &
!!     &                         :: v_pwr(num_vol_spectr)
!!
!!      subroutine sum_mean_square_on_sphere(sph_params, pwr)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_mean_squares), intent(inout) :: pwr
!!      subroutine sum_mean_square_on_volume                            &
!!     &         (sph_params, ntot_rms_rj, num_vol_spectr, v_pwr)
!!      type(sph_shell_parameters), intent(in) :: sph_params
!!      type(sph_vol_mean_squares), intent(inout)                       &
!!     &                         :: v_pwr(num_vol_spectr)
!!@endverbatim
!
      module cal_rms_fields_by_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_phys_data
      use t_phys_address
      use t_sum_sph_rms_data
      use t_rms_4_sph_spectr
      use t_sph_volume_mean_square
      use t_schmidt_poly_on_rtm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_mean_squre_in_shell(sph_params,                    &
     &          sph_rj, ipol, rj_fld, leg, pwr, WK_pwr)
!
      use calypso_mpi
!
      use volume_average_4_sph
      use cal_ave_4_rms_vector_sph
      use sum_sph_rms_data
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(phys_address), intent(in) :: ipol
      type(legendre_4_sph_trans), intent(in) :: leg
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
!
      if(pwr%ntot_comp_sq .eq. 0) return
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_sph_layerd_pwr'
      call sum_sph_layerd_pwr(sph_params%l_truncation, sph_rj,  pwr,    &
     &                        ipol, rj_fld, leg%g_sph_rj, WK_pwr)
!
      if(iflag_debug .gt. 0) write(*,*) 'global_sum_sph_layerd_square'
      call global_sum_sph_layerd_square                                 &
     &    (sph_params%l_truncation, WK_pwr, pwr)
      call global_sum_sph_volume_square                                 &
     &    (sph_params%l_truncation, pwr%ntot_comp_sq, WK_pwr,           &
     &     pwr%num_vol_spectr, pwr%v_spectr)
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_volume_average_sph'
      call cal_volume_average_sph(sph_rj, rj_fld, pwr)
!
      call sum_mean_square_on_sphere(sph_params, pwr)
      call sum_mean_square_on_volume(sph_params, pwr%ntot_comp_sq,      &
     &    pwr%num_vol_spectr, pwr%v_spectr)
!
      end subroutine cal_mean_squre_in_shell
!
! ----------------------------------------------------------------------
!
      subroutine cal_correlate_in_shell(sph_params,                     &
     &          sph_rj, rj_fld1, rj_fld2, leg, cor, WK_pwr)
!
      use calypso_mpi
!
      use volume_average_4_sph
      use cal_ave_4_rms_vector_sph
      use sum_sph_rms_data
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld1, rj_fld2
      type(sph_shell_parameters), intent(in) :: sph_params
      type(legendre_4_sph_trans), intent(in) :: leg
!
      type(sph_mean_squares), intent(inout) :: cor
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
!
      if(cor%ntot_comp_sq .eq. 0) return
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_sph_layerd_correlate'
      call sum_sph_layerd_correlate(sph_params%l_truncation, sph_rj,    &
     &    cor, rj_fld1, rj_fld2, leg%g_sph_rj, WK_pwr)
!
      if(iflag_debug .gt. 0) write(*,*) 'global_sum_sph_layerd_square'
      call global_sum_sph_layerd_square                                 &
     &    (sph_params%l_truncation, WK_pwr, cor)
      call global_sum_sph_volume_square                                 &
     &    (sph_params%l_truncation, cor%ntot_comp_sq, WK_pwr,           &
     &     cor%num_vol_spectr, cor%v_spectr)
!
      call sum_mean_square_on_sphere(sph_params, cor)
      call sum_mean_square_on_volume(sph_params, cor%ntot_comp_sq,      &
     &    cor%num_vol_spectr, cor%v_spectr)
!
      end subroutine cal_correlate_in_shell
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine global_sum_sph_layerd_square                           &
     &         (l_truncation, WK_pwr, pwr)
!
      use calypso_mpi
      use calypso_mpi_real
!
      use cal_ave_4_rms_vector_sph
      use radial_int_for_sph_spec
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_mean_square_work), intent(in) :: WK_pwr
!
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint_gl) :: num64
!
!
      if(pwr%nri_rms .le. 0) return
      num64 = pwr%ntot_comp_sq * pwr%nri_rms * (l_truncation + 1)
      call calypso_mpi_reduce_real(WK_pwr%shl_l_local, pwr%shl_l,       &
     &    num64, MPI_SUM, pwr%irank_l)
      call calypso_mpi_reduce_real(WK_pwr%shl_m_local, pwr%shl_m,       &
     &    num64, MPI_SUM, pwr%irank_m)
      call calypso_mpi_reduce_real(WK_pwr%shl_lm_local, pwr%shl_lm,     &
     &    num64, MPI_SUM, pwr%irank_lm)
!
      end subroutine global_sum_sph_layerd_square
!
! -----------------------------------------------------------------------
!
      subroutine global_sum_sph_volume_square                           &
     &         (l_truncation, ntot_rms_rj, WK_pwr,                      &
     &          num_vol_spectr, v_pwr)
!
      use calypso_mpi
      use calypso_mpi_real
      use radial_int_for_sph_spec
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: ntot_rms_rj
      type(sph_mean_square_work), intent(in) :: WK_pwr
!
      integer(kind = kint), intent(in) :: num_vol_spectr
      type(sph_vol_mean_squares), intent(inout)                         &
     &                         :: v_pwr(num_vol_spectr)
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: num64
!
!
      num64 = ntot_rms_rj * (l_truncation + 1)
      do i = 1, num_vol_spectr
        call calypso_mpi_reduce_real(WK_pwr%vol_l_local(0,1,i),         &
     &      v_pwr(i)%v_l, num64, MPI_SUM, v_pwr(i)%irank_l)
!
        call calypso_mpi_reduce_real(WK_pwr%vol_m_local(0,1,i),         &
     &      v_pwr(i)%v_m, num64, MPI_SUM, v_pwr(i)%irank_m)
!
        call calypso_mpi_reduce_real(WK_pwr%vol_lm_local(0,1,i),        &
     &      v_pwr(i)%v_lm, num64, MPI_SUM, v_pwr(i)%irank_lm)
      end do
!
      end subroutine global_sum_sph_volume_square
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_mean_square_on_sphere(sph_params, pwr)
!
      use calypso_mpi
      use cal_ave_4_rms_vector_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      if(my_rank .eq. pwr%irank_m) then
        call sum_sph_rms_all_modes                                      &
     &     (sph_params%l_truncation, pwr%nri_rms, pwr%ntot_comp_sq,     &
     &      pwr%shl_m, pwr%shl_sq)
        call pick_axis_sph_power                                        &
     &     (sph_params%l_truncation, pwr%nri_rms, pwr%ntot_comp_sq,     &
     &      pwr%shl_m, pwr%shl_sq, pwr%shl_m0, pwr%ratio_shl_m0)
!
        call surf_ave_4_sph_rms(pwr%nri_rms, pwr%r_4_rms(1,2),          &
     &                          pwr%ntot_comp_sq, pwr%shl_sq)
        call surf_ave_4_sph_rms(pwr%nri_rms, pwr%r_4_rms(1,2),          &
     &                          pwr%ntot_comp_sq, pwr%shl_m0)
!
      else if(my_rank .eq. pwr%irank_l) then
        call sum_sph_rms_all_modes                                      &
     &     (sph_params%l_truncation, pwr%nri_rms, pwr%ntot_comp_sq,     &
     &      pwr%shl_l, pwr%shl_sq)
        call surf_ave_4_sph_rms(pwr%nri_rms, pwr%r_4_rms(1,2),          &
     &                          pwr%ntot_comp_sq, pwr%shl_sq)
!
      else if(my_rank .eq. pwr%irank_lm) then
        call sum_sph_rms_all_modes                                      &
     &     (sph_params%l_truncation, pwr%nri_rms, pwr%ntot_comp_sq,     &
     &      pwr%shl_lm, pwr%shl_sq)
        call surf_ave_4_sph_rms(pwr%nri_rms, pwr%r_4_rms(1,2),          &
     &                          pwr%ntot_comp_sq, pwr%shl_sq)
      end if
!
      if(my_rank .eq. pwr%irank_m) then
        call surf_ave_4_sph_rms_int(sph_params%l_truncation,            &
     &      pwr%nri_rms, pwr%r_4_rms(1,2), pwr%ntot_comp_sq, pwr%shl_m)
      end if
      if(my_rank .eq. pwr%irank_l) then
        call surf_ave_4_sph_rms_int(sph_params%l_truncation,            &
     &      pwr%nri_rms, pwr%r_4_rms(1,2), pwr%ntot_comp_sq, pwr%shl_l)
      end if
      if(my_rank .eq. pwr%irank_lm) then
        call surf_ave_4_sph_rms_int(sph_params%l_truncation,            &
     &     pwr%nri_rms, pwr%r_4_rms(1,2), pwr%ntot_comp_sq, pwr%shl_lm)
      end if
!
      end subroutine sum_mean_square_on_sphere
!
! -----------------------------------------------------------------------
!
      subroutine sum_mean_square_on_volume                              &
     &         (sph_params, ntot_rms_rj, num_vol_spectr, v_pwr)
!
      use calypso_mpi
!
      use cal_ave_4_rms_vector_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      integer(kind = kint), intent(in) :: ntot_rms_rj
!
      integer(kind = kint), intent(in) :: num_vol_spectr
      type(sph_vol_mean_squares), intent(inout)                         &
     &                         :: v_pwr(num_vol_spectr)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_vol_spectr
!
        if(my_rank .eq. v_pwr(i)%irank_m) then
          call sum_sph_vol_rms_all_modes(sph_params%l_truncation,       &
     &        ntot_rms_rj, v_pwr(i)%v_m, v_pwr(i)%v_sq)
          call pick_axis_sph_vol_pwr(sph_params%l_truncation,           &
     &        ntot_rms_rj, v_pwr(i)%v_m, v_pwr(i)%v_sq,                 &
     &        v_pwr(i)%v_m0, v_pwr(i)%v_ratio_m0)
!
          call vol_ave_4_rms_sph                                        &
     &         (ntot_rms_rj, v_pwr(i)%avol, v_pwr(i)%v_sq)
          call vol_ave_4_rms_sph                                        &
     &         (ntot_rms_rj, v_pwr(i)%avol, v_pwr(i)%v_m0)
          call vol_ave_4_rms_sph_int(sph_params%l_truncation,           &
     &        ntot_rms_rj, v_pwr(i)%avol, v_pwr(i)%v_m)
        end if
!
        if(my_rank .eq. v_pwr(i)%irank_l) then
          call sum_sph_vol_rms_all_modes(sph_params%l_truncation,       &
     &        ntot_rms_rj, v_pwr(i)%v_l, v_pwr(i)%v_sq)
          call vol_ave_4_rms_sph                                        &
     &         (ntot_rms_rj, v_pwr(i)%avol, v_pwr(i)%v_sq)
          call vol_ave_4_rms_sph_int(sph_params%l_truncation,           &
     &        ntot_rms_rj, v_pwr(i)%avol, v_pwr(i)%v_l)
        end if
!
        if(my_rank .eq. v_pwr(i)%irank_lm) then
          call sum_sph_vol_rms_all_modes(sph_params%l_truncation,       &
     &        ntot_rms_rj, v_pwr(i)%v_lm, v_pwr(i)%v_sq)
          call vol_ave_4_rms_sph                                        &
     &         (ntot_rms_rj, v_pwr(i)%avol, v_pwr(i)%v_sq)
          call vol_ave_4_rms_sph_int(sph_params%l_truncation,           &
     &        ntot_rms_rj, v_pwr(i)%avol, v_pwr(i)%v_lm)
        end if
      end do
!
      end subroutine sum_mean_square_on_volume
!
! -----------------------------------------------------------------------
!
      end module cal_rms_fields_by_sph
