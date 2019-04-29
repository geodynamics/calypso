!>@file   cal_rms_fields_by_sph.f90
!!@brief      module cal_rms_fields_by_sph
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief evaluate mean square data from spectr data
!!
!!@verbatim
!!      subroutine init_rms_4_sph_spectr                                &
!!     &         (sph_params, sph_rj, rj_fld, pwr, WK_pwr)
!!      subroutine cal_mean_squre_in_shell(sph_params,                  &
!!     &          sph_rj, ipol, rj_fld, g_sph_rj, pwr, WK_pwr)
!!      subroutine cal_correlate_in_shell(sph_params,                   &
!!     &          sph_rj, rj_fld1, rj_fld2, g_sph_rj, cor, WK_pwr)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
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
!
      implicit none
!
      private :: find_radial_grid_index, global_sum_sph_layerd_rms
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_rms_4_sph_spectr                                  &
     &         (sph_params, sph_rj, rj_fld, pwr, WK_pwr)
!
      use calypso_mpi
!
      use sum_sph_rms_data
      use volume_average_4_sph
      use cal_ave_4_rms_vector_sph
      use quicksort
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: i_fld, j_fld
      integer(kind = kint) :: i, k, knum, num_field
!
!
      if(pwr%nri_rms .eq. -1) then
        call alloc_num_spec_layer(sph_rj%nidx_rj(1), pwr)
!
        do k = 1, sph_rj%nidx_rj(1)
          pwr%kr_4_rms(k) = k
        end do
      end if
!
      do i = 1, pwr%num_vol_spectr
        call find_radial_grid_index(sph_rj, sph_params%nlayer_ICB,      &
     &      pwr%v_spectr(i)%r_inside, pwr%v_spectr(i)%kr_inside)
        call find_radial_grid_index(sph_rj, sph_params%nlayer_CMB,      &
     &      pwr%v_spectr(i)%r_outside, pwr%v_spectr(i)%kr_outside)
!
        if(iflag_debug .gt. 0) write(*,*) 'cal_one_over_volume'
        call cal_one_over_volume                                        &
     &     (pwr%v_spectr(i)%kr_inside, pwr%v_spectr(i)%kr_outside,      &
     &      sph_rj%nidx_rj(i), sph_rj%radius_1d_rj_r,                   &
     &      pwr%v_spectr(i)%avol)
      end do
!
      num_field = 0
      do i_fld = 1, rj_fld%num_phys
        num_field = num_field + rj_fld%iflag_monitor(i_fld)
      end do
!
      call alloc_rms_name_sph_spec(num_field, pwr)
!
      j_fld = 0
      do i_fld = 1, rj_fld%num_phys
        if(rj_fld%iflag_monitor(i_fld) .gt. 0) then
          j_fld = j_fld + 1
          pwr%id_field(j_fld) =   i_fld
          pwr%num_comp_sq(j_fld) =    rj_fld%num_component(i_fld)
          pwr%istack_comp_sq(j_fld) = pwr%istack_comp_sq(j_fld-1)       &
     &                              + rj_fld%num_component(i_fld)
          pwr%pwr_name(j_fld) =   rj_fld%phys_name(i_fld)
        end if
      end do
!
      call quicksort_int                                                &
     &   (pwr%nri_rms, pwr%kr_4_rms, ione, pwr%nri_rms)
!
      call alloc_rms_4_sph_spectr                                       &
     &   (my_rank, sph_params%l_truncation, pwr)
      call alloc_ave_4_sph_spectr                                       &
     &   (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(1), pwr)
      call allocate_rms_sph_local_data                                  &
     &   (sph_params%l_truncation, sph_rj%nidx_rj,                      &
     &    pwr%num_vol_spectr, pwr%nri_rms, pwr%ntot_comp_sq, WK_pwr)
!
      call set_sum_table_4_sph_spectr(sph_params%l_truncation,          &
     &    sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                        &
     &    WK_pwr%num_mode_sum_l,  WK_pwr%num_mode_sum_m,                &
     &    WK_pwr%num_mode_sum_lm, WK_pwr%istack_mode_sum_l,             &
     &    WK_pwr%istack_mode_sum_m, WK_pwr%istack_mode_sum_lm,          &
     &    WK_pwr%item_mode_sum_l, WK_pwr%item_mode_sum_m,               &
     &    WK_pwr%item_mode_sum_lm)
!
!
      do knum = 1, pwr%nri_rms
        k = pwr%kr_4_rms(knum)
        if(k .le. 0) then
          pwr%r_4_rms(knum) = 0.0d0
        else
          pwr%r_4_rms(knum) = sph_rj%radius_1d_rj_r(k)
        end if
      end do
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'volume mean square file area:'
        do i = 1, pwr%num_vol_spectr
          write(*,*) i, pwr%v_spectr(i)%iflag_volume_rms_spec,          &
     &                  trim(pwr%v_spectr(i)%fhead_rms_v),              &
     &                  pwr%v_spectr(i)%avol
        end do
        write(*,*) 'volume mean square file area:'
        do i = 1, pwr%num_vol_spectr
          write(*,*) i, pwr%v_spectr(i)%iflag_volume_ave_sph,           &
     &                  trim(pwr%v_spectr(i)%fhead_ave)
        end do
        write(*,*) 'Integration area:'
        do i = 1, pwr%num_vol_spectr
          write(*,*) i,                                                 &
     &        pwr%v_spectr(i)%kr_inside, pwr%v_spectr(i)%kr_outside,    &
     &        pwr%v_spectr(i)%r_inside,  pwr%v_spectr(i)%r_outside
        end do
      end if
!
      end subroutine init_rms_4_sph_spectr
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_mean_squre_in_shell(sph_params,                    &
     &          sph_rj, ipol, rj_fld, g_sph_rj, pwr, WK_pwr)
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
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: i
!
!
      if(pwr%ntot_comp_sq .eq. 0) return
!
      call calypso_mpi_barrier
      if(iflag_debug .gt. 0) write(*,*) 'sum_sph_layerd_rms'
      call sum_sph_layerd_rms                                           &
     &   (sph_params%l_truncation, sph_rj, ipol, g_sph_rj, rj_fld,      &
     &    pwr%nri_rms, pwr%num_fld_sq, pwr%istack_comp_sq,              &
     &    pwr%id_field, pwr%kr_4_rms, pwr%num_vol_spectr,               &
     &    pwr%v_spectr, WK_pwr)
!
      if(iflag_debug .gt. 0) write(*,*) 'global_sum_sph_layerd_rms'
      call global_sum_sph_layerd_rms                                    &
     &    (sph_params%l_truncation, pwr%nri_rms, pwr%ntot_comp_sq,      &
     &     WK_pwr, pwr%shl_l, pwr%shl_m, pwr%shl_lm,                    &
     &     pwr%shl_sq, pwr%shl_m0, pwr%ratio_shl_m0,                    &
     &     pwr%num_vol_spectr, pwr%v_spectr)
!
!
      call calypso_mpi_barrier
      if(my_rank .eq. 0) then
        if(iflag_debug .gt. 0) write(*,*) 'surf_ave_4_sph_rms_int'
        call surf_ave_4_sph_rms_int(sph_params%l_truncation,            &
     &      sph_rj%nidx_rj(1), sph_rj%a_r_1d_rj_r,                      &
     &      pwr%nri_rms, pwr%ntot_comp_sq, pwr%kr_4_rms,                &
     &      pwr%shl_l, pwr%shl_m, pwr%shl_lm, pwr%shl_sq, pwr%shl_m0)
        do i = 1, pwr%num_vol_spectr
          call vol_ave_4_rms_sph                                        &
     &       (sph_params%l_truncation, pwr%ntot_comp_sq,                &
     &        pwr%v_spectr(i)%avol, pwr%v_spectr(i)%v_l,                &
     &        pwr%v_spectr(i)%v_m, pwr%v_spectr(i)%v_lm,                &
     &        pwr%v_spectr(i)%v_sq, pwr%v_spectr(i)%v_m0)
        end do
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_volume_average_sph'
      call cal_volume_average_sph(sph_rj, rj_fld, pwr)
!
      end subroutine cal_mean_squre_in_shell
!
! ----------------------------------------------------------------------
!
      subroutine cal_correlate_in_shell(sph_params,                     &
     &          sph_rj, rj_fld1, rj_fld2, g_sph_rj, cor, WK_pwr)
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
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(sph_mean_squares), intent(inout) :: cor
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: i
!
!
      if(cor%ntot_comp_sq .eq. 0) return
!
      call calypso_mpi_barrier
      if(iflag_debug .gt. 0) write(*,*) 'sum_sph_layerd_correlate'
      call sum_sph_layerd_correlate                                     &
     &   (sph_params%l_truncation, sph_rj, g_sph_rj, rj_fld1, rj_fld2,  &
     &    cor%nri_rms, cor%num_fld_sq, cor%istack_comp_sq,              &
     &    cor%id_field, cor%kr_4_rms, cor%num_vol_spectr,               &
     &    cor%v_spectr, WK_pwr)
!
      if(iflag_debug .gt. 0) write(*,*) 'global_sum_sph_layerd_rms'
      call global_sum_sph_layerd_rms                                    &
     &    (sph_params%l_truncation, cor%nri_rms, cor%ntot_comp_sq,      &
     &     WK_pwr, cor%shl_l, cor%shl_m, cor%shl_lm,                    &
     &     cor%shl_sq, cor%shl_m0, cor%ratio_shl_m0,                    &
     &     cor%num_vol_spectr, cor%v_spectr)
!
!
      call calypso_mpi_barrier
      if(my_rank .eq. 0) then
        if(iflag_debug .gt. 0) write(*,*) 'surf_ave_4_sph_rms_int'
        call surf_ave_4_sph_rms_int(sph_params%l_truncation,            &
     &      sph_rj%nidx_rj(1), sph_rj%a_r_1d_rj_r,                      &
     &      cor%nri_rms, cor%ntot_comp_sq, cor%kr_4_rms,                &
     &      cor%shl_l, cor%shl_m, cor%shl_lm, cor%shl_sq, cor%shl_m0)
        do i = 1, cor%num_vol_spectr
          call vol_ave_4_rms_sph                                        &
     &       (sph_params%l_truncation, cor%ntot_comp_sq,                &
     &        cor%v_spectr(i)%avol, cor%v_spectr(i)%v_l,                &
     &        cor%v_spectr(i)%v_m, cor%v_spectr(i)%v_lm,                &
     &        cor%v_spectr(i)%v_sq, cor%v_spectr(i)%v_m0)
        end do
      end if
!
      end subroutine cal_correlate_in_shell
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine global_sum_sph_layerd_rms                              &
     &         (l_truncation, nri_rms, ntot_rms_rj, WK_pwr,             &
     &          rms_sph_l, rms_sph_m, rms_sph_lm,                       &
     &          rms_sph, rms_sph_m0, ratio_sph_m0, num_vol_spectr, v_pwr)
!
      use calypso_mpi
!
      use cal_ave_4_rms_vector_sph
      use radial_int_for_sph_spec
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nri_rms
      integer(kind = kint), intent(in) :: ntot_rms_rj
      type(sph_mean_square_work), intent(in) :: WK_pwr
!
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sph_l(nri_rms,0:l_truncation,ntot_rms_rj)
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sph_m(nri_rms,0:l_truncation,ntot_rms_rj)
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sph_lm(nri_rms,0:l_truncation,ntot_rms_rj)
!
      real(kind = kreal), intent(inout) :: rms_sph(nri_rms,ntot_rms_rj)
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sph_m0(nri_rms,ntot_rms_rj)
      real(kind = kreal), intent(inout)                                 &
     &          :: ratio_sph_m0(nri_rms,ntot_rms_rj)
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
        call calypso_mpi_reduce_real                                    &
     &     (WK_pwr%vol_l_local(0,1,i), v_pwr(i)%v_l, num64, MPI_SUM, 0)
        call calypso_mpi_reduce_real                                    &
     &     (WK_pwr%vol_m_local(0,1,i), v_pwr(i)%v_m, num64, MPI_SUM, 0)
        call calypso_mpi_reduce_real                                    &
     &     (WK_pwr%vol_lm_local(0,1,i), v_pwr(i)%v_lm,                  &
     &      num64, MPI_SUM, 0)
!
        if(my_rank .eq. 0) then
          call sum_sph_vol_rms_all_modes(l_truncation, ntot_rms_rj,     &
     &      v_pwr(i)%v_l, v_pwr(i)%v_sq)
          call pick_axis_sph_vol_pwr                                    &
     &     (l_truncation, ntot_rms_rj, v_pwr(i)%v_m, v_pwr(i)%v_sq,     &
     &      v_pwr(i)%v_m0, v_pwr(i)%v_ratio_m0)
        end if
      end do
!
      if(nri_rms .le. 0) return
      num64 = ntot_rms_rj * nri_rms * (l_truncation + 1)
      call calypso_mpi_reduce_real                                      &
     &   (WK_pwr%shl_l_local, rms_sph_l, num64, MPI_SUM, 0)
      call calypso_mpi_reduce_real                                      &
     &   (WK_pwr%shl_m_local, rms_sph_m, num64, MPI_SUM, 0)
      call calypso_mpi_reduce_real                                      &
     &   (WK_pwr%shl_lm_local, rms_sph_lm, num64, MPI_SUM, 0)
!
      if(my_rank .gt. 0) return
      call sum_sph_rms_all_modes(l_truncation, nri_rms, ntot_rms_rj,    &
     &    rms_sph_l, rms_sph)
      call pick_axis_sph_power(l_truncation, nri_rms, ntot_rms_rj,      &
     &    rms_sph_m, rms_sph, rms_sph_m0, ratio_sph_m0)
!
      end subroutine global_sum_sph_layerd_rms
!
! -----------------------------------------------------------------------
!
      subroutine find_radial_grid_index                                 &
     &         (sph_rj, kr_default, r_target, kr_target)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: kr_default
!
      integer(kind = kint), intent(inout) :: kr_target
      real(kind = kreal), intent(inout) :: r_target
!
      integer(kind = kint) :: k
      real(kind = kreal) :: dr1, dr2
!
      if(r_target .eq. -1.0) then
        kr_target = kr_default
      else if(r_target .eq. 0.0) then
        kr_target = 0
      else if(r_target .le. sph_rj%radius_1d_rj_r(1)) then
        kr_target = 1
      else
        kr_target = sph_rj%nidx_rj(1)
        do k = 2, sph_rj%nidx_rj(1)
          dr1 = r_target - sph_rj%radius_1d_rj_r(k-1)
          dr2 = r_target - sph_rj%radius_1d_rj_r(k  )
!
          if(dr1*dr2 .le. zero) then
            if(abs(dr1) .lt. abs(dr2)) then
              kr_target = k - 1
            else
              kr_target = k
            end if
            exit
          end if
        end do
      end if
!
      if(kr_target .eq. 0) then
        r_target = 0.0
      else
        r_target = sph_rj%radius_1d_rj_r(kr_target)
      end if
!
      end subroutine find_radial_grid_index
!
! -----------------------------------------------------------------------
!
      end module cal_rms_fields_by_sph
