!>@file   init_rms_4_sph_spectr.f90
!!@brief      module init_rms_4_sph_spectr
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief evaluate mean square data from spectr data
!!
!!@verbatim
!!      subroutine s_init_rms_4_sph_spectr(sph_params, sph_rj, rj_fld,  &
!!     &         iflag_dipolarity, pwr, WK_pwr)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        integer(kind = kint), intent(in) :: iflag_dipolarity
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!@endverbatim
!
      module init_rms_4_sph_spectr
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
!
      implicit none
!
      private :: find_radial_grid_index, set_domains_4_spectr_output
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_init_rms_4_sph_spectr(sph_params, sph_rj, rj_fld,    &
     &         iflag_dipolarity, pwr, WK_pwr)
!
      use calypso_mpi
!
      use sum_sph_rms_data
      use volume_average_4_sph
      use cal_ave_4_rms_vector_sph
      use set_parallel_file_name
      use quicksort
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: iflag_dipolarity
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      logical :: false_flag
      integer(kind = kint) :: i_fld, j_fld
      integer(kind = kint) :: i, k, knum, num_field
      integer(kind = kint), allocatable :: kr_tmp(:)
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
      if(iflag_dipolarity .gt. 0) then
        false_flag = .TRUE.
        do knum = 1, pwr%nri_rms
          if(pwr%kr_4_rms(knum) .eq. sph_params%nlayer_CMB) then
            false_flag = .FALSE.
            exit
          end if
        end do
!
        if(false_flag) then
          allocate(kr_tmp(1:pwr%nri_rms))
          if(pwr%nri_rms .gt. 0) then
            kr_tmp(1:pwr%nri_rms) = pwr%kr_4_rms(1:pwr%nri_rms)
          end if
          call dealloc_num_spec_layer(pwr)
!
          knum = pwr%nri_rms + 1
          call alloc_num_spec_layer(knum, pwr)
          if(pwr%nri_rms .gt. 1) then
            pwr%kr_4_rms(1:pwr%nri_rms-1) = kr_tmp(1:pwr%nri_rms-1)
          end if
          pwr%kr_4_rms(pwr%nri_rms) = sph_params%nlayer_CMB
          deallocate(kr_tmp)
        end if
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
        if(rj_fld%flag_monitor(i_fld)) num_field = num_field + 1
      end do
!
      call alloc_rms_name_sph_spec(num_field, pwr)
!
      j_fld = 0
      do i_fld = 1, rj_fld%num_phys
        if(rj_fld%flag_monitor(i_fld)) then
          j_fld = j_fld + 1
          pwr%id_field(j_fld) =   i_fld
          pwr%num_comp_sq(j_fld) =    rj_fld%num_component(i_fld)
          pwr%istack_comp_sq(j_fld) = pwr%istack_comp_sq(j_fld-1)       &
     &                              + rj_fld%num_component(i_fld)
          pwr%pwr_name(j_fld) =   rj_fld%phys_name(i_fld)
        end if
      end do
!
      if(pwr%nri_rms .gt. 1) then
        call quicksort_int                                              &
     &     (pwr%nri_rms, pwr%kr_4_rms, ione, pwr%nri_rms)
      end if
!
      call set_domains_4_spectr_output(sph_rj, pwr)
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
      end subroutine s_init_rms_4_sph_spectr
!
! ----------------------------------------------------------------------
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
      subroutine set_domains_4_spectr_output(sph_rj, pwr)
!
      use calypso_mpi
      use calypso_mpi_int4
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer :: ip_ave, ip_lc
      integer(kind = kint) :: icou, i
      real(kind = kreal) :: rinc
!
!
      icou = 1 + 3 * pwr%num_vol_spectr
      if(pwr%nri_ave .gt. 0) icou = icou + 3
      rinc = dble(nprocs-1) / dble(icou)
!
      ip_lc = 0
      if(sph_rj%idx_rj_degree_zero .gt. 0)  ip_lc = my_rank
      call calypso_mpi_allreduce_one_int4(ip_lc, ip_ave, MPI_SUM)
!
      pwr%irank_m = -1
      pwr%irank_l = -1
      pwr%irank_lm = -1
      do i = 1, pwr%num_vol_spectr
        pwr%v_spectr(i)%irank_m =  -1
        pwr%v_spectr(i)%irank_l =  -1
        pwr%v_spectr(i)%irank_lm = -1
      end do
!
      icou = 0
      if(pwr%nri_rms .gt. izero)  then
        pwr%irank_m =  mod(int((icou+1)*rinc)+ip_ave, nprocs)
        pwr%irank_l =  mod(int((icou+2)*rinc)+ip_ave, nprocs)
        pwr%irank_lm = mod(int((icou+3)*rinc)+ip_ave, nprocs)
        icou = icou + 3
      end if
      do i = 1, pwr%num_vol_spectr
        pwr%v_spectr(i)%irank_m                                         &
     &        =  mod(int((icou+1)*rinc)+ip_ave, nprocs)
        pwr%v_spectr(i)%irank_l                                         &
     &        =  mod(int((icou+2)*rinc)+ip_ave, nprocs)
        pwr%v_spectr(i)%irank_lm                                        &
     &        = mod(int((icou+3)*rinc)+ip_ave, nprocs)
        icou = icou + 3
      end do
!
      if(iflag_debug .le. 0) return
!
      write(*,*) 'ip_ave', ip_ave, rinc
      write(*,*) 'Layers, irank_l, irank_m, irank_lm'
      write(*,*) pwr%irank_l, pwr%irank_m, pwr%irank_lm
      write(*,*) 'Volumes, irank_l, irank_m, irank_lm'
      do i = 1, pwr%num_vol_spectr
        write(*,*) i, pwr%v_spectr(i)%irank_l,                          &
     &      pwr%v_spectr(i)%irank_m, pwr%v_spectr(i)%irank_lm
      end do
!
      end subroutine set_domains_4_spectr_output
!
! -----------------------------------------------------------------------
!
      end module init_rms_4_sph_spectr
