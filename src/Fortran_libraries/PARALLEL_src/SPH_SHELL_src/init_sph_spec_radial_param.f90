!>@file   init_sph_spec_radial_param.f90
!!@brief      module init_sph_spec_radial_param
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief evaluate mean square data from spectr data
!!
!!@verbatim
!!      subroutine init_sph_vol_spectr_r_param(sph_params, sph_rj,      &
!!     &                                       v_pwr)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_vol_mean_squares), intent(inout) :: v_pwr
!!      subroutine set_all_layer_sph_spectr(sph_params, sph_rj, pwr)
!!      subroutine append_CMB_layer_f_dipolarity(sph_params, pwr)
!!      subroutine set_layers_4_sph_spectr(sph_rj, pwr)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_mean_squares), intent(inout) :: pwr
!!@endverbatim
      module init_sph_spec_radial_param
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use t_sph_volume_mean_square
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_vol_spectr_r_param(sph_params, sph_rj,        &
     &                                       v_pwr)
!
      use set_radial_interpolation
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(sph_vol_mean_squares), intent(inout) :: v_pwr
!
      integer(kind = kint) :: kr_st
!
!
        if(v_pwr%r_inside .le. zero) then
          v_pwr%kr_inside(1:2) = sph_params%nlayer_ICB
          v_pwr%r_inside = sph_params%radius_ICB
          v_pwr%c_inter_in = one
        else if(v_pwr%r_inside .eq. zero) then
          v_pwr%kr_inside(1:2) = 0
          v_pwr%c_inter_in = one
        else if(v_pwr%r_inside .le. sph_rj%radius_1d_rj_r(1)) then
          v_pwr%kr_inside(1) = 0
          v_pwr%kr_inside(2) = 1
          v_pwr%c_inter_in = v_pwr%r_inside / sph_rj%radius_1d_rj_r(1)
        else
          kr_st = 1
          call s_set_radial_interpolation(sph_rj%nidx_rj(1),            &
     &        sph_rj%radius_1d_rj_r, v_pwr%r_inside, kr_st,             &
     &        v_pwr%kr_inside(1), v_pwr%kr_inside(2), v_pwr%c_inter_in)
        end if
!
        if(abs(v_pwr%c_inter_in) .lt. 1.0d-6) then
          kr_st = v_pwr%kr_inside(1)
          v_pwr%kr_inside(2) = kr_st
          v_pwr%r_inside =     sph_rj%radius_1d_rj_r(kr_st)
          v_pwr%c_inter_in =   one
        else if(abs(one - v_pwr%c_inter_in) .lt. 1.0d-6) then
          kr_st = v_pwr%kr_inside(2)
          v_pwr%kr_inside(1) = kr_st
          v_pwr%r_inside =     sph_rj%radius_1d_rj_r(kr_st)
          v_pwr%c_inter_in =   one
        end if
!
        if(v_pwr%r_outside .le. zero) then
          v_pwr%kr_outside(1:2) = sph_params%nlayer_CMB
          v_pwr%r_outside = sph_params%radius_CMB
          v_pwr%c_inter_out = one
        else if(v_pwr%r_outside                                         &
     &      .ge. sph_rj%radius_1d_rj_r(sph_rj%nidx_rj(1))) then
          v_pwr%kr_outside(1:2) = sph_rj%nidx_rj(1)
          v_pwr%r_outside = sph_rj%radius_1d_rj_r(sph_rj%nidx_rj(1))
          v_pwr%c_inter_out = one
        else
          call s_set_radial_interpolation(sph_rj%nidx_rj(1),            &
     &        sph_rj%radius_1d_rj_r, v_pwr%r_outside, kr_st,            &
     &        v_pwr%kr_outside(1), v_pwr%kr_outside(2), &
     &        v_pwr%c_inter_out)
        end if
!
        if(abs(v_pwr%c_inter_out) .lt. 1.0d-6) then
          kr_st = v_pwr%kr_outside(1)
          v_pwr%kr_outside(2) = kr_st
          v_pwr%r_outside =     sph_rj%radius_1d_rj_r(kr_st)
          v_pwr%c_inter_out =   one
        else if(abs(one - v_pwr%c_inter_out) .lt. 1.0d-6) then
          kr_st = v_pwr%kr_outside(2)
          v_pwr%kr_outside(1) = kr_st
          v_pwr%r_outside =     sph_rj%radius_1d_rj_r(kr_st)
          v_pwr%c_inter_out =   one
        end if
!
      end subroutine init_sph_vol_spectr_r_param
!
! ----------------------------------------------------------------------
!
      subroutine set_all_layer_sph_spectr(sph_params, sph_rj, pwr)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: k
!
!
      if(pwr%nri_rms .eq. -1) then
        call alloc_num_spec_layer(sph_rj%nidx_rj(1), pwr)
!
        do k = 1, sph_rj%nidx_rj(1)
          pwr%kr_4_rms(k,1) = k
          pwr%r_4_rms(k,1) = -one
        end do
      end if
!
      end subroutine set_all_layer_sph_spectr
!
! ----------------------------------------------------------------------
!
      subroutine append_CMB_layer_f_dipolarity(sph_params, pwr)
!
      type(sph_shell_parameters), intent(in) :: sph_params
!
      type(sph_mean_squares), intent(inout) :: pwr
!
      logical :: false_flag
      integer(kind = kint) :: k
      integer(kind = kint), allocatable :: kr_tmp(:)
      real(kind = kreal), allocatable :: r_tmp(:)
!
!
        false_flag = .TRUE.
        do k = 1, pwr%nri_rms
          if(pwr%kr_4_rms(k,1) .eq. sph_params%nlayer_CMB) then
            false_flag = .FALSE.
            exit
          end if
          if(abs(pwr%r_4_rms(k,1) - sph_params%radius_CMB)              &
     &                                           .lt. 1.0e-7) then
            pwr%r_4_rms(k,1) = sph_params%radius_CMB
            false_flag = .FALSE.
            exit
          end if
        end do
!
        if(false_flag) then
          allocate(kr_tmp(1:pwr%nri_rms))
          allocate(r_tmp(1:pwr%nri_rms))
          if(pwr%nri_rms .gt. 0) then
            kr_tmp(1:pwr%nri_rms) = pwr%kr_4_rms(1:pwr%nri_rms,1)
            r_tmp(1:pwr%nri_rms) =  pwr%r_4_rms(1:pwr%nri_rms,1)
          end if
          call dealloc_num_spec_layer(pwr)
!
          k = pwr%nri_rms + 1
          call alloc_num_spec_layer(k, pwr)
          if(pwr%nri_rms .gt. 1) then
            pwr%kr_4_rms(1:pwr%nri_rms-1,1) = kr_tmp(1:pwr%nri_rms-1)
            pwr%r_4_rms(1:pwr%nri_rms-1,1) =  r_tmp(1:pwr%nri_rms-1)
          end if
          pwr%kr_4_rms(pwr%nri_rms,1) = sph_params%nlayer_CMB
          pwr%r_4_rms(pwr%nri_rms,1) =  -one
          deallocate(kr_tmp)
        end if
!
      end subroutine append_CMB_layer_f_dipolarity
!
! ----------------------------------------------------------------------
!
      subroutine set_layers_4_sph_spectr(sph_rj, pwr)
!
      use set_radial_interpolation
      use quicksort
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: k, kg, kr_st
!
!
      do k = 1, pwr%nri_rms
        kg = pwr%kr_4_rms(k,1)
        if(kg .gt. 0) then
          pwr%r_4_rms(k,1) = sph_rj%radius_1d_rj_r(kg)
        end if
      end do
!
      if(pwr%nri_rms .gt. 1) then
        call quicksort_real_w_index(pwr%nri_rms,                        &
     &      pwr%r_4_rms(1,1), ione, pwr%nri_rms, pwr%kr_4_rms(1,1))
      end if
!
      kr_st = 1
      do k = 1, pwr%nri_rms
        if(pwr%r_4_rms(k,1) .eq. zero) then
          pwr%kr_4_rms(k,2) = pwr%kr_4_rms(k,1)
          pwr%c_gl_itp(k) = one
        else if(pwr%kr_4_rms(k,1) .gt. 0) then
          pwr%kr_4_rms(k,2) = pwr%kr_4_rms(k,1)
          pwr%c_gl_itp(k) = one
        else
          call s_set_radial_interpolation(sph_rj%nidx_rj(1),            &
     &        sph_rj%radius_1d_rj_r, pwr%r_4_rms(k,1), kr_st,           &
     &        pwr%kr_4_rms(k,1), pwr%kr_4_rms(k,2), pwr%c_gl_itp(k))
        end if
!
        if(abs(pwr%c_gl_itp(k)) .lt. 1.0d-6) then
          kr_st = pwr%kr_4_rms(k,1)
          pwr%kr_4_rms(k,2) = kr_st
          pwr%r_4_rms(k,1) =  sph_rj%radius_1d_rj_r(kr_st)
          pwr%c_gl_itp(k) =   one
        else if(abs(one - pwr%c_gl_itp(k)) .lt. 1.0d-6) then
          kr_st = pwr%kr_4_rms(k,2)
          pwr%kr_4_rms(k,1) = kr_st
          pwr%r_4_rms(k,1) =  sph_rj%radius_1d_rj_r(kr_st)
          pwr%c_gl_itp(k) =   one
        end if
!
        if(pwr%r_4_rms(k,1) .eq. zero) then
          pwr%r_4_rms(k,2) = zero
        else
          pwr%r_4_rms(k,2) = one / pwr%r_4_rms(k,1)
        end if
      end do
!
      end subroutine set_layers_4_sph_spectr
!
! ----------------------------------------------------------------------
!
      end module init_sph_spec_radial_param
