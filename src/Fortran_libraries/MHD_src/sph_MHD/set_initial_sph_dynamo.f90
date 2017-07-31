!>@file   set_initial_sph_dynamo.f90
!!@brief  module set_initial_sph_dynamo
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine sph_initial_data_control                             &
!!     &         (MHD_files, reftemp_rj, sph_params, sph_rj,            &
!!     &          ref_param_T, sph_bc_B, ipol, idpdr, itor, rj_fld,     &
!!     &          MHD_step)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        type(phys_address), intent(in) :: ipol
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
!
      module set_initial_sph_dynamo
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_reference_scalar_param
      use t_MHD_file_parameter
      use t_IO_step_parameter
      use t_time_data
      use t_spheric_rj_data
      use t_phys_address
      use t_boundary_params_sph_MHD
!
      implicit none
!
      private :: set_initial_temp_sph, set_initial_magne_sph
      private :: set_initial_light_sph, set_initial_velo_sph
      private :: set_ini_reference_temp_sph, set_all_part_temp_sph
      private :: set_noize_scalar_sph, reduce_initial_magne_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_data_control                               &
     &         (MHD_files, reftemp_rj, sph_params, sph_rj,              &
     &          ref_param_T, sph_bc_B, ipol, idpdr, itor, rj_fld,       &
     &          MHD_step)
!
      use m_machine_parameter
      use m_initial_field_control
!
      use t_MHD_step_parameter
      use t_reference_scalar_param
      use t_spheric_parameter
      use t_phys_data
!
      use set_sph_restart_IO
      use sph_mhd_rst_IO_control
      use initial_magne_dynamobench
      use initial_magne_dbench_qvc
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(sph_boundary_type), intent(in) :: sph_bc_B
      real(kind=kreal), intent(in) :: reftemp_rj(sph_rj%nidx_rj(1),0:2)
      type(phys_address), intent(in) :: ipol, idpdr, itor
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: isig
!
!
      if (iflag_restart .eq. i_rst_by_file) then
        if(iflag_debug .gt. 0) write(*,*) 'read_alloc_sph_restart_data'
        call read_alloc_sph_restart_data(MHD_files%fst_file_IO,         &
     &      MHD_step%init_d, rj_fld, MHD_step%rst_step)
!
!   for dynamo benchmark
!
      else if(iflag_restart .eq. i_rst_dbench0                          &
     &   .or. iflag_restart .eq. i_rst_dbench1                          &
     &   .or. iflag_restart .eq. i_rst_dbench2                          &
     &   .or. iflag_restart .eq. i_rst_dbench_qcv) then
        isig = 400
        call set_initial_velo_sph(ipol%i_velo,                          &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        if(ipol%i_temp .gt. 0) then
          call set_ini_reference_temp_sph(ipol%i_temp, reftemp_rj,      &
     &        sph_rj, ref_param_T%iflag_reference,                      &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
          call set_initial_temp_sph(isig, ipol%i_temp, sph_rj,          &
     &        sph_params%radius_ICB, sph_params%radius_CMB,             &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(ipol%i_light .gt. 0) then
          call set_initial_light_sph(isig, ipol%i_light, sph_rj,        &
     &        sph_params%radius_ICB, sph_params%radius_CMB,             &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB, reftemp_rj, &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
        if(iflag_restart .eq. i_rst_dbench1) then
          if(ipol%i_magne .gt. 0) then
            call initial_b_dynamobench_1(sph_rj, ipol, idpdr, itor,     &
     &          sph_params%radius_ICB, sph_params%radius_CMB,           &
     &          sph_params%nlayer_ICB, sph_params%nlayer_CMB,           &
     &          rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
          end if
        else if(iflag_restart .eq. i_rst_dbench2) then
          if(ipol%i_magne .gt. 0) then
            call initial_b_dynamobench_2(sph_rj, ipol, idpdr, itor,     &
     &          sph_params%nlayer_CMB, sph_params%radius_CMB,           &
     &          rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
          end if
        else if(iflag_restart .eq. i_rst_dbench_qcv) then
          if(ipol%i_magne .gt. 0) then
           call initial_b_dynamobench_qcv(sph_rj, ipol, idpdr, itor,    &
     &         sph_params%radius_ICB, sph_params%radius_CMB,            &
     &         sph_params%nlayer_ICB, sph_params%nlayer_CMB,            &
     &         rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
          end if
        end if
!
!   set small seed magnetic field
!
      else if (iflag_restart .eq. i_rst_no_file) then
        if(ipol%i_temp .gt. 0)  then
          call set_noize_scalar_sph(ipol%i_temp, reftemp_rj,            &
     &        sph_rj, ref_param_T%iflag_reference,                      &
     &        sph_params%radius_ICB, sph_params%radius_CMB,             &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(ipol%i_light .gt. 0) then
          call set_noize_scalar_sph(ipol%i_light, reftemp_rj,           &
     &        sph_rj, ref_param_T%iflag_reference,                      &
     &        sph_params%radius_ICB, sph_params%radius_CMB,             &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(ipol%i_magne .gt. 0) then
          call set_initial_magne_sph                                    &
     &       (sph_rj, sph_bc_B, ipol, idpdr, itor,                      &
     &        sph_params%radius_ICB, sph_params%radius_CMB,             &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
          call reduce_initial_magne_sph                                 &
     &       (ipol, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
      else if (iflag_restart .eq. i_rst_licv) then
        call set_ini_reference_temp_sph(ipol%i_temp, reftemp_rj,        &
     &      sph_rj, ref_param_T%iflag_reference,                        &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call set_all_part_temp_sph(ipol%i_temp, sph_rj,                 &
     &      sph_params%radius_ICB, sph_params%radius_CMB,               &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 'init_output_sph_restart_file'
      call copy_time_step_data(MHD_step%init_d, MHD_step%time_d)
      call init_output_sph_restart_file(rj_fld)
!
      if (iflag_restart.ne.i_rst_by_file                                &
     &     .and. MHD_step%init_d%i_time_step.eq.0) then
        if(iflag_debug .gt. 0) write(*,*) 'output_sph_restart_control'
        call output_sph_restart_control(MHD_files%fst_file_IO,          &
     &      MHD_step%time_d, rj_fld, MHD_step%rst_step)
      end if
!
      end subroutine sph_initial_data_control
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_velo_sph                                   &
     &         (is_velo, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer ( kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,is_velo  ) = zero
        d_rj(inod,is_velo+1) = zero
        d_rj(inod,is_velo+2) = zero
      end do
!$omp end parallel do
!
      end subroutine set_initial_velo_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_ini_reference_temp_sph                             &
     &         (is_temp, reftemp_rj, sph_rj, iflag_reftemp,             &
     &          nlayer_ICB, nlayer_CMB, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: iflag_reftemp
      integer(kind = kint), intent(in) :: is_temp
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind=kreal), intent(in) :: reftemp_rj(sph_rj%nidx_rj(1),0:1)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer ( kind = kint) :: inod, k, jj
!
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,is_temp) = zero
      end do
!$omp end parallel do
!
!   set reference temperature (l = m = 0)
      if (sph_rj%idx_rj_degree_zero .gt. 0) then
        if (iflag_reftemp .eq. id_sphere_ref_temp) then
          do k = 1, sph_rj%nidx_rj(1)
            inod = sph_rj%idx_rj_degree_zero + (k-1) * sph_rj%nidx_rj(2)
            d_rj(inod,is_temp) = reftemp_rj(k,0)
          end do
        else
          do k = 1, nlayer_ICB-1
            inod = local_sph_node_address(sph_rj, k, jj)
            d_rj(inod,is_temp) = 1.0d0
          end do
          do k = nlayer_ICB, nlayer_CMB
            inod = sph_rj%idx_rj_degree_zero                            &
     &            + (k-1) * sph_rj%nidx_rj(2)
            d_rj(inod,is_temp)                                          &
     &           = (sph_rj%ar_1d_rj(k,1) * 20.d0/13.0d0 - 1.0d0 )       &
     &            * 7.0d0 / 13.0d0
          end do
        end if
      end if
!
!    Center
      if(sph_rj%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj, 0, 0)
        inod = local_sph_node_address(sph_rj, 1, jj)
        d_rj(sph_rj%inod_rj_center,is_temp) = d_rj(inod,is_temp)
      end if
!
      end subroutine set_ini_reference_temp_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_all_part_temp_sph(is_temp, sph_rj,                 &
     &          r_ICB, r_CMB, nlayer_ICB, nlayer_CMB,                   &
     &          n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: is_temp
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer ( kind = kint) :: inod, j, k, jj
      real (kind = kreal) :: pi, xr, shell
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
      do j = 1, sph_rj%nidx_rj(2)
        do k = nlayer_ICB, nlayer_CMB
          xr = two * sph_rj%radius_1d_rj_r(k)                           &
     &        - one * (r_CMB+r_ICB) / shell
          inod = j + (k-1) * sph_rj%nidx_rj(2)
!
          d_rj(inod,is_temp) = (one-three*xr**2+three*xr**4-xr**6)      &
     &                            * 0.1d0 * six / (sqrt(pi))
        end do
      end do
!
!    Center
      if(sph_rj%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj, 0, 0)
        inod = local_sph_node_address(sph_rj, 1, jj)
        d_rj(sph_rj%inod_rj_center,is_temp) = d_rj(inod,is_temp)
      end if
!
      end subroutine set_all_part_temp_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_temp_sph(isig, is_temp, sph_rj,            &
     &          r_ICB, r_CMB, nlayer_ICB, nlayer_CMB,                   &
     &          n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      integer ( kind = kint), intent(in) :: isig, is_temp
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = 4) ::  m
      integer(kind = kint) :: inod, k, jj
      real (kind = kreal) :: pi, xr, shell
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
      m = int( mod(isig,ikilo) / icent )
      jj = find_local_sph_address(sph_rj, m, m)
!
      if (jj .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          xr = two * sph_rj%radius_1d_rj_r(k)                           &
     &        - one * (r_CMB+r_ICB) / shell
          inod = jj + (k-1) * sph_rj%nidx_rj(2)
!
          d_rj(inod,is_temp) = (one-three*xr**2+three*xr**4-xr**6)      &
     &                            * 0.1d0 * three / (sqrt(two*pi))
        end do
      end if
!
!    Center
      if(sph_rj%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj, 0, 0)
        inod = local_sph_node_address(sph_rj, 1, jj)
        d_rj(sph_rj%inod_rj_center,is_temp) = d_rj(inod,is_temp)
      end if
!
      end subroutine set_initial_temp_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_light_sph(isig, is_fld, sph_rj,            &
     &          r_ICB, r_CMB, nlayer_ICB, nlayer_CMB,                   &
     &          reftemp_rj, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      integer ( kind = kint), intent(in) :: isig, is_fld
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind=kreal), intent(in) :: reftemp_rj(sph_rj%nidx_rj(1),0:1)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = 4) :: m
      integer ( kind = kint) :: inod, k, jj
      real (kind = kreal) :: pi, xr, shell
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,is_fld) = zero
      end do
!$omp end parallel do
!
!
      if (sph_rj%idx_rj_degree_zero .gt. 0) then
        do k = 1, sph_rj%nidx_rj(1)
          inod = sph_rj%idx_rj_degree_zero + (k-1) * sph_rj%nidx_rj(2)
          d_rj(inod,is_fld) = reftemp_rj(k,0)
        end do
      end if
!
!
      m = int( mod(isig,ikilo) / icent )
      jj = find_local_sph_address(sph_rj, m, m)
!
!
      if (jj .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          xr = two * sph_rj%radius_1d_rj_r(k)                           &
     &        - one * (r_CMB+r_ICB) / shell
          inod = jj + (k-1) * sph_rj%nidx_rj(2)
          d_rj(inod,is_fld) = (one-three*xr**2+three*xr**4-xr**6)       &
     &                            * 0.1d0 * three / (sqrt(two*pi))
        end do
      end if
!
!    Center
      if(sph_rj%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj, 0, 0)
        inod = local_sph_node_address(sph_rj, 1, jj)
        d_rj(sph_rj%inod_rj_center,is_fld) = d_rj(inod,is_fld)
      end if
!
      end subroutine set_initial_light_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_magne_sph                                  &
     &         (sph_rj, sph_bc_B, ipol, idpdr, itor, r_ICB, r_CMB,      &
     &          nlayer_ICB, nlayer_CMB, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol, idpdr, itor
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: is, it, k, js, jt
!
!
      js = find_local_sph_address(sph_rj, 1,0)
      jt = find_local_sph_address(sph_rj, 2,0)
!
      pi = four * atan(one)
!
!$omp parallel do
      do is = 1, n_point
        d_rj(is,ipol%i_magne  ) = zero
        d_rj(is,ipol%i_magne+1) = zero
        d_rj(is,ipol%i_magne+2) = zero
        d_rj(is,ipol%i_current  ) = zero
        d_rj(is,ipol%i_current+1) = zero
        d_rj(is,ipol%i_current+2) = zero
      end do
!$omp end parallel do
!
      if (sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
!
        if (js .gt. 0) then
          do k = nlayer_ICB, nlayer_CMB
            is = js + (k-1) * sph_rj%nidx_rj(2)
            rr = sph_rj%radius_1d_rj_r(k)
!
            d_rj(is,ipol%i_magne) =  (five / eight) * (-three * rr**3   &
     &                       + four * r_CMB * rr**2 - r_ICB**4 / rr)
            d_rj(is,idpdr%i_magne) = (five / eight) * (-dnine * rr**2   &
     &                         + eight * r_CMB * rr + r_ICB**4 / rr**2)
            d_rj(is,itor%i_current) =  (five*three / two) * rr
          end do
        end if
!
        if (jt .gt. 0) then
          do k = nlayer_ICB, nlayer_CMB
            it = jt + (k-1) * sph_rj%nidx_rj(2)
            rr = sph_rj%radius_1d_rj_r(k)
            d_rj(it,itor%i_magne)                                       &
     &            =  (ten/three) * rr * sin(pi*(rr-r_ICB))
            d_rj(it,ipol%i_current) =  d_rj(it,itor%i_magne)
            d_rj(it,idpdr%i_current)                                    &
     &            = (ten / three) * (sin(pi*(rr-r_ICB))  &
     &                          + pi * rr * cos(pi*(rr-r_ICB)) )
          end do
        end if
!
      else
!
        if (js .gt. 0) then
          do k = 1, nlayer_CMB
            is = js + (k-1) * sph_rj%nidx_rj(2)
            rr = sph_rj%radius_1d_rj_r(k)
            d_rj(is,ipol%i_magne) =  (five / two) * rr**2               &
     &                       * (four*r_CMB - three*rr) / (r_CMB+three)
            d_rj(is,idpdr%i_magne) = (five / two) * rr                  &
     &                       * (eight*r_CMB - dnine*rr) / (r_CMB+three)
            d_rj(is,itor%i_current) =  five*six * rr / (three +r_CMB)
          end do
        end if
!
        if (jt .gt. 0) then
          do k = 1, nlayer_CMB
            it = jt + (k-1) * sph_rj%nidx_rj(2)
            rr = sph_rj%radius_1d_rj_r(k)
!
            d_rj(it,itor%i_magne)                                       &
     &          =  (ten / three) * rr * sin(pi*rr/r_CMB)
            d_rj(it,ipol%i_current) =  d_rj(it,itor%i_magne)
            d_rj(it,idpdr%i_current)                                    &
     &          = (ten / three) * (sin(pi*rr/r_CMB)    &
     &                          + (pi/r_CMB) * rr * cos(pi*rr/r_CMB) )
          end do
        end if
!
      end if
!
      end subroutine set_initial_magne_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_noize_scalar_sph(is_fld, reftemp_rj,               &
     &          sph_rj, iflag_reftemp, r_ICB, r_CMB,                    &
     &          nlayer_ICB, nlayer_CMB, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: iflag_reftemp
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind=kreal), intent(in) :: reftemp_rj(sph_rj%nidx_rj(1),0:1)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer( kind = kint) :: inod, j, k, jj
      real (kind = kreal) :: pi, xr, shell
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,is_fld) = zero
      end do
!$omp end parallel do
!
!
      if (sph_rj%idx_rj_degree_zero .gt. 0) then
        if(iflag_reftemp .eq. id_sphere_ref_temp) then
          do k = 1, sph_rj%nidx_rj(1)
            inod = sph_rj%idx_rj_degree_zero + (k-1)*sph_rj%nidx_rj(2)
            d_rj(inod,is_fld) = reftemp_rj(k,0)
          end do
        end if
      end if
!
!
      do j = 1+sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2)
        do k = nlayer_ICB+2, nlayer_CMB-2
          inod = j + (k-1) * sph_rj%nidx_rj(2)
!
          xr = two * sph_rj%radius_1d_rj_r(k)                           &
     &       - (sph_rj%radius_1d_rj_r(nlayer_ICB+2)                     &
     &         + sph_rj%radius_1d_rj_r(nlayer_CMB-2) ) / shell
          d_rj(inod,is_fld) = (one-three*xr**2+three*xr**4-xr**6)       &
     &                       * 1.0d-4 * six / (sqrt(pi))
        end do
      end do
!
!    Center
      if(sph_rj%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj, 0, 0)
        inod = local_sph_node_address(sph_rj, 1, jj)
        d_rj(sph_rj%inod_rj_center,is_fld) = d_rj(inod,is_fld)
      end if
!
      end subroutine set_noize_scalar_sph
!
!-----------------------------------------------------------------------
!
      subroutine reduce_initial_magne_sph                               &
     &         (ipol, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: is
!
!
!$omp parallel do
      do is = 1, nnod_rj
        d_rj(is,ipol%i_magne  ) = 1.0d-3 * d_rj(is,ipol%i_magne  )
        d_rj(is,ipol%i_magne+1) = 1.0d-3 * d_rj(is,ipol%i_magne+1)
        d_rj(is,ipol%i_magne+2) = 1.0d-3 * d_rj(is,ipol%i_magne+2)
        d_rj(is,ipol%i_current  ) = 1.0d-3 * d_rj(is,ipol%i_current  )
        d_rj(is,ipol%i_current+1) = 1.0d-3 * d_rj(is,ipol%i_current+1)
        d_rj(is,ipol%i_current+2) = 1.0d-3 * d_rj(is,ipol%i_current+2)
      end do
!$omp end parallel do
!
      end subroutine reduce_initial_magne_sph
!
!-----------------------------------------------------------------------
!
      end module set_initial_sph_dynamo
