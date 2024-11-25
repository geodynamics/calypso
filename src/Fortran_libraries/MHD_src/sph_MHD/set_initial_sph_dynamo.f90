!>@file   set_initial_sph_dynamo.f90
!!@brief  module set_initial_sph_dynamo
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine read_sph_initial_data_control(MHD_files, SPH_model,  &
!!     &          sph, ipol, MHD_step, rj_fld, sph_fst_IO)
!!      subroutine sph_initial_data_control                             &
!!     &         (MHD_files, SPH_model, SPH_MHD, MHD_step, sph_fst_IO)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_address), intent(in) :: ipol
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(field_IO), intent(inout) :: sph_fst_IO
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
      use t_SPH_MHD_model_data
      use t_MHD_file_parameter
      use t_IO_step_parameter
      use t_time_data
      use t_spheric_parameter
      use t_boundary_params_sph_MHD
      use t_radial_reference_field
      use t_field_data_IO
      use t_phys_address
      use t_phys_data
!
      implicit none
!
      private :: sph_initial_data_4_benchmarks
      private :: set_initial_velo_sph, set_initial_magne_sph
      private :: reduce_initial_magne_sph, sph_initial_data_w_seed_B
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_sph_initial_data_control(MHD_files, SPH_model,    &
     &          sph, ipol, MHD_step, rj_fld, sph_fst_IO)
!
      use m_machine_parameter
      use m_initial_field_control
!
      use t_MHD_step_parameter
!
      use set_sph_restart_IO
      use sph_mhd_rst_IO_control
      use set_sph_restart_IO
      use sph_radial_grad_4_magne
      use calypso_mpi
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SPH_MHD_model_data), intent(in) :: SPH_model 
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO), intent(inout) :: sph_fst_IO
!
!
      if (iflag_restart .ne. i_rst_by_file) return
        if(iflag_debug .gt. 0) write(*,*) 'read_alloc_sph_restart_data'
        call read_alloc_sph_restart_data                                &
     &     (MHD_files%fst_file_IO, MHD_step%init_d, MHD_step%time_d,    &
     &      rj_fld, MHD_step%rst_step)
!
        call extend_by_potential_with_j                                 &
     &     (sph%sph_rj, SPH_model%sph_MHD_bc%sph_bc_B,                  &
     &      ipol%base%i_magne, ipol%base%i_current, rj_fld)
!
        if(iflag_debug .gt. 0) write(*,*) 'copy_time_step_data'
        call set_sph_restart_num_to_IO(rj_fld, sph_fst_IO)
        call calypso_mpi_barrier
!
      end subroutine read_sph_initial_data_control
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_initial_data_control(MHD_files, SPH_model,         &
     &          sph, ipol, MHD_step, rj_fld, sph_fst_IO)
!
      use m_machine_parameter
      use m_initial_field_control
!
      use t_MHD_step_parameter
!
      use set_sph_restart_IO
      use sph_mhd_rst_IO_control
      use initial_magne_dynamobench
      use initial_magne_dbench_qvc
      use set_initial_sph_scalars
      use set_sph_restart_IO
      use sph_radial_grad_4_magne
      use calypso_mpi
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SPH_MHD_model_data), intent(in) :: SPH_model 
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(MHD_step_param), intent(in) :: MHD_step
!
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO), intent(inout) :: sph_fst_IO
!
!
      if (iflag_restart .eq. i_rst_by_file) return
!
!   for dynamo benchmark
      if(     iflag_restart .eq. i_rst_dbench0                          &
     &   .or. iflag_restart .eq. i_rst_dbench1                          &
     &   .or. iflag_restart .eq. i_rst_dbench2                          &
     &   .or. iflag_restart .eq. i_rst_dbench_qcv) then
        call sph_initial_data_4_benchmarks(sph%sph_params, sph%sph_rj,  &
     &                                     ipol, rj_fld)
!
!   set small seed magnetic field
      else if (iflag_restart .eq. i_rst_no_file) then
        call sph_initial_data_w_seed_B                                  &
     &     (SPH_model%refs, sph%sph_params, sph%sph_rj,                 &
     &      SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                   &
     &      ipol, rj_fld)
      else if (iflag_restart .eq. i_rst_licv) then
        call sph_initial_field_4_licv(SPH_model%refs,                   &
     &      sph%sph_params, sph%sph_rj,                                 &
     &      SPH_model%MHD_prop, ipol, rj_fld)
      end if
!
      call extend_by_potential_with_j                                   &
     &   (sph%sph_rj, SPH_model%sph_MHD_bc%sph_bc_B,                    &
     &    ipol%base%i_magne, ipol%base%i_current, rj_fld)
!
      if(iflag_debug .gt. 0) write(*,*) 'copy_time_step_data'
      call set_sph_restart_num_to_IO(rj_fld, sph_fst_IO)
!
      if (iflag_restart.ne.i_rst_by_file                                &
     &     .and. MHD_step%init_d%i_time_step.eq.0) then
        if(iflag_debug .gt. 0) write(*,*) 'output_sph_restart_control'
        call output_sph_restart_control(MHD_step%init_d%i_time_step,    &
     &      MHD_files%fst_file_IO, MHD_step%time_d, rj_fld,             &
     &      MHD_step%rst_step, sph_fst_IO)
      end if
      call calypso_mpi_barrier
!
      end subroutine sph_initial_data_control
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_initial_data_4_benchmarks                          &
     &         (sph_params, sph_rj, ipol, rj_fld)
!
      use m_machine_parameter
      use m_initial_field_control
!
      use t_reference_scalar_param
      use t_spheric_parameter
      use t_phys_data
!
      use initial_magne_dynamobench
      use initial_magne_dbench_qvc
      use set_initial_sph_scalars
      use calypso_mpi
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: isig
!
!
        call calypso_mpi_barrier
        if(iflag_debug .gt. 0) write(*,*) 'set_initial_velo_sph'
        isig = 400
        call set_initial_velo_sph(ipol%base%i_velo,                     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        if(ipol%base%i_temp .gt. 0) then
          call calypso_mpi_barrier
          if(iflag_debug.gt.0) write(*,*) 'initilal for temperature'
          call set_ini_ref_temp_benchmark                               &
     &       (sph_rj, sph_params%nlayer_ICB, sph_params%nlayer_CMB,     &
     &        rj_fld%d_fld(1,ipol%base%i_temp))
          call set_initial_temp_sph(isig, sph_rj,                       &
     &        sph_params%radius_ICB, sph_params%radius_CMB,             &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        rj_fld%d_fld(1,ipol%base%i_temp))
        end if
        if(ipol%base%i_light .gt. 0) then
          call calypso_mpi_barrier
          if(iflag_debug.gt.0) write(*,*) 'initilal for composition'
          call set_ini_ref_temp_benchmark                               &
     &       (sph_rj, sph_params%nlayer_ICB, sph_params%nlayer_CMB,     &
     &        rj_fld%d_fld(1,ipol%base%i_light))
!
          call set_initial_temp_sph(isig, sph_rj,                       &
     &        sph_params%radius_ICB, sph_params%radius_CMB,             &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        rj_fld%d_fld(1,ipol%base%i_light))
        end if
!
        if(iflag_restart .eq. i_rst_dbench1) then
          if(ipol%base%i_magne .gt. 0) then
            call calypso_mpi_barrier
            if(iflag_debug.gt.0) write(*,*) 'initilal for magnetic'
            call initial_b_dynamobench_1(sph_rj, ipol,                  &
     &          sph_params%radius_ICB, sph_params%radius_CMB,           &
     &          sph_params%nlayer_ICB, sph_params%nlayer_CMB,           &
     &          rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
          end if
        else if(iflag_restart .eq. i_rst_dbench2) then
          if(ipol%base%i_magne .gt. 0) then
            call calypso_mpi_barrier
            if(iflag_debug.gt.0) write(*,*) 'initilal for magnetic'
            call initial_b_dynamobench_2(sph_rj, ipol,                  &
     &          sph_params%nlayer_CMB, sph_params%radius_CMB,           &
     &          rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
          end if
        else if(iflag_restart .eq. i_rst_dbench_qcv) then
          if(ipol%base%i_magne .gt. 0) then
           call initial_b_dynamobench_qcv(sph_rj, ipol,                 &
     &         sph_params%radius_ICB, sph_params%radius_CMB,            &
     &         sph_params%nlayer_ICB, sph_params%nlayer_CMB,            &
     &         rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
          end if
        end if
!
      end subroutine sph_initial_data_4_benchmarks
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_data_w_seed_B                              &
     &         (refs, sph_params, sph_rj, MHD_prop, sph_MHD_bc,         &
     &          ipol, rj_fld)
!
      use t_MHD_step_parameter
      use t_reference_scalar_param
      use t_spheric_parameter
      use t_phys_data
!
      use set_initial_sph_scalars
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(radial_reference_field), intent(in) :: refs
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
        if(ipol%base%i_temp .gt. 0)  then
          call set_ini_reference_temp_sph(sph_rj, MHD_prop%ref_param_T, &
     &        refs%ref_field%d_fld(1,refs%iref_base%i_temp),            &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        rj_fld%d_fld(1,ipol%base%i_temp))
          call set_noize_scalar_sph(sph_rj,                             &
     &        sph_params%radius_ICB, sph_params%radius_CMB,             &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        rj_fld%d_fld(1,ipol%base%i_temp))
        end if
        if(ipol%base%i_light .gt. 0) then
          call set_ini_reference_temp_sph(sph_rj, MHD_prop%ref_param_C, &
     &        refs%ref_field%d_fld(1,refs%iref_base%i_light),           &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        rj_fld%d_fld(1,ipol%base%i_light))
          call set_noize_scalar_sph(sph_rj,                             &
     &        sph_params%radius_ICB, sph_params%radius_CMB,             &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        rj_fld%d_fld(1,ipol%base%i_light))
        end if
        if(ipol%base%i_magne .gt. 0) then
          call set_initial_magne_sph                                    &
     &       (sph_rj, sph_MHD_bc%sph_bc_B, ipol,                        &
     &        sph_params%radius_ICB, sph_params%radius_CMB,             &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
          call reduce_initial_magne_sph                                 &
     &       (ipol, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
      end subroutine sph_initial_data_w_seed_B
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_field_4_licv(refs, sph_params, sph_rj,     &
     &          MHD_prop, ipol, rj_fld)
!
      use t_MHD_step_parameter
      use t_reference_scalar_param
      use t_spheric_parameter
      use t_phys_data
!
      use set_initial_sph_scalars
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(radial_reference_field), intent(in) :: refs
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call set_ini_reference_temp_sph(sph_rj, MHD_prop%ref_param_T,     &
     &    refs%ref_field%d_fld(1,refs%iref_base%i_temp),                &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    rj_fld%d_fld(1,ipol%base%i_temp))
      call set_noize_scalar_sph(sph_rj,                                 &
     &    sph_params%radius_ICB, sph_params%radius_CMB,                 &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    rj_fld%d_fld(1,ipol%base%i_temp))
!
      end subroutine sph_initial_field_4_licv
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
      subroutine set_initial_magne_sph(sph_rj, sph_bc_B, ipol,          &
     &          r_ICB, r_CMB, nlayer_ICB, nlayer_CMB,                   &
     &          n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
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
        d_rj(is,ipol%base%i_magne  ) = zero
        d_rj(is,ipol%base%i_magne+1) = zero
        d_rj(is,ipol%base%i_magne+2) = zero
        d_rj(is,ipol%base%i_current  ) = zero
        d_rj(is,ipol%base%i_current+1) = zero
        d_rj(is,ipol%base%i_current+2) = zero
      end do
!$omp end parallel do
!
      if (sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
!
!         Poloidal magnetic field
        if (js .gt. 0) then
          do k = nlayer_ICB, nlayer_CMB
            is = js + (k-1) * sph_rj%nidx_rj(2)
            rr = sph_rj%radius_1d_rj_r(k)
!
            d_rj(is,ipol%base%i_magne  ) =  (five / eight)              &
     &        * (-three * rr**3 + four * r_CMB * rr**2 - r_ICB**4/rr)
            d_rj(is,ipol%base%i_magne+1) = (five / eight)               &
     &        * (-dnine * rr**2 + eight * r_CMB * rr + r_ICB**4/rr**2)
            d_rj(is,ipol%base%i_current+2) = (five*three / two) * rr
          end do
        end if
!
!         Toroidal magnetic field
        if (jt .gt. 0) then
          do k = nlayer_ICB, nlayer_CMB
            it = jt + (k-1) * sph_rj%nidx_rj(2)
            rr = sph_rj%radius_1d_rj_r(k)
            d_rj(it,ipol%base%i_magne+2)                                &
     &            =  (ten/three) * rr * sin(pi*(rr-r_ICB))
            d_rj(it,ipol%base%i_current  )                              &
     &            = d_rj(it,ipol%base%i_magne+2)
            d_rj(it,ipol%base%i_current+1)                              &
     &            = (ten / three) * (sin(pi*(rr-r_ICB))                 &
     &                          + pi * rr * cos(pi*(rr-r_ICB)) )
          end do
        end if
!
      else
!
!         Poloidal magnetic field
        if (js .gt. 0) then
          do k = 1, nlayer_CMB
            is = js + (k-1) * sph_rj%nidx_rj(2)
            rr = sph_rj%radius_1d_rj_r(k)
            d_rj(is,ipol%base%i_magne  ) =  (five / two) * rr**2        &
     &                       * (four*r_CMB - three*rr) / (r_CMB+three)
            d_rj(is,ipol%base%i_magne+1) = (five / two) * rr            &
     &                       * (eight*r_CMB - dnine*rr) / (r_CMB+three)
            d_rj(is,ipol%base%i_current+2)                              &
     &         =  five*six * rr / (three +r_CMB)
          end do
        end if
!
!         Toroidal magnetic field
        if (jt .gt. 0) then
          do k = 1, nlayer_CMB
            it = jt + (k-1) * sph_rj%nidx_rj(2)
            rr = sph_rj%radius_1d_rj_r(k)
!
            d_rj(it,ipol%base%i_magne+2)                                &
     &          =  (ten / three) * rr * sin(pi*rr/r_CMB)
            d_rj(it,ipol%base%i_current  )                              &
     &          = d_rj(it,ipol%base%i_magne+2)
            d_rj(it,ipol%base%i_current+1)                              &
     &          = (ten / three) * (sin(pi*rr/r_CMB)                     &
     &                          + (pi/r_CMB) * rr * cos(pi*rr/r_CMB) )
          end do
        end if
!
      end if
!
      end subroutine set_initial_magne_sph
!
!-----------------------------------------------------------------------
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
        d_rj(is,ipol%base%i_magne  )                                    &
     &         = 1.0d-3 * d_rj(is,ipol%base%i_magne  )
        d_rj(is,ipol%base%i_magne+1)                                    &
     &         = 1.0d-3 * d_rj(is,ipol%base%i_magne+1)
        d_rj(is,ipol%base%i_magne+2)                                    &
     &         = 1.0d-3 * d_rj(is,ipol%base%i_magne+2)
        d_rj(is,ipol%base%i_current  )                                  &
     &         = 1.0d-3 * d_rj(is,ipol%base%i_current  )
        d_rj(is,ipol%base%i_current+1)                                  &
     &         = 1.0d-3 * d_rj(is,ipol%base%i_current+1)
        d_rj(is,ipol%base%i_current+2)                                  &
     &         = 1.0d-3 * d_rj(is,ipol%base%i_current+2)
      end do
!$omp end parallel do
!
      end subroutine reduce_initial_magne_sph
!
!-----------------------------------------------------------------------
!
      end module set_initial_sph_dynamo
