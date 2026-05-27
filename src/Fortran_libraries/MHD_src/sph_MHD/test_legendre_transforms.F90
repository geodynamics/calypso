!>@file   test_legendre_transforms.F90
!!@brief  module test_legendre_transforms
!!
!!@date  Programmed by H.Matsui in Oct., 2009
!!@n     Modified by H.Matsui in March, 2013
!!@n     Modified by H.Matsui in Sep., 2020
!!@n     Modified by H.Matsui in May, 2026
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine test_legendre_trans(ntest, list, sph, comms_sph,     &
!!     &          fl_prop, sph_bc_U, omega_sph, trans_p, gt_cor,        &
!!     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,&
!!     &          rj_fld, trns_MHD, WK_leg, WK_FFTs_MHD,                &
!!     &          cor_rlm, SR_sig, SR_r, etime_shortest, id_shortest)
!!        integer(kind = kint), intent(in) :: ntest
!!        integer(kind = kint), intent(in) :: list(ntest)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!!        integer(kind = kint), intent(in) :: ncomp_max_trans
!!        integer(kind = kint), intent(in) :: nvector_max_trans
!!        integer(kind = kint), intent(in) :: nscalar_max_trans
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(legendre_trns_works), intent(inout) :: WK_leg
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs_MHD
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        real(kind = kreal), intent(inout) :: etime_shortest
!!        integer(kind = kint), intent(inout) :: id_shortest
!!@endverbatim
!!
      module test_legendre_transforms
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_sph_trans_comm_tbl
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_legendre_trans_select
      use t_sph_FFT_selector
      use t_const_wz_coriolis_rtp
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_boundary_data_sph_MHD
      use t_solver_SR
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine test_legendre_trans(ntest, list, sph, comms_sph,       &
     &          fl_prop, sph_bc_U, omega_sph, trans_p, gt_cor,          &
     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,  &
     &          rj_fld, trns_MHD, WK_leg, WK_FFTs_MHD,                  &
     &          cor_rlm, SR_sig, SR_r, etime_shortest, id_shortest)
!
      use calypso_mpi_real
      use sph_transforms_4_MHD
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ntest
      integer(kind = kint), intent(in) :: list(ntest)
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
      integer(kind = kint), intent(in) :: nvector_max_trans
      integer(kind = kint), intent(in) :: nscalar_max_trans
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs_MHD
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      real(kind = kreal), intent(inout) :: etime_shortest
      integer(kind = kint), intent(inout) :: id_shortest
!
      real(kind = kreal) :: starttime
      real(kind = kreal) :: endtime(ntest)
      real(kind = kreal) :: etime_trans(ntest)
      real(kind = kreal) :: etime_max(ntest)
!
      integer(kind = kint) :: id
!
!
      endtime(1:ntest) = zero
      do id = 1, ntest
        WK_leg%id_legendre = list(id)
        if(my_rank .eq. 0) write(*,*) 'Test SPH transform for ',        &
     &             trim(chosen_legendre_name(WK_leg%id_legendre))
        call sel_init_legendre_trans                                    &
     &     (ncomp_max_trans, nvector_max_trans, nscalar_max_trans,      &
     &      sph%sph_params, sph%sph_rtm, sph%sph_rlm,                   &
     &      trans_p%leg, trans_p%idx_trns, WK_leg)
!
        starttime = MPI_WTIME()
        call sph_back_trans_4_MHD(sph, comms_sph, fl_prop, sph_bc_U,    &
     &      omega_sph, trans_p, gt_cor, rj_fld,                         &
     &      trns_MHD%b_trns, trns_MHD%f_trns, trns_MHD%backward,        &
     &      WK_leg, WK_FFTs_MHD, cor_rlm, SR_sig, SR_r)
        call sph_forward_trans_4_MHD(sph, comms_sph, fl_prop,           &
     &      trans_p, cor_rlm, trns_MHD%f_trns, trns_MHD%forward,        &
     &      WK_leg, WK_FFTs_MHD, rj_fld, SR_sig, SR_r)
        endtime(id) = MPI_WTIME() - starttime
!
        call sel_finalize_legendre_trans(WK_leg)
      end do
!
      etime_trans(1:ntest) = zero
      etime_max(1:ntest) =   zero
      call calypso_mpi_allreduce_real(endtime, etime_trans,             &
     &    cast_long(ntest), MPI_SUM)
      call calypso_mpi_allreduce_real(endtime, etime_max,               &
     &    cast_long(ntest), MPI_MAX)
      etime_trans(1:ntest) = etime_trans(1:ntest) / dble(nprocs)
!
      do id = 1, ntest
        if(etime_max(id) .lt. etime_shortest) then
          id_shortest =    list(id)
          etime_shortest = etime_max(id)
        end if
      end do
!
      if(my_rank .gt. 0) return
      write(*,'(a)') 'Loop ID: type, maximum time, average time'
      do id = 1, ntest
        call write_elapsed_4_legendre                                   &
     &     (list(id), etime_max(id), etime_trans(id))
      end do
!
      end subroutine test_legendre_trans
!
!-----------------------------------------------------------------------
!
      end module test_legendre_transforms
