!>@file   test_legendre_transforms.F90
!!@brief  module test_legendre_transforms
!!
!!@date  Programmed by H.Matsui in Oct., 2009
!!@n     Modified by H.Matsui in March, 2013
!!@n     Modified by H.Matsui in Sep., 2020
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine s_test_legendre_transforms(sph, comms_sph,           &
!!     &          fl_prop, sph_bc_U, omega_sph, trans_p, gt_cor,        &
!!     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,&
!!     &          rj_fld, trns_MHD, WK_leg, WK_FFTs_MHD, cor_rlm)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(legendre_trns_works), intent(inout) :: WK_leg
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs_MHD
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!        type(phys_data), intent(inout) :: rj_fld
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
!
      implicit  none
!
#ifdef BLAS
      integer(kind = kint), parameter :: num_test =   6
#else
      integer(kind = kint), parameter :: num_test =   5
#endif
!
      integer(kind = kint), parameter :: list_test(num_test)            &
     &        = (/iflag_leg_symmetry,                                   &
     &            iflag_leg_sym_spin_loop,                              &
     &            iflag_leg_sym_matmul,                                 &
     &            iflag_leg_sym_matmul_big,                             &
#ifdef BLAS
     &            iflag_leg_sym_dgemm_big,                              &
#endif
     &            iflag_on_the_fly_matprod/)
!
      private :: num_test, list_test
      private :: write_time_4_transform
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_test_legendre_transforms(sph, comms_sph,             &
     &          fl_prop, sph_bc_U, omega_sph, trans_p, gt_cor,          &
     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,  &
     &          rj_fld, trns_MHD, WK_leg, WK_FFTs_MHD, cor_rlm)
!
      use calypso_mpi_real
      use sph_transforms_4_MHD
      use transfer_to_long_integers
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
!
      real(kind = kreal) :: starttime, etime_shortest
      real(kind = kreal) :: endtime(maxindex_Leg_trans_loop)
      real(kind = kreal) :: etime_trans(maxindex_Leg_trans_loop)
      real(kind = kreal) :: etime_max(maxindex_Leg_trans_loop)
!
      integer(kind = kint) :: id, iloop_type
!
!
      if(WK_leg%id_legendre .ne. iflag_leg_undefined) return
!
      endtime(1:maxindex_Leg_trans_loop) =     zero
      etime_trans(1:maxindex_Leg_trans_loop) = zero
      etime_max(1:maxindex_Leg_trans_loop) =   zero
      do iloop_type = 1, num_test
        WK_leg%id_legendre = list_test(iloop_type)
        if(my_rank .eq. 0) write(*,*)                                   &
     &            'Test SPH transform for ', WK_leg%id_legendre
        call sel_init_legendre_trans                                    &
     &     (ncomp_max_trans, nvector_max_trans, nscalar_max_trans,      &
     &      sph%sph_params, sph%sph_rtm, sph%sph_rlm,                   &
     &      trans_p%leg, trans_p%idx_trns, WK_leg)
!
        starttime = MPI_WTIME()
        call sph_back_trans_4_MHD(sph, comms_sph, fl_prop, sph_bc_U,    &
     &      omega_sph, trans_p, gt_cor, rj_fld, trns_MHD%b_trns,        &
     &      trns_MHD%backward, WK_leg, WK_FFTs_MHD, cor_rlm)
        call sph_forward_trans_4_MHD(sph, comms_sph, fl_prop,           &
     &      trans_p, cor_rlm, trns_MHD%f_trns, trns_MHD%forward,        &
     &      WK_leg, WK_FFTs_MHD, rj_fld)
        endtime(WK_leg%id_legendre) = MPI_WTIME() - starttime
!
        call sel_finalize_legendre_trans(WK_leg)
      end do
!
      call calypso_mpi_allreduce_real(endtime, etime_trans,             &
     &    cast_long(maxindex_Leg_trans_loop), MPI_SUM)
      call calypso_mpi_allreduce_real(endtime, etime_max,               &
     &    cast_long(maxindex_Leg_trans_loop), MPI_SUM)
      etime_trans(1:maxindex_Leg_trans_loop)                            &
     &      = etime_trans(1:maxindex_Leg_trans_loop) / dble(nprocs)
!
      etime_shortest =  1.0d30
      do iloop_type = 1, num_test
        id = list_test(iloop_type)
        if(etime_max(id) .lt. etime_shortest) then
          WK_leg%id_legendre = id
          etime_shortest =       etime_max(id)
        end if
      end do
!
      if(my_rank .gt. 0) return
      call write_time_4_transform(etime_trans, etime_max)
!
      end subroutine s_test_legendre_transforms
!
!-----------------------------------------------------------------------
!
      subroutine write_time_4_transform(etime_trans, etime_max)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: etime_trans(maxindex_Leg_trans_loop)
      real(kind = kreal), intent(in)                                    &
     &                   :: etime_max(maxindex_Leg_trans_loop)
!
!
        write(*,'(a)') 'Loop ID: type, maximum time, average time'
        if(etime_trans(iflag_leg_symmetry) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_symmetry,                         &
     &          ': elapsed by original loop with symmetric'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_org_loop),  ':  ',      &
     &            etime_max(iflag_leg_symmetry),                        &
     &            etime_trans(iflag_leg_symmetry)
        end if
!
        if(etime_trans(iflag_leg_sym_spin_loop) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_spin_loop,                    &
     &            ': elapsed by sym. outer radius'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_spin_loop),  ':  ',     &
     &            etime_max(iflag_leg_sym_spin_loop),                   &
     &            etime_trans(iflag_leg_sym_spin_loop)
        end if
!
        if(etime_trans(iflag_leg_sym_matmul) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_matmul,                       &
     &          ': elapsed by using matmul with radial SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_matmul),  ':  ',        &
     &            etime_max(iflag_leg_sym_matmul),                      &
     &            etime_trans(iflag_leg_sym_matmul)
        end if
!
        if(etime_trans(iflag_leg_sym_matmul_big) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_matmul_big,                   &
     &          ': elapsed by big matmul with radial SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_matmul_big),  ':  ',    &
     &            etime_max(iflag_leg_sym_matmul_big),                  &
     &            etime_trans(iflag_leg_sym_matmul_big)
        end if
!
        if(etime_trans(iflag_leg_sym_dgemm_big) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_dgemm_big,                    &
     &          ': elapsed by big BLAS with radial SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_dgemm_big),  ':  ',     &
     &            etime_max(iflag_leg_sym_dgemm_big),                   &
     &            etime_trans(iflag_leg_sym_dgemm_big)
        end if
!
        if(etime_trans(iflag_leg_sym_mat_tj) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_mat_tj,                       &
     &          ': elapsed by matmul for Ptj with theta SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_mat_tj),  ':  ',        &
     &            etime_max(iflag_leg_sym_mat_tj),                      &
     &            etime_trans(iflag_leg_sym_mat_tj)
        end if
!
        if(etime_trans(iflag_leg_sym_dgemm_tj) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_dgemm_tj,                     &
     &          ': elapsed by BLAS for Ptj with theta SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_dgemm_tj),  ':  ',          &
     &            etime_max(iflag_leg_sym_dgemm_tj),                    &
     &            etime_trans(iflag_leg_sym_dgemm_tj)
        end if
!
!
        if(etime_trans(iflag_on_the_fly_matmul) .gt. zero) then
          write(*,'(i3,a)') iflag_on_the_fly_matmul,                    &
     &          ': elapsed by matmul with on-the-fly Plm'
          write(*,'(2a,1p2e16.6)') trim(leg_dgemm_tj),  ':  ',          &
     &            etime_max(iflag_on_the_fly_matmul),                   &
     &            etime_trans(iflag_on_the_fly_matmul)
        end if
!
        if(etime_trans(iflag_on_the_fly_dgemm) .gt. zero) then
          write(*,'(i3,a)') iflag_on_the_fly_dgemm,                     &
     &          ': elapsed by BLAS with on-the-fly Plm'
          write(*,'(2a,1p2e16.6)') trim(leg_dgemm_tj),  ':  ',          &
     &            etime_max(iflag_on_the_fly_dgemm),                    &
     &            etime_trans(iflag_on_the_fly_dgemm)
        end if
!
        if(etime_trans(iflag_on_the_fly_matprod) .gt. zero) then
          write(*,'(i3,a)') iflag_on_the_fly_matprod,                   &
     &          ': elapsed by simple loop with on-the-fly Plm'
          write(*,'(2a,1p2e16.6)') trim(leg_dgemm_tj),  ':  ',          &
     &            etime_max(iflag_on_the_fly_matprod),                  &
     &            etime_trans(iflag_on_the_fly_matprod)
        end if
!
      end subroutine write_time_4_transform
!
!-----------------------------------------------------------------------
!
      end module test_legendre_transforms
