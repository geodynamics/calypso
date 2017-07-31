!>@file   init_sphrical_transform_MHD.f90
!!@brief  module init_sphrical_transform_MHD
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine init_sph_transform_MHD                               &
!!     &         (MHD_prop, sph_MHD_bc, ipol, idpdr, itor, iphys,       &
!!     &          sph, comms_sph, omega_sph, trans_p, WK, rj_fld)
!!      subroutine sel_sph_transform_MHD                                &
!!     &         (ipol, fl_prop, sph_bc_U, sph, comms_sph, omega_sph,   &
!!     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,&
!!     &          trans_p, trns_MHD, WK_sph, MHD_mul_FFTW,              &
!!     &          gt_cor, cor_rlm, rj_fld)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_address), intent(in) :: ipol, idpdr, itor
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(gaunt_coriolis_rlm), intent(inout) :: gt_cor
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
      module init_sphrical_transform_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_control_parameter
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_addresses_sph_transform
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_multi_FFTW
      use t_legendre_trans_select
      use t_sph_transforms
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_boundary_data_sph_MHD
!
      implicit  none
!
      integer(kind = kint), parameter :: num_test =  6
      integer(kind = kint), parameter :: list_test(num_test)            &
     &        = (/iflag_leg_krloop_inner,                               &
     &            iflag_leg_sym_spin_loop,                              &
     &            iflag_leg_sym_matmul,                                 &
     &            iflag_leg_sym_dgemm,                                  &
     &            iflag_leg_sym_matmul_big,                             &
     &            iflag_leg_sym_dgemm_big/)
!
      private :: num_test, list_test
      private :: select_legendre_transform
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_transform_MHD                                 &
     &         (MHD_prop, sph_MHD_bc, ipol, idpdr, itor, iphys,         &
     &          sph, comms_sph, omega_sph, trans_p, WK, rj_fld)
!
      use set_address_sph_trans_MHD
      use set_address_sph_trans_snap
      use set_address_sph_trans_tmp
      use pole_sph_transform
      use MHD_FFT_selector
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(phys_address), intent(in) :: iphys
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: rj_fld
!
!>      total number of components for spherical harmonics transform
      integer(kind = kint), save :: ncomp_max_trans = 0
!>      total number of vectors for spherical harmonics transform
      integer(kind = kint), save :: nvector_max_trans = 0
!>      total number of svalars for spherical harmonics transform
      integer(kind = kint), save :: nscalar_max_trans = 0
!
!
      call init_pole_transform(sph%sph_rtp)
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_trans_sph_MHD'
      call set_addresses_trans_sph_MHD(MHD_prop, ipol, WK%trns_MHD,     &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_snapshot_trans(ipol, iphys, WK%trns_snap,      &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_temporal_trans(ipol, iphys, WK%trns_tmp,       &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      if(iflag_debug .ge. iflag_routine_msg) then
        call check_address_trans_sph_MHD(ipol, idpdr, itor, iphys,      &
     &      WK%trns_MHD, ncomp_max_trans)
        call check_address_trans_sph_snap(ipol, idpdr, itor, iphys,     &
     &      WK%trns_snap)
        call check_address_trans_sph_tmp(ipol, idpdr, itor, iphys,      &
     &      WK%trns_tmp)
      end if
!
      call alloc_sph_trans_address(sph%sph_rtp, WK)
!
      call sel_sph_transform_MHD(izero, ipol, MHD_prop%fl_prop,         &
     &    sph_MHD_bc%sph_bc_U, sph, comms_sph, omega_sph,               &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    WK%trns_MHD, WK%trns_SGS, WK%WK_sph,                          &
     &    WK%MHD_mul_FFTW, WK%SGS_mul_FFTW, trans_p,                    &
     &    WK%gt_cor, WK%cor_rlm, rj_fld)
!
      end subroutine init_sph_transform_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_transform_MHD(iflag_SGS, ipol,                 &
     &          fl_prop, sph_bc_U, sph, comms_sph, omega_sph,           &
     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,  &
     &          trns_MHD, trns_SGS, WK_sph, MHD_mul_FFTW, SGS_mul_FFTW, &
     &          trans_p, gt_cor, cor_rlm, rj_fld)
!
      use init_sph_trans
      use init_FFT_4_MHD
      use const_wz_coriolis_rtp
      use pole_sph_transform
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: iflag_SGS
      type(phys_address), intent(in) :: ipol
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
      integer(kind = kint), intent(in) :: nvector_max_trans
      integer(kind = kint), intent(in) :: nscalar_max_trans
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(address_4_sph_trans), intent(inout) :: trns_MHD, trns_SGS
      type(gaunt_coriolis_rlm), intent(inout) :: gt_cor
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
      type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_legendre_trans'
      call initialize_legendre_trans(ncomp_max_trans,                   &
     &    sph, comms_sph, trans_p%leg, trans_p%idx_trns)
      call init_fourier_transform_4_MHD(iflag_SGS, ncomp_max_trans,     &
     &    sph%sph_rtp, comms_sph%comm_rtp, trns_MHD, trns_SGS,          &
     &    WK_sph%WK_FFTs, MHD_mul_FFTW, SGS_mul_FFTW)
!
      if (iflag_debug.eq.1) write(*,*) 'set_colatitude_rtp'
      call set_colatitude_rtp(sph%sph_rtp, sph%sph_rj, trans_p%leg)
      if (iflag_debug.eq.1) write(*,*) 'init_sum_coriolis_rlm'
      call init_sum_coriolis_rlm(sph%sph_params%l_truncation,           &
     &    sph%sph_rlm, sph_bc_U, trans_p%leg, gt_cor, cor_rlm)
!
      if (iflag_debug.eq.1) write(*,*) 'select_legendre_transform'
      call select_legendre_transform(sph, comms_sph,                    &
     &    fl_prop, sph_bc_U, omega_sph, trans_p, gt_cor, ipol,          &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    rj_fld, trns_MHD, WK_sph, MHD_mul_FFTW, cor_rlm)
!
      call sel_init_legendre_trans                                      &
     &   (ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    sph%sph_rtm, sph%sph_rlm, trans_p%leg, trans_p%idx_trns,      &
     &    WK_sph%WK_leg)
!
      if(my_rank .ne. 0) return
      call display_selected_legendre_mode(WK_sph%WK_leg%id_legendre)
!
      end subroutine sel_sph_transform_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine select_legendre_transform(sph, comms_sph,              &
     &          fl_prop, sph_bc_U, omega_sph, trans_p, gt_cor, ipol,    &
     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,  &
     &          rj_fld, trns_MHD, WK_sph, MHD_mul_FFTW, cor_rlm)
!
      use sph_transforms_4_MHD
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
      type(phys_address), intent(in) :: ipol
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
      integer(kind = kint), intent(in) :: nvector_max_trans
      integer(kind = kint), intent(in) :: nscalar_max_trans
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(phys_data), intent(inout) :: rj_fld
!
      real(kind = kreal) :: starttime, etime_shortest
      real(kind = kreal) :: endtime(ntype_Leg_trans_loop)
      real(kind = kreal) :: etime_trans(ntype_Leg_trans_loop)
      real(kind = kreal) :: etime_max(ntype_Leg_trans_loop)
!
      integer(kind = kint) :: id, iloop_type
!
!
      if(WK_sph%WK_leg%id_legendre .ne. iflag_leg_undefined) return
!
      endtime(1:ntype_Leg_trans_loop) =     zero
      etime_trans(1:ntype_Leg_trans_loop) = zero
      etime_max(1:ntype_Leg_trans_loop) =   zero
      do iloop_type = 1, num_test
        WK_sph%WK_leg%id_legendre = list_test(iloop_type)
        if(my_rank .eq. 0) write(*,*)                                   &
     &            'Test SPH transform for ', WK_sph%WK_leg%id_legendre
        call sel_init_legendre_trans                                    &
     &     (ncomp_max_trans, nvector_max_trans, nscalar_max_trans,      &
     &      sph%sph_rtm, sph%sph_rlm, trans_p%leg, trans_p%idx_trns,    &
     &      WK_sph%WK_leg)
!
        starttime = MPI_WTIME()
        call sph_back_trans_4_MHD(sph, comms_sph, fl_prop, sph_bc_U,    &
     &      omega_sph, trans_p, gt_cor, ipol, rj_fld, trns_MHD,         &
     &      WK_sph, MHD_mul_FFTW, cor_rlm)
        call sph_forward_trans_4_MHD(sph, comms_sph, fl_prop, trans_p,  &
     &      cor_rlm, ipol, trns_MHD, WK_sph, MHD_mul_FFTW, rj_fld)
        endtime(WK_sph%WK_leg%id_legendre) = MPI_WTIME() - starttime
!
        call sel_finalize_legendre_trans(WK_sph%WK_leg)
      end do
!
      call MPI_allREDUCE (endtime, etime_trans, ntype_Leg_trans_loop,   &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE (endtime, etime_max, ntype_Leg_trans_loop,     &
     &    CALYPSO_REAL, MPI_MAX, CALYPSO_COMM, ierr_MPI)
      etime_trans(1:ntype_Leg_trans_loop)                               &
     &      = etime_trans(1:ntype_Leg_trans_loop) / dble(nprocs)
!
      etime_shortest =  1.0d30
      do iloop_type = 1, num_test
        id = list_test(iloop_type)
        if(etime_max(id) .lt. etime_shortest) then
          WK_sph%WK_leg%id_legendre = id
          etime_shortest =       etime_max(id)
        end if
      end do
!
      if(my_rank .gt. 0) return
        write(*,'(a)') 'Loop ID: type, maximum time, average time'
        if(etime_trans(iflag_leg_orginal_loop) .gt. zero)               &
     &  write(*,'(a,1p2e16.6)') ' 1: elapsed by original loop:      ',  &
     &            etime_max(iflag_leg_orginal_loop),  &
     &            etime_trans(iflag_leg_orginal_loop)
        if(etime_trans(iflag_leg_blocked) .gt. zero)                    &
     &  write(*,'(a,1p2e16.6)') ' 2: elapsed by blocked loop:      ',   &
     &            etime_max(iflag_leg_blocked),                         &
     &            etime_trans(iflag_leg_blocked)
        if(etime_trans(iflag_leg_krloop_inner) .gt. zero)               &
     &  write(*,'(a,1p2e16.6)') ' 3: elapsed by inner radius loop:  ',  &
     &            etime_max(iflag_leg_krloop_inner),                    &
     &            etime_trans(iflag_leg_krloop_inner)
        if(etime_trans(iflag_leg_krloop_outer) .gt. zero)               &
     &  write(*,'(a,1p2e16.6)') ' 4: elapsed by outer radius loop:  ',  &
     &            etime_max(iflag_leg_krloop_outer),                    &
     &            etime_trans(iflag_leg_krloop_outer)
        if(etime_trans(iflag_leg_symmetry) .gt. zero)                   &
     &  write(*,'(a,1p2e16.6)')                                         &
     &          ' 5: elapsed by original loop with symmetric: ',        &
     &            etime_max(iflag_leg_symmetry),                        &
     &            etime_trans(iflag_leg_symmetry)
        if(etime_trans(iflag_leg_sym_spin_loop) .gt. zero)              &
     &  write(*,'(a,1p2e16.6)') ' 6: elapsed by sym. outer radius: ',   &
     &            etime_max(iflag_leg_sym_spin_loop),                   &
     &            etime_trans(iflag_leg_sym_spin_loop)
        if(etime_trans(iflag_leg_matmul) .gt. zero)                     &
     &  write(*,'(a,1p2e16.6)') ' 7: elapsed by matmul: ',              &
     &            etime_max(iflag_leg_matmul),                          &
     &            etime_trans(iflag_leg_matmul)
        if(etime_trans(iflag_leg_dgemm) .gt. zero)                      &
     &  write(*,'(a,1p2e16.6)') ' 8: elapsed by BLAS: ',                &
     &            etime_max(iflag_leg_dgemm),                           &
     &            etime_trans(iflag_leg_dgemm)
        if(etime_trans(iflag_leg_matprod) .gt. zero)                    &
     &  write(*,'(a,1p2e16.6)') ' 9: elapsed by matrix product: ',      &
     &            etime_max(iflag_leg_matprod),                         &
     &            etime_trans(iflag_leg_matprod)
        if(etime_trans(iflag_leg_sym_matmul) .gt. zero)                 &
     &  write(*,'(a,1p2e16.6)')                                         &
     &          '10: elapsed by matmul with symmetric: ',               &
     &            etime_max(iflag_leg_sym_matmul),                      &
     &            etime_trans(iflag_leg_sym_matmul)
        if(etime_trans(iflag_leg_sym_dgemm) .gt. zero)                  &
     &  write(*,'(a,1p2e16.6)') '11: elapsed by BLAS with symmetric: ', &
     &            etime_max(iflag_leg_sym_dgemm),                       &
     &            etime_trans(iflag_leg_sym_dgemm)
        if(etime_trans(iflag_leg_sym_matprod) .gt. zero)                &
     &  write(*,'(a,1p2e16.6)')                                         &
     &          '12: elapsed by matrix prod. with symm.: ',             &
     &            etime_max(iflag_leg_sym_matprod),                     &
     &            etime_trans(iflag_leg_sym_matprod)
        if(etime_trans(iflag_leg_sym_matmul_big) .gt. zero)             &
     &  write(*,'(a,1p2e16.6)')                                         &
     &          '13: elapsed by big matmul with symmetric: ',           &
     &            etime_max(iflag_leg_sym_matmul_big),                  &
     &            etime_trans(iflag_leg_sym_matmul_big)
        if(etime_trans(iflag_leg_sym_dgemm_big) .gt. zero)              &
     &  write(*,'(a,1p2e16.6)')                                         &
     &          '14: elapsed by big BLAS with symmetric: ',             &
     &            etime_max(iflag_leg_sym_dgemm_big),                   &
     &            etime_trans(iflag_leg_sym_dgemm_big)
        if(etime_trans(iflag_leg_sym_matprod_big) .gt. zero)            &
     &  write(*,'(a,1p2e16.6)')                                         &
     &          '15: elapsed by big matrix prod. with symm.: ',         &
     &            etime_max(iflag_leg_sym_matprod_big),                 &
     &            etime_trans(iflag_leg_sym_matprod_big)
!
      end subroutine select_legendre_transform
!
!-----------------------------------------------------------------------
!
      end module init_sphrical_transform_MHD
