!>@file   sph_transforms_4_MHD.f90
!!@brief  module sph_transforms_4_MHD
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine init_sph_transform_MHD
!!
!!      subroutine sph_back_trans_4_MHD
!!      subroutine sph_forward_trans_4_MHD
!!
!!      subroutine sph_transform_4_licv
!!
!!      subroutine sph_back_trans_snapshot_MHD
!!      subroutine sph_forward_trans_snapshot_MHD
!!@endverbatim
!!
      module sph_transforms_4_MHD
!
      use m_precision
      use m_constants
!
      implicit  none
!
      private :: select_legendre_transform
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_transform_MHD
!
      use calypso_mpi
      use m_machine_parameter
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
      use m_work_4_sph_trans
      use init_sph_trans
      use const_wz_coriolis_rtp
      use const_coriolis_sph_rlm
      use legendre_transform_select
!
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_trans_sph_MHD'
      call set_addresses_trans_sph_MHD
      call set_addresses_snapshot_trans
!
      if(iflag_debug .ge. iflag_routine_msg) then
        call check_add_trans_sph_MHD
        call check_addresses_snapshot_trans
      end if
!
      call initialize_sph_trans
!
      call set_colatitude_rtp
      call init_sum_coriolis_rlm
!
      if(id_legendre_transfer .eq. iflag_leg_undefined) then
        call select_legendre_transform
      end if
!
      call sel_alloc_legendre_trans(nb_sph_trans)
!
      end subroutine init_sph_transform_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_4_MHD
!
      use m_addresses_trans_sph_MHD
      use copy_MHD_4_sph_trans
!
      use sph_trans_w_coriols
      use sph_trans_scalar
!
!
      if(nvector_rj_2_rtp .gt. 0) then
        call copy_mhd_vec_spec_to_trans
        call sph_b_trans_w_coriolis(nvector_rj_2_rtp)
        call copy_mhd_vec_fld_from_trans
      end if
!
      if(nscalar_rj_2_rtp .gt. 0) then
        call copy_mhd_scl_spec_to_trans
        call sph_b_trans_scalar(nscalar_rj_2_rtp)
        call copy_mhd_scl_fld_from_trans
      end if
!
      end subroutine sph_back_trans_4_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_4_MHD
!
      use m_addresses_trans_sph_MHD
      use copy_MHD_4_sph_trans
!
      use sph_trans_w_coriols
      use sph_trans_scalar
!
!
      if(nvector_rtp_2_rj .gt. 0) then
        call copy_mhd_vec_fld_to_trans
        call sph_f_trans_w_coriolis(nvector_rtp_2_rj)
        call copy_mhd_vec_spec_from_trans
      end if
!
      end subroutine sph_forward_trans_4_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_snapshot_MHD
!
      use sph_trans_scalar
      use sph_trans_vector
      use copy_snap_4_sph_trans
!
!
!   transform for vectors
      call copy_snap_vec_spec_to_trans
      call sph_b_trans_vector(nvector_snap_rj_2_rtp)
      call copy_snap_vec_fld_from_trans
!
!   transform for scalars
      call copy_snap_scl_spec_to_trans
      call sph_b_trans_scalar(nscalar_snap_rj_2_rtp)
      call copy_snap_scl_fld_from_trans
!
      end subroutine sph_back_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_snapshot_MHD
!
      use sph_trans_scalar
      use sph_trans_vector
      use copy_snap_4_sph_trans
!
!
!   transform for vectors
      if(nvector_snap_rtp_2_rj .gt. 0) then
        call copy_snap_vec_fld_to_trans
        call sph_f_trans_vector(nvector_snap_rtp_2_rj)
        call copy_snap_vec_spec_from_trans
      end if
!
!   transform for scalars
      if(nscalar_snap_rtp_2_rj .gt. 0) then
        call copy_snap_scl_fld_to_trans
        call sph_f_trans_scalar(nscalar_snap_rtp_2_rj)
        call copy_snap_scl_spec_from_trans
      end if
!
      end subroutine sph_forward_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_transform_4_licv
!
      use m_addresses_trans_sph_MHD
      use copy_MHD_4_sph_trans
!
      use sph_trans_w_coriols
!
!
      if(nvector_rj_2_rtp .gt. 0) then
        call copy_mhd_vec_spec_to_trans
        call sph_b_trans_licv(nvector_rj_2_rtp)
      end if
!
!
      if(nvector_rtp_2_rj .gt. 0) then
        call sph_f_trans_licv(nvector_rtp_2_rj)
        call copy_mhd_vec_spec_from_trans
      end if
!
      end subroutine sph_transform_4_licv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine select_legendre_transform
!
      use calypso_mpi
      use m_machine_parameter
      use m_work_4_sph_trans
      use legendre_transform_select
!
      real(kind = kreal) :: stime, etime_shortest
      real(kind = kreal) :: etime(5), etime_trans(5)
!
      integer(kind = kint) :: iloop_type
!
!
      do iloop_type = 1, 5
        id_legendre_transfer = iloop_type
        call sel_alloc_legendre_trans(nb_sph_trans)
!
        stime = MPI_WTIME()
        call sph_back_trans_4_MHD
        call sph_forward_trans_4_MHD
        etime(id_legendre_transfer) = MPI_WTIME() - stime
!
        call sel_dealloc_legendre_trans
      end do
!
      call MPI_allREDUCE (etime, etime_trans, ifive,                    &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      etime_trans(1:5) = etime_trans(1:5) / dble(nprocs)
!
      id_legendre_transfer = iflag_leg_orginal_loop
      etime_shortest =       etime_trans(iflag_leg_orginal_loop)
!
      do iloop_type = 2, 5
        if(etime_trans(iloop_type) .lt. etime_shortest) then
          id_legendre_transfer = iloop_type
          etime_shortest =       etime_trans(iloop_type)
        end if
      end do
!
      if(my_rank .gt. 0) return
        write(*,'(a,i4)', advance='no')                                 &
     &         'Selected id_legendre_transfer: ', id_legendre_transfer
!
        if     (id_legendre_transfer .eq. iflag_leg_orginal_loop) then
          write(*,'(a,a)') ' (ORIGINAL_LOOP) '
        else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
          write(*,'(a,a)') ' (INNER_RADIAL_LOOP) '
        else if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
          write(*,'(a,a)') ' (OUTER_RADIAL_LOOP) '
        else if(id_legendre_transfer .eq. iflag_leg_long_loop) then
          write(*,'(a,a)') ' (LONG_LOOP) '
        else if(id_legendre_transfer .eq. iflag_lef_fdout_loop) then
          write(*,'(a,a)') ' (OUTER_FIELD_LOOP) '
        end if
!
        write(*,*) '1: elapsed by original loop:      ',                &
     &            etime_trans(iflag_leg_orginal_loop)
        write(*,*) '2: elapsed by inner radius loop:  ',                &
     &            etime_trans(iflag_leg_krloop_inner)
        write(*,*) '3: elapsed by outer radius loop:  ',                &
     &            etime_trans(iflag_leg_krloop_outer)
        write(*,*) '4: elapsed by long loop:          ',                &
     &            etime_trans(iflag_leg_long_loop)
        write(*,*) '5: elapsed by outmost field loop: ',                &
     &            etime_trans(iflag_lef_fdout_loop)
!
      end subroutine select_legendre_transform
!
!-----------------------------------------------------------------------
!
      end module sph_transforms_4_MHD
