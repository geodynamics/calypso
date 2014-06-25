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
!!      subroutine sph_back_trans_snapshot_MHD
!!      subroutine sph_forward_trans_snapshot_MHD
!!
!!      subroutine sph_back_trans_tmp_snap_MHD
!!      subroutine sph_forward_trans_tmp_snap_MHD
!!
!!      subroutine sph_transform_4_licv
!!@endverbatim
!!
      module sph_transforms_4_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
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
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
      use m_addresses_trans_sph_tmp
      use m_work_4_sph_trans
      use init_sph_trans
      use const_wz_coriolis_rtp
      use const_coriolis_sph_rlm
      use legendre_transform_select
      use skip_comment_f
!
      character(len=kchara) :: tmpchara
!
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_trans_sph_MHD'
      call set_addresses_trans_sph_MHD
      call set_addresses_snapshot_trans
      call set_addresses_temporal_trans
!
      if(iflag_debug .ge. iflag_routine_msg) then
        call check_add_trans_sph_MHD
        call check_addresses_snapshot_trans
        call check_addresses_temporal_trans
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_sph_trans'
      call initialize_sph_trans
!
      if (iflag_debug.eq.1) write(*,*) 'set_colatitude_rtp'
      call set_colatitude_rtp
      if (iflag_debug.eq.1) write(*,*) 'init_sum_coriolis_rlm'
      call init_sum_coriolis_rlm
!
      if(id_legendre_transfer .eq. iflag_leg_undefined) then
        if (iflag_debug.eq.1) write(*,*) 'select_legendre_transform'
        call select_legendre_transform
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'sel_alloc_legendre_trans'
      call sel_alloc_legendre_trans(ncomp_sph_trans)
!
!
      if(my_rank .ne. 0) return
        if     (id_legendre_transfer .eq. iflag_leg_orginal_loop) then
          write(tmpchara,'(a)') trim(leg_orginal_loop)
        else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
          write(tmpchara,'(a)') trim(leg_krloop_inner)
        else if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
          write(tmpchara,'(a)') trim(leg_krloop_outer)
        else if(id_legendre_transfer .eq. iflag_leg_long_loop) then
          write(tmpchara,'(a)') trim(leg_long_loop)
        else if(id_legendre_transfer .eq. iflag_leg_fdout_loop) then
          write(tmpchara,'(a)') trim(leg_fdout_loop)
        end if
        call change_2_upper_case(tmpchara)
!
        write(*,'(a,i4)', advance='no')                                 &
     &         'Selected id_legendre_transfer: ', id_legendre_transfer
        write(*,'(a,a,a)') ' (', trim(tmpchara), ') '
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
      use sph_trans_w_coriols
!
!
      if(ncomp_rj_2_rtp .eq. 0) return
      call copy_mhd_vec_spec_to_trans
      call copy_mhd_scl_spec_to_trans
!
      call sph_b_trans_w_coriolis(ncomp_rj_2_rtp,                       &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp)
!
      call copy_mhd_vec_fld_from_trans
      call copy_mhd_scl_fld_from_trans
!
      end subroutine sph_back_trans_4_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_4_MHD
!
      use m_addresses_trans_sph_MHD
      use copy_MHD_4_sph_trans
      use sph_trans_w_coriols
!
!
      if(ncomp_rtp_2_rj .eq. 0) return
      call copy_mhd_vec_fld_to_trans
!
      call sph_f_trans_w_coriolis(ncomp_rtp_2_rj,                       &
     &    nvector_rtp_2_rj, nscalar_rtp_2_rj)
!
      call copy_mhd_vec_spec_from_trans
!
      end subroutine sph_forward_trans_4_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_snapshot_MHD
!
      use m_addresses_trans_sph_snap
      use sph_transforms
      use copy_snap_4_sph_trans
!
!
      if(ncomp_snap_rj_2_rtp .le. 0) return
!
      call copy_snap_vec_spec_to_trans
      call copy_snap_scl_spec_to_trans
!
!   transform for vectors
      call sph_backward_transforms(ncomp_snap_rj_2_rtp,                 &
     &    nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                 &
     &    ntensor_snap_rj_2_rtp)
!
      call copy_snap_vec_fld_from_trans
      call copy_snap_scl_fld_from_trans
!
      end subroutine sph_back_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_snapshot_MHD
!
      use m_addresses_trans_sph_snap
      use sph_transforms
      use copy_snap_4_sph_trans
!
!
      if(ncomp_snap_rtp_2_rj .le. 0) return
!
      call copy_snap_scl_fld_to_trans
      call copy_snap_vec_fld_to_trans
!
!   transform for vectors and scalars
      call sph_forward_transforms(ncomp_snap_rtp_2_rj,                  &
     &   nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj, izero)
!
      call copy_snap_vec_spec_from_trans
      call copy_snap_scl_spec_from_trans
!
      end subroutine sph_forward_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_tmp_snap_MHD
!
      use m_addresses_trans_sph_tmp
      use sph_transforms
      use copy_temporal_4_sph_trans
!
!
      if(ncomp_tmp_rj_2_rtp .le. 0) return
!
      call copy_tmp_vec_spec_to_trans
!      call copy_tmp_scl_spec_to_trans
!
!   transform for vectors
      call sph_backward_transforms(ncomp_tmp_rj_2_rtp,                  &
     &    nvector_tmp_rj_2_rtp, nscalar_tmp_rj_2_rtp,                   &
     &    ntensor_tmp_rj_2_rtp)
!
      call copy_tmp_vec_fld_from_trans
!      call copy_tmp_scl_fld_from_trans
!
      end subroutine sph_back_trans_tmp_snap_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_tmp_snap_MHD
!
      use m_addresses_trans_sph_tmp
      use sph_transforms
      use copy_temporal_4_sph_trans
!
!
      if(ncomp_tmp_rtp_2_rj .eq. 0) return
!
      call copy_tmp_scl_fld_to_trans
!      call copy_tmp_vec_fld_to_trans
!
!   transform for vectors and scalars
      call sph_forward_transforms(ncomp_tmp_rtp_2_rj,                   &
     &   nvector_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj, izero)
!
!      call copy_tmp_vec_spec_from_trans
      call copy_tmp_scl_spec_from_trans
!
      end subroutine sph_forward_trans_tmp_snap_MHD
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
      if((ncomp_rj_2_rtp*ncomp_rtp_2_rj) .eq. 0) return
!
      call copy_mhd_vec_spec_to_trans
!
      call sph_b_trans_licv(ncomp_rj_2_rtp)
      call sph_f_trans_licv(ncomp_rtp_2_rj)
!
      call copy_mhd_vec_spec_from_trans
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
        call sel_alloc_legendre_trans(ncomp_sph_trans)
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
        write(*,*) '1: elapsed by original loop:      ',                &
     &            etime_trans(iflag_leg_orginal_loop)
        write(*,*) '2: elapsed by inner radius loop:  ',                &
     &            etime_trans(iflag_leg_krloop_inner)
        write(*,*) '3: elapsed by outer radius loop:  ',                &
     &            etime_trans(iflag_leg_krloop_outer)
        write(*,*) '4: elapsed by long loop:          ',                &
     &            etime_trans(iflag_leg_long_loop)
        write(*,*) '5: elapsed by outmost field loop: ',                &
     &            etime_trans(iflag_leg_fdout_loop)
!
      end subroutine select_legendre_transform
!
!-----------------------------------------------------------------------
!
      end module sph_transforms_4_MHD
