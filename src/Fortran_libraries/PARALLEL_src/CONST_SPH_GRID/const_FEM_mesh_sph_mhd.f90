!>@file   const_FEM_mesh_sph_mhd.f90
!!@brief  module const_FEM_mesh_sph_mhd
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Construct FEM mesh from spherical harmonics transform data
!!
!!@verbatim
!!      subroutine const_FEM_mesh_4_sph_mhd(mesh, group)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::  group
!!@endverbatim
!
      module const_FEM_mesh_sph_mhd
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      private :: const_global_sph_FEM, const_global_rtp_mesh
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_FEM_mesh_4_sph_mhd(mesh, group)
!
      use calypso_mpi
      use t_mesh_data
      use t_group_data
      use m_spheric_parameter
      use m_group_data_sph_specr
      use m_gauss_points
      use m_spheric_global_ranks
      use set_FEM_mesh_4_sph
      use const_1d_ele_connect_4_sph
      use set_sph_groups
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::  group
!
!
      call allocate_gauss_points(nidx_global_rtp(2))
      call allocate_gauss_colatitude
      call construct_gauss_coefs
      call set_gauss_colatitude
!
!
      call const_global_sph_FEM
      call s_const_1d_ele_connect_4_sph
!
      nidx_local_fem(1:3) = nidx_rtp(1:3)
      nidx_local_fem(3) =   m_folding * nidx_local_fem(3)
!
      call s_const_FEM_mesh_for_sph                                     &
     &   (my_rank, radius_1d_rj_r, mesh, group)
!
      call deallocate_nnod_nele_sph_mesh
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      end subroutine const_FEM_mesh_4_sph_mhd
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_global_sph_FEM
!
      use m_spheric_global_ranks
      use m_sph_global_parameter
      use m_sph_1d_global_index
      use m_2d_sph_trans_table
      use set_sph_1d_domain_id
!
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rtp_mesh'
      call const_global_rtp_mesh
!
      call allocate_sph_1d_domain_id
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_1d_domain_id_rtp'
      call set_sph_1d_domain_id_rtp
!
      if(iflag_debug .gt. 0) then
        write(50,*) 'idx_global_rtp_r', idx_global_rtp_r
      end if
!
      end subroutine const_global_sph_FEM
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_global_rtp_mesh
!
      use calypso_mpi
      use m_spheric_parameter
      use m_group_data_sph_specr
      use m_spheric_global_ranks
      use m_sph_global_parameter
      use m_sph_1d_global_index
      use m_2d_sph_trans_table
      use const_global_sph_grids_modes
      use set_global_spherical_param
!
      integer(kind = kint) :: ist, ip, inc_r, inc_t, ip_rank
      integer(kind = kint) :: igrp, inum, inod
!
!
      call MPI_allREDUCE(sph_rank_rtp, ndomain_rtp, ithree,             &
     &    CALYPSO_INTEGER, MPI_MAX, CALYPSO_COMM, ierr_MPI)
      ndomain_rtp(1:3) = ndomain_rtp(1:3) + 1
!
      ndomain_sph = nprocs
      call allocate_sph_ranks
      call allocate_sph_gl_parameter
!
      iglobal_rank_rtp(1:3,my_rank) = sph_rank_rtp(1:3)
      do ip = 0, nprocs-1
        call MPI_Bcast(iglobal_rank_rtp(1,ip), ithree, CALYPSO_INTEGER, &
     &      ip, CALYPSO_COMM, ierr_MPI)
      end do
      if(iglobal_rank_rtp(1,1) .eq. iglobal_rank_rtp(1,0)) then
        inc_r = ndomain_rtp(2)
      else
        inc_r = 1
      end if
      if(iglobal_rank_rtp(2,1) .eq. iglobal_rank_rtp(2,0)) then
        inc_t = ndomain_rtp(1)
      else
        inc_t = 1
      end if
!
      call allocate_nidx_local
      call allocate_sph_1d_global_stack
!
      ip = sph_rank_rtp(1) + 1
      nidx_local_rtp_r(ip)= nidx_rtp(1)
      istack_idx_local_rtp_r(ip-1) = ist_rtp(1) - 1
      istack_idx_local_rtp_r(ip) =   ied_rtp(1)
      do ip = 1, ndomain_rtp(1)
        ip_rank = (ip-1) * inc_r
        call MPI_Bcast(nidx_local_rtp_r(ip), ione,                      &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(istack_idx_local_rtp_r(ip-1), itwo,              &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      ip = sph_rank_rtp(2) + 1
      nidx_local_rtp_t(ip)= nidx_rtp(2)
      istack_idx_local_rtp_t(ip-1) = ist_rtp(2) - 1
      istack_idx_local_rtp_t(ip) =   ied_rtp(2)
      do ip = 1, ndomain_rtp(2)
        ip_rank = (ip-1) * inc_t
        call MPI_Bcast(nidx_local_rtp_t(ip), ione,                      &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(istack_idx_local_rtp_t(ip-1), itwo,              &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      ip = sph_rank_rtp(3) + 1
      nidx_local_rtp_p(ip)= nidx_rtp(3)
      istack_idx_local_rtp_p(ip-1) = ist_rtp(3) - 1
      istack_idx_local_rtp_p(ip) =   ied_rtp(3)
!
!
!
      call allocate_sph_gl_bc_param
!
      ip = sph_rank_rtp(1) + 1
      do igrp = 1, num_radial_grp_rtp
        if(name_radial_grp_rtp(igrp) .eq. OC_ele_grp_name) then
          nidx_local_rtp_OC(ip) =  istack_radial_grp_rtp(igrp)          &
     &                           - istack_radial_grp_rtp(igrp-1)
          ist = istack_radial_grp_rtp(igrp-1) + 1
          inum = item_radial_grp_rtp(ist)
          ist_idx_local_rtp_OC(ip) = idx_gl_1d_rtp_r(inum) - 1
          exit
        end if
      end do
!
      do igrp = 1, num_radial_grp_rtp
        if(name_radial_grp_rtp(igrp) .eq. IC_ele_grp_name) then
          nidx_local_rtp_IC(ip) =  istack_radial_grp_rtp(igrp)          &
     &                           - istack_radial_grp_rtp(igrp-1)
          ist = istack_radial_grp_rtp(igrp-1) + 1
          inum = item_radial_grp_rtp(ist)
          ist_idx_local_rtp_IC(ip) = idx_gl_1d_rtp_r(inum) - 1
          exit
        end if
      end do
!
      nidx_local_rtp_MT(ip) =  nidx_rtp(1) - nidx_local_rtp_OC(ip)      &
     &                                     - nidx_local_rtp_IC(ip)
      if(nidx_local_rtp_MT(ip) .gt. 0) then
        do igrp = 1, num_radial_grp_rtp
          if(name_radial_grp_rtp(igrp) .eq. OC_ele_grp_name) then
            ist = istack_radial_grp_rtp(igrp)
            inum = item_radial_grp_rtp(ist) + 1
            ist_idx_local_rtp_MT(ip) = idx_gl_1d_rtp_r(inum) - 1
            exit
          end if
        end do
      end if
!
      do ip = 1, ndomain_rtp(1)
        ip_rank = (ip-1) * inc_r
        call MPI_Bcast(nidx_local_rtp_OC(ip), ione,                     &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(nidx_local_rtp_IC(ip), ione,                     &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(nidx_local_rtp_MT(ip), ione,                     &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(ist_idx_local_rtp_OC(ip), ione,                  &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(ist_idx_local_rtp_IC(ip), ione,                  &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(ist_idx_local_rtp_MT(ip), ione,                  &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      call set_gl_nnod_spherical(ndomain_sph,                           &
     &    ndomain_rtp(1), ndomain_rtp(2), ndomain_rtp(3),               &
     &    iglobal_rank_rtp, nidx_local_rtp_r, nidx_local_rtp_t,         &
     &    nidx_local_rtp_p, nidx_local_rtp, nnod_local_rtp)
!
      call allocate_sph_1d_global_idx
!
      do inum = 1, nidx_rtp(1)
        inod = ist_rtp(1) + inum - 1
        idx_global_rtp_r(inod) = idx_gl_1d_rtp_r(inum)
      end do
      do ip = 1, ndomain_rtp(1)
        ip_rank = (ip-1) * inc_r
        ist = istack_idx_local_rtp_r(ip-1) + 1
        call MPI_Bcast(idx_global_rtp_r(ist), nidx_local_rtp_r(ip),     &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      do inum = 1, nidx_rtp(2)
        inod = ist_rtp(2) + inum - 1
        idx_global_rtp_t(inod) = idx_gl_1d_rtp_t(inum)
      end do
      do ip = 1, ndomain_rtp(2)
        ip_rank = (ip-1) * inc_t
        ist = istack_idx_local_rtp_t(ip-1) + 1
        call MPI_Bcast(idx_global_rtp_t(ist), nidx_local_rtp_t(ip),     &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      do inod = 1, nidx_rtp(3)
        idx_global_rtp_p(inod,1) = idx_gl_1d_rtp_p(inod,1)
        idx_global_rtp_p(inod,2) = idx_gl_1d_rtp_p(inod,2)
      end do
!
      call deallocate_nidx_local
!
      end subroutine const_global_rtp_mesh
!
! -----------------------------------------------------------------------
!
      end module const_FEM_mesh_sph_mhd
