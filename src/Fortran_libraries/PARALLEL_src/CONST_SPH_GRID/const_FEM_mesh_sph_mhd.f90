!>@file   const_FEM_mesh_sph_mhd.f90
!!@brief  module const_FEM_mesh_sph_mhd
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Construct FEM mesh from spherical harmonics transform data
!!
!!@verbatim
!!      subroutine const_FEM_mesh_4_sph_mhd                             &
!!     &         (sph_params, sph_rtp, sph_rj, radial_rtp_grp,          &
!!     &          radial_rj_grp, mesh, group, mesh_file, gen_sph)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(group_data), intent(in) :: radial_rtp_grp, radial_rj_grp
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::  group
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!@endverbatim
!
      module const_FEM_mesh_sph_mhd
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_file_IO_parameter
      use t_spheric_parameter
      use t_mesh_data
      use t_group_data
      use t_gauss_points
      use t_const_spherical_grid
      use t_sph_local_parameter
      use t_sph_mesh_1d_connect
!
      implicit none
!
      type(gauss_points), save :: gauss_SF
      type(sph_local_1d_param), save :: sph_lc1_SF
      type(comm_table_make_sph), save :: stbl_SF
      type(sph_local_default_BC), save :: sph_dbc_SF
!
      private :: gauss_SF, sph_lc1_SF, stbl_SF, sph_dbc_SF
!
      private :: const_global_sph_FEM, const_global_rtp_mesh
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_FEM_mesh_4_sph_mhd                               &
     &         (sph_params, sph_rtp, sph_rj, radial_rtp_grp,            &
     &          radial_rj_grp, mesh, group, mesh_file, gen_sph)
!
      use calypso_mpi
      use set_FEM_mesh_4_sph
      use const_1d_ele_connect_4_sph
      use set_sph_groups
      use gen_sph_grids_modes
      use set_FEM_mesh_4_sph
      use mpi_load_mesh_data
      use sph_file_IO_select
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rtp_grp, radial_rj_grp
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::  group
      type(field_IO_params), intent(inout) ::  mesh_file
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!
      call const_gauss_colatitude(sph_rtp%nidx_global_rtp(2), gauss_SF)
!
!
      call const_global_sph_FEM                                         &
     &   (sph_rtp, sph_rj, radial_rtp_grp, gen_sph)
      call s_const_1d_ele_connect_4_sph                                 &
     &   (sph_params%iflag_shell_mode, sph_params%m_folding, sph_rtp,   &
     &    gen_sph%s3d_ranks, gen_sph%stk_lc1d, gen_sph%sph_gl1d,        &
     &    stbl_SF)
!
!      write(*,*) 's_const_FEM_mesh_for_sph',                           &
!     &          sph_params%iflag_shell_mode, iflag_MESH_w_center
      call s_const_FEM_mesh_for_sph                                     &
     &   (my_rank, sph_rtp%nidx_rtp, sph_rj%radius_1d_rj_r, gauss_SF,   &
     &    gen_sph%s3d_ranks, gen_sph%stk_lc1d, gen_sph%sph_gl1d,        &
     &    sph_params, sph_rtp, radial_rj_grp, mesh, group, stbl_SF)
!
! Output mesh data
      if(iflag_output_mesh .gt. 0) then
        mesh_file%file_prefix = sph_file_head
        call mpi_output_mesh(mesh_file, mesh, group)
        write(*,'(a,i6,a)')                                             &
     &          'FEM mesh for domain', my_rank, ' is done.'
      end if
!
      call dealloc_nnod_nele_sph_mesh(stbl_SF)
      call dealloc_gauss_colatitude(gauss_SF)
!
      end subroutine const_FEM_mesh_4_sph_mhd
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_global_sph_FEM                                   &
     &         (sph_rtp, sph_rj, radial_rtp_grp, gen_sph)
!
      use set_sph_1d_domain_id
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rtp_grp
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rtp_mesh'
      call const_global_rtp_mesh(sph_rtp, radial_rtp_grp,               &
     &    gen_sph%s3d_ranks, sph_dbc_SF, gen_sph%sph_lcp,               &
     &    gen_sph%stk_lc1d, gen_sph%sph_gl1d)
!
      call alloc_sph_1d_domain_id(sph_rtp, sph_rj, gen_sph%s3d_ranks)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_1d_domain_id_rtp'
      call set_sph_1d_domain_id_rtp                                     &
     &   (gen_sph%stk_lc1d, gen_sph%sph_gl1d, gen_sph%s3d_ranks)
!
      if(iflag_debug .gt. 0) then
        write(50,*) 'idx_global_rtp_r',                                 &
     &                     gen_sph%sph_gl1d%idx_global_rtp_r
      end if
!
      end subroutine const_global_sph_FEM
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_global_rtp_mesh(sph_rtp, radial_rtp_grp,         &
     &          s3d_ranks, sph_dbc, sph_lcp, stk_lc1d, sph_gl1d)
!
      use calypso_mpi
      use const_global_sph_grids_modes
      use set_global_spherical_param
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(group_data), intent(in) :: radial_rtp_grp
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
      type(sph_local_default_BC), intent(inout) :: sph_dbc
      type(sph_local_parameters), intent(inout) :: sph_lcp
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
      integer(kind = kint) :: ist, ip, inc_r, inc_t, ip_rank
      integer(kind = kint) :: igrp, inum, inod
!
!
      call MPI_allREDUCE(sph_rtp%irank_sph_rtp, s3d_ranks%ndomain_rtp,  &
     &    ithree, CALYPSO_INTEGER, MPI_MAX, CALYPSO_COMM, ierr_MPI)
      s3d_ranks%ndomain_rtp(1:3) = s3d_ranks%ndomain_rtp(1:3) + 1
!
      s3d_ranks%ndomain_sph = nprocs
      call alloc_sph_ranks(s3d_ranks)
      call alloc_sph_gl_parameter(s3d_ranks, sph_lcp)
!
      s3d_ranks%iglobal_rank_rtp(1:3,my_rank)                           &
     &           = sph_rtp%irank_sph_rtp(1:3)
      do ip = 0, nprocs-1
        call MPI_Bcast(s3d_ranks%iglobal_rank_rtp(1,ip), ithree,        &
     &       CALYPSO_INTEGER, ip, CALYPSO_COMM, ierr_MPI)
      end do
      if(s3d_ranks%iglobal_rank_rtp(1,1)                                &
     &       .eq. s3d_ranks%iglobal_rank_rtp(1,0)) then
        inc_r = s3d_ranks%ndomain_rtp(2)
      else
        inc_r = 1
      end if
      if(s3d_ranks%iglobal_rank_rtp(2,1)                                &
     &       .eq. s3d_ranks%iglobal_rank_rtp(2,0)) then
        inc_t = s3d_ranks%ndomain_rtp(1)
      else
        inc_t = 1
      end if
!
      call alloc_nidx_local(s3d_ranks, sph_lc1_SF)
      call alloc_sph_1d_global_stack(s3d_ranks, stk_lc1d)
!
      ip = sph_rtp%irank_sph_rtp(1) + 1
      sph_lc1_SF%nidx_local_rtp_r(ip)= sph_rtp%nidx_rtp(1)
      stk_lc1d%istack_idx_local_rtp_r(ip-1) = sph_rtp%ist_rtp(1) - 1
      stk_lc1d%istack_idx_local_rtp_r(ip) =   sph_rtp%ied_rtp(1)
      do ip = 1, s3d_ranks%ndomain_rtp(1)
        ip_rank = (ip-1) * inc_r
        call MPI_Bcast(sph_lc1_SF%nidx_local_rtp_r(ip), ione,           &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(stk_lc1d%istack_idx_local_rtp_r(ip-1), itwo,     &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      ip = sph_rtp%irank_sph_rtp(2) + 1
      sph_lc1_SF%nidx_local_rtp_t(ip)= sph_rtp%nidx_rtp(2)
      stk_lc1d%istack_idx_local_rtp_t(ip-1) = sph_rtp%ist_rtp(2) - 1
      stk_lc1d%istack_idx_local_rtp_t(ip) =   sph_rtp%ied_rtp(2)
!
      do ip = 1, s3d_ranks%ndomain_rtp(2)
        ip_rank = (ip-1) * inc_t
        call MPI_Bcast(sph_lc1_SF%nidx_local_rtp_t(ip), ione,           &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(stk_lc1d%istack_idx_local_rtp_t(ip-1), itwo,     &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      ip = sph_rtp%irank_sph_rtp(3) + 1
      sph_lc1_SF%nidx_local_rtp_p(ip)= sph_rtp%nidx_rtp(3)
      stk_lc1d%istack_idx_local_rtp_p(ip-1) = sph_rtp%ist_rtp(3) - 1
      stk_lc1d%istack_idx_local_rtp_p(ip) =   sph_rtp%ied_rtp(3)
!
!
!
      call alloc_sph_gl_bc_param(s3d_ranks, sph_dbc)
!
      ip = sph_rtp%irank_sph_rtp(1) + 1
      do igrp = 1, radial_rtp_grp%num_grp
        if(radial_rtp_grp%grp_name(igrp) .eq. OC_ele_grp_name) then
          sph_dbc%nidx_local_rtp_OC(ip)                                 &
     &         =  radial_rtp_grp%istack_grp(igrp)                       &
     &           - radial_rtp_grp%istack_grp(igrp-1)
          ist = radial_rtp_grp%istack_grp(igrp-1) + 1
          inum = radial_rtp_grp%item_grp(ist)
          sph_dbc%ist_idx_local_rtp_OC(ip)                              &
     &         = sph_rtp%idx_gl_1d_rtp_r(inum) - 1
          exit
        end if
      end do
!
      do igrp = 1, radial_rtp_grp%num_grp
        if(radial_rtp_grp%grp_name(igrp) .eq. IC_ele_grp_name) then
          sph_dbc%nidx_local_rtp_IC(ip)                                 &
     &         =  radial_rtp_grp%istack_grp(igrp)                       &
     &          - radial_rtp_grp%istack_grp(igrp-1)
          ist = radial_rtp_grp%istack_grp(igrp-1) + 1
          inum = radial_rtp_grp%item_grp(ist)
          sph_dbc%ist_idx_local_rtp_IC(ip)                              &
     &         = sph_rtp%idx_gl_1d_rtp_r(inum) - 1
          exit
        end if
      end do
!
      sph_dbc%nidx_local_rtp_MT(ip) =  sph_rtp%nidx_rtp(1)              &
     &                               - sph_dbc%nidx_local_rtp_OC(ip)    &
     &                               - sph_dbc%nidx_local_rtp_IC(ip)
      if(sph_dbc%nidx_local_rtp_MT(ip) .gt. 0) then
        do igrp = 1, radial_rtp_grp%num_grp
          if(radial_rtp_grp%grp_name(igrp) .eq. OC_ele_grp_name) then
            ist = radial_rtp_grp%istack_grp(igrp)
            inum = radial_rtp_grp%item_grp(ist) + 1
            sph_dbc%ist_idx_local_rtp_MT(ip)                            &
     &            = sph_rtp%idx_gl_1d_rtp_r(inum) - 1
            exit
          end if
        end do
      end if
!
      do ip = 1, s3d_ranks%ndomain_rtp(1)
        ip_rank = (ip-1) * inc_r
        call MPI_Bcast(sph_dbc%nidx_local_rtp_OC(ip), ione,             &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(sph_dbc%nidx_local_rtp_IC(ip), ione,             &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(sph_dbc%nidx_local_rtp_MT(ip), ione,             &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(sph_dbc%ist_idx_local_rtp_OC(ip), ione,          &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(sph_dbc%ist_idx_local_rtp_IC(ip), ione,          &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(sph_dbc%ist_idx_local_rtp_MT(ip), ione,          &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      call set_gl_nnod_spherical(s3d_ranks%ndomain_sph,                 &
     &    s3d_ranks%ndomain_rtp(1), s3d_ranks%ndomain_rtp(2),           &
     &    s3d_ranks%ndomain_rtp(3), s3d_ranks%iglobal_rank_rtp,         &
     &    sph_lc1_SF%nidx_local_rtp_r, sph_lc1_SF%nidx_local_rtp_t,     &
     &    sph_lc1_SF%nidx_local_rtp_p, sph_lcp%nidx_local_rtp,          &
     &    sph_lcp%nnod_local_rtp)
!
      call alloc_sph_1d_global_idx(s3d_ranks, stk_lc1d, sph_gl1d)
!
      do inum = 1, sph_rtp%nidx_rtp(1)
        inod = sph_rtp%ist_rtp(1) + inum - 1
        sph_gl1d%idx_global_rtp_r(inod) = sph_rtp%idx_gl_1d_rtp_r(inum)
      end do
      do ip = 1, s3d_ranks%ndomain_rtp(1)
        ip_rank = (ip-1) * inc_r
        ist = stk_lc1d%istack_idx_local_rtp_r(ip-1) + 1
        call MPI_Bcast(sph_gl1d%idx_global_rtp_r(ist),                  &
     &      sph_lc1_SF%nidx_local_rtp_r(ip), CALYPSO_INTEGER,           &
     &      ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      do inum = 1, sph_rtp%nidx_rtp(2)
        inod = sph_rtp%ist_rtp(2) + inum - 1
        sph_gl1d%idx_global_rtp_t(inod) = sph_rtp%idx_gl_1d_rtp_t(inum)
      end do
      do ip = 1, s3d_ranks%ndomain_rtp(2)
        ip_rank = (ip-1) * inc_t
        ist = stk_lc1d%istack_idx_local_rtp_t(ip-1) + 1
        call MPI_Bcast(sph_gl1d%idx_global_rtp_t(ist),                  &
     &      sph_lc1_SF%nidx_local_rtp_t(ip), CALYPSO_INTEGER, ip_rank,  &
     &      CALYPSO_COMM, ierr_MPI)
      end do
!
      do inod = 1, sph_rtp%nidx_rtp(3)
        sph_gl1d%idx_global_rtp_p(inod,1)                               &
     &        = sph_rtp%idx_gl_1d_rtp_p(inod,1)
        sph_gl1d%idx_global_rtp_p(inod,2)                               &
     &        = sph_rtp%idx_gl_1d_rtp_p(inod,2)
      end do
!
      call dealloc_nidx_local(sph_lc1_SF)
      call dealloc_sph_gl_bc_param(sph_dbc)
!
      end subroutine const_global_rtp_mesh
!
! -----------------------------------------------------------------------
!
      end module const_FEM_mesh_sph_mhd
