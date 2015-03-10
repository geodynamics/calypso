!>@file   para_gen_sph_grids_modes.f90
!!@brief  module para_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Parallellized version)
!!
!!
!!@verbatim
!!      subroutine para_gen_sph_rlm_grids
!!      subroutine para_gen_sph_rtm_grids
!!
!!      subroutine para_gen_sph_rj_modes
!!      subroutine para_gen_sph_rtp_grids
!!
!!      subroutine para_gen_fem_mesh_for_sph
!!@endverbatim
!
      module para_gen_sph_grids_modes
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use set_local_sphere_by_global
!
      implicit none
!
      integer(kind = kint), allocatable :: nneib_rlm_lc(:)
      integer(kind = kint), allocatable :: nneib_rlm_gl(:)
      integer(kind = kint), allocatable :: nneib_rtm_lc(:)
      integer(kind = kint), allocatable :: nneib_rtm_gl(:)
!
      private :: nneib_rlm_lc, nneib_rtm_lc, nneib_rlm_gl, nneib_rtm_gl
      private :: allocate_nneib_sph_rlm_tmp
      private :: deallocate_nneib_sph_rlm_tmp
      private :: allocate_nneib_sph_rtm_tmp
      private :: deallocate_nneib_sph_rtm_tmp
      private :: bcast_comm_stacks_rlm, bcast_comm_stacks_rtm
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nneib_sph_rlm_tmp(ndomain_sph)
!
      integer(kind = kint), intent(in) :: ndomain_sph
!
      allocate(nneib_rlm_lc(ndomain_sph))
      allocate(nneib_rlm_gl(ndomain_sph))
      nneib_rlm_lc = 0
      nneib_rlm_gl = 0
!
      end subroutine allocate_nneib_sph_rlm_tmp
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nneib_sph_rtm_tmp(ndomain_sph)
!
      integer(kind = kint), intent(in) :: ndomain_sph
!
      allocate(nneib_rtm_lc(ndomain_sph))
      allocate(nneib_rtm_gl(ndomain_sph))
      nneib_rtm_lc = 0
      nneib_rtm_gl = 0
!
      end subroutine allocate_nneib_sph_rtm_tmp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nneib_sph_rlm_tmp
!
!
      deallocate(nneib_rlm_lc, nneib_rlm_gl)
!
      end subroutine deallocate_nneib_sph_rlm_tmp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nneib_sph_rtm_tmp
!
!
      deallocate(nneib_rtm_lc, nneib_rtm_gl)
!
      end subroutine deallocate_nneib_sph_rtm_tmp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine para_gen_sph_rlm_grids
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use copy_sph_comm_table_4_type
!
      integer(kind = kint) :: ip_rank, ip
!
!
      call allocate_nneib_sph_rlm_tmp(ndomain_sph)
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        if(mod(ip_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'start rlm table generation for',                    &
     &            ip_rank, 'on ', my_rank, nprocs
        call const_sph_rlm_modes(ip_rank)
        call copy_comm_rlm_num_to_type(sph_para(ip)%sph_comms%comm_rlm)
!
        nneib_rlm_lc(ip)                                                &
     &       = sph_para(ip)%sph_comms%comm_rlm%nneib_domain
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &        'output_modes_rlm_sph_trans', ip_rank
        call output_modes_rlm_sph_trans(ip_rank)
!
        write(*,'(a,i6,a)') 'Spherical transform table for domain',     &
     &          ip_rank, ' is done.'
      end do
      call bcast_comm_stacks_rlm
      call deallocate_nneib_sph_rlm_tmp
!
      end subroutine para_gen_sph_rlm_grids
!
! -----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtm_grids
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use copy_sph_comm_table_4_type
!
      integer(kind = kint) :: ip_rank, ip
!
!
      call allocate_nneib_sph_rtm_tmp(ndomain_sph)
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        if(mod(ip_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'start rtm table generation for',                    &
     &            ip_rank, 'on ', my_rank, nprocs
        call const_sph_rtm_grids(ip_rank)
!
        call copy_comm_rtm_num_to_type(sph_para(ip)%sph_comms%comm_rtm)
        nneib_rtm_lc(ip)                                                &
     &       = sph_para(ip_rank+1)%sph_comms%comm_rtm%nneib_domain
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &        'output_geom_rtm_sph_trans', ip_rank
        call output_geom_rtm_sph_trans(ip_rank)
 
        write(*,'(a,i6,a)') 'Legendre transform table rtm',             &
     &          ip_rank, ' is done.'
      end do
      call bcast_comm_stacks_rtm
      call deallocate_nneib_sph_rtm_tmp
!
      end subroutine para_gen_sph_rtm_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rj_modes
!
      use m_spheric_parameter
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
!
      integer(kind = kint) :: ip_rank
!
!
      call allocate_rj_1d_local_idx
      do ip_rank = 0, ndomain_sph-1
        if(mod(ip_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical modes for domain ',             &
     &            ip_rank,  ' on ', my_rank
        call const_sph_rj_modes(ip_rank)
      end do
      call deallocate_rj_1d_local_idx
!
      end subroutine para_gen_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtp_grids
!
      use m_spheric_parameter
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
!
      integer(kind = kint) :: ip_rank
!
!
      call allocate_rtp_1d_local_idx
      do ip_rank = 0, ndomain_sph-1
        if(mod(ip_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical grids for domain ',             &
     &            ip_rank,  ' on ', my_rank
        call const_sph_rtp_grids(ip_rank)
      end do
      call deallocate_rtp_1d_local_idx
!
      end subroutine para_gen_sph_rtp_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_fem_mesh_for_sph
!
      use m_gauss_points
      use m_group_data_sph_specr
      use m_sph_mesh_1d_connect
      use const_1d_ele_connect_4_sph
      use set_local_index_table_sph
      use set_sph_groups
      use gen_sph_grids_modes
!
      integer(kind = kint) :: ip_rank
!
!
      if(iflag_excluding_FEM_mesh .gt. 0) return
!
      n_point = nidx_global_rtp(2)
      call allocate_gauss_points
      call allocate_gauss_colatitude
      call construct_gauss_coefs
      call set_gauss_colatitude
!
      call s_const_1d_ele_connect_4_sph
      call set_rj_radial_grp
!
      do ip_rank = 0, ndomain_sph-1
        if(mod(ip_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct FEM mesh for domain ', ip_rank,           &
     &             ' on ', my_rank
!
        call const_fem_mesh_for_sph(ip_rank)
      end do
!
      call deallocate_rj_r_grp_item
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      end subroutine para_gen_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine bcast_comm_stacks_rlm
!
      use m_spheric_parameter
      use set_comm_table_rtp_rj
!
      integer(kind = kint) :: ip_rank, ip, iroot, nneib
!
!
      if(i_debug .gt. 0) write(*,*) 'barrier for rlm', my_rank
      call calypso_MPI_barrier
      if(my_rank .eq. 0) write(*,*) 'barrier finished for rlm'
!
      call MPI_allREDUCE(nneib_rlm_lc(1), nneib_rlm_gl(1),              &
     &      ndomain_sph, CALYPSO_INTEGER, MPI_SUM,                      &
     &      CALYPSO_COMM, ierr_MPI)
!
      sph_para(1:ndomain_sph)%sph_comms%comm_rlm%nneib_domain           &
     &        = nneib_rlm_gl(1:ndomain_sph)
!
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        if(mod(ip_rank,nprocs) .eq. my_rank) cycle
        call alloc_type_sph_comm_stack(sph_para(ip)%sph_comms%comm_rlm)
      end do
!
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        iroot = mod(ip_rank,nprocs)
        nneib = sph_para(ip)%sph_comms%comm_rlm%nneib_domain
        call MPI_Bcast(sph_para(ip)%sph_comms%comm_rlm%id_domain(1),    &
     &      nneib, CALYPSO_INTEGER, iroot, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(sph_para(ip)%sph_comms%comm_rlm%istack_sr(1),    &
     &      nneib, CALYPSO_INTEGER, iroot, CALYPSO_COMM, ierr_MPI)
      end do
!
      end subroutine bcast_comm_stacks_rlm
!
! ----------------------------------------------------------------------
!
      subroutine bcast_comm_stacks_rtm
!
      use m_spheric_parameter
      use set_comm_table_rtp_rj
!
      integer(kind = kint) :: ip_rank, ip, iroot, nneib
!
!
      if(i_debug .gt. 0) write(*,*) 'barrier for rtm', my_rank
      call calypso_MPI_barrier
      if(my_rank .eq. 0) write(*,*) 'barrier finished for rtm'
!
      call MPI_allREDUCE(nneib_rtm_lc(1), nneib_rtm_gl(1),              &
     &      ndomain_sph, CALYPSO_INTEGER, MPI_SUM,                      &
     &      CALYPSO_COMM, ierr_MPI)
!
      sph_para(1:ndomain_sph)%sph_comms%comm_rtm%nneib_domain           &
     &        = nneib_rtm_gl(1:ndomain_sph)
!
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        if(mod(ip_rank,nprocs) .eq. my_rank) cycle
        call alloc_type_sph_comm_stack(sph_para(ip)%sph_comms%comm_rtm)
      end do
!
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        iroot = mod(ip_rank,nprocs)
        nneib = sph_para(ip)%sph_comms%comm_rtm%nneib_domain
        call MPI_Bcast(sph_para(ip)%sph_comms%comm_rtm%id_domain(1),    &
     &      nneib, CALYPSO_INTEGER, iroot, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(sph_para(ip)%sph_comms%comm_rtm%istack_sr(1),    &
     &      nneib, CALYPSO_INTEGER, iroot, CALYPSO_COMM, ierr_MPI)
      end do
!
      end subroutine bcast_comm_stacks_rtm
!
! ----------------------------------------------------------------------
!
      end module para_gen_sph_grids_modes
