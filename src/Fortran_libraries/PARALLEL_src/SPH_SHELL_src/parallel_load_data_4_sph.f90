!>@file   parallel_load_data_4_sph.f90
!!@brief  module parallel_load_data_4_sph
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief Load spherical harmonics indexing data on multiple processes
!!
!!@verbatim
!!      subroutine load_para_SPH_and_FEM_mesh
!!      subroutine load_FEM_mesh_4_SPH
!!      subroutine load_para_sph_mesh
!!@endverbatim
!
      module parallel_load_data_4_sph
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: count_interval_4_each_dir, self_comm_flag
      private :: set_fem_center_mode_4_SPH
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_para_SPH_and_FEM_mesh
!
      call load_para_sph_mesh
      call load_FEM_mesh_4_SPH
!
      end subroutine load_para_SPH_and_FEM_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine load_FEM_mesh_4_SPH
!
      use calypso_mpi
      use t_mesh_data
      use t_group_data
!
      use m_geometry_data
      use m_spheric_constants
      use m_spheric_parameter
      use load_mesh_data
      use const_FEM_mesh_sph_mhd
      use copy_mesh_from_type
      use mesh_IO_select
!
      type(mesh_geometry) :: mesh
      type(mesh_groups) ::  group
!
!
!  --  load FEM mesh data
      if(check_exist_mesh(my_rank) .eq. 0) then
        if (iflag_debug.gt.0) write(*,*) 'input_mesh'
        call input_mesh(my_rank)
        call set_fem_center_mode_4_SPH(node1%internal_node)
        return
      end if
!
!  --  Construct FEM mesh
      if(iflag_shell_mode .eq. iflag_no_FEMMESH) then
        if(iflag_rj_center .gt. 0) then
          iflag_shell_mode =  iflag_MESH_w_center
        else
          iflag_shell_mode = iflag_MESH_same
        end if
      end if
!
      call const_FEM_mesh_4_sph_mhd(mesh, group)
!      call compare_mesh_type_vs_1st(my_rank, mesh, group)
!
      call set_mesh_from_type(mesh, group)
!
      end subroutine load_FEM_mesh_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine load_para_sph_mesh
!
      use calypso_mpi
      use m_machine_parameter
!
      use load_data_for_sph_IO
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_geom_rtp_sph_trans'
      call input_geom_rtp_sph_trans(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rj_sph_trans'
      call input_modes_rj_sph_trans(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'input_geom_rtm_sph_trans'
      call input_geom_rtm_sph_trans(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rlm_sph_trans'
      call input_modes_rlm_sph_trans(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'set_reverse_tables_4_SPH'
      call set_reverse_tables_4_SPH
!
      end subroutine load_para_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_reverse_tables_4_SPH
!
      use calypso_mpi
      use m_machine_parameter
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      use count_num_sph_smp
      use set_special_sph_lm_flags
!
      use set_from_recv_buf_rev
!
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.gt.0) write(*,*) 's_count_num_sph_smp'
      call s_count_num_sph_smp(ierr)
!      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message_Rsmp)
!
      call set_reverse_import_table(nnod_rtp, ntot_item_sr_rtp,         &
     &    item_sr_rtp, irev_sr_rtp)
      call set_reverse_import_table(nnod_rtm, ntot_item_sr_rtm,         &
     &    item_sr_rtm, irev_sr_rtm)
      call set_reverse_import_table(nnod_rlm, ntot_item_sr_rlm,         &
     &    item_sr_rlm, irev_sr_rlm)
      call set_reverse_import_table(nnod_rj, ntot_item_sr_rj,           &
     &    item_sr_rj, irev_sr_rj)
!
      iflag_self_rtp = self_comm_flag(nneib_domain_rtp, id_domain_rtp)
      iflag_self_rtm = self_comm_flag(nneib_domain_rtm, id_domain_rtm)
      iflag_self_rlm = self_comm_flag(nneib_domain_rlm, id_domain_rlm)
      iflag_self_rj =  self_comm_flag(nneib_domain_rj,  id_domain_rj)
!
      call count_interval_4_each_dir(ithree, nnod_rtp, idx_global_rtp,  &
     &    istep_rtp)
      call count_interval_4_each_dir(ithree, nnod_rtm, idx_global_rtm,  &
     &    istep_rtm)
      call count_interval_4_each_dir(itwo,   nnod_rlm, idx_global_rlm,  &
     &    istep_rlm)
      call count_interval_4_each_dir(itwo,   nnod_rj,  idx_global_rj,   &
     &    istep_rj)
!
      m_folding = 2 * idx_gl_1d_rtp_p(2,2) / nidx_rtp(3)
!
      call set_special_degree_order_flags(nidx_rj(2), nidx_rlm(2),      &
     &    idx_gl_1d_rj_j, idx_gl_1d_rlm_j, idx_rj_degree_zero,          &
     &    idx_rj_degree_one,  ist_rtm_order_zero,                       &
     &    ist_rtm_order_1s, ist_rtm_order_1c)
!
!
      call set_sph_rj_center_flag(nnod_rj, nidx_rj, inod_rj_center)
!
      iflag_rj_center = 0
      call MPI_allREDUCE(inod_rj_center, iflag_rj_center, ione,         &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      if(iflag_rj_center .gt. 0) iflag_rj_center = 1
!
      end subroutine set_reverse_tables_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine set_fem_center_mode_4_SPH(internal_node)
!
      use calypso_mpi
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_constants
!
      integer(kind = kint), intent(in) :: internal_node
!
      integer(kind = kint) :: iflag_shell_local, nsample
      integer(kind = kint) :: nnod_full_shell
!
!
      nnod_full_shell = nnod_rtp * m_folding
      nsample = internal_node
      iflag_shell_mode = 0
      if(nsample .le. nnod_full_shell) then
        iflag_shell_local = iflag_MESH_same
      else if(nsample .eq. nnod_full_shell+nidx_rtp(1)) then
        iflag_shell_local = iflag_MESH_w_pole
      else if(nsample .eq. nnod_full_shell+2*nidx_rtp(1)) then
        iflag_shell_local = iflag_MESH_w_pole
      else if(nsample .eq. nnod_full_shell+nidx_rtp(1)+1) then
        iflag_shell_local = iflag_MESH_w_center
      else if(nsample .eq. nnod_full_shell+2*nidx_rtp(1)+1) then
        iflag_shell_local = iflag_MESH_w_center
      end if
!
      if(i_debug .eq. iflag_full_msg) write(*,*) 'iflag_shell_local',   &
     &     my_rank, iflag_shell_local, internal_node, nnod_full_shell
      call MPI_allreduce(iflag_shell_local, iflag_shell_mode, ione,     &
     &    CALYPSO_INTEGER, MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if(i_debug .eq. iflag_full_msg) write(*,*) 'iflag_shell_mode',    &
     &     my_rank, iflag_shell_mode
!
      end subroutine set_fem_center_mode_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine load_para_rj_mesh
!
      use calypso_mpi
      use m_machine_parameter
      use m_spheric_parameter
!
      use load_data_for_sph_IO
      use count_num_sph_smp
      use set_special_sph_lm_flags
!
      use set_from_recv_buf_rev
!
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rj_sph_trans'
      call input_modes_rj_sph_trans(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 's_count_num_sph_smp'
      call s_count_num_sph_smp(ierr)
!      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message_Rsmp)
!
      call set_reverse_import_table(nnod_rj, ntot_item_sr_rj,           &
     &    item_sr_rj, irev_sr_rj)
      iflag_self_rj =  self_comm_flag(nneib_domain_rj,  id_domain_rj)
!
      call count_interval_4_each_dir(itwo,   nnod_rj,  idx_global_rj,   &
     &    istep_rj)
!
      call set_sph_rj_center_flag(nnod_rj, nidx_rj, inod_rj_center)
!
      iflag_rj_center = 0
      call MPI_allREDUCE(inod_rj_center, iflag_rj_center, ione,         &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      if(iflag_rj_center .gt. 0) iflag_rj_center = 1
!
      end subroutine load_para_rj_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_interval_4_each_dir(numdir, nnod, idx_global,    &
     &    istep)
!
      integer(kind = kint), intent(in) :: numdir, nnod
      integer(kind = kint), intent(in) :: idx_global(nnod,numdir)
!
      integer(kind = kint), intent(inout) :: istep(numdir)
!
      integer(kind = kint) :: nd, inod, iref
!
!
      do nd = 1, numdir
        iref = idx_global(1,nd)
        do inod = 2, nnod
          if(idx_global(inod,nd) .ne. iref) then
            istep(nd) = inod - 1
            exit
          end if
        end do
      end do
!
      end subroutine count_interval_4_each_dir
!
! -----------------------------------------------------------------------
!
      integer function self_comm_flag(num_neib, id_neib)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      self_comm_flag = 0
      if(id_neib(num_neib) .eq. my_rank) self_comm_flag = 1
!
      end function self_comm_flag
!
! -----------------------------------------------------------------------
!
      end module parallel_load_data_4_sph
