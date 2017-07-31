!>@file   set_comm_table_rtp_rj.f90
!!@brief  module set_comm_table_rtp_rj
!!
!!@author  H. Matsui
!!@date Programmed on July, 2007
!
!
!> @brief Construct communication table for rj and rtp grid
!!
!!@verbatim
!!      subroutine const_sph_rj_modes                                   &
!!     &         (ip_rank, ndomain_sph, comm_rlm_mul, added_radial_grp, &
!!     &          s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,   &
!!     &          sph_params, sph_rj, sph_rlm, sph_file, sph_lcx)
!!        type(spheric_global_rank), intent(in) :: s3d_ranks
!!        type(spheric_global_radius), intent(in) :: s3d_radius
!!        type(sph_local_parameters), intent(in) :: sph_lcp
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_1d_global_index), intent(in) :: sph_gl1d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_comm_tbl), intent(in) :: comm_rlm_mul(ndomain_sph)
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!        type(sph_local_1d_index), intent(inout) :: sph_lcx
!!      subroutine const_sph_rtp_grids                                  &
!!     &         (ip_rank, ndomain_sph, comm_rtm_mul,                   &
!!     &          added_radial_grp, r_layer_grp, med_layer_grp,         &
!!     &          s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,   &
!!     &          sph_params, sph_rtp, sph_rtm, sph_file, sph_lcx)
!!        type(spheric_global_rank), intent(in) :: s3d_ranks
!!        type(spheric_global_radius), intent(in) :: s3d_radius
!!        type(sph_local_parameters), intent(in) :: sph_lcp
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_1d_global_index), intent(in) :: sph_gl1d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_comm_tbl), intent(in) :: comm_rtm_mul(ndomain_sph)
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!        type(sph_local_1d_index), intent(inout) :: sph_lcx
!!@endverbatim
!
      module set_comm_table_rtp_rj
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_mesh
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_group_data
      use t_spheric_data_IO
      use t_spheric_global_ranks
      use t_sph_local_parameter
      use t_sph_1d_global_index
      use t_sph_mesh_1d_connect
      use t_control_1D_layering
      use t_sph_local_index
!
      implicit none
!
      integer(kind = kint), allocatable :: id_domain_tmp(:)
      integer(kind = kint), allocatable :: nnod_sr_tmp(:)
!
      type(sph_group_data), save :: sph_grp_lc
!
!
      private :: id_domain_tmp, nnod_sr_tmp, sph_grp_lc
!
      private :: allocate_domain_sr_tmp,  deallocate_domain_sr_tmp
      private :: const_comm_table_4_rj,  const_comm_table_4_rtp
      private :: count_comm_table_4_rj,  set_comm_table_4_rj
      private :: count_comm_table_4_rtp, set_comm_table_4_rtp
      private :: set_comm_stack_rtp_rj
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_domain_sr_tmp(ndomain_sph)
!
      integer(kind = kint), intent(in) :: ndomain_sph
!
      allocate( id_domain_tmp(ndomain_sph) )
      allocate( nnod_sr_tmp(ndomain_sph) )
      id_domain_tmp = 0
      nnod_sr_tmp = 0
!
      end subroutine allocate_domain_sr_tmp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_domain_sr_tmp
!
      deallocate( id_domain_tmp )
      deallocate( nnod_sr_tmp )
!
      end subroutine deallocate_domain_sr_tmp
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_sph_rj_modes                                     &
     &         (ip_rank, ndomain_sph, comm_rlm_mul, added_radial_grp,   &
     &          s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,     &
     &          sph_params, sph_rj, sph_rlm, sph_file, sph_lcx)
!
      use load_data_for_sph_IO
      use set_sph_groups
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_sphere_by_global
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(ndomain_sph)
      type(layering_group_list), intent(in) :: added_radial_grp
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(spheric_global_radius), intent(in) :: s3d_radius
      type(sph_local_parameters), intent(in) :: sph_lcp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
!
      type(sph_rj_grid), intent(inout) :: sph_rj
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_file_data_type), intent(inout) :: sph_file
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      type(sph_comm_tbl) :: comm_rj_lc
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                'copy_gl_2_local_rj_param', ip_rank
      call copy_gl_2_local_rj_param                                     &
     &   (ip_rank, s3d_ranks, sph_lcp, stk_lc1d, sph_rj)
!
      call add_center_mode_rj(ip_rank, sph_gl1d, sph_rj)
!      nnod_rj = sph_rj%nnod_rj
!      nidx_rj(1:2) = sph_rj%nidx_rj(1:2)
!
      call alloc_type_spheric_param_rj(sph_rj)
      call alloc_type_sph_1d_index_rj(sph_rj)
!
      call copy_sph_1d_gl_idx_rj(s3d_radius, sph_gl1d, sph_rj)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'set_global_sph_rj_id', ip_rank
      call set_global_sph_rj_id(s3d_ranks, stk_lc1d, sph_rj)
!
      if(iflag_debug .gt. 0) then
        call check_type_spheric_param_rj(ip_rank, sph_rj)
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'const_comm_table_4_rj', ip_rank
      call const_comm_table_4_rj(ip_rank, ndomain_sph, comm_rlm_mul,    &
     &   s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,            &
     &   sph_rj, sph_rlm, comm_rj_lc, sph_lcx)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                  'set_sph_rj_groups', ip_rank
      call set_sph_rj_groups(sph_params, sph_rj, added_radial_grp,      &
     &    sph_grp_lc%radial_rj_grp, sph_grp_lc%sphere_rj_grp)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'output_modes_rj_sph_trans', ip_rank
      call output_modes_rj_sph_trans(sph_params,                        &
     &    sph_rj, comm_rj_lc, sph_grp_lc, sph_file)
!
      end subroutine const_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine const_sph_rtp_grids                                    &
     &         (ip_rank, ndomain_sph, comm_rtm_mul,                     &
     &          added_radial_grp, r_layer_grp, med_layer_grp,           &
     &          s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,     &
     &          sph_params, sph_rtp, sph_rtm, sph_file, sph_lcx)
!
      use load_data_for_sph_IO
      use set_sph_groups
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_sphere_by_global
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(ndomain_sph)
      type(layering_group_list), intent(in) :: added_radial_grp
      type(layering_group_list), intent(in) :: r_layer_grp
      type(layering_group_list), intent(in) :: med_layer_grp
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(spheric_global_radius), intent(in) :: s3d_radius
      type(sph_local_parameters), intent(in) :: sph_lcp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_file_data_type), intent(inout) :: sph_file
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      type(sph_comm_tbl) :: comm_rtp_lc
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                'copy_gl_2_local_rtp_param', ip_rank
      call copy_gl_2_local_rtp_param                                    &
     &   (ip_rank, s3d_ranks, sph_lcp, stk_lc1d, sph_rtp)
!
      call alloc_type_spheric_param_rtp(sph_rtp)
      call alloc_type_sph_1d_index_rtp(sph_rtp)
!
      call copy_sph_1d_gl_idx_rtp(s3d_radius, sph_gl1d, sph_rtp)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'set_global_sph_rtp_id', ip_rank
      call set_global_sph_rtp_id(s3d_ranks, stk_lc1d, sph_rtp)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'check_spheric_param_rtp', ip_rank
        call check_type_spheric_param_rtp(ip_rank, sph_rtp)
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'const_comm_table_4_rtp', ip_rank
      call const_comm_table_4_rtp(ip_rank, ndomain_sph, comm_rtm_mul,   &
     &    s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,           &
     &    sph_rtp, sph_rtm, comm_rtp_lc, sph_lcx)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_rtp_groups', ip_rank
      call set_sph_rtp_groups(sph_params, sph_rtp,                      &
     &    added_radial_grp, r_layer_grp, med_layer_grp,                 &
     &    sph_grp_lc%bc_rtp_grp, sph_grp_lc%radial_rtp_grp,             &
     &    sph_grp_lc%theta_rtp_grp, sph_grp_lc%zonal_rtp_grp)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'output_geom_rtp_sph_trans', ip_rank
      call output_geom_rtp_sph_trans(sph_params,                        &
     &    sph_rtp, comm_rtp_lc, sph_grp_lc, sph_file)
!
      end subroutine const_sph_rtp_grids
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rj                                  &
     &         (ip_rank, ndomain_sph, comm_rlm_mul,                     &
     &          s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,     &
     &          sph_rj, sph_rlm, comm_rj, sph_lcx)
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(ndomain_sph)
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(spheric_global_radius), intent(in) :: s3d_radius
      type(sph_local_parameters), intent(in) :: sph_lcp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rj
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      integer(kind = kint) :: icou
!
!
      call allocate_domain_sr_tmp(ndomain_sph)
!
      comm_rj%nneib_domain = 0
      call count_comm_table_4_rj                                        &
     &   (ip_rank, ndomain_sph, comm_rlm_mul, comm_rj)
!
      call alloc_type_sph_comm_stack(comm_rj)
!
      call set_comm_stack_rtp_rj                                        &
     &   (comm_rj%nneib_domain, comm_rj%id_domain,                      &
     &    comm_rj%istack_sr, comm_rj%ntot_item_sr)
!
      call deallocate_domain_sr_tmp
      call alloc_type_sph_comm_item(sph_rj%nnod_rj,  comm_rj)
!
      icou = 0
      call set_comm_table_4_rj(ip_rank, ndomain_sph, comm_rlm_mul,      &
     &    s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,           &
     &    sph_rj, sph_rlm, comm_rj, sph_lcx, icou)
!
      end subroutine const_comm_table_4_rj
!
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rtp                                 &
     &         (ip_rank, ndomain_sph, comm_rtm_mul,                     &
     &          s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,     &
     &          sph_rtp, sph_rtm, comm_rtp, sph_lcx)
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(ndomain_sph)
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(spheric_global_radius), intent(in) :: s3d_radius
      type(sph_local_parameters), intent(in) :: sph_lcp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      integer(kind = kint) :: icou
!
!
      call allocate_domain_sr_tmp(ndomain_sph)
!
      call count_comm_table_4_rtp(ip_rank, ndomain_sph, comm_rtm_mul,   &
     &    comm_rtp%nneib_domain)
!
      call alloc_type_sph_comm_stack(comm_rtp)
!
      call set_comm_stack_rtp_rj                                        &
     &   (comm_rtp%nneib_domain, comm_rtp%id_domain,                    &
     &    comm_rtp%istack_sr, comm_rtp%ntot_item_sr)
!      write(*,*) 'nneib_domain_rtp', comm_rtp%nneib_domain
!      write(*,*) 'id_domain_rtp',    comm_rtp%id_domain
!      write(*,*) 'ntot_item_sr_rtp', comm_rtp%ntot_item_sr
!      write(*,*) 'istack_sr_rtp',    comm_rtp%istack_sr
!
      call deallocate_domain_sr_tmp
      call alloc_type_sph_comm_item(sph_rtp%nnod_rtp, comm_rtp)
!
      icou = 0
      call set_comm_table_4_rtp(ip_rank, ndomain_sph, comm_rtm_mul,     &
     &    s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,           &
     &    sph_rtp, sph_rtm, comm_rtp, sph_lcx, icou)
!
      end subroutine const_comm_table_4_rtp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_comm_table_4_rj                                  &
     &         (ip_rank, ndomain_sph, comm_rlm, comm_rj)
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rlm(ndomain_sph)
!
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
      integer(kind = kint) :: ip1, jp, id_org_rank, ip_org
      integer(kind = kint) :: iflag_jp
!
!
      do ip1 = 1, ndomain_sph
        id_org_rank = mod((ip_rank+ip1),ndomain_sph)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, comm_rlm(ip_org)%nneib_domain
          if(comm_rlm(ip_org)%id_domain(jp) .eq. ip_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        comm_rj%nneib_domain = comm_rj%nneib_domain + 1
        id_domain_tmp(comm_rj%nneib_domain) = id_org_rank
        nnod_sr_tmp(comm_rj%nneib_domain)                               &
     &     =  comm_rlm(ip_org)%istack_sr(iflag_jp)                      &
     &      - comm_rlm(ip_org)%istack_sr(iflag_jp-1)
      end do
!
      end subroutine count_comm_table_4_rj
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_table_4_rj                                    &
     &         (ip_rank, ndomain_sph, comm_rlm_mul,                     &
     &          s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,     &
     &          sph_rj, sph_rlm, comm_rj, sph_lcx, icou)
!
      use t_spheric_rlm_data
!
      use gen_sph_grids_modes
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(ndomain_sph)
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(spheric_global_radius), intent(in) :: s3d_radius
      type(sph_local_parameters), intent(in) :: sph_lcp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rj
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      integer(kind = kint), intent(inout) :: icou
!
      type(sph_comm_tbl) :: comm_rlm_lc
!
      integer(kind = kint) :: jst, jed, j, jnod, k_tmp, j_tmp
      integer(kind = kint) :: k_glb, j_glb
      integer(kind = kint) :: ip1, jp, id_org_rank, ip_org
      integer(kind = kint) :: iflag_jp
!
!
      call set_local_idx_table_rj(sph_rj, sph_lcx)
!
      do ip1 = 1, ndomain_sph
        id_org_rank = mod((ip_rank+ip1),ndomain_sph)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, comm_rlm_mul(ip_org)%nneib_domain
          if(comm_rlm_mul(ip_org)%id_domain(jp) .eq. ip_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        call const_sph_rlm_modes (id_org_rank,                          &
     &     s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,          &
     &      sph_rlm, comm_rlm_lc)
!
        jst = comm_rlm_lc%istack_sr(iflag_jp-1)+1
        jed = comm_rlm_lc%istack_sr(iflag_jp)
        do j =  jst, jed
          icou = icou + 1
          jnod = comm_rlm_lc%item_sr(j)
          k_glb = sph_rlm%idx_global_rlm(jnod,1)
          j_glb = sph_rlm%idx_global_rlm(jnod,2)
          k_tmp = sph_lcx%idx_local_rj_r(k_glb)
          j_tmp = sph_lcx%idx_local_rj_j(j_glb)
          comm_rj%item_sr(icou) =  j_tmp                                &
     &                           + (k_tmp-1) * sph_rj%nidx_rj(2)
        end do
!
        call dealloc_type_sph_comm_item(comm_rlm_lc)
        call dealloc_type_sph_1d_index_rlm(sph_rlm)
        call dealloc_type_spheric_param_rlm(sph_rlm)
      end do
!
      end subroutine set_comm_table_4_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_comm_table_4_rtp(ip_rank, ndomain_sph, comm_rtm, &
     &          nneib_domain_rtp)
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rtm(ndomain_sph)
!
      integer(kind = kint), intent(inout) :: nneib_domain_rtp
!
      integer(kind = kint) :: ip1, jp, id_org_rank, ip_org
      integer(kind = kint) :: iflag_jp
!
!
      nneib_domain_rtp = 0
!
      do ip1 = 1, ndomain_sph
        id_org_rank = mod((ip_rank+ip1),ndomain_sph)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, comm_rtm(ip_org)%nneib_domain
          if(comm_rtm(ip_org)%id_domain(jp) .eq. ip_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        nneib_domain_rtp = nneib_domain_rtp + 1
        id_domain_tmp(nneib_domain_rtp) = id_org_rank
        nnod_sr_tmp(nneib_domain_rtp)                                   &
     &     =  comm_rtm(ip_org)%istack_sr(iflag_jp)                      &
     &      - comm_rtm(ip_org)%istack_sr(iflag_jp-1)
      end do
!
      end subroutine count_comm_table_4_rtp
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_table_4_rtp                                   &
     &         (ip_rank, ndomain_sph, comm_rtm_mul,                     &
     &          s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,     &
     &          sph_rtp, sph_rtm, comm_rtp, sph_lcx, icou)
!
      use gen_sph_grids_modes
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(ndomain_sph)
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(spheric_global_radius), intent(in) :: s3d_radius
      type(sph_local_parameters), intent(in) :: sph_lcp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
!
      integer(kind = kint), intent(inout) :: icou
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      type(sph_comm_tbl) :: comm_rtm_lc
!
      integer(kind = kint) :: jst, jed, j, jnod
      integer(kind = kint) :: k_tmp, l_tmp, m_tmp, k_glb, l_glb, m_glb
      integer(kind = kint) :: ip1, jp, id_org_rank, ip_org, iflag_jp
!
!
      call set_local_idx_table_rtp(sph_rtp, sph_lcx)
!
      do ip1 = 1, ndomain_sph
        id_org_rank = mod((ip_rank+ip1),ndomain_sph)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, comm_rtm_mul(ip_org)%nneib_domain
          if(comm_rtm_mul(ip_org)%id_domain(jp) .eq. ip_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        call const_sph_rtm_grids(id_org_rank,                           &
     &      s3d_ranks, s3d_radius, sph_lcp, stk_lc1d, sph_gl1d,         &
     &      sph_rtm, comm_rtm_lc)
!
        jst = comm_rtm_lc%istack_sr(iflag_jp-1)+1
        jed = comm_rtm_lc%istack_sr(iflag_jp)
        do j =  jst, jed
          icou = icou + 1
          jnod = comm_rtm_lc%item_sr(j)
          k_glb = sph_rtm%idx_global_rtm(jnod,1)
          l_glb = sph_rtm%idx_global_rtm(jnod,2)
          m_glb = sph_rtm%idx_global_rtm(jnod,3)
          k_tmp = sph_lcx%idx_local_rtp_r(k_glb)
          l_tmp = sph_lcx%idx_local_rtp_t(l_glb)
          m_tmp = sph_lcx%idx_local_rtp_p(m_glb)
          comm_rtp%item_sr(icou) =  k_tmp                               &
     &                             + (l_tmp-1) * sph_rtp%nidx_rtp(1)    &
     &                             + (m_tmp-1) * sph_rtp%nidx_rtp(1)    &
     &                                         * sph_rtp%nidx_rtp(2)
        end do
!
        call dealloc_type_sph_comm_item(comm_rtm_lc)
        call dealloc_type_sph_1d_index_rtm(sph_rtm)
        call dealloc_type_spheric_param_rtm(sph_rtm)
      end do
!
      end subroutine set_comm_table_4_rtp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_comm_stack_rtp_rj(nneib_domain, id_domain,         &
     &          istack_sr, ntot_item_sr)
!
      integer(kind = kint), intent(in) :: nneib_domain
      integer(kind = kint), intent(inout) :: ntot_item_sr
      integer(kind = kint), intent(inout) :: id_domain(nneib_domain)
      integer(kind = kint), intent(inout) :: istack_sr(0:nneib_domain)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, nneib_domain
        id_domain(ip) = id_domain_tmp(ip)
        istack_sr(ip) = istack_sr(ip-1) + nnod_sr_tmp(ip)
      end do
      ntot_item_sr = istack_sr(nneib_domain)
!
      end subroutine set_comm_stack_rtp_rj
!
! -----------------------------------------------------------------------
!
      end module set_comm_table_rtp_rj
