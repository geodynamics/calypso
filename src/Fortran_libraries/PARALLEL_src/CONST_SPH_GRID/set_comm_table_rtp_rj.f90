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
!!     &         (id_rank, num_pe, comm_rlm_mul, gen_sph,               &
!!     &          sph_params, sph_rtp, sph_rj, comm_rj_lc, sph_grp_lc)
!!        type(sph_comm_tbl), intent(in) :: comm_rlm_mul(ndomain_sph)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!        type(sph_comm_tbl), intent(inout) :: comm_rj_lc
!!        type(sph_group_data), intent(inout) :: sph_grp_lc
!!      subroutine const_sph_rtp_grids(id_rank, num_pe, comm_rtm_mul,   &
!!     &          gen_sph, sph_params, sph_rtp, comm_rtp_lc, sph_grp_lc)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_comm_tbl), intent(in) :: comm_rtm_mul(ndomain_sph)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_comm_tbl), intent(inout) :: comm_rtp_lc
!!        type(sph_group_data), intent(inout) :: sph_grp_lc
!!@endverbatim
!
      module set_comm_table_rtp_rj
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_group
      use t_spheric_data_IO
      use t_sph_mesh_1d_connect
      use t_sph_local_index
      use t_const_spherical_grid
!
      implicit none
!
      integer(kind = kint), allocatable :: id_domain_tmp(:)
      integer(kind = kint), allocatable :: nnod_sr_tmp(:)
!
      type(sph_local_1d_index_rtp), private :: sph_lcx_rtp
      type(sph_local_1d_index_rj), private :: sph_lcx_rj
!
      private :: id_domain_tmp, nnod_sr_tmp
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
      subroutine allocate_domain_sr_tmp(num_pe)
!
      integer, intent(in) :: num_pe
!
      allocate( id_domain_tmp(num_pe) )
      allocate( nnod_sr_tmp(num_pe) )
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
     &         (id_rank, num_pe, comm_rlm_mul, gen_sph,                 &
     &          sph_params, sph_rtp, sph_rj, comm_rj_lc, sph_grp_lc)
!
      use calypso_mpi
      use set_sph_groups
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_sphere_by_global
      use set_loaded_data_4_sph
!
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(num_pe)
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(sph_rj_grid), intent(inout) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: comm_rj_lc
      type(sph_group_data), intent(inout) :: sph_grp_lc
!
!
      call copy_gl_2_local_rj_param(id_rank,                            &
     &    gen_sph%s3d_ranks, gen_sph%sph_lcp, gen_sph%stk_lc1d, sph_rj)
!
      call add_center_mode_rj(id_rank, gen_sph%sph_gl1d, sph_rj)
!      nnod_rj = sph_rj%nnod_rj
!      nidx_rj(1:2) = sph_rj%nidx_rj(1:2)
!
      call alloc_spheric_param_rj(sph_rj)
      call alloc_sph_1d_index_rj(sph_rj)
!
      call copy_sph_1d_gl_idx_rj(gen_sph%s3d_radius,                    &
     &                           gen_sph%sph_gl1d, sph_rj)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'set_global_sph_rj_id', id_rank
      call set_global_sph_rj_id(sph_rj)
      call count_interval_4_each_dir(itwo, sph_rj%nnod_rj,              &
     &    sph_rj%idx_global_rj, sph_rj%istep_rj)
!
      if(iflag_debug .gt. 0) then
        call check_spheric_param_rj(id_rank, sph_rj)
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'const_comm_table_4_rj', id_rank
      call const_comm_table_4_rj                                        &
     &   (id_rank, num_pe, comm_rlm_mul, gen_sph,                       &
     &    sph_rtp%nidx_global_rtp(1), sph_params, sph_rj, comm_rj_lc)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                  'set_sph_rj_groups', id_rank
      call set_sph_rj_groups                                            &
     &   (sph_params, sph_rj, gen_sph%added_radial_grp,                 &
     &    sph_grp_lc%radial_rj_grp, sph_grp_lc%sphere_rj_grp)
!
      end subroutine const_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine const_sph_rtp_grids(id_rank, num_pe, comm_rtm_mul,     &
     &          gen_sph, sph_params, sph_rtp, comm_rtp_lc, sph_grp_lc)
!
      use set_sph_groups
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_sphere_by_global
      use set_loaded_data_4_sph
!
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(num_pe)
      type(construct_spherical_grid), intent(in) :: gen_sph
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtp_lc
      type(sph_group_data), intent(inout) :: sph_grp_lc
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                'copy_gl_2_local_rtp_param', id_rank
      call copy_gl_2_local_rtp_param(id_rank, gen_sph%s3d_ranks,        &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, sph_rtp)
!
      call alloc_spheric_param_rtp(sph_rtp)
      call alloc_sph_1d_index_rtp(sph_rtp)
!
      call copy_sph_1d_gl_idx_rtp(gen_sph%s3d_radius,                   &
     &                            gen_sph%sph_gl1d, sph_rtp)
!
      if(gen_sph%s3d_ranks%rtp_rin_flag) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                 'set_global_sph_rtp_id', id_rank
        call set_global_sph_rtp_id(sph_rtp)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                 'set_global_sph_prt_id', id_rank
        call set_global_sph_prt_id(sph_rtp)
      end if
      call count_interval_4_each_dir(ithree, sph_rtp%nnod_rtp,          &
     &    sph_rtp%idx_global_rtp, sph_rtp%istep_rtp)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'check_spheric_param_rtp', id_rank
        call check_spheric_param_rtp(id_rank, sph_rtp)
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'const_comm_table_4_rtp', id_rank
      call const_comm_table_4_rtp(id_rank, num_pe, comm_rtm_mul,        &
     &    gen_sph, sph_params, sph_rtp, comm_rtp_lc)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_rtp_groups', id_rank
      call set_sph_rtp_groups                                           &
     &   (sph_params, sph_rtp, gen_sph%added_radial_grp,                &
     &    gen_sph%r_layer_grp, gen_sph%med_layer_grp,                   &
     &    sph_grp_lc%bc_rtp_grp, sph_grp_lc%radial_rtp_grp,             &
     &    sph_grp_lc%theta_rtp_grp, sph_grp_lc%zonal_rtp_grp)
!
      end subroutine const_sph_rtp_grids
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rj                                  &
     &         (id_rank, num_pe, comm_rlm_mul, gen_sph,                 &
     &          nidx_global_rtp_r, sph_params, sph_rj, comm_rj)
!
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(num_pe)
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nidx_global_rtp_r
!
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
      integer(kind = kint) :: icou
!
      call allocate_domain_sr_tmp(num_pe)
!
      comm_rj%nneib_domain = 0
      call count_comm_table_4_rj                                        &
     &   (id_rank, num_pe, comm_rlm_mul, comm_rj)
!
      call alloc_sph_comm_stack(comm_rj)
!
      call set_comm_stack_rtp_rj                                        &
     &   (comm_rj%nneib_domain, comm_rj%id_domain,                      &
     &    comm_rj%istack_sr, comm_rj%ntot_item_sr)
!
      call deallocate_domain_sr_tmp
      call alloc_sph_comm_item(sph_rj%nnod_rj, comm_rj)
!
      icou = 0
      call set_comm_table_4_rj                                          &
     &   (id_rank, num_pe, comm_rlm_mul, gen_sph,                       &
     &    nidx_global_rtp_r, sph_params, sph_rj, comm_rj, icou)
!
      end subroutine const_comm_table_4_rj
!
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rtp(id_rank, num_pe, comm_rtm_mul,  &
     &          gen_sph, sph_params, sph_rtp, comm_rtp)
!
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(num_pe)
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(sph_shell_parameters), intent(in) :: sph_params
!
      type(sph_comm_tbl), intent(inout) :: comm_rtp
!
      integer(kind = kint) :: icou
!
!
      call allocate_domain_sr_tmp(num_pe)
!
      call count_comm_table_4_rtp(id_rank, num_pe, comm_rtm_mul,        &
     &    comm_rtp%nneib_domain)
!
      call alloc_sph_comm_stack(comm_rtp)
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
      call alloc_sph_comm_item(sph_rtp%nnod_rtp, comm_rtp)
!
      icou = 0
      call set_comm_table_4_rtp(id_rank, num_pe, comm_rtm_mul,          &
     &    gen_sph, sph_params, sph_rtp, comm_rtp, icou)
!
      end subroutine const_comm_table_4_rtp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_comm_table_4_rj                                  &
     &         (id_rank, num_pe, comm_rlm, comm_rj)
!
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
      type(sph_comm_tbl), intent(in) :: comm_rlm(num_pe)
!
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
      integer(kind = kint) :: jp
      integer(kind = kint) :: iflag_jp
      integer :: id_org_rank, ip_org, ip1
!
!
      do ip1 = 1, num_pe
        id_org_rank = mod((id_rank+ip1),num_pe)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, comm_rlm(ip_org)%nneib_domain
          if(comm_rlm(ip_org)%id_domain(jp) .eq. id_rank) then
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
     &         (id_rank, num_pe, comm_rlm_mul,  gen_sph,                &
     &          nidx_global_rtp_r, sph_params, sph_rj, comm_rj, icou)
!
      use t_spheric_rlm_data
      use gen_sph_grids_modes
      use set_global_spherical_param
!
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(num_pe)
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      integer(kind = kint), intent(in) :: nidx_global_rtp_r
!
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
      integer(kind = kint), intent(inout) :: icou
!
      type(sph_rlm_grid) :: sph_rlm_lc
      type(sph_comm_tbl) :: comm_rlm_lc
!
      integer(kind = kint) :: jst, jed, j, jnod, k_tmp, j_tmp
      integer(kind = kint) :: k_glb, j_glb
      integer(kind = kint) :: jp
      integer(kind = kint) :: iflag_jp
      integer :: id_org_rank, ip_org, ip1
!
!
      call set_global_rlm_resolution                                    &
     &   (sph_params%l_truncation, sph_params%m_folding,                &
     &    nidx_global_rtp_r, sph_rlm_lc)
      call init_local_idx_table_rj(sph_rj, sph_lcx_rj)
!
      do ip1 = 1, num_pe
        id_org_rank = int(mod((id_rank+ip1),num_pe))
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, comm_rlm_mul(ip_org)%nneib_domain
          if(comm_rlm_mul(ip_org)%id_domain(jp) .eq. id_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
!        sph_rlm_lc%nidx_rlm(1) = 17
!        sph_rlm_lc%nidx_rlm(2) = 16384
!        call alloc_sph_1d_index_rlm(sph_rlm_lc)
!      if(id_rank .eq. 0) write(*,*) 'nidx_rlm', sph_rlm_lc%nidx_rlm(:)
!        call dealloc_sph_1d_index_rlm(sph_rlm_lc)
!
        call const_sph_rlm_modes(id_org_rank, gen_sph,                  &
     &                           sph_rlm_lc, comm_rlm_lc)
!
        jst = comm_rlm_lc%istack_sr(iflag_jp-1)+1
        jed = comm_rlm_lc%istack_sr(iflag_jp)
        do j =  jst, jed
          icou = icou + 1
          jnod = comm_rlm_lc%item_sr(j)
          k_glb = sph_rlm_lc%idx_global_rlm(jnod,1)
          j_glb = sph_rlm_lc%idx_global_rlm(jnod,2)
          k_tmp = sph_lcx_rj%idx_local_rj_r(k_glb)
          j_tmp = sph_lcx_rj%idx_local_rj_j(j_glb)
          comm_rj%item_sr(icou) =  1 + (k_tmp-1) * sph_rj%istep_rj(1)   &
     &                               + (j_tmp-1) * sph_rj%istep_rj(2)
        end do
!
        call dealloc_sph_comm_item(comm_rlm_lc)
        call dealloc_sph_1d_index_rlm(sph_rlm_lc)
        call dealloc_spheric_param_rlm(sph_rlm_lc)
      end do
      call dealloc_rj_1d_local_idx(sph_lcx_rj)
!
      end subroutine set_comm_table_4_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_comm_table_4_rtp(id_rank, num_pe, comm_rtm,      &
     &          nneib_domain_rtp)
!
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
      type(sph_comm_tbl), intent(in) :: comm_rtm(num_pe)
!
      integer(kind = kint), intent(inout) :: nneib_domain_rtp
!
      integer(kind = kint) :: jp
      integer(kind = kint) :: iflag_jp
      integer :: id_org_rank, ip_org, ip1
!
!
      nneib_domain_rtp = 0
!
      do ip1 = 1, num_pe
        id_org_rank = int(mod((id_rank+ip1),num_pe))
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, comm_rtm(ip_org)%nneib_domain
          if(comm_rtm(ip_org)%id_domain(jp) .eq. id_rank) then
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
      subroutine set_comm_table_4_rtp(id_rank, num_pe, comm_rtm_mul,    &
     &          gen_sph, sph_params, sph_rtp, comm_rtp, icou)
!
      use gen_sph_grids_modes
      use set_global_spherical_param
!
      integer, intent(in) :: id_rank
      integer, intent(in) :: num_pe
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(num_pe)
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(sph_shell_parameters), intent(in) :: sph_params
!
      integer(kind = kint), intent(inout) :: icou
      type(sph_comm_tbl), intent(inout) :: comm_rtp
!
      type(sph_rtm_grid) :: sph_rtm_lc
      type(sph_comm_tbl) :: comm_rtm_lc
!
      integer(kind = kint) :: jst, jed, j, jnod
      integer(kind = kint) :: k_tmp, l_tmp, m_tmp, k_glb, l_glb, m_glb
      integer(kind = kint) :: jp, iflag_jp
      integer :: id_org_rank, ip_org, ip1
!
!
      call set_global_rtm_resolution                                    &
     &   (sph_params%l_truncation, sph_params%m_folding,                &
     &    sph_rtp, sph_rtm_lc)
      call init_local_idx_table_rtp(sph_rtp, sph_lcx_rtp)
!
      do ip1 = 1, num_pe
        id_org_rank = int(mod((id_rank+ip1),num_pe))
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, comm_rtm_mul(ip_org)%nneib_domain
          if(comm_rtm_mul(ip_org)%id_domain(jp) .eq. id_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        call const_sph_rtm_grids(id_org_rank, gen_sph,                  &
     &                           sph_rtm_lc, comm_rtm_lc)
!
        jst = comm_rtm_lc%istack_sr(iflag_jp-1)+1
        jed = comm_rtm_lc%istack_sr(iflag_jp)
        do j =  jst, jed
          icou = icou + 1
          jnod = comm_rtm_lc%item_sr(j)
          k_glb = sph_rtm_lc%idx_global_rtm(jnod,1)
          l_glb = sph_rtm_lc%idx_global_rtm(jnod,2)
          m_glb = sph_rtm_lc%idx_global_rtm(jnod,3)
          k_tmp = sph_lcx_rtp%idx_local_rtp_r(k_glb)
          l_tmp = sph_lcx_rtp%idx_local_rtp_t(l_glb)
          m_tmp = sph_lcx_rtp%idx_local_rtp_p(m_glb)
          comm_rtp%item_sr(icou) = 1 + (k_tmp-1) * sph_rtp%istep_rtp(1)  &
     &                               + (l_tmp-1) * sph_rtp%istep_rtp(2)  &
     &                               + (m_tmp-1) * sph_rtp%istep_rtp(3)
        end do
!
        call dealloc_sph_comm_item(comm_rtm_lc)
        call dealloc_sph_1d_index_rtm(sph_rtm_lc)
        call dealloc_spheric_param_rtm(sph_rtm_lc)
      end do
      call dealloc_rtp_1d_local_idx(sph_lcx_rtp)
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
