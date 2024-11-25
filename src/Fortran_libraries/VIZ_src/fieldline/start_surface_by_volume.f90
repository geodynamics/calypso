!>@file   start_surface_by_volume.f90
!!@brief  module start_surface_by_volume
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Element group list to set seed points
!!
!!@verbatim
!!      subroutine s_start_surface_by_volume(node, ele, ele_grp,        &
!!     &          nod_fld, fln_prm, fln_src, fln_tce)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: ele_grp
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!@endverbatim
!
      module start_surface_by_volume
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_control_params_4_fline
      use t_fline_seeds_ele_group
      use t_source_of_filed_line
      use t_tracing_data
!
      implicit  none
!
      private :: start_element_by_random, start_element_witout_random
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_start_surface_by_volume(node, ele, ele_grp,          &
     &          nod_fld, fln_prm, fln_src, fln_tce)
!
      use calypso_mpi_real
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      type(fieldline_seeds_ele_group) :: seed_ele_grp
!
      integer(kind = kint) :: i, ip
!
      real(kind = kreal) :: tot_flux_start, tot_flux_start_l
      real(kind = kreal) :: abs_flux_start, abs_flux_start_l
      real(kind = kreal) :: dencity_4_each_line
!
!
      call init_density_on_seed_ele(node, ele, ele_grp, nod_fld,        &
     &                              fln_prm, seed_ele_grp)
!
      abs_flux_start_l = 0.0d0
      tot_flux_start_l = 0.0d0
      do i = 1, seed_ele_grp%nele_seed
        abs_flux_start_l                                                &
     &          = abs_flux_start_l + abs(seed_ele_grp%density_seed(i))
        tot_flux_start_l                                                &
     &          = tot_flux_start_l + seed_ele_grp%density_seed(i)
      end do
!
      call calypso_mpi_allreduce_one_real                               &
     &   (tot_flux_start_l, tot_flux_start, MPI_SUM)
      call calypso_mpi_allgather_one_real                               &
     &   (abs_flux_start_l, fln_src%flux_stack_fline(1))
!
      fln_src%flux_stack_fline(0) = 0.0d0
      do ip = 1, nprocs
        fln_src%flux_stack_fline(ip)                                    &
     &                         = fln_src%flux_stack_fline(ip-1)         &
     &                          + fln_src%flux_stack_fline(ip)
      end do
      abs_flux_start = fln_src%flux_stack_fline(nprocs)
      dencity_4_each_line                                               &
     &      = abs_flux_start / dble(fln_prm%num_each_field_line)
!
      fln_tce%istack_current_fline(0) = 0
      do ip = 1, nprocs
        fln_tce%istack_current_fline(ip)                                &
     &     = nint((fln_src%flux_stack_fline(ip)                         &
     &      - fln_src%flux_stack_fline(ip-1)) / dencity_4_each_line)
      end do
      fln_src%num_line_local                                            &
     &     = fln_tce%istack_current_fline(my_rank+1)
!
      if(i_debug .gt. 0) then
        write(my_rank+50,*)  'abs_flux_start',                          &
     &                      abs_flux_start_l, abs_flux_start
        write(my_rank+50,*)  'tot_flux_start',                          &
     &                      tot_flux_start_l, tot_flux_start
        write(my_rank+50,*)  'original num_line_local',                 &
     &                      fln_src%num_line_local
        write(my_rank+50,*)  'dencity_4_each_line', dencity_4_each_line
      end if
!
      if(fln_src%num_line_local .gt. 0) then
        dencity_4_each_line                                             &
     &       = abs_flux_start_l / dble(fln_src%num_line_local)
      end if
      write(my_rank+50,*)  'adjusted dencity_4_each_line',              &
     &                     dencity_4_each_line
!
      if(fln_prm%num_each_field_line .gt. 0) then
        if(fln_prm%id_seed_distribution  .eq. iflag_no_random) then
          if(iflag_debug .gt. 0)                                        &
     &        write(*,*) 'start_element_witout_random'
          call start_element_witout_random                              &
     &       (seed_ele_grp, fln_src%num_line_local, abs_flux_start_l,   &
     &        fln_prm%num_each_field_line, fln_prm%id_surf_start_fline)
        else
          if(iflag_debug .gt. 0) write(*,*) 'start_element_by_random'
          call start_element_by_random                                  &
     &       (seed_ele_grp, fln_src%num_line_local, abs_flux_start_l,   &
     &        fln_prm%num_each_field_line, fln_prm%id_surf_start_fline)
        end if
      end if
!
     call dealloc_density_on_seed_ele(seed_ele_grp)
!
      end subroutine s_start_surface_by_volume
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine start_element_by_random                                &
     &         (seed_ele_grp, num_line_local, abs_flux_start_l,         &
     &          num_line, id_surf_start_fline)
!
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      type(fieldline_seeds_ele_group), intent(in) :: seed_ele_grp
      integer(kind = kint), intent(in) :: num_line_local
      real(kind = kreal), intent(in) :: abs_flux_start_l
!
      integer(kind = kint), intent(in) :: num_line
!
      integer(kind = kint), intent(inout)                               &
     &                          :: id_surf_start_fline(2,num_line)
!
      integer(kind = kint) :: i, inum
      real(kind = kreal) :: flux, flux_new
!
!
      real(kind = 8), allocatable :: r_rnd(:)
      real(kind = kreal), allocatable :: rnd_flux(:)
!
      integer(kind = 4) :: nRand = 2
      integer(kind = 4) ::  count, clock
      integer(kind = 4), allocatable :: seed(:)
!
!
      call random_seed(size = nRand)
!
      allocate(seed(nRand))
      allocate(r_rnd(num_line_local))
      allocate(rnd_flux(num_line_local))
!
      if(iflag_debug .gt. 0) write(*,*)  'system_clock', num_line_local
      call system_clock(count = clock)
      seed = clock
!
      if(num_line_local .gt. 0) then
        if(iflag_debug .gt. 0) write(*,*)  'random_seed'
        call random_seed(put = seed)
        if(iflag_debug .gt. 0) write(*,*)  'random_number'
        call random_number(r_rnd) 
        do i = 1, num_line_local
          rnd_flux(i) = r_rnd(i) * abs_flux_start_l
!
          flux = 0.0d0
          do inum = 1, seed_ele_grp%nele_seed
            flux_new = flux + abs(seed_ele_grp%density_seed(inum))
            if(rnd_flux(i) .gt. flux                                    &
     &           .and. rnd_flux(i) .le. flux_new) exit
            flux = flux_new
          end do
          id_surf_start_fline(1,i)                                      &
     &           = seed_ele_grp%iele_grp_seed_item(inum)
          id_surf_start_fline(2,i) = 0
        end do
      end if
!
      deallocate(rnd_flux, r_rnd, seed)
!
      end subroutine start_element_by_random
!
!  ---------------------------------------------------------------------
!
      subroutine start_element_witout_random                            &
     &         (seed_ele_grp, num_line_local, abs_flux_start_l,         &
     &          num_line, id_surf_start_fline)
!
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      integer(kind = kint), intent(in) :: num_line_local
      type(fieldline_seeds_ele_group), intent(in) :: seed_ele_grp
      real(kind = kreal), intent(in) :: abs_flux_start_l
      integer(kind = kint), intent(in) :: num_line
!
      integer(kind = kint), intent(inout)                               &
     &                          :: id_surf_start_fline(2,num_line)
!
      integer(kind = kint) :: icou, inum
      real(kind = kreal) :: flux, ref_flux
!
!
      if(num_line_local .le. 0) return
!
      ref_flux = abs_flux_start_l / dble(num_line_local+1)
      icou = 0
      flux = 0.0d0
      do inum = 1, seed_ele_grp%nele_seed
        flux = flux + abs(seed_ele_grp%density_seed(inum))
        if(flux .ge. ref_flux) then
          icou = icou + 1
          id_surf_start_fline(1,icou)                                  &
     &               = seed_ele_grp%iele_grp_seed_item(inum)
          id_surf_start_fline(2,icou) = 0
          flux = 0.0d0
        end if
        if(icou .ge. num_line_local) exit
      end do
!
      end subroutine start_element_witout_random
!
!  ---------------------------------------------------------------------
!
      end module start_surface_by_volume
