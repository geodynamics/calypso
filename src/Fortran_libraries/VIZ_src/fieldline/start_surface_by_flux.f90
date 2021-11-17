!start_surface_by_flux.f90
!
!      module start_surface_by_flux
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_start_surface_by_flux(node, ele, surf,             &
!!     &          fln_prm, fln_src, fln_tce)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!
      module start_surface_by_flux
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
      use t_source_of_filed_line
!
      implicit  none
!
      private :: cal_flux_for_1sgrp, cal_area_for_1sgrp
      private :: start_surface_by_random, start_surface_witout_random
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_start_surface_by_flux(node, ele, surf,               &
     &          fln_prm, fln_src, fln_tce)
!
      use calypso_mpi_real
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i, ip
!
      real(kind = kreal) :: tot_flux_start, tot_flux_start_l
      real(kind = kreal) :: abs_flux_start, abs_flux_start_l
      real(kind = kreal) :: flux_4_each_line
!
!
      if(     fln_prm%id_seed_distribution .eq. iflag_random_by_area    &
     &   .or. fln_prm%id_seed_distribution .eq. iflag_no_random) then
        if(iflag_debug .gt. 0) write(*,*) 'cal_area_for_1sgrp'
        call cal_area_for_1sgrp(ele%numele, surf%numsurf,               &
     &      surf%isf_4_ele, ele%interior_ele, surf%area_surf,           &
     &      fln_src%nele_start_grp, fln_src%iele_start_item,            &
     &      fln_src%flux_start)
      else
        if(iflag_debug .gt. 0) write(*,*) 'cal_flux_for_1sgrp'
        call cal_flux_for_1sgrp(node%numnod, ele%numele, surf%numsurf,  &
     &      surf%nnod_4_surf, surf%ie_surf, surf%isf_4_ele,             &
     &      ele%interior_ele, surf%vnorm_surf, surf%area_surf,          &
     &      fln_src%nele_start_grp, fln_src%iele_start_item,            &
     &      fln_src%vector_nod_fline, fln_src%flux_start)
      end if
!
      abs_flux_start_l = 0.0d0
      tot_flux_start_l = 0.0d0
      do i = 1, fln_src%nele_start_grp
        abs_flux_start_l                                                &
     &            = abs_flux_start_l + abs(fln_src%flux_start(i))
        tot_flux_start_l                                                &
     &            = tot_flux_start_l + fln_src%flux_start(i)
      end do
!
      call calypso_mpi_allreduce_one_real                               &
     &   (tot_flux_start_l, tot_flux_start, MPI_SUM)
      call calypso_mpi_allgather_one_real                               &
     &   (abs_flux_start_l, fln_tce%flux_stack_fline(1))
!
      fln_tce%flux_stack_fline(0) = 0.0d0
      do ip = 1, nprocs
        fln_tce%flux_stack_fline(ip)                                    &
     &                         = fln_tce%flux_stack_fline(ip-1)         &
     &                          + fln_tce%flux_stack_fline(ip)
      end do
      abs_flux_start = fln_tce%flux_stack_fline(nprocs)
      flux_4_each_line                                                  &
     &      = abs_flux_start / dble(fln_prm%num_each_field_line)
!
      do ip = 1, nprocs
        fln_tce%num_current_fline(ip)                                   &
     &     = nint((fln_tce%flux_stack_fline(ip)                         &
     &      - fln_tce%flux_stack_fline(ip-1)) / flux_4_each_line)
      end do
      fln_src%num_line_local                                            &
     &     = fln_tce%num_current_fline(my_rank+1)
!
      if(i_debug .gt. 0) then
        write(my_rank+50,*)  'abs_flux_start',                          &
     &                      abs_flux_start_l, abs_flux_start
        write(my_rank+50,*)  'tot_flux_start',                          &
     &                      tot_flux_start_l, tot_flux_start
        write(my_rank+50,*)  'original num_line_local',                 &
     &                      fln_src%num_line_local
        write(my_rank+50,*)  'flux_4_each_line', flux_4_each_line
      end if
!
      if(fln_src%num_line_local .gt. 0) then
        flux_4_each_line                                                &
     &       = abs_flux_start_l / dble(fln_src%num_line_local)
      end if
      write(my_rank+50,*)  'adjusted flux_4_each_line',                 &
     &                     flux_4_each_line
!
      if(fln_prm%num_each_field_line .gt. 0) then
        if(fln_prm%id_seed_distribution  .eq. iflag_no_random) then
          if(iflag_debug .gt. 0)                                        &
     &        write(*,*) 'start_surface_witout_random'
          call start_surface_witout_random(fln_src, abs_flux_start_l,   &
     &        fln_prm%num_each_field_line, fln_prm%id_surf_start_fline)
        else
          if(iflag_debug .gt. 0) write(*,*) 'start_surface_by_random'
          call start_surface_by_random(fln_src, abs_flux_start_l,       &
     &        fln_prm%num_each_field_line, fln_prm%id_surf_start_fline)
        end if
      end if
!
!
      end subroutine s_start_surface_by_flux
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_flux_for_1sgrp(numnod, numele, numsurf,            &
     &          nnod_4_surf, ie_surf, isf_4_ele, interior_ele,          &
     &          vnorm_surf, area_surf, num_sgrp,                        &
     &          isurf_grp, d_nod, flux)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: interior_ele(numele)
      real(kind = kreal), intent(in) :: vnorm_surf(numsurf,3)
      real(kind = kreal), intent(in) :: area_surf(numsurf)
      real(kind = kreal), intent(in) :: d_nod(numnod,3)
!
      real(kind = kreal), intent(inout) :: flux(num_sgrp)
!
      integer (kind = kint) :: iele, isf, isurf, inum
      integer (kind = kint) :: i1,  i2,  i3,  i4
      real(kind = kreal) :: sign_surf, d_surf(3)
!
!
      flux(1:num_sgrp) = 0.0d0
!
!$omp  parallel do                                                      &
!$omp& private(inum,iele,isf,isurf,sign_surf,i1,i2,i3,i4,d_surf)
!$cdir nodep
      do inum = 1, num_sgrp
        iele = isurf_grp(1,inum)
        isf =  isurf_grp(2,inum)
        isurf = abs(isf_4_ele(iele,isf))
        sign_surf = dble(isf_4_ele(iele,isf) / isurf)
!
        i1 =  ie_surf(isurf, 1)
        i2 =  ie_surf(isurf, 2)
        i3 =  ie_surf(isurf, 3)
        i4 =  ie_surf(isurf, 4)
!
        d_surf(1) = quad * (d_nod(i1,1) + d_nod(i2,1)                   &
     &                    + d_nod(i3,1) + d_nod(i4,1))
        d_surf(2) = quad * (d_nod(i1,2) + d_nod(i2,2)                   &
     &                    + d_nod(i3,2) + d_nod(i4,2))
        d_surf(3) = quad * (d_nod(i1,3) + d_nod(i2,3)                   &
     &                    + d_nod(i3,3) + d_nod(i4,3))
!
        flux(inum) = flux(inum) + ( vnorm_surf(isurf,1) * d_surf(1)     &
     &                            + vnorm_surf(isurf,2) * d_surf(2)     &
     &                            + vnorm_surf(isurf,3) * d_surf(3) )   &
     &              * area_surf(isurf) * sign_surf                      &
     &              * dble(interior_ele(iele))
      end do
!$omp end parallel do
!
      end subroutine cal_flux_for_1sgrp
!
!  ---------------------------------------------------------------------
!
      subroutine cal_area_for_1sgrp                                     &
     &         (numele, numsurf, isf_4_ele, interior_ele, area_surf,    &
     &          num_sgrp, isurf_grp, flux)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numele, numsurf
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: interior_ele(numele)
      real(kind = kreal), intent(in) :: area_surf(numsurf)
!
      real(kind = kreal), intent(inout) :: flux(num_sgrp)
!
      integer (kind = kint) :: iele, isf, isurf, inum
!
!
      flux(1:num_sgrp) = 0.0d0
!
!$omp  parallel do private(inum,iele,isf,isurf)
      do inum = 1, num_sgrp
        iele = isurf_grp(1,inum)
        isf =  isurf_grp(2,inum)
        isurf = abs(isf_4_ele(iele,isf))
!
        flux(inum) = flux(inum)                                         &
     &              + area_surf(isurf)  * dble(interior_ele(iele))
      end do
!$omp end parallel do
!
      end subroutine cal_area_for_1sgrp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine start_surface_by_random(fln_src, abs_flux_start_l,     &
     &          num_line, id_surf_start_fline)
!
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      type(each_fieldline_source), intent(in) :: fln_src
      real(kind = kreal), intent(in) :: abs_flux_start_l
!
      integer(kind = kint), intent(in) :: num_line
!
      integer(kind = kint), intent(inout)                               &
     &                          :: id_surf_start_fline(2,num_line)
!
      integer(kind = kint) :: i, inum, num
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
      write(my_rank+50,*)  'random_seed',                               &
     &                      nRand, fln_src%num_line_local
      call random_seed(size = nRand)
!
      num = fln_src%num_line_local
      allocate(seed(nRand))
      allocate(r_rnd(num))
      allocate(rnd_flux(num))
!
      if(iflag_debug .gt. 0) write(*,*)  'system_clock', num
      call system_clock(count = clock)
      seed = clock
!
      if(num .gt. 0) then
        if(iflag_debug .gt. 0) write(*,*)  'random_seed'
        call random_seed(put = seed)
        if(iflag_debug .gt. 0) write(*,*)  'random_number'
        call random_number(r_rnd) 
        do i = 1, fln_src%num_line_local
          rnd_flux(i) = r_rnd(i) * abs_flux_start_l
!
          flux = 0.0d0
          do inum = 1, fln_src%nele_start_grp
            flux_new = flux + abs(fln_src%flux_start(inum))
            if(rnd_flux(i) .gt. flux                                    &
     &           .and. rnd_flux(i) .le. flux_new) exit
            flux = flux_new
          end do
          id_surf_start_fline(1,i) = fln_src%iele_start_item(1,inum)
          id_surf_start_fline(2,i) = fln_src%iele_start_item(2,inum)
        end do
      end if
!
      deallocate(rnd_flux, r_rnd, seed)
!
      end subroutine start_surface_by_random
!
!  ---------------------------------------------------------------------
!
      subroutine start_surface_witout_random(fln_src, abs_flux_start_l, &
     &          num_line, id_surf_start_fline)
!
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      type(each_fieldline_source), intent(in) :: fln_src
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
      if(fln_src%num_line_local .le. 0) return
!
      ref_flux = abs_flux_start_l / dble(fln_src%num_line_local+1)
      icou = 0
      flux = 0.0d0
      do inum = 1, fln_src%nele_start_grp
        flux = flux + abs(fln_src%flux_start(inum))
        if(flux .ge. ref_flux) then
          icou = icou + 1
          id_surf_start_fline(1,icou)                                  &
     &               = fln_src%iele_start_item(1,inum)
          id_surf_start_fline(2,icou)                                  &
     &               = fln_src%iele_start_item(2,inum)
          flux = 0.0d0
        end if
        if(icou .ge. fln_src%num_line_local) exit
      end do
!
      write(*,*) 'icou', my_rank, icou, num_line, fln_src%num_line_local
      write(50+my_rank,*) 'nele_start_grp', fln_src%nele_start_grp
      write(50+my_rank,*) 'id_surf_start_fline', id_surf_start_fline(1,:)
!
!
      end subroutine start_surface_witout_random
!
!  ---------------------------------------------------------------------
!
      end module start_surface_by_flux
