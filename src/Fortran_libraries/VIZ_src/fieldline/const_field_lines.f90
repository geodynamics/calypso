!const_field_lines.f90
!
!      module const_field_lines
!
!      Written by H. Matsui on Aug., 2011
!
!>@file   const_field_lines.f90
!!@brief  module const_field_lines
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine s_const_field_lines                                  &
!!     &         (node, ele, surf, ele_4_nod, nod_comm,                 &
!!     &          fln_prm, fln_src, fln_tce, fline_lc)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(communication_table), intent(in) :: nod_comm
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(in) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(local_fieldline), intent(inout) :: fline_lc
!!@endverbatim
!
      module const_field_lines
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
      private :: recover_local_fline_start
      private :: set_fline_start_2_bcast, set_fline_start_from_neib
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_field_lines                                    &
     &         (node, ele, surf, ele_4_nod, nod_comm,                   &
     &          fln_prm, fln_src, fln_tce, fline_lc)
!
      use t_control_params_4_fline
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_comm_table
      use t_next_node_ele_4_node
      use t_local_fline
      use t_source_of_filed_line
      use calypso_mpi_real
      use calypso_mpi_int
      use transfer_to_long_integers
      use extend_field_line
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(element_around_node), intent(in) :: ele_4_nod
      type(communication_table), intent(in) :: nod_comm
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(local_fieldline), intent(inout) :: fline_lc
!
      integer(kind = kint) :: iflag_comm
      integer(kind = kint) :: i, ist, ied, ip, nline, inum
      integer(kind = kint_gl) :: num64
      integer :: src_rank
!
!
      if(i_debug .gt. iflag_full_msg) then
        write(my_rank+50,*)                                             &
     &         'num_current_fline', fln_tce%num_current_fline(:)
        write(my_rank+50,*)                                             &
     &         'istack_current_fline', fln_tce%istack_current_fline(:)
        ist = fln_tce%istack_current_fline(my_rank) + 1
        ied = fln_tce%istack_current_fline(my_rank+1)
        write(my_rank+50,*) 'isf_fline_start(1:3,inum)'
        do inum = ist, ied
          write(my_rank+50,*) inum, fln_tce%isf_fline_start(1:3,inum)
        end do
      end if
      call calypso_MPI_barrier
!
      iflag_comm = 0
      call reset_fline_start(fline_lc)
!
      do
        ist = fln_tce%istack_current_fline(my_rank) + 1
        ied = fln_tce%istack_current_fline(my_rank+1)
        write(*,*) 'fln_tce%istack_current_fline', my_rank,             &
     &            fln_tce%istack_current_fline(my_rank:my_rank+1),      &
     &            fln_tce%num_current_fline(my_rank+1)
        do inum = ist, ied
          call s_extend_field_line(node, ele, surf,                     &
     &        fln_prm%max_line_stepping,                                &
     &        fln_prm%iflag_fline_used_ele,                             &
     &        fln_tce%iflag_fline(inum),                                &
     &        fln_src%vector_nod_fline, fln_src%color_nod_fline,        &
     &        fln_tce%isf_fline_start(1,inum),                          &
     &        fln_tce%xx_fline_start(1,inum),                           &
     &        fln_tce%v_fline_start(1,inum),                            &
     &        fln_tce%c_fline_start(inum), fln_tce%icount_fline(inum),  &
     &        iflag_comm, fline_lc)
          write(50+my_rank,*) 'extension end for ', inum, iflag_comm
!
          call set_fline_start_2_bcast(iflag_comm, inum, node%numnod,   &
     &          ele%numele, node%inod_global, ele%iele_global,          &
     &          nod_comm%num_neib, nod_comm%id_neib,                    &
     &          nod_comm%ntot_import,  nod_comm%istack_import,          &
     &          nod_comm%item_import, fln_tce)
        end do
        call calypso_MPI_barrier
!
        do ip = 1, nprocs
          src_rank = int(ip - 1)
          ist = fln_tce%istack_current_fline(ip-1)
          num64 = 9 * fln_tce%num_current_fline(ip)
          if(num64 .gt. 0) then
            call calypso_mpi_bcast_int                                  &
     &         (fln_tce%id_fline_export(1,ist+1), num64, src_rank)
            call calypso_mpi_bcast_real                                 &
     &         (fln_tce%fline_export(1,ist+1), num64, src_rank)
          end if
        end do
!
        if(iflag_debug .gt. 0) then
          write(my_rank+50,*) 'i, new_start_pe, id_fline_export'
          do i = 1, fln_tce%istack_current_fline(nprocs)
            write(my_rank+50,'(10i16)')                                 &
     &              i, fln_tce%id_fline_export(1:3,i)
          end do
        end if
!
        call recover_local_fline_start                                  &
     &     (node%numnod, ele%numele, surf%numsurf, ele%iele_global,     &
     &      surf%isf_4_ele, surf%iele_4_surf, ele_4_nod%ntot,           &
     &      ele_4_nod%istack_4_node, ele_4_nod%iele_4_node,             &
     &      nod_comm%num_neib, nod_comm%id_neib, nod_comm%ntot_export,  &
     &      nod_comm%istack_export, nod_comm%item_export, fln_tce)
        call set_fline_start_from_neib(fln_tce)
!
        nline = fln_tce%istack_current_fline(nprocs)                    &
     &         - fln_tce%istack_current_fline(0)
        if(i_debug .gt. 0) then
          write(my_rank+50,*) 'istack_current_fline',                   &
     &                       fln_tce%istack_current_fline(:)
!
          write(my_rank+50,*) 'number of lines: ', nline
          write(*,*) 'number of lines: ', my_rank, nline
        end if
        if(nline .le. 0) exit
      end do
!
!      call check_local_fline_dx( (my_rank+60), fline_lc)
!
      end subroutine s_const_field_lines
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_2_bcast(iflag_comm, i,                 &
     &          numnod, numele, inod_global, iele_global,               &
     &          num_neib, id_neib, ntot_import, istack_import,          &
     &          item_import, fln_tce)
!
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: iflag_comm, i
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      integer(kind = kint_gl), intent(in) :: iele_global(numele)
!
      integer(kind = kint), intent(in) :: num_neib, ntot_import
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in) :: item_import(ntot_import)
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: inod, iele, isf, ip, ist, ied, inum
!
!
      if(iflag_comm .eq. ione) then
        iele = fln_tce%isf_fline_start(1,i)
        isf =  fln_tce%isf_fline_start(2,i)
        inod = fln_tce%isf_fline_start(3,i)
        do ip = 1, num_neib
          ist = istack_import(ip-1) + 1
          ied = istack_import(ip)
          do inum = ist, ied
            if(item_import(inum) .eq. inod) then
              fln_tce%id_fline_export(1,i) = id_neib(ip)
              fln_tce%id_fline_export(7,i) = 1 + inum - ist
              exit
            end if
          end do
        end do
!
        fln_tce%id_fline_export(2,i) = fln_tce%iflag_fline(i)
        fln_tce%id_fline_export(3,i) = fln_tce%icount_fline(i)
        fln_tce%id_fline_export(4,i) = int(iele_global(iele))
        fln_tce%id_fline_export(5,i) = isf
        fln_tce%id_fline_export(6,i) = int(inod_global(inod))
!
        fln_tce%fline_export(1:4,i) = fln_tce%xx_fline_start(1:4,i)
        fln_tce%fline_export(5:8,i) = fln_tce%v_fline_start(1:4,i)
        fln_tce%fline_export(9,i) = fln_tce%c_fline_start(i)
      else
        fln_tce%id_fline_export(1,i) =   -ione
        fln_tce%id_fline_export(2:7,i) = izero
        fln_tce%fline_export(1:9,i) =     zero
      end if
!
      end subroutine set_fline_start_2_bcast
!
!  ---------------------------------------------------------------------
!
      subroutine recover_local_fline_start                              &
     &         (numnod, numele, numsurf, iele_global,                   &
     &          isf_4_ele, iele_4_surf, ntot_ele_4_node,                &
     &          iele_stack_4_node, iele_4_node, num_neib, id_neib,      &
     &          ntot_export, istack_export, item_export, fln_tce)
!
      use m_geometry_constants
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer (kind=kint_gl), intent(in) :: iele_global(numele)
      integer (kind=kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer (kind=kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      integer (kind=kint), intent(in) :: ntot_ele_4_node
      integer (kind=kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer (kind=kint), intent(in) :: iele_4_node(ntot_ele_4_node)
!
      integer(kind = kint), intent(in) :: num_neib, ntot_export
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in) :: item_export(ntot_export)
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: ip, ip_org, ist_lin, ied_lin, i
      integer(kind = kint) :: inum, inod, ist_ele, ied_ele, jnum, jele
      integer(kind = kint) :: isf, isurf
!
!
      do ip = 1, num_neib
        ip_org = id_neib(ip) + 1
!
        ist_lin = fln_tce%istack_current_fline(ip_org-1) + 1
        ied_lin = fln_tce%istack_current_fline(ip_org)
        do i = ist_lin, ied_lin
          if(fln_tce%id_fline_export(1,i) .eq. my_rank) then
            inum = fln_tce%id_fline_export(7,i) + istack_export(ip-1)
            inod = item_export(inum)
!            write(60+my_rank,*) 'recover node', inod,                  &
!     &           inod_global(inod),fln_tce%id_fline_export(6,i)
!
            fln_tce%id_fline_export(6,i) = inod
            ist_ele = iele_stack_4_node(inod-1) + 1
            ied_ele = iele_stack_4_node(inod)
            do jnum = ist_ele, ied_ele
              jele = iele_4_node(jnum)
              if(iele_global(jele)                                      &
     &               .eq. fln_tce%id_fline_export(4,i)) then
!                write(60+my_rank,*) 'recover ele',                     &
!      &                      jele, iele_global(jele)
                isf =  fln_tce%id_fline_export(5,i)
                isurf = abs(isf_4_ele(jele,isf))
!
                if(isf_4_ele(jele,isf) .lt. 0) then
                  fln_tce%id_fline_export(4,i) = iele_4_surf(isurf,1,1)
                  fln_tce%id_fline_export(5,i) = iele_4_surf(isurf,1,2)
                else
                  fln_tce%id_fline_export(4,i) = iele_4_surf(isurf,2,1)
                  fln_tce%id_fline_export(5,i) = iele_4_surf(isurf,2,2)
                end if
!                write(60+my_rank,*) 'recover surf',                    &
!     &                         fln_tce%id_fline_export(4:5,i)
!
                exit
              end if
            end do
!
          end if
        end do
      end do
!
      end subroutine recover_local_fline_start
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_from_neib(fln_tce)
!
      use calypso_mpi_int
      use t_source_of_filed_line
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: ied_lin, i, icou, ip
!
!
      ied_lin = fln_tce%istack_current_fline(nprocs)
      icou = 0
      do i = 1, ied_lin
        if(fln_tce%id_fline_export(1,i) .eq. my_rank) then
          icou = icou + 1
        end if
      end do
!
      call calypso_mpi_allgather_one_int                                &
     &   (icou, fln_tce%num_current_fline)
!
      do ip = 1, nprocs
        fln_tce%istack_current_fline(ip)                                &
     &                   = fln_tce%istack_current_fline(ip-1)           &
     &                    + fln_tce%num_current_fline(ip)
      end do
!
      icou =   fln_tce%istack_current_fline(my_rank)
      do i = 1, ied_lin
        if(fln_tce%id_fline_export(1,i) .eq. my_rank) then
          icou = icou + 1
          fln_tce%iflag_fline(icou) =  fln_tce%id_fline_export(2,i)
          fln_tce%icount_fline(icou) = fln_tce%id_fline_export(3,i)
          fln_tce%isf_fline_start(1:3,icou)                             &
     &         = fln_tce%id_fline_export(4:6,i)
!
          fln_tce%xx_fline_start(1:4,icou)                              &
     &         = fln_tce%fline_export(1:4,i)
          fln_tce%v_fline_start(1:4,icou) = fln_tce%fline_export(5:8,i)
          fln_tce%c_fline_start(icou) = fln_tce%fline_export(9,i)
        end if
      end do
!
      end subroutine set_fline_start_from_neib
!
!  ---------------------------------------------------------------------
!
      end module const_field_lines
