!
!      module write_diff_4_comm_test
!
!!     subroutine output_diff_node_comm_test(file_name, nod_check)
!!     subroutine output_diff_mesh_comm_test(file_name,                 &
!!    &          nod_check, ele_check, surf_check, edge_check)
!!      subroutine write_diff_comm_test                                 &
!!     &         (istack_diff_pe, id_diff_IO, x_diff_IO)
!
!     Written by H. Matsui on Sep., 2007
!
      module write_diff_4_comm_test
!
      use m_precision
!
      use calypso_mpi
      use t_work_for_comm_check
!
      implicit  none
!
!
      integer(kind = kint), parameter :: id_comm_test = 31
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_diff_node_comm_test(file_name, nod_check)
!
      character(len=kchara), intent(in) :: file_name
      type(work_for_comm_check), intent(in) :: nod_check
!
      if(my_rank .gt. 0) return
      if(nod_check%istack_diff_pe(nprocs) .eq. 0) then
        write(*,*) 'No wrong communication for nodes'
        return
      end if
!
      open(id_comm_test, file = file_name)
!
      write(id_comm_test,*) 'ntot_nod_diff_pe',                         &
     &                     nod_check%istack_diff_pe(nprocs)
      write(id_comm_test,*) 'domain, local_nod_id, ',                   &
     &      'global_nod_org, global_nod_get, ',                         &
     &      'xx_org, yy_org, zz_org, xx_get, yy_get, zz_get'
!
      call write_diff_comm_test(nod_check%istack_diff_pe,               &
     &    nod_check%i_diff_IO, nod_check%x_diff_IO)
!
      close(id_comm_test)
!
      end subroutine output_diff_node_comm_test
!
!  ---------------------------------------------------------------------
!
      subroutine output_diff_mesh_comm_test(file_name,                  &
     &          nod_check, ele_check, surf_check, edge_check)
!
      character(len=kchara), intent(in) :: file_name
      type(work_for_comm_check), intent(in) :: nod_check, ele_check
      type(work_for_comm_check), intent(in) :: surf_check, edge_check
!
      integer(kind = kint) :: ntot_error
!
      if(my_rank .gt. 0) return
      ntot_error = nod_check%istack_diff_pe(nprocs)                     &
     &            + ele_check%istack_diff_pe(nprocs)                    &
     &            + surf_check%istack_diff_pe(nprocs)                   &
     &            + edge_check%istack_diff_pe(nprocs)
      if(ntot_error .eq. 0) then
        write(*,*) 'No wrong communication for mesh'
        return
      end if
!
      open(id_comm_test, file = file_name)
!
      write(id_comm_test,*) 'ntot_nod_diff_pe ',                        &
     &                     nod_check%istack_diff_pe(nprocs)
      write(id_comm_test,*) 'ntot_ele_diff_pe ',                        &
     &                     ele_check%istack_diff_pe(nprocs) 
      write(id_comm_test,*) 'ntot_surf_diff_pe',                        &
     &                     surf_check%istack_diff_pe(nprocs)
      write(id_comm_test,*) 'ntot_edge_diff_pe',                        &
     &                     edge_check%istack_diff_pe(nprocs)
      write(*,*) 'ntot_nod_diff_pe ', nod_check%istack_diff_pe(nprocs)
      write(*,*) 'ntot_ele_diff_pe ', ele_check%istack_diff_pe(nprocs) 
      write(*,*) 'ntot_surf_diff_pe', surf_check%istack_diff_pe(nprocs)
      write(*,*) 'ntot_edge_diff_pe', edge_check%istack_diff_pe(nprocs)
!
!
      write(id_comm_test,*) 'domain, local_nod_id, ',                   &
     &      'global_nod_org, global_nod_get, ',                         &
     &      'xx_org, yy_org, zz_org, xx_get, yy_get, zz_get'
      call write_diff_comm_test(nod_check%istack_diff_pe,               &
     &    nod_check%i_diff_IO, nod_check%x_diff_IO)
!
      write(id_comm_test,*) 'domain, local_ele_id, ',                   &
     &      'global_ele_org, global_ele_get, ',                         &
     &      'xx_org, yy_org, zz_org, xx_get, yy_get, zz_get'
      call write_diff_comm_test(ele_check%istack_diff_pe,               &
     &    ele_check%i_diff_IO, ele_check%x_diff_IO)
!
      write(id_comm_test,*) 'domain, local_surf_id, ',                  &
     &      'global_surf_org, global_surf_get, ',                       &
     &      'xx_org, yy_org, zz_org, xx_get, yy_get, zz_get'
      call write_diff_comm_test(surf_check%istack_diff_pe,              &
     &    surf_check%i_diff_IO, surf_check%x_diff_IO)
!
      write(id_comm_test,*) 'domain, local_edge_id, ',                  &
     &      'global_edge_org, global_edge_get, ',                       &
     &      'xx_org, yy_org, zz_org, xx_get, yy_get, zz_get'
      call write_diff_comm_test(edge_check%istack_diff_pe,              &
     &    edge_check%i_diff_IO, edge_check%x_diff_IO)
!
      close(id_comm_test)
!
      end subroutine output_diff_mesh_comm_test
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_diff_comm_test                                   &
     &         (istack_diff_pe, id_diff_IO, x_diff_IO)
!
      integer(kind = kint), intent(in) :: istack_diff_pe(0:nprocs)
      integer(kind = kint), intent(in)                                  &
     &                     :: id_diff_IO(istack_diff_pe(nprocs))
      real(kind = kreal), intent(in)                                    &
     &                     :: x_diff_IO(6*istack_diff_pe(nprocs))
!
      integer(kind = kint) :: ip, id_rank, ist, ied, inum
      integer(kind = kint) :: j1, j2, k1, k2
!
!
      do ip = 1, nprocs
        id_rank = ip - 1
        ist = istack_diff_pe(ip-1) + 1
        ied = istack_diff_pe(ip)
        do inum = ist, ied
          j1 = 2*inum-1
          j2 = 2*inum
          k1 = 6*inum-5
          k2 = 6*inum
          write(id_comm_test,1000) id_rank, id_diff_IO(inum),           &
     &           x_diff_IO(k1:k2)
        end do
      end do
!
 1000 format(2i16, 1p6e20.12)
!
      end subroutine write_diff_comm_test
!
!  ---------------------------------------------------------------------
!
      end module write_diff_4_comm_test
