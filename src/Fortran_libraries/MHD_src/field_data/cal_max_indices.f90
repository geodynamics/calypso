!
!      module cal_max_indices
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2007
!
!      subroutine s_cal_max_indices
!
      module cal_max_indices
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_max_indices
!
      use m_parallel_var_dof
      use m_cal_max_indices
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
!
!
      integer (kind = kint) :: nd, inod
!
!
       do nd = 1, num_tot_nod_phys_vis
!
        node_max(nd) = 1
        do inod=1, numnod 
         if ( d_nod(inod,nd) > d_nod(node_max(nd),nd) ) then
           node_max(nd) = inod
         end if
        end do
!
        node_min(nd) = 1
        do inod=1, numnod 
         if (d_nod(inod,nd) < d_nod(node_min(nd),nd)) then
          node_min(nd) = inod
         end if
        end do
!
       end do
!
       do nd = 1, num_tot_nod_phys_vis
        phys_max_local(nd) = d_nod(node_max(nd),nd)
        phys_min_local(nd) = d_nod(node_min(nd),nd)
       end do
!
        call MPI_allREDUCE (phys_max_local(1), phys_max(1),             &
     &       num_tot_nod_phys_vis, MPI_DOUBLE_PRECISION, MPI_MAX,       &
     &       SOLVER_COMM, ierr)
!
        call MPI_allREDUCE (phys_min_local(1), phys_min(1),             &
     &        num_tot_nod_phys_vis, MPI_DOUBLE_PRECISION, MPI_MIN,      &
     &        SOLVER_COMM, ierr)
!
        node_max_local = 0
        node_min_local = 0
!
       do nd = 1, num_tot_nod_phys_vis
        if ( phys_max(nd) .eq. phys_max_local(nd) ) then
         inod = node_max(nd)
         node_max_local(nd) = globalnodid(inod)
        end if
        if ( phys_min(nd) .eq. phys_min_local(nd) ) then
         inod = node_min(nd)
         node_min_local(nd) = globalnodid(inod)
        end if
       end do
!
        call MPI_allREDUCE (node_max_local(1), node_max(1),             &
     &        num_tot_nod_phys_vis, MPI_INTEGER, MPI_SUM,               &
     &        SOLVER_COMM, ierr)
!
        call MPI_allREDUCE (node_min_local(1), node_min(1),             &
     &        num_tot_nod_phys_vis, MPI_INTEGER, MPI_SUM,               &
     &        SOLVER_COMM, ierr)
!
!
      end subroutine s_cal_max_indices
!
!  ---------------------------------------------------------------------
!
      end module cal_max_indices
