!
!     module m_cal_max_indices
!.......................................................................
!
!       subroutine allocate_phys_range
!       subroutine deallocate_phys_range
!
      module m_cal_max_indices
!
      use m_precision
!
      implicit  none
!
      integer(kind=kint), dimension(:), allocatable :: node_min
!
      integer(kind=kint), dimension(:), allocatable :: node_max
!
      real(kind=kreal), dimension(:), allocatable :: phys_min
!
      real(kind=kreal), dimension(:), allocatable :: phys_max
!

      integer(kind=kint), dimension(:), allocatable :: node_min_local
!
      integer(kind=kint), dimension(:), allocatable :: node_max_local
!
      real(kind=kreal), dimension(:), allocatable :: phys_min_local
!
      real(kind=kreal), dimension(:), allocatable :: phys_max_local
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine allocate_phys_range
!
       use m_node_phys_data
!
       allocate (node_min(num_tot_nod_phys_vis))
       allocate (node_max(num_tot_nod_phys_vis))
       allocate (phys_min(num_tot_nod_phys_vis))
       allocate (phys_max(num_tot_nod_phys_vis))
!
       allocate (node_min_local(num_tot_nod_phys_vis))
       allocate (node_max_local(num_tot_nod_phys_vis))
       allocate (phys_min_local(num_tot_nod_phys_vis))
       allocate (phys_max_local(num_tot_nod_phys_vis))
!
       node_min = 0
       node_max = 0
       phys_min = 0.0d0
       phys_max = 0.0d0
!
       node_min_local = 0
       node_max_local = 0
       phys_min_local = 1.0d15
       phys_max_local =-1.0d15
!
       end subroutine allocate_phys_range
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_phys_range
!
       deallocate (node_min, node_min_local)
       deallocate (node_max, node_max_local)
       deallocate (phys_min, phys_min_local)
       deallocate (phys_max, phys_max_local)
!
       end subroutine deallocate_phys_range
!
! ----------------------------------------------------------------------
!
      end module m_cal_max_indices
