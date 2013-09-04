!m_element_group.f90
!     module m_element_group
!
!> @brief element group data
!
!     Written by H. Matsui
!
!      subroutine allocate_material_data
!      subroutine deallocate_material_data
!
!      subroutine clear_material_data
!
!       subroutine allocate_material_param_smp
!       subroutine deallocate_material_param_smp
!
!      subroutine check_mat_4_sheard_para(my_rank)
!
      module m_element_group
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint) :: num_mat
!<      number of element group
      integer (kind=kint) :: num_mat_bc
!<      total number of nodes for element group
! 
      integer (kind=kint), allocatable, target :: mat_istack(:)
!<      end address of each element group
      integer (kind=kint), allocatable, target :: mat_item(:)
!<      local element ID for element group
! 
      character (len=kchara), allocatable, target :: mat_name(:)
!<      element group name
!
      integer( kind=kint )  ::  num_mat_smp
!<      number of element group for SMP process
      integer( kind=kint ), allocatable :: imat_smp_stack(:)
!<      end address of each element group for SMP process
!
      integer( kind=kint )  ::  max_mat_4_smp
!<      maximum number of element group for SMP process
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_material_data
!
       allocate(mat_istack(0:num_mat))
       allocate(mat_name(num_mat))
       allocate(mat_item(num_mat_bc))
!
      call clear_material_data
!
      end subroutine allocate_material_data
!
! ----------------------------------------------------------------------
!
      subroutine clear_material_data
!
      mat_istack = 0
      if(num_mat_bc .gt. 0) mat_item = 0
!
      end subroutine clear_material_data
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_material_data
!
       deallocate(mat_istack)
       deallocate(mat_name)
       deallocate(mat_item)
!
      end subroutine deallocate_material_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine allocate_material_param_smp
!
       allocate( imat_smp_stack(0:num_mat_smp))
       imat_smp_stack = 0
!
       end subroutine allocate_material_param_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_material_param_smp
!
       deallocate(imat_smp_stack)
!
       end subroutine deallocate_material_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_mat_4_sheard_para(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank, 'num_mat ', num_mat
       write(*,*) 'PE: ', my_rank, 'num_mat_smp ', num_mat_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &            'imat_smp_stack ', imat_smp_stack
!
      end subroutine check_mat_4_sheard_para
!
!-----------------------------------------------------------------------
!
      end module m_element_group
