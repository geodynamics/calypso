!m_surface_group.f90
!     module m_surface_group
!
!> @brief surface group data
!
!      written by H. Matsui
!
!      subroutine allocate_surface_data
!      subroutine clear_surface_data
!      subroutine deallocate_surface_data
!
!      subroutine allocate_surface_param_smp
!      subroutine deallocate_surface_param_smp
!
!      subroutine check_surf_4_sheard_para(my_rank)
!      subroutine check_surf_nod_4_sheard_para(my_rank)
!
      module m_surface_group
!
      use m_precision
!
      implicit  none
!
      integer(kind=kint) :: num_surf
!<      number of surface group
      integer(kind=kint) :: num_surf_bc
!<      total number of surface for surface group
!
      integer(kind=kint),   allocatable, target :: surf_istack(:)
!<      end address of each surface group
      integer(kind=kint),    allocatable, target :: surf_item(:,:)
!<      local surface ID for surface group
!<      surf_item(1,:):  local element ID
!<      surf_item(2,:):  surface ID for each element
!
      character(len=kchara), allocatable, target :: surf_name(:)
!<      surface group name
!
      integer( kind=kint )  ::  num_surf_smp
!<      number of surface group for SMP process
      integer( kind=kint ), allocatable :: isurf_grp_smp_stack(:)
!<      end address of each surface group for SMP process
      integer( kind=kint )  ::  max_sf_grp_4_smp
!<      maximum number of surface group for SMP process
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surface_data
!
       allocate(surf_istack(0:num_surf))
       allocate(surf_name(num_surf))
       allocate(surf_item(2,num_surf_bc))
!
      call clear_surface_data
!
      end subroutine allocate_surface_data
!
!-----------------------------------------------------------------------
!
      subroutine clear_surface_data
!
       surf_istack=0
       surf_item=0
!
      end subroutine clear_surface_data
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surface_data
!
       deallocate(surf_istack)
       deallocate(surf_name)
       deallocate(surf_item)
!
      end subroutine deallocate_surface_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine allocate_surface_param_smp
!
       allocate( isurf_grp_smp_stack(0:num_surf_smp))
       isurf_grp_smp_stack = 0
!
       end subroutine allocate_surface_param_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_surface_param_smp
!
!
       deallocate(isurf_grp_smp_stack)
!
       end subroutine deallocate_surface_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_surf_4_sheard_para(my_rank)
!
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank, 'num_surf ', num_surf
       write(*,*) 'PE: ', my_rank, 'num_surf_smp ', num_surf_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &            'isurf_grp_smp_stack ', isurf_grp_smp_stack
!
      end subroutine check_surf_4_sheard_para
!
!-----------------------------------------------------------------------
!
      end module m_surface_group
