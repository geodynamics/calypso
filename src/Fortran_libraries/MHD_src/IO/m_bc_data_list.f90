!>@file   m_bc_data_list.f90
!!@brief  module m_bc_data_list
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>@brief  Boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!      subroutine allocate_nod_bc_list_temp
!!      subroutine allocate_nod_bc_list_velo
!!      subroutine allocate_nod_bc_list_press
!!      subroutine allocate_nod_bc_list_vecp
!!      subroutine allocate_nod_bc_list_magne
!!      subroutine allocate_nod_bc_list_mag_p
!!      subroutine allocate_nod_bc_list_j
!!      subroutine allocate_nod_bc_list_composit
!!
!!      subroutine deallocate_nod_bc_list_temp
!!      subroutine deallocate_nod_bc_list_velo
!!      subroutine deallocate_nod_bc_list_press
!!      subroutine deallocate_nod_bc_list_vecp
!!      subroutine deallocate_nod_bc_list_magne
!!      subroutine deallocate_nod_bc_list_mag_p
!!      subroutine deallocate_nod_bc_list_j
!!      subroutine deallocate_nod_bc_list_composit
!!@endverbatim
!
      module m_bc_data_list
!
      use m_precision
!
      implicit  none
!
!
!>       Structure for surface group data list
      type nod_bc_list_type
!>       number of boundary condition list
        integer (kind=kint) :: num_bc
!>       Value for the boundary condition
        real (kind=kreal), pointer :: bc_magnitude(:)
!>       Type of the boundary condition
        integer (kind=kint), pointer :: ibc_type(:)
!>       Name of group to apply the boundary condition
        character (len=kchara), pointer :: bc_name(:)
      end type nod_bc_list_type
!
!>       Node group data list for velocity
      type(nod_bc_list_type), save :: velo_nod
!>       Node group data list for pressure
      type(nod_bc_list_type), save :: press_nod
!
!>       Node group data list for temperarure
      type(nod_bc_list_type), save :: temp_nod
!>       Node group data list for composition
      type(nod_bc_list_type), save :: light_nod
!
!>       Node group data list for magnetic field
      type(nod_bc_list_type), save :: magne_nod
!>       Node group data list for magnetic vector potential
      type(nod_bc_list_type), save :: a_potential_nod
!>       Node group data list for electric scalar potential
      type(nod_bc_list_type), save :: e_potential_nod
!>       Node group data list for current density
      type(nod_bc_list_type), save :: current_nod
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
      subroutine alloc_bc_type_ctl(nod_bc_list)
!
      type(nod_bc_list_type), intent(inout) :: nod_bc_list
!
!
      allocate(nod_bc_list%bc_magnitude(nod_bc_list%num_bc))
      allocate(nod_bc_list%ibc_type(nod_bc_list%num_bc))
      allocate(nod_bc_list%bc_name(nod_bc_list%num_bc))
!
      if(nod_bc_list%num_bc .gt. 0) then
        nod_bc_list%ibc_type =     0
        nod_bc_list%bc_magnitude = 0.0d0
      end if
!
      end subroutine alloc_bc_type_ctl
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_bc_type_ctl(nod_bc_list)
!
      type(nod_bc_list_type), intent(inout) :: nod_bc_list
!
!
      deallocate(nod_bc_list%bc_magnitude)
      deallocate(nod_bc_list%ibc_type)
      deallocate(nod_bc_list%bc_name)
!
      end subroutine dealloc_bc_type_ctl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_temp
!
!
      call alloc_bc_type_ctl(temp_nod)
!
      end subroutine allocate_nod_bc_list_temp
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_velo
!
!
      call alloc_bc_type_ctl(velo_nod)
!
      end subroutine allocate_nod_bc_list_velo
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_press
!
!
      call alloc_bc_type_ctl(press_nod)
!
      end subroutine allocate_nod_bc_list_press
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_vecp
!
!
      call alloc_bc_type_ctl(a_potential_nod)
!
      end subroutine allocate_nod_bc_list_vecp
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_magne
!
!
      call alloc_bc_type_ctl(magne_nod)
!
      end subroutine allocate_nod_bc_list_magne
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_j
!
!
      call alloc_bc_type_ctl(current_nod)
!
      end subroutine allocate_nod_bc_list_j
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_mag_p
!
!
      call alloc_bc_type_ctl(e_potential_nod)
!
      end subroutine allocate_nod_bc_list_mag_p
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_composit
!
!
      call alloc_bc_type_ctl(light_nod)
!
      end subroutine allocate_nod_bc_list_composit
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_temp
!
!
      call dealloc_bc_type_ctl(temp_nod)
!
      end subroutine deallocate_nod_bc_list_temp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_velo
!
!
      call dealloc_bc_type_ctl(velo_nod)
!
      end subroutine deallocate_nod_bc_list_velo
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_press
!
!
      call dealloc_bc_type_ctl(press_nod)
!
      end subroutine deallocate_nod_bc_list_press
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_vecp
!
!
      call dealloc_bc_type_ctl(a_potential_nod)
!
      end subroutine deallocate_nod_bc_list_vecp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_magne
!
!
      call dealloc_bc_type_ctl(magne_nod)
!
      end subroutine deallocate_nod_bc_list_magne
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_mag_p
!
!
      call dealloc_bc_type_ctl(e_potential_nod)
!
      end subroutine deallocate_nod_bc_list_mag_p
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_j
!
!
      call dealloc_bc_type_ctl(current_nod)
!
      end subroutine deallocate_nod_bc_list_j
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_composit
!
!
      call dealloc_bc_type_ctl(light_nod)
!
      end subroutine deallocate_nod_bc_list_composit
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_list
